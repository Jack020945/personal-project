
	subroutine motor_initialize(time, xdim, x, xd, xo, udim, uc,
     .       aircraft, wing, propeller, battery, control)

c	James D. Walker, Southwest Research Institute  
c
c	4/7/21  This is to intialize the motor omega, or at least 
c	determine what it is expected to be.  It is a tricky step
c	and requires lots of information.
c       1/18/22 Made change so that amps cannot be less than I0.



	use iso_c_binding
	implicit none
	include 'parts.h'

	integer xdim,udim

	double precision time, x(xdim), xd(xdim), xo(1500), uc(udim)

	double precision motor_omega(50), motor_omegadot(50)


	double precision Vt, U, V, W
	double precision Vi, Viw
	double precision x_w, y_w, z_w, U_w, v_w, w_w
	logical          iflinear
	integer          i, k, ierror
	double precision J, n, D, Vpath, Thrust, Power,  Torque, Cp, Ct
	double precision temp, tau, current, voltage
	double precision Udot, Vdot, Wdot
	double precision ox, oy, oz, denom, grav
	double precision Unorth, Veast, Wdown
	double precision Xnorth, Yeast, Zdown
	double precision q0   , q1   , q2   , q3
	double precision Unwind, Vewind, Wdwind
	double precision Uwind, Vwind, Wwind, Up, Vp, Wp
	integer          icontrol, icontrol1, icontrol2
	double precision dxkm1, dxk, dylm1, dyl
	double precision Thrust_x, Thrust_y, Thrust_z
	double precision a, b, c, Kt, Kv, Rw, omega, amps, Power_tot
	integer          jdw, jdw2, l, kpast, lpast
	double precision omega0, omega1, ff, ffp, a1, b1, omega2, DeltaJ
	double precision rho
	double precision J1, J2, Om1, Om2, Cp11, Cp21, Cp22, Cp12
	double precision aa, bb, cc, dd, ee,   a0,   a2,   a3,  ff1, Cp2
	double precision I0, motor_efficiency

cc	write (6,*) ' Time = ',time


c	Read in the current states

	U      = x(1)
	V      = x(2)
	W      = x(3)

	q0     = x(7)
	q1     = x(8)
	q2     = x(9)
	q3     = x(10)

	do i= 1, aircraft%num_propellers
           motor_omega(i) = x(13 + i)
	enddo

c	Set atmospheric conditions.  Right now these are constants.
c	In the future they could be functions of altitude (rho) and
c	time (wind speed, gusts, etc.)
	xo(1) = aircraft%Unwind
	xo(2) = aircraft%Vewind
	xo(3) = aircraft%Wdwind
	rho   = aircraft%rho

	Unwind = xo(1)  ! U north world speed of wind
	Vewind = xo(2)  ! V east world speed of wind
	Wdwind = xo(3)  ! W down world speed of wind

c	Some pure geometry (no physics); Direction cosine matrix (dcm) from the quaternions.
	xo(11) = q0**2 + q1**2 - q2**2 - q3**2   ! xx
	xo(12) = 2.d0*(q1*q2 + q0*q3)            ! xy
	xo(13) = 2.d0*(q1*q3 - q0*q2)            ! xz
	xo(14) = 2.d0*(q1*q2 - q0*q3)            ! yx
	xo(15) = q0**2 - q1**2 + q2**2 - q3**2   ! yy
	xo(16) = 2.d0*(q2*q3 + q0*q1)            ! yz
	xo(17) = 2.d0*(q1*q3 + q0*q2)            ! zx
	xo(18) = 2.d0*(q2*q3 - q0*q1)            ! zy
	xo(19) = q0**2 - q1**2 - q2**2 + q3**2   ! zz

c	Now we get the speed of the wind in the body fixed frame
	Uwind = xo(11)*Unwind + xo(12)*Vewind + xo(13)*Wdwind
	Vwind = xo(14)*Unwind + xo(15)*Vewind + xo(16)*Wdwind
	Wwind = xo(17)*Unwind + xo(18)*Vewind + xo(19)*Wdwind

c	The air speed is given by U prime - sometimes we need it, sometimes
c	we need the ground speed which is just regular U.
	Up = U - Uwind
	Vp = V - Vwind
	Wp = W - Wwind

c	Right now the induced velocity is not being determined (from the propeller, in
c	case it affects other propellers).  If we include that it would be as
c	an adjustment to Up, Vp, and Wp, the velocity of the propeller with 
c	respect to the air.  Since it depends on other propellers, it would
c	presumably appear here.

c	Now we do the loop over the propellers, computing thrust force and 
c	related moments, including gyroscopic moments.
c	It is assumed each propeller has its own electric motor and that it
c	is direct drive (no gearing).

	Power_tot = 0.d0

	do i = 1, aircraft%num_propellers

c	   We go to the table to get the information of interest.
c	   First we need the velocity in the direction of travel for the
c	   propeller so that we can then compute the advanced ratio.

	   D = 2.d0*propeller(i)%radius
	   n = motor_omega(i)/twopi  ! to get reveolutions per second from radians per second
c	   Right now the induced velocity is not being determined (from the propeller, in
c	   case it affects other propellers).  If we include that it would be as
c	   an adjustment to Up, Vp, and Wp, the velocity of the propeller with 
c	   respect to the air.  Since it depends on other propellers, it would
c	   presumably appear here.  - so form of Vi or Viw.
	   Vpath = Up*propeller(i)%nx + Vp*propeller(i)%ny
     .           + Wp*propeller(i)%nz

	   if (n.gt.0.d0) then  ! the propeller is spinning

c	      J could conceivably be negative, which is not in the tables,
c	      so in that case we set it to zero.  (This could be due to me not
c	      computing an induced velocity.)
	      if (Vpath.lt.0.d0) Vpath = 0.d0

	      J = Vpath / (n*D) ! J = V/nD

cold	      call table (propeller(i)%prop_num_pts, J, propeller(i)%J
cold     .                , dxkm1, dxk, k, ierror)
cold	      Ct = dxkm1*propeller(i)%Ct(k-1) + dxk*propeller(i)%Ct(k)
cold	      Cp = dxkm1*propeller(i)%Cp(k-1) + dxk*propeller(i)%Cp(k)

	      call table (propeller(i)%prop_num_pts_J, J, propeller(i)%J
     .                , dxkm1, dxk, k, kpast, ierror)
	      call table (propeller(i)%prop_num_pts_omega,motor_omega(i)
     .                , propeller(i)%omega
     .                , dylm1, dyl, l, lpast, ierror)
c	      I believe the first index is for J, the second index is for omega
	      Cp11 = propeller(i)%Ct(k-1,l-1)
	      Cp21 = propeller(i)%Ct(k-1,l  )
	      Cp22 = propeller(i)%Ct(k  ,l  )
	      Cp12 = propeller(i)%Ct(k  ,l-1)
	      Ct   = Cp11*dxkm1*dylm1 + Cp21*dxkm1*dyl
     .             + Cp22*dxk  *dyl   + Cp12*dxk  *dylm1
	      Cp11 = propeller(i)%Cp(k-1,l-1)
	      Cp21 = propeller(i)%Cp(k-1,l  )
	      Cp22 = propeller(i)%Cp(k  ,l  )
	      Cp12 = propeller(i)%Cp(k  ,l-1)
	      Cp   = Cp11*dxkm1*dylm1 + Cp21*dxkm1*dyl
     .             + Cp22*dxk  *dyl   + Cp12*dxk  *dylm1

	      Power  = Cp*rho*n**3*D**5      ! P = Cp*rho*n^3*D^5
              Torque = Power/motor_omega(i)  ! Q = P/motor omega

              if (Power.lt.0.d0) then   ! 1/18/22
                 Power  = 0.d0
                 Torque = 0.d0
              endif

	   else

cold	      Ct = propeller(i)%Ct(1)
cold	      Cp = propeller(i)%Cp(1)

	      Ct = propeller(i)%Ct(1,1)
	      Cp = propeller(i)%Cp(1,1)

	      Thrust = 0.d0
	      Power  = 0.d0
              Torque = 0.d0

	   endif

c	   We are assuming the this is a speed controlled response, so that the speed
c	   is directly proportional to the voltage.

	   Kt  = propeller(i)%Kt
	   Kv  = twopi*propeller(i)%Kv/60.d0  ! unit change from rpm/volt to rad per sec/volt
	   Rw  = propeller(i)%Rw
	   I0  = propeller(i)%I_idle
	   voltage = propeller(i)%ac*uc(propeller(i)%icontrol)
     .             + propeller(i)%bc ! voltage signal from the controls.

c	   If the control request is u = 0, then the motor is not spinning.
	   if (uc(propeller(i)%icontrol).le.0.d0) then
              omega = 0.d0
	      n     = 0.d0
	      goto 200
	   endif

	   a   =   (Rw/Kt)*(Cp*rho*D**5/twopi**3)
	   b   =   1.d0/Kv
cold	   c   = - voltage
	   c   = - voltage + Rw*I0

	   omega = (-b+sqrt(b**2-4.d0*a*c))/(2.d0*a)  ! this is the requested rotation rate

cold	   do jdw = 1, 5  ! takes 5 iterates to converge, based on examples
cold
cold	      n = omega/twopi  ! to get reveolutions per second from radians per second
cold
cold	      J = Vpath / (n*D) ! J = V/nD
cold
cold	      call table (propeller(i)%prop_num_pts, J, propeller(i)%J
cold     .                , dxkm1, dxk, k, ierror)
cold
coldc	      Same as above, only this a is a/Cp above
cold	      a   =   (Rw/Kt)*(rho*D**5/twopi**3)
cold	      b   =   1.d0/Kv
cold	      c   = - voltage
cold	      deltaJ = propeller(i)%J(k)-propeller(i)%J(k-1)
cold
cold	      a1  = a*(propeller(i)%Cp(k-1)*propeller(i)%J(k  )
cold     .             -propeller(i)%Cp(k  )*propeller(i)%J(k-1))/deltaJ
cold	      b1  = b + twopi*Vpath*a*
cold     .            (propeller(i)%Cp(k  )-propeller(i)%Cp(k-1))/(deltaJ*D)
cold
cold	      ff = a1*omega**2+b1*omega+c
cold	      ffp = 2.d0*a1*omega + b1
cold	      omega = omega - ff/ffp
cold
coldcc	      write (6,*) ' Omega ',i,jdw,omega
cold
cold	   enddo


c	   Now we use a Newton method
c  	   write (6,*) ' initial omega,cp ',omega,cp

	   do jdw = 1, 5  ! takes 5 iterates to converge, based on examples

	      n = omega/twopi  ! to get reveolutions per second from radians per second

	      J = Vpath / (n*D) ! J = V/nD

	      call table (propeller(i)%prop_num_pts_J, J, propeller(i)%J
     .                , dxkm1, dxk, k, kpast, ierror)
	      call table (propeller(i)%prop_num_pts_omega, omega
     .                , propeller(i)%omega
     .                , dylm1, dyl, l, lpast, ierror)
c	      I believe the first index is for J, the second index is for omega
	      J1   = propeller(i)%J    (k-1)
	      J2   = propeller(i)%J    (k  )
	      Om1  = propeller(i)%omega(l-1)
	      Om2  = propeller(i)%omega(l  )
	      Cp11 = propeller(i)%Cp(k-1,l-1)
	      Cp21 = propeller(i)%Cp(k-1,l  )
	      Cp22 = propeller(i)%Cp(k  ,l  )
	      Cp12 = propeller(i)%Cp(k  ,l-1)
	      aa = Cp11        - Cp21        + Cp22        - Cp12
	      bb =-Cp11   *J2  + Cp21   *J2  - Cp22   *J1  + Cp12    *J1
	      cc =-Cp11*Om2    + Cp21*Om1    - Cp22*Om1    + Cp12*Om2
	      dd = Cp11*Om2*J2 - Cp21*Om1*J2 + Cp22*Om1*J1 - Cp12*Om2*J1

c	      Same as above, only this a is a/Cp above
	      a   =   (Rw/Kt)*(rho*D**5/twopi**3) 
	      b   =   1.d0/Kv
	      c   = - voltage + Rw*I0

	      ee = a/((J2 - J1)*(Om2 - Om1))

	      a3 = ee*bb
	      a2 = ee*(aa*twopi*Vpath/D+dd)
	      a1 = ee* cc*twopi*Vpath/D + b
	      a0 = c

c	      Cp = ((omega-Om2)*(J-J2)*Cp11-(omega-Om1)*(J-J2)*Cp21
c     .             +(omega-Om1)*(J-J1)*Cp22-(omega-Om2)*(J-J1)*Cp12)
c     .             /((J2 - J1)*(Om2 - Om1))
c	      It seems ff1 and ff should be exactly the same.  Unclear why there
c	      are occassional differences.  Still puzzled why it is converging so fast.
c	      Cp seems to exactly match.
c	      ff1 = a*Cp*omega**2+b*omega+c
c	      Cp2 = (aa*omega*J+bb*omega+cc*J+dd)/((J2-J1)*(Om2-Om1))
c	      write (6,*) ' 0.,a,b,c    ',0.d0,a*Cp,b,c
c	      write (6,*) ' a3,a2,a1,a0 ',a3,a2,a1,a0,a3*omega+a2

	      ff    =     a3*omega**3 +      a2*omega**2 + a1*omega + a0
	      ffp   =3.d0*a3*omega**2 + 2.d0*a2*omega    + a1
	      omega = omega - ff/ffp

c             write (6,*) ' jdw,omega,Cp,Cp2,ff,ff1 ',jdw,omega,Cp,Cp2,ff,ff1
c	      write (6,*) ' Om1,Om2, J1, J2 ',Om1,Om2,J1,J2
c	      write (6,*) ' Cp11, Cp21, Cp22, Cp12 ',Cp11,Cp21,Cp22,Cp12
c
	   enddo

	   n = omega/twopi 
	   J = Vpath / (n*D) ! J = V/nD
	   Cp = ((omega-Om2)*(J-J2)*Cp11-(omega-Om1)*(J-J2)*Cp21
     .          +(omega-Om1)*(J-J1)*Cp22-(omega-Om2)*(J-J1)*Cp12)
     .          /((J2 - J1)*(Om2 - Om1))

 200	   continue ! come to here when u = 0.

           x(13 + i) = omega

	   Power  = Cp*rho*n**3*D**5   ! P = Cp*rho*n^3*D^5
	   if (Power.lt.0.d0) Power = 0.d0  ! 1/18/22

cold	   amps = (Power/(propeller(i)%efficiency*voltage)) 
cold     .          + propeller(i)%I_idle  ! to change to watts
	   amps = (voltage - omega/Kv)/Rw 
	   amps = max(amps,I0)  !  1/18/22

c	   The efficiency is for the speed controller.
c	   We add up estimated power usage for analysis purposes
	   xo (29 + i) = voltage*amps/propeller(i)%efficiency_ESC
	   Power_tot   = Power_tot + xo(29 + i)

c	   Motor efficiency
	   if (voltage*amps.eq.0.d0) then
	      motor_efficiency = 0.d0
	   else
	      motor_efficiency = Power/(voltage*amps)
	   endif

c	   write (6,*) ' motor efficiency ',i,motor_efficiency
c     .                ,power,voltage,amps
c	   write (6,*) Cp,rho,n,D   ! P = Cp*rho*n^3*D^5
c	   write (6,*) Cp11,Cp21,Cp22,Cp12
c	   write (6,*) omega,Om1,Om2,J,J1,J2

c	   Our first order ODE to head towards the the requested omega
c	   motor_omegadot(i) = (omega - motor_omega(i))
c     .                    /propeller(i)%tau_ESC

	enddo ! loop on i over the propellers

	xo(29) = Power_tot

	return
	end

