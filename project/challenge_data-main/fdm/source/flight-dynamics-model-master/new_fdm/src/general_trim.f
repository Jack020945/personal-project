c	This version the trim uses the Levenberg-Marquardt method from
c	Minpack, by Argonne Naitinoal Laboratory (1980).


	subroutine general_trim(xdim,x,xd,udim,uc,aircraft,wing,propeller
     .                ,battery,control,UVWPQR, KK, imode, radius, irad)
c	This subroutine determines trim states for anything using the
c	flight dynamics model.  
c	James Walker  3 April 2021
c	4/3/21
c	7/21/21  This is the general trim state, where by that we
c	         are matching accelerations.  Based on subroutine trim.
c	         It is built from minpack_trim.f
c
c	Input: general list of inputs, but in particular  
c		    Unorth = UVWPQR(1) is the world frame velocity (m/s)
c	            Veast  = UVWPQR(2)
c	            Wdown  = UVWPQR(3)
c	            Pworld = UVWPQR(4) is the world rotation (rad/s)
c	            Qworld = UVWPQR(5)
c	            Rworld = UVWPQR(6)
c	            The former allows climb states, etc.
c	            The latter allows turns.
c
c                   I comment that for my intended use conceptually Veast 
c                   should be zero, since the perspective is that we are computing
c		    one trim state to use often.  It could be nonzero if the
c		    thinking is that many trim states will be computed - essentially
c	 	    following the path of the vehicle and computing them again and
c		    again.
c
c	            If we are in a fixed radius turn in the plane, then R = speed/radius,
c	            with the sign deterined by whether we are going north to east
c		    (positive) or north to west (negative).
c
c	            imode  This runs this routine in a different mode - namemly many
c			   runs to store trim states (and to know what they are)
c			   = 1  to run just one case, as described above
c	                   = 2  to run multiple cases, where
c		    Unorth = UVWPQR(1) is given the world frame velocity (m/s)
c	            Veast  = 0.d0
c	            Wdown  = 0.d0
c	            Pworld = 0.d0 is the world rotation (rad/s)
c	            Qworld = 0.d0
c	            Rworld = UVWPQR(1)/radius
c
c	            radius = positive if turning to right, negative if turning to left
c
c		    irad = number of turning radius for storage information, 1 or 2
c			       
c	
c	Output: x  the trim state, including omega
c	        uc the trim state controls
c	        KK the trim state control law
c
	use iso_c_binding
	implicit none
	include 'parts.h'

	integer xdim, udim

	integer  i, j, k, np
	double precision x (xdim), xd(xdim), xo(150), uc(udim)

	double precision error

	double precision xd3previous, uc1previous, dxd3

	double precision time, dt, dt_output, time_end

	double precision theta, Unorth, za(75), ya(75), tolerance, phi
	double precision Veast, Wdown, Pworld, Qworld, Rworld
	double precision UVWPQR(6), radius
	double precision time_of_bat, amps, temp
	integer imode, irad, jend
	logical iwarn, ifirst
	integer itot
	parameter (itot = 50)
	logical sum_iwarn(1:itot), lsolved
	double precision sum_speed (1:itot), sum_pitch(1:itot)
	double precision sum_thrust(1:itot), sum_lift (1:itot)
        double precision sum_drag  (1:itot), sum_amps (1:itot)
	double precision sum_power (1:itot), sum_dist (1:itot)
	double precision sum_ftime (1:itot), ratio
	double precision sum_roll  (1:itot)
	double precision sum_motorampmax(1:itot),sum_motorampmin(1:itot)
	double precision sum_motorpowmax(1:itot),sum_motorpowmin(1:itot)
	double precision sum_batampmax  (1:itot),sum_uc         (1:itot)
	character*7 sum_csolve(1:itot)
	character*3 sum_ARE(1:itot)
	double precision distancemax0

	integer iMINPACK, iSIMPLEX

c	Minpack variables
        integer m,n,info,lwa
	parameter (lwa = 2500)
        integer iwa(75)  ! equals n at least
        double precision tol
        double precision wa(lwa)  ! see min value below
        external fdcostgen

c	fdcost variables
	integer iflag

c	Variables for the simplex algorithm.
	integer   pdim 
	parameter (pdim = 50)
        double precision pam(pdim,pdim), yam(pdim)
        integer ier
        integer iter
        double precision f, diff
        integer   ibigzero
        integer   pass
	logical   nonlinearsimplex
	integer   isimp
	integer   np1
        double precision ::  minimum_diff  = 1.d-05

c	Variables for the autopilot
	integer nA, iARE
	double precision Alin(12,12), Blin(12,50), KK(50,12)
	logical isuccess

c	For the general case
	integer qskip
	double precision q0, q1, q2, q3

	time      = aircraft%time
	dt        = aircraft%dt
	dt_output = aircraft%dt_output
	time_end  = aircraft%time_end

	Unorth = UVWPQR(1)
	Veast  = UVWPQR(2)
	Wdown  = UVWPQR(3)
	Pworld = UVWPQR(4)
	Qworld = UVWPQR(5)
	Rworld = UVWPQR(6)

	if (imode.eq.1) then
	   jend = 1  ! only doing one state
	else
	   jend = 50 ! trying to compute 50 trim states
	endif

	do i= 1, jend

	   sum_csolve(i) = ''
	   sum_ARE   (i) = ''

	enddo

	iMINPACK = 0
	iSIMPLEX = 0
	iARE     = 0

c	What we are looking to do is to set all the xd to zero to a specified value.
c       The inputs are all the control variables.  We adjust the uc and the quaternions
c	until all the xd near the specified values.

c	What are the specified values.  Reminding ourselves that the state variable is
c	(U V W P Q R q0 q1 q2 q3 X Y Z omegas batteries charge).  We will use the regular
c	trim state routine for steady level flight, but we need to some something for
c	curving flight.  Assuming it is in a circle and that the orientation of the 
c	body frame to the tangent of the flight does not change, it appears that 






c	Every call to fderiv is a new test state - we are not trying to fly.  An important
c	point we have is that we need to set the motor speed, given there is a motor
c	control.  This may prove to be quite difficult due to the fact that the propeller
c	is based on a lookup table rather than a function.
c
c	So in particular, the inputs are Unorth for the requested speed and then the
c	various control imputs.  The objective is to have all the xd come out to zero,
c	but we will probably also need to include someting like a minimization on the
c	power or energy used, since otherwise there will probably be lots of solutions
c	for multicopter or multicopter+wing configurations.  (Of course, the larger the
c	dimension, the bigger the challenges with the optimization.  And the MINPACK
c       routine requires more equations.)

c	We assume we have an initial guess (provided by the input or by the previous
c	iterate) and so we then look for the solution.  Thee first one is simple, in
c	that we are just looking for hover (we will do this one to see if we know
c	what is going on).  We first find out the motor speed, then we find out
c	the vehavior, and we do it in a loop that has a "no intelligence" convergence.

c	Now we do a loop through a number of Unorth velocities.  We also need to consider pitch
c	which we will index with the pitch angle theta, becaause we don't want to deal with
c	all the possibilities of the quaternion.  It seems there is physics here that should
c	be very beneficial to us, but right now I am unclear on how to take advantage of it.
c	So we will try the brute force ameoba routine (sigh).  Let's do a loop on many iterations

c	

	theta = 0.d0
        tolerance = aircraft%tolerance
	tol = tolerance

	Write (6,*)

	write (6,*) ' This routine finds steady solutions for general'
     .        ,' trim states, including turns.'
	write (6,*) ' A steady state (trimmed) flight condition is'
     .             ,' achieved when UVWdot and PQRdot are zero.'
	write (6,*) ' It is usually assumed that the flight direction'
     .              ,' is X.'

	do j = 1, jend

	   iwarn = .false.

	   if (imode.ne.1) then

	      UVWPQR(1) = dfloat(j)
	      UVWPQR(2) = 0.d0
	      UVWPQR(3) = 0.d0
	      UVWPQR(4) = 0.d0
	      UVWPQR(5) = 0.d0
	      UVWPQR(6) = UVWPQR(1)/radius

	   endif

	   Unorth = UVWPQR(1)
	   Veast  = UVWPQR(2)
	   Wdown  = UVWPQR(3)
	   Pworld = UVWPQR(4)
	   Qworld = UVWPQR(5)
	   Rworld = UVWPQR(6)
             

	   temp = (((100.d0/2.54d0)/12.d0)/5280.d0)*3600.d0
	   write (6,*)
	   write (6,30) Unorth - aircraft%Unwind,
     .                  Veast  - aircraft%Vewind,
     .                  Wdown  - aircraft%Wdwind,
     .                  Pworld, Qworld, Rworld

	   write (6,31) radius, Rworld

 30	   format(' Objective steady state speed is UVW world = '
     .             ,3f8.2,' m/s; PQR world = ',3f8.2, ' rad/s')

 31        format('  Turning radius ', f10.2, ' m (positive means '
     .                ,'clockwise looking down), Rworld = ', f10.4
     .                , ' radians/s')


c	   We need initial estimates for the simplex.  For now it is hardwired
c	   As we try to get something of interest (for four variables, five verticies)
c   	   Recall, all control variables run from 0 to 1

	   m = 6 + aircraft%num_propellers  ! 6 accelerations and power for propellers
cjdw	   n = udim + 3  ! all control channels plus the three independent quaternion components
cjdw	   n = udim + 1  ! all control channels plus the three independent quaternion components
	   n = udim + 2  ! pitch and roll

c	   This is now the set up - picking up the inputs from the last state
c	   (or the input state for the first case).
	   do i = 1, udim
	      ya(i) = uc(i)
	      ya(i) = max(0.d0, ya(i))
	      ya(i) = min(1.d0, ya(i))
	   enddo
	   theta = 0.d0
	   phi   = 0.d0
	   q0 = 1.d0
	   q1 = 0.d0
	   q2 = 0.d0
	   q3 = 0.d0

	   q0 =   cos(theta/2.d0) * cos(phi/2.d0)
	   q1 =   cos(theta/2.d0) * sin(phi/2.d0)
	   q2 =   sin(theta/2.d0) * cos(phi/2.d0)
	   q3 = - sin(theta/2.d0) * sin(phi/2.d0)
	   
c	   theta = 0.d0 ! an extremenly important initial condition; otherwise the MINPACK
c	                  routines don't find the solution.
c	   We will assume skip is as desired
	   qskip = control%qskip
cjdw	   if (qskip.eq.0) then
cjdw	      ya(udim + 1) = q1
cjdw	      ya(udim + 2) = q2
cjdw	      ya(udim + 3) = q3
cjdw	   else if (qskip.eq.1) then
cjdw	      ya(udim + 1) = q0
cjdw	      ya(udim + 2) = q2
cjdw	      ya(udim + 3) = q3
cjdw	   else if (qskip.eq.2) then
cjdw	      ya(udim + 1) = q0
cjdw	      ya(udim + 2) = q1
cjdw	      ya(udim + 3) = q3
cjdw	   else
cjdw	      ya(udim + 1) = q0
cjdw	      ya(udim + 2) = q1
cjdw	      ya(udim + 3) = q2
cjdw	   endif
	   ya(n-1) = theta  ! the zero set for theta
	   ya(n  ) = phi    ! the zero set for phi

c	   We seem to have problems if one of the motor controllers is set to zero
c	   do i = 1, aircraft%num_propellers
c	      if (ya(i).eq.0.d0) ya(i) = 0.5d0
c	   enddo
c	   We choose to set them all equal to 0.5
	   do i = 1, udim
	      ya(i) = 0.5d0
	   enddo

c       m is a positive integer input variable set to the number
c         of functions.
c       n is a positive integer input variable set to the number
c         of variables. n must not exceed m.
c	      This last is unfortunate, why n <= m ?  I can envision lots of control variables,
c	      but we will still only have 6 or 7 items in our minimization.  Hmmm.  It would
c	      really seem that we want lots of variables to play with. 
c       lwa is a positive integer input variable not less than
c         m*n+5*n+m.

	   if (lwa.lt.(m*n+5*n+m)) write (6,*) 
     .                      ' Warning: lwa not large enough '

c	   We skip this call if only the simplex is being used
	   if (aircraft%simplex.ne.1) then
              call lmdif1(fdcostgen,m,n,ya,za,tol,info,iwa,wa,lwa,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	   endif

c          begin nonlinear simplex

c	   This is the nonliner simplex call, as an alternate to the MINPACK (or if there are 
c	   convergence problems)

	   nonlinearsimplex = .false.
	   do k = 1, 6
	      if (abs(xd(k)).gt.0.01d0) then
	         nonlinearsimplex = .true.
	      endif
	   enddo

	   if (.not.nonlinearsimplex) then
              iMINPACK = iMINPACK + 1
	      sum_csolve(j) = 'MINPACK'
	   endif

	   if (nonlinearsimplex.or.(aircraft%simplex.eq.1)) then

c	      Variables for the simplex algorithm.

c	      What we are looking to do is to set all the xd to zero.  The inputs are all the
c	      control variables.  We adjust the uc until all the xd are zero, or at leat 
c	      close to zero.

c	      Every call to fderiv is a new test state - we are not trying to fly.  An important
c	      point we have is that we need to set the motor speed, given there is a motor
c	      control.  This may prove to be quite difficult due to the fact that the propeller
c	      is based on a lookup table rather than a function.  We will use (for now) the Nelder
c	      Mead simplex routine implementation I write 30 years ago.
c
c	      Thus, all x variables should be zero for each call
c	
c   	      Recall, all control variables run from 0 to 1

c	      We plan to go through the algorithm twice, if necessary.

cn	write (6,*) ' Here 6 n, udim, np1 ',n,udim,np1

	      n   = udim + 2
	      np1 = n    + 1
	
	      do pass = 1, 2

	         if (pass.eq.1) then

	            do i = 1, udim
	               uc(i) = 0.3d0
	            enddo
	            theta = 0.d0
		    phi   = 0.d0
		    diff = 0.4d0

		 else if (pass.eq.2) then

	            do i = 1, udim
	               uc(i) = 0.7d0
	            enddo
	            theta = 0.5d0
		    phi   = 0.5d0
		    diff = -0.4d0
	
	         endif

	         do i = 1, np1
	            do k = 1, udim
	   	       pam(i,k) = uc(k)
	            enddo
	            pam(i,n-1) = theta
	            pam(i,n  ) = phi
	         enddo
	         do i = 1, n
	            pam(i+1,i) = pam(i+1,i) + diff  ! try an offset for everything
	         enddo

c                Initialize these guys
                 do i = 1, np1
		    do k = 1, n
                       ya(k) = pam(i,k)
		    enddo
		    call fdcostgen2(m, n, ya, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
		    yam(i) = 0.d0
                    do k = 1, m
		       yam(i) = yam(i) + za(k)**2
		    enddo
                 enddo

                 call simplexg (pdim,pam,yam,np1,n,n,tolerance,iter,
cjdw              call simplex (pam,yam,4,3,3,tolerance,iter,
     .	               m,n,UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)

cn              write (6,*) ' iter, uc1, uc2, theta, rdcost  ',iter
cn     .          ,uc(1), uc(2), xo(6), yam(1)  ! these are most recent call

	     
	         do i = 1, udim
	            uc(i) = pam(1,i)
		    ya(i) = uc(i)
	         enddo
	         theta = pam(1,n-1)
	         phi   = pam(1,n  )
		 ya(n-1) = theta
		 ya(n  ) = phi

	         lsolved = .true.
	         do k = 1, 6
	            if (abs(xd(k)).gt.0.01d0) then
	               lsolved = .false.
	            endif
	         enddo
	         if (lsolved) then
		    iSIMPLEX = iSIMPLEX + 1
		    sum_csolve(j) = 'SIMPLEX'
		    goto 2000
	         endif
	
	      enddo ! to pass

 2000	      continue

	   endif  ! to nonlinear simplex

c	   end nonlinear simplex

 111	continue

c	   theta = ya(n) ! but see above, it needs to be zero going back into the minimizer.

	   write (6,21) info
 21	   format('  Finished lmdif1 call; info = ',i2, 
     .            ' (should be 1, 2 or 3; see MINPACK documentation)')
	   if (nonlinearsimplex) write (6,19) iter
 19	   format('  Nonlinear  simplex invoked: iterations = ',i5)
	   write (6,20) ' UVW world, body   (m/s)',xd(11),xd(12),xd(13)
     .                                    ,x(1),x(2),x(3)
	   write (6,20) ' UVWdot,PQRdot (m|r/s^2)',xd(1:6)
	   do k = 1, 6
	      if (abs(xd(k)).gt.0.01d0) then
                 write (6,*) '   Warning: at least one value of '
     .                      ,'UVWdot or PQRdot is large enough '
     .                      ,'that steady flight is unlikely'
     .                      ,' (not a trim state).'
	         iwarn = .true.
	         goto 96
	      endif
	   enddo
 96	   continue
	   write (6,20) ' Roll  angle phi   (deg)',rad_to_deg*xo(5)
     .             ,rad_to_deg*phi,  rad_to_deg*ya(n  )
	   write (6,20) ' Pitch angle theta (deg)',rad_to_deg*xo(6)
     .             ,rad_to_deg*theta,rad_to_deg*ya(n-1)
	   write (6,20) ' Yaw   angle psi   (deg)',rad_to_deg*xo(7)

c	   Now we are going to write out some flight qualities information
c	   xo(11 to 19) stores the direction cosine matrix taking the world
c	   frame to the body frame, so in this case we use the inverse
c	   (transpose) to get the world from from the body frame forces.
	   write (6,20) ' Thrust world, body  (N)'
     .              , xo(11)*xo(26) + xo(14)*xo(27) + xo(17)*xo(28)
     .              , xo(12)*xo(26) + xo(15)*xo(27) + xo(18)*xo(28)
     .              , xo(13)*xo(26) + xo(16)*xo(27) + xo(19)*xo(28)
     .              ,        xo(26),         xo(27),         xo(28)
	   write (6,20) ' Lift world, body    (N)'
     .              , xo(11)*xo(20) + xo(14)*xo(21) + xo(17)*xo(22)
     .              , xo(12)*xo(20) + xo(15)*xo(21) + xo(18)*xo(22)
     .              , xo(13)*xo(20) + xo(16)*xo(21) + xo(19)*xo(22)
     .              ,        xo(20),         xo(21),         xo(22)
	   write (6,20) ' Drag world, body    (N)'
     .              , xo(11)*xo(23) + xo(14)*xo(24) + xo(17)*xo(25)
     .              , xo(12)*xo(23) + xo(15)*xo(24) + xo(18)*xo(25)
     .              , xo(13)*xo(23) + xo(16)*xo(24) + xo(19)*xo(25)
     .              ,        xo(23),         xo(24),         xo(25)

c
c	Some additional information, for better understanding of what is occuring
c
c	Fx_lift = xo(20)                         Lift forces in body fixed frame
c	Fy_lift = xo(21)
c	Fz_lift = xo(22)
c
c	Fx_drag = xo(23)		         Drag forces in body fixed frame
c	Fy_drag = xo(24)
c	Fz_drag = xo(25)
c
c       Thrust_x = xo(26)			 Total thrust
c	Thrust_y = x0(27)
c	Thrust_z = xo(28)
c
c       Power_tot = xo(29)                       Estimated total power from all propellers
c	Power(i)  = xo(29+i)                       Estimated power from each individual motor.  Primarily 
c	current at each motor (i) = xo(29+numpropellers+i)  Estimated current draw for each motor
c	Current at each battery (i) = xo(29+2*numpropellers+i)  Eastimated current for each battery
c
c	Control variables (input)
c
c	uc(icontrol)                             These are the control variables.  They are assumed to range from 0 to 1.
c						 All ranges limits are hence checked "by definition."
c

	   if (udim.le.6) then
	      write (6,22) ' Controls   ', 1, udim, uc( 1:udim)
	   else if (udim.le.12) then
	      write (6,20) ' Controls    1 -  6     ', uc( 1:6   ) 
	      write (6,22) ' Controls   ', 7, udim, uc( 7:udim)
	   else if (udim.le.18) then
	      write (6,20) ' Controls    1 -  6     ', uc( 1:6   ) 
	      write (6,20) ' Controls    7 - 12     ', uc( 7:12  ) 
	      write (6,22) ' Controls   ',13, udim, uc(13:udim)
	   else if (udim.le.24) then
	      write (6,20) ' Controls    1 -  6     ', uc( 1:6   ) 
	      write (6,20) ' Controls    7 - 12     ', uc( 7:12  ) 
	      write (6,20) ' Controls   13 - 18     ', uc(13:18  ) 
	      write (6,22) ' Controls   ',19, udim, uc(19:udim)
	   else  ! lots of controls - if they have more than 30 they can ask for help
	      write (6,20) ' Controls    1 -  6     ', uc( 1:6   ) 
	      write (6,20) ' Controls    7 - 12     ', uc( 7:12  ) 
	      write (6,20) ' Controls   13 - 18     ', uc(13:18  ) 
	      write (6,20) ' Controls   19 - 24     ', uc(19:24  ) 
	      write (6,22) ' Controls   ',25, min(udim,30),
     .                  uc(25:min(udim,30)) 
	   endif
	   sum_uc(j) = 0.d0
	   do k = 1, udim
	      sum_uc(j) = max(sum_uc(j),uc(k))
	   enddo
	   do k = 1, udim
	      if ((uc(k).le.0.d0).or.(uc(k).ge.1.d0)) then
                 write (6,*) '   Caution: at least one control channel'
     .                      ,' is outside range of 0 < uc < 1,'
     .                ,' hence steady flight may be unlikely.'
	         goto 97
	      endif
	   enddo
 97	   continue

c	   Here we write out amperages at each of the motors
	   np = aircraft%num_propellers
	   if (np.le.6) then
	      write (6,22) ' Motor RPM  ', 1, np, x(13+ 1:13+np)
     .                             * 60.d0/twopi
	   else if (np.le.12) the n
	      write (6,20) ' Motor RPM   1 -  6     ', x(13+ 1:13+ 6) 
     .                             * 60.d0/twopi
	      write (6,22) ' Motor RPM  ', 7, np, x(13+ 7:13+np)
     .                             * 60.d0/twopi
	   else if (np.le.18) then
	      write (6,20) ' Motor RPM   1 -  6     ', x(13+ 1:13+ 6) 
     .                             * 60.d0/twopi
	      write (6,20) ' Motor RPM   7 - 12     ', x(13+ 7:13+12) 
     .                             * 60.d0/twopi
	      write (6,22) ' Motor RPM  ',13, np, x(13+13:13+np)
     .                             * 60.d0/twopi
	   else if (np.le.24) then
	      write (6,20) ' Motor RPM   1 -  6     ', x(13+ 1:13+ 6) 
     .                             * 60.d0/twopi
	      write (6,20) ' Motor RPM   7 - 12     ', x(13+ 7:13+12) 
     .                             * 60.d0/twopi
	      write (6,20) ' Motor RPM  13 - 18     ', x(13+13:13+18) 
     .                             * 60.d0/twopi
	      write (6,22) ' Motor RPM  ',19, np, x(13+19:13+np)
     .                             * 60.d0/twopi
	   else  ! lots of controls - if they have more than 30 they can ask for help
	      write (6,20) ' Motor RPM   1 -  6     ', x(13+ 1:13+ 6) 
     .                             * 60.d0/twopi
	      write (6,20) ' Motor RPM   7 - 12     ', x(13+ 7:13+12) 
     .                             * 60.d0/twopi
	      write (6,20) ' Motor RPM  13 - 18     ', x(13+13:13+18) 
     .                             * 60.d0/twopi
	      write (6,20) ' Motor RPM  19 - 24     ', x(13+19:13+24) 
     .                             * 60.d0/twopi
	      write (6,22) ' Motor RPM  ',25, min(13+np,13+30),
     .                         x(13+25:min(13+np,13+30)) 
     .                             * 60.d0/twopi
	   endif

c	   Here we write out amperages at each of the motors
	   np = aircraft%num_propellers
	   if (np.le.6) then
	      write (6,22) ' Motor Amps ', 1, np, xo(29+np+ 1:29+np+np)
	   else if (np.le.12) then
	      write (6,22) ' Motor Amps ', 1,  6, xo(29+np+ 1:29+np+ 6)
	      write (6,22) ' Motor Amps ', 7, np, xo(29+np+ 7:29+np+np)
	   else if (np.le.18) then
	      write (6,22) ' Motor Amps ', 1,  6, xo(29+np+ 1:29+np+ 6)
	      write (6,22) ' Motor Amps ', 7, 12, xo(29+np+ 7:29+np+12)
	      write (6,22) ' Motor Amps ',13, np, xo(29+np+13:29+np+np)
	   else if (np.le.24) then
	      write (6,22) ' Motor Amps ', 1,  6, xo(29+np+ 1:29+np+ 6)
	      write (6,22) ' Motor Amps ', 7, 12, xo(29+np+ 7:29+np+12)
	      write (6,22) ' Motor Amps ',13, 18, xo(29+np+13:29+np+18)
	      write (6,22) ' Motor Amps ',19, np, xo(29+np+19:29+np+np)
	   else  ! lots of controls - if they have more than 30 they can ask for help
	      write (6,22) ' Motor Amps ', 1,  6, xo(29+np+ 1:29+np+ 6)
	      write (6,22) ' Motor Amps ', 7, 12, xo(29+np+ 7:29+np+12)
	      write (6,22) ' Motor Amps ',13, 18, xo(29+np+13:29+np+18)
	      write (6,22) ' Motor Amps ',19, 24, xo(29+np+19:29+np+24)
	      write (6,20) ' Motor Amps ',25, min(29+np+np,29+np+30),
     .                            xo(29+np+25:min(29+np+np,29+np+30)) 
	   endif

c	   Now we look at whether we are exceeding the motor's allowed amperage
	   do k = 1, np
	      amps = xo(29+np+k) * propeller(k)%efficiency_ESC
	      ratio = amps/propeller(k)%I_max
	      if (k.eq.1) then
	         sum_motorampmax(j) = ratio
	         sum_motorampmin(j) = ratio
	      else if (ratio.gt.sum_motorampmax(j)) then
	         sum_motorampmax(j) = ratio
	      else if (ratio.lt.sum_motorampmin(j)) then
	         sum_motorampmin(j) = ratio
	      endif
	      ratio = amps * battery(propeller(k)%ibattery)%voltage
     .              / propeller(k)%maxpower
	      if (k.eq.1) then
	         sum_motorpowmax(j) = ratio
	         sum_motorpowmin(j) = ratio
	      else if (ratio.gt.sum_motorpowmax(j)) then
	         sum_motorpowmax(j) = ratio
	      else if (ratio.lt.sum_motorpowmin(j)) then
	         sum_motorpowmin(j) = ratio
	      endif
	   enddo
	   if (sum_motorampmax(j).gt.1.d0) then
c              write (6,*) '   Caution: at least one motor'
              write (6,*) '   Warning: at least one motor'
     .                   ,' maximum current exceeded'
	      iwarn = .true.
	   endif
	   if (sum_motorpowmax(j).gt.1.d0) then
c              write (6,*) '   Caution: at least one motor'
              write (6,*) '   Warning: at least one motor'
     .                   ,' maximum power exceeded'
	      iwarn = .true.
	   endif

	   ifirst = .true.
	   do k = 1, aircraft%num_batteries
c	      Battery capacity units are milli amp hours
	      amps = xo(29 + 2*aircraft%num_propellers + k)
	      sum_amps(j) = sum_amps(j) + amps
c	      We are interested in time to 20% power
	      time_of_bat = 0.8d0*(battery(k)%Capacity*3600.d0/1000.d0)
     .                      /amps
	      if (iwarn) then
                 write (6,41)  k, amps, time_of_bat, xd(11)*time_of_bat
	      else if (ifirst) then
                 write (6,40)  k, amps, time_of_bat, xd(11)*time_of_bat
	         distancemax0 = xd(11)*time_of_bat
	         sum_ftime(j) = time_of_bat
	         ifirst = .false.
	      else
                 write (6,40)  k, amps, time_of_bat, xd(11)*time_of_bat
		 if ((xd(11)*time_of_bat).lt.distancemax0) then
		    distancemax0 = xd(11)*time_of_bat
	            sum_ftime(j) = time_of_bat
		 endif 
	      endif
c	      We also ask about how much of the battery's potential is used
c	      Capacity    ! (mAh = milli amp hours) Total battery charge (1 milli amp hour = 3.6 coulombs)
c	      C_Continuous         ! In regular use, max amp is (C / 1000) * capacity in mAh
	      ratio = amps/(battery(k)%C_Continuous
     .                    * battery(k)%Capacity/1000.d0)
	      if (k.eq.1) then
	         sum_batampmax(j) = ratio
	      else if (ratio.gt.sum_batampmax(j)) then
                 sum_batampmax(j) = ratio
	      endif
	   enddo
	   if (sum_batampmax(j).gt.1.d0) 
     .        write (6,*) '    Caution: current exceeds contiuous'
     .                   ,' maximum for at least one of the batteries.'
	   write (6,51) xo(29)

	   sum_iwarn (j) = iwarn
	   sum_speed (j) = sqrt(xd(11)**2 + xd(12)**2 + xd(13)**2)
	   sum_pitch (j) = rad_to_deg*ya(n-1)
	   sum_roll  (j) = rad_to_deg*ya(n)
	   sum_power (j) = xo(29)
	   sum_thrust(j) = sqrt(xo(26)**2 + xo(27)**2 + xo(28)**2)
	   sum_lift  (j) = sqrt(xo(20)**2 + xo(21)**2 + xo(22)**2)
	   sum_drag  (j) = sqrt(xo(23)**2 + xo(24)**2 + xo(25)**2)
	   sum_dist  (j) = distancemax0

c	   Saving information for the control algorithm (potentially as a fallback).
	   if (imode.ne.1) then
	      if (irad.eq.1) then
	         control%turn_1_trim_if (j) = .not. sum_iwarn (j)  ! is this a legitimate state
	         if (.not.sum_iwarn(j)) then
	            do k = 1, udim
                       control%turn_1_trim_uc (j,k) = Uc(k)   ! the turning trim control settings
	            enddo
                    control%turn_1_trim_theta (j)   = ya(n-1) ! the turning trim pitch angle
                    control%turn_1_trim_phi   (j)   = ya(n)   ! the turning trim roll angle
                    control%turn_1_num_trim_max     = j       ! the number of trim states in the array.
	            control%turn_1_trim_speed (j)   = Unorth  ! the world frame north speed
	         endif
	      endif
	      if (irad.eq.2) then
	         control%turn_2_trim_if (j) = .not. sum_iwarn (j)  ! is this a legitimate state
	         if (.not.sum_iwarn(j)) then
	            do k = 1, udim
                       control%turn_2_trim_uc (j,k) = Uc(k)   ! the turning trim control settings
	            enddo
                    control%turn_2_trim_theta (j)   = ya(n-1) ! the turning trim pitch angle
                    control%turn_2_trim_phi   (j)   = ya(n)   ! the turning trim roll angle
                    control%turn_2_num_trim_max     = j       ! the number of trim states in the array.
	            control%turn_2_trim_speed (j)   = Unorth  ! the world frame north speed
	         endif
	      endif
	   endif

c	   Now, if requested, we determine the controls for the trim states, and store them.

	   if (control%compute_A.and.(.not.iwarn)) then

	      Alin = control%Alin
	      Blin = control%Blin

c	      write (6,*) ' trim Alin = '
c	      do k = 1, 12
c	         write (6,1000) (Alin(k,nA),nA=1,12)
c	      enddo
c	      write (6,*) ' trim Blin = '
c	      do k = 1, 12
c	         write (6,1000) (Blin(k,nA),nA=1,udim)
c	      enddo
 1000	      format(12e12.5)

c	      write (66,1010) Unorth,((Alin(k,nA),k=1,12),nA=1,12)
c	      write (67,1010) Unorth,((Blin(k,nA),k=1,12),nA=1,udim)
 1010	      format(145e12.4)
	

	      nA = 12

	      call are_control(nA, udim, Alin, Blin
     .                    , control%Q_position, control%Q_velocity
     .                    , control%Q_angular_velocity, control%Q_angles
     .			  , control%R
     .                    , KK,isuccess)

c	      control%lateral_trim_if (j) = isuccess    8/25/21  commented out - bug fix for trim state label
	      if (irad.eq.1) control%turn_1_trim_if (j) = isuccess  ! this a legitimate control state  8/25/21  bug fix
	      if (irad.eq.2) control%turn_2_trim_if (j) = isuccess  ! 8/25/21  bug fix
	      if (isuccess) then
	         iARE = iARE + 1
	         write (6,*) ' Controls K where uc = uc_trim'
     .                      ,' + K(x - x_trim)'
	         do k = 1, udim
		    write (6,56)(KK(k,nA),nA=1,12)
	         enddo
 56	         format(12f12.5)
		 sum_ARE(j) = 'ARE'
c	         write (68,1010) Unorth,((KK(k,nA),k=1,udim),nA=1,12)
	         if (imode.eq.2) then
		    if (irad.eq.1) then
	               control%turn_1_trim_qskip(j) = control%qskip
	               do k = 1, udim
	                  do nA = 1, 12
		             control%turn_1_trim_K(j,k,nA) = KK(k,nA)
		          enddo
	               enddo
	               do k = 1, xdim 
	   	          control%turn_1_trim_x(j,k) = x(k )
	               enddo
	            endif
		    if (irad.eq.2) then
	               control%turn_2_trim_qskip(j) = control%qskip
	               do k = 1, udim
	                  do nA = 1, 12
		             control%turn_2_trim_K(j,k,nA) = KK(k,nA)
		          enddo
	               enddo
	               do k = 1, xdim 
	   	          control%turn_2_trim_x(j,k) = x(k )
	               enddo
	            endif
	         endif
	      endif

	   endif  ! to compute_A

	enddo

	write (6,*)
	write (6,27) iMINPACK, iSIMPLEX
 27	format(' Trim states: ',i2,' found by MINPACK, ',i2
     .                          ,' found by nonlinear simplex.')
	if (control%compute_A) write (6,28) iARE
 28	format(' Control states found by RICPACK solving the ',
     .         'Algebraic Riccati Equation (ARE): ',i2)

	write (6,*) 
	write (6,*) 'Summary of no-warning steady state trims'
     .     ,' (thrust, lift, and drag are vector magnitudes).'
     .     ,'  Frac amp is the ratio of the current in the '
	write (6,*) 'motor divided by the maximum allowed '
     .     ,'current, then the maximum is taken over all the motors.'
     .     ,'  Frac pow is the same thing for motor power.'
	write (6,*) 'Frac current is the battery current divided by'
     .     ,' maximum allowable battery current, taken over all the'
     .     ,' batteries.'
	write (6,*) 'These Frac values should be less than 1.'
     .     ,'  Max uc is the max control variable uc (or throttle).'
	write (6,*)
	write (6,33) radius
 33     format(' Turning radius ', f10.2, ' m (positive means '
     .                ,'clockwise looking down)')
	write (6,*)
	write (6,*) ' Tangent speed Distance '
     .     ,'Flight time Pitch angle Roll angle  Max uc    Thrust '
     .     ,'     Lift      Drag  Current  Total power'
     .     ,' Frac amp  Frac pow  Frac current'
	write (6,*) '     (m/s)        (m)        '
     .     ,' (s)        (deg)     (deg)       (-)      (N)   '
     .     ,'    (N)       (N)      (amp)    (watt)'
     .     ,'     (-)       (-)       (-)'
	iwarn = .true.
	do i = 1, jend
	   if (.not.sum_iwarn(i)) then
              write (6,50) sum_speed (i), sum_dist (i)
     .                    ,sum_ftime (i), sum_pitch(i)
     .                    ,sum_roll  (i), sum_uc   (i)
     .                    ,sum_thrust(i), sum_lift (i), sum_drag (i) 
     .                    ,sum_amps  (i), sum_power(i)
     .                    ,sum_motorampmax(i), sum_motorpowmax(i)
     .                    ,sum_batampmax(i),sum_csolve(i),sum_ARE(i)
	      iwarn = .false.
	   endif
	enddo

c	Some identified metrics for plots
	write (6,*)
	write (6,*) '#Metrics'
	if (iwarn) then
	   write (6,*) ' Requested trim condition not found'
	else


	endif

 20	format (a25, 6f15.8)
 22	format (a13,i2,' - ',i2,'     ',6f15.8)
 40	format ('  Battery #',i2,' Current = ',f8.3,' amps, ',
     .          ' Time to 20% charge remaining = ',f6.1,' s, ',
     .          ' Flight distance = ',f10.3,' m')
 41	format ('  Battery #',i2,' Current = ',f8.3,' amps, ',
     .          ' Time to 20% charge remaining = ',f6.1,' s, ',
     .          ' Flight distance = ',f10.3,' m with warning')
 45	format (' Of all cases, maximum no-warning distance was at'
     .         ,' speed ',f4.1,' m/s with distance ',f10.3,' m')
 46	format (' Of all cases, maximum no-warning speed was a'
     .         ,' speed of  ',f4.1,' m/s with distance ',f10.3,' m')
 50	format (f11.2,3f12.2,7f10.2,3f10.3,5x,a7,2x,a3)
 51     format(' Total power from batteries ',f8.3, ' watts')

	return
	end



	subroutine fdcostgen(m, n, ya, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)

c	Ths subroutines calls the flight dynamics model (fderiv) to determine
c	the derivatives of the state variables to then look for a static solution.
c	James Walker  5 April 2021
c	4/5/21

c       This is what MINPACK wants, so you can compare with what we have
c
c         subroutine fcn(m,n,x,fvec,iflag)
c         integer m,n,iflag
c         double precision x(n),fvec(m)
c         ----------
c         calculate the functions at x and
c         return this vector in fvec.
c         ----------
c         return
c         end

c	The input array of variables is ya.  The output cost is za.
c	Currently, we have
c
c	Input (example for n = 3 input variables)
c	   UVWPQR                 requested north velocity (Veast, Zdown are zero for now)
c	   ya(1) = uc(1) = uc(2)  leading edge control
c	   ya(2) = uc(3) = uc(2)  trailing edge control
c	   ya(3) = theta          pitch angle
c
c	Output (example for m = 7 output values)
c	   za(1) = weight(1)*xd(1)    Udot in body frame
c	   za(2) = weight(1)*xd(2)    Vdot in body frame
c	   za(3) = weight(1)*xd(3)    Wdot in body frame
c	   za(4) = weight(1)*xd(4)    Pdot
c	   za(5) = weight(2)*xd(5)    Qdot
c	   za(6) = weight(2)*xd(6)    Rdot
c	   za(7) = weight(3)*xo(29)   total power   may not be used right away
c	currently I'm wondering if I can include motor power separately, thus
c	solving my number of equations problems.  I don't think power is degenerate,
c	so if I minimize the list of squareroot(power) it should give total power.
c	Notice that in order to insure that m >= n as required by MINPACK, we include
c	each motors power separately.


	use iso_c_binding
	implicit none
	include 'parts.h'

	integer xdim, udim

	integer  i, m, n, iflag, qskip
	double precision x (xdim), xd(xdim), xo(150), uc(udim)
	double precision time, Unorth, ya(n), za(m), UVWPQR(6)
	double precision weight1, weight2, weight3
	double precision theta, q0, q1, q2, q3, phi
	double precision temp1, temp2, temp3
	double precision Veast, Wdown, Pworld, Qworld, Rworld

	logical :: ip = .false.

c	aircraft%debug = 1

	if (aircraft%debug.gt.0) ip = .true.

c	ip = .true.  ! debug

	Unorth = UVWPQR(1)
	Veast  = UVWPQR(2)
	Wdown  = UVWPQR(3)
	Pworld = UVWPQR(4)
	Qworld = UVWPQR(5)
	Rworld = UVWPQR(6)

c	There is a bunch of set up at this point.

c	So what are our unknows?  They are all the control variables
c	plus the angle theta.  So n = udim + 1

	if (ip) write (60,60) Unorth,(ya(i),i=1,n)
 60	format (' Unorth, ya(1...n)      ',12e13.5)

	do i=1, udim
	   uc(i) = ya(i)
c	   The fact that 0 <= uc <= 1
	   uc(i) = max(0.d0, uc(i))
	   uc(i) = min(1.d0, uc(i))
	enddo
cjdw	qskip = control%qskip
cjdw	if (qskip.eq.0) then
cjdw	   q1 = ya(udim + 1)
cjdw	   q2 = ya(udim + 2)
cjdw	   q3 = ya(udim + 3)
c	   Now knowing what else to do here,
c	   in the context of signs.
cjdw           q0 = sqrt(1.d0 - q1**2 - q2**2 - q3**2)
cjdw	   if (x(7).lt.0.d0) q0 = - q0
cjdw	else if (qskip.eq.1) then
cjdw	   q0 = ya(udim + 1)
cjdw	   q2 = ya(udim + 2)
cjdw	   q3 = ya(udim + 3)
cjdw           q1 = sqrt(1.d0 - q0**2 - q2**2 - q3**2)
cjdw	   if (x(8).lt.0.d0) q1 = - q1
cjdw	else if (qskip.eq.2) then
cjdw	   q0 = ya(udim + 1)
cjdw	   q1 = ya(udim + 2)
cjdw	   q3 = ya(udim + 3)
cjdw           q2 = sqrt(1.d0 - q0**2 - q1**2 - q3**2)
cjdw	   if (x(9).lt.0.d0) q2 = - q2
cjdw	else
cjdw	   q0 = ya(udim + 1)
cjdw	   q1 = ya(udim + 2)
cjdw	   q2 = ya(udim + 3)
cjdw           q3 = sqrt(1.d0 - q0**2 - q1**2 - q2**2)
cjdw	   if (x(10).lt.0.d0) q3 = - q3
cjdw	endif

	theta = ya(n-1)
	phi   = ya(n  )

	theta = max(theta, -halfpi)
	theta = min(theta,  halfpi)

	phi   = max(phi  , -halfpi)
	phi   = min(phi  ,  halfpi)

cjdw	q0 = cos(theta/2.d0)
cjdw	q1 = 0.d0
cjdw	q2 = sin(theta/2.d0)
cjdw	q3 = 0.d0
	q0 =   cos(theta/2.d0) * cos(phi/2.d0)
	q1 =   cos(theta/2.d0) * sin(phi/2.d0)
	q2 =   sin(theta/2.d0) * cos(phi/2.d0)
	q3 = - sin(theta/2.d0) * sin(phi/2.d0)

c       Some pure geometry (no physics); Direction cosine matrix (dcm) from the quaternions.
c	This rotation takes world frame to body frame.
	xo(11) = q0**2 + q1**2 - q2**2 - q3**2   ! xx
	xo(12) = 2.d0*(q1*q2 + q0*q3)            ! xy
	xo(13) = 2.d0*(q1*q3 - q0*q2)            ! xz
	xo(14) = 2.d0*(q1*q2 - q0*q3)            ! yx
	xo(15) = q0**2 - q1**2 + q2**2 - q3**2   ! yy
	xo(16) = 2.d0*(q2*q3 + q0*q1)            ! yz
	xo(17) = 2.d0*(q1*q3 + q0*q2)            ! zx
	xo(18) = 2.d0*(q2*q3 - q0*q1)            ! zy
	xo(19) = q0**2 - q1**2 - q2**2 + q3**2   ! zz
c

c	We call fderiv for the sole purpose of getting the rotation matrix (so it is
c	wasteful, but that's the way it is).  The rotation matrix takes world frame
c	to body frame.

c	The picture is
c                                      _ - >  x forward   U = Unorth*cos(theta) > 0
c            +------> x  North      +-    )  theta        W = Unorth*sin(theta) > 0
c            |                       |
c            |                        |
c            V z  Down                 V z down
c
c       Because of this, our multicopter will want to pitch down (i.e., theta < 0)

c	This allows us to compute the expected velocity in body coordinates
	x(1) = xo(11)*Unorth + xo(12)*Veast + xo(13)*Wdown
	x(2) = xo(14)*Unorth + xo(15)*Veast + xo(16)*Wdown
	x(3) = xo(17)*Unorth + xo(18)*Veast + xo(19)*Wdown

c	We don't want any rotational acceleration
	x(4) = xo(11)*Pworld + xo(12)*Qworld + xo(13)*Rworld
	x(5) = xo(14)*Pworld + xo(15)*Qworld + xo(16)*Rworld
	x(6) = xo(17)*Pworld + xo(18)*Qworld + xo(19)*Rworld

c	write (6,*) ' Rworld ', rworld

c	We store the unit quaternion
	x(7)  = q0
	x(8)  = q1
	x(9)  = q2
	x(10) = q3

c	And the fact there is no displacement
	x(11) = 0.d0
	x(12) = 0.d0	
	x(13) = 0.d0

c	write (6,*) ' fdcost q0 q1 q2 q3 ',q0,q1,q2,q3

c	The motor omega terms are set based on the given voltages
	call motor_initialize (time, xdim, x, xd, xo, udim, uc,
     .              aircraft, wing, propeller, battery, control)

c	Now we can call the fderiv
	call fderiv(time, xdim, x, xd, xo, udim, uc,
     .              aircraft, wing, propeller, battery, control)

c	Now with the values, we develop the cost function.  The first three entries
c	are the linear acceleration in the body frame (which we want to be zero) and
c	the second three entries are the rotational acceleration entries, which we
c	multiply by a term to put "radians" on a similar setting to the distance
c	measure.  The xo(29) term is the power usage, where the intent is to get
c	this to be small to avoid a local minimums.

c	In the general case, which can involve turns, the quaternion dots may not
c	be zero, but we are not going to address them (i.e., we are setting them
c	because we are setting PQR, hence the qdot terms fall of out that).

c	za      =  weight1*(xd(1)**2 + xd(2)**2 + xd(3)**2)          ! UVW dots
c     .          + weight2*(xd(4)**2 + xd(5)**2 + xd(6)**2)          ! PQR dots
c     .          + weight3*xo(29)             ! total power, to avoid a local min

c	For the outputs, there are at least having to do with the accelerations.
c	The other unknowns are related to power, and we list them separately
c	and then have the minpack software add them up.  I am taking the 
c	square root so that the sume of squares gives the power itself.
	za(1) = aircraft%weight(1)*xd(1)          ! U dot
	za(2) = aircraft%weight(1)*xd(2)          ! V dot
	za(3) = aircraft%weight(1)*xd(3)          ! W dot
	za(4) = aircraft%weight(2)*xd(4)          ! P dot
	za(5) = aircraft%weight(2)*xd(5)          ! Q dot
	za(6) = aircraft%weight(2)*xd(6)          ! R dot
	do i = 1, aircraft%num_propellers   
           za(6+i) = aircraft%weight(3)*sqrt(abs(xo(29+i)))   ! root of power
	enddo
c	Za(7) = aircraft%weight(3)*xo(29)         ! total power, to avoid a local min

	if (ip) then

	   write (6,10) ' UVW world, body ',xd(11),xd(12),xd(13)
     .                                    ,x(1),x(2),x(3)
	   write (6,10) ' xdots ',xd(1:6)
	   write (6,10) ' uc1 - uc4, theta ', uc(1:4), 
     .                    rad_to_deg*theta 
	   write (6,10) ' dots (1 - 6) ', za(1:6)
cc	   write (6,10) ' dots (7 -12) ', za(7:12)
	   write (6,10) ' power 1 -4, tot ',xo(30:33),xo(29)
	   write (6,10) ' amps  1 -4, tot ',xo(34:37),xo(38)
	   write (6,10) ' omega dot 1-4 ',xd(13+1:13+6)
	   write (6,10) ' weighted za 1 - 6 ',za(1:6)
	   write (6,10) ' rest za 7 - 12 ' 
     .          ,za(7:6+max(1,min(aircraft%num_propellers,6)))
	   temp1 = 0.d0
	   temp2 = 0.d0
	   temp3 = 0.d0
	   do i = 1, 3
	      temp1 = temp1 + za(i)**2
	   enddo
	   do i = 4, 6
	      temp2 = temp2 + za(i)**2
	   enddo
	   do i = 7, 6 + aircraft%num_propellers
	      temp3 = temp3 + za(i)**2
	   enddo
	   write (6,10) ' no wgt final costs '
     .                                 ,temp1/aircraft%weight(1)**2
     .                                 ,temp2/aircraft%weight(2)**2
     .                                 ,temp3/aircraft%weight(3)**2
	   write (6,10) ' wgted final costs ',temp1,temp2,temp3
     .                                       ,temp1+temp2+temp3

	   write (6,*)

	endif

 10	format (a20, 6f15.8)

c	We are going to change ya to what we are really using, to see if
c	that makes any difference (or if it is just passed in)
	do i=1, udim
	   ya(i) = uc(i)
	enddo
cjdw	if (qskip.eq.0) then
cjdw	   ya(udim + 1) = q1
cjdw	   ya(udim + 2) = q2
cjdw	   ya(udim + 3) = q3
cjdw	else if (qskip.eq.1) then
cjdw	   ya(udim + 1) = q0
cjdw	   ya(udim + 2) = q2
cjdw	   ya(udim + 3) = q3
cjdw	else if (qskip.eq.2) then
cjdw	   ya(udim + 1) = q0
cjdw	   ya(udim + 2) = q1
cjdw	   ya(udim + 3) = q3
cjdw	else
cjdw	   ya(udim + 1) = q0
cjdw	   ya(udim + 2) = q1
cjdw	   ya(udim + 3) = q2
cjdw	endif

	theta = min(theta,  halfpi)
	theta = max(theta, -halfpi)

	phi   = min(phi  ,  halfpi)
	phi   = max(phi  , -halfpi)

c	ya(n-1) = theta
c	ya(n  ) = phi

	if (ip) write (60,61) Unorth,(ya(i),i=1,n),temp1+temp2+temp3
 61	format (' Unorth, ya(1...n),cost ',12e13.5)

	return
	end


      subroutine simplexg (pdim,p,y,mp,np,ndim,tolerance,iterations,
     .	               m,n,UVWPQR,
     .                 time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)

c     4/3/21  Modified for the DARPA project, to do the call to the
c	      flight software.  We now pass the various files through
c	      the subroutine calls since there is really no other
c	      way to do it.  JDW
cc
c     7/24/21 The version for the general solve


	use iso_c_binding
	implicit none
	include 'parts.h'

	integer xdim, udim

	double precision x (xdim), xd(xdim), xo(150), uc(udim)

	double precision Unorth, za(m), time, UVWPQR(6)

c------------------------------------------------------------------------------
c           SOUTHWEST RESEARCH INSTITUTE  COPYRIGHT (c) 1989-1991
c                             (512) 522-3181
c                           ALL RIGHTS RESERVED
c------------------------------------------------------------------------------

c     This is the simplex routine of Nelder and Mead as described
c     in their 1965 Computer Journal article "A Simplex Method
c     for Function Minimization."  The article says the authors
c     are at the "National Vegetable Research Station", in 
c     Wellesbourne, Warwick.
c 
c     Input assumes p(which corner, coordinate number).
c
c*---------------------------------------------------------------------
c* Revision History
c*    James Walker   18 December 1989
c*---------------------------------------------------------------------

      integer itmax
      integer mp
      integer np
      integer iterations
      integer ndim
      integer ihigh
      integer inexthigh
      integer low
      integer i
      integer j
      integer pdim

c     New for DARPA flight sim
      integer m,n
      integer iflag

      double precision alpha, beta, gamma, zero, one, two
      parameter (alpha=1.0d0, beta=0.5d0, gamma=2.0d0, itmax=2000)
      double precision p(pdim,np), y(mp), tolerance, pstar(50), ystar
      double precision p2star(50), y2star, pbar(50), tol

      zero = 0.d0
      one = 1.d0
      two = one+one

c     It is assumed that upon entrance the y variable has been initialized.
      iterations = 0
      tol = two * tolerance

c     This is the outer loop, which iterates.
      do while (tol .gt. tolerance .and. iterations .lt. itmax)
         iterations = iterations+1

cn	write (6,*) ' iter, mp, np ',iterations,mp,np

c        We first find the highest, lowest, and next to highest.
         if (y(1) .gt. y(2)) then
            ihigh = 1
            inexthigh = 2
         else
            ihigh = 2
            inexthigh = 1
         endif
         low = inexthigh
         do i = 3, ndim+1
            if (y(i) .gt. y(ihigh)) then
               inexthigh = ihigh
               ihigh = i
            elseif (y(i) .gt. y(inexthigh)) then
               inexthigh = i
            endif
            if (y(i) .lt. y(low)) low = i
         enddo

c        We now find the bar (center of opposite face) and single star 
c        (reflection of high point through that face) variables, as in paper.
         do i = 1, ndim
            pbar(i) = zero
            do j = 1, ndim + 1
               pbar(i) = pbar(i) + p(j,i)
            enddo
            pbar(i) = (pbar(i) - p(ihigh,i)) / dfloat(ndim)
         enddo
         do i = 1, ndim
            pstar(i) = (one + alpha) * pbar(i) - alpha * p(ihigh,i)
         enddo
cc         ystar = funk(pstar)
	 call fdcostgen2(m, n, pstar, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
cold	 call fdcost(UVWPQR, pstar, za,
cold     .                 time, xdim, x, xd, xo, udim, uc,
cold     .                 aircraft, wing, propeller, battery, control)
	 ystar = 0.d0
	 do i = 1, m
	    ystar = ystar + za(i)**2
	 enddo

c        Now we enter the comparison stage of the algorithm.
         if (ystar .lt. y(low)) then
c           We form the double star terms.
            do i = 1, ndim
               p2star(i) = (one - gamma) * pbar(i) + gamma * pstar(i)
            enddo
cc            y2star = funk (p2star)
	    call fdcostgen2(m, n, p2star, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)

cold	    call fdcost(Unorth, p2star, za,
cold     .                 time, xdim, x, xd, xo, udim, uc,
cold     .                 aircraft, wing, propeller, battery, control)
	    y2star = 0.d0
	    do i = 1, m
	       y2star = y2star + za(i)**2
	    enddo
            if (y2star .lt. y(low)) then
               do i = 1, ndim
                  p(ihigh,i) = p2star(i)
               enddo
               y(ihigh) = y2star
            else
c              So y2star is greater than ylow
               do i = 1, ndim
                  p(ihigh,i) = pstar(i)
               enddo
               y(ihigh) = ystar
            endif
         else
c           So ystar is greater than ylow
            if (ystar .lt. y(ihigh)) then
c              This should be the "general" case, with ystar being
c              between ylow and yhigh.
               do i = 1, ndim
                  p(ihigh,i) = pstar(i)
               enddo
               y(ihigh) = ystar
            endif
            if (ystar .gt. y(inexthigh)) then
c              It is time for a contraction.
               do i = 1, ndim
                  p2star(i) = (one - beta) * pbar(i) + beta * p(ihigh,i)
               enddo
cc               y2star = funk (p2star)
	       call fdcostgen2(m, n, p2star, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
cold	       call fdcost(Unorth, p2star, za,
cold     .                 time, xdim, x, xd, xo, udim, uc,
cold     .                 aircraft, wing, propeller, battery, control)
cold	       y2star = za
	       y2star = 0.d0
	       do i = 1, m
	          y2star = y2star + za(i)**2
	       enddo
               if (y2star .gt. y(ihigh)) then
c                 Worst case - we need to contract everything: p = (p+plow)/2
                  do i = 1,ndim
                     pstar(i) = p(low,i)
                  enddo
                  do i = 1,ndim+1
                     do j = 1,ndim
                        p(i,j) = (p(i,j)+pstar(j))/two
                        p2star(j) = p(i,j)
                     enddo
cc                     y(i) = funk(p2star)
	             call fdcostgen2(m, n, p2star, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
cold	             call fdcost(Unorth, p2star, za,
cold     .                 time, xdim, x, xd, xo, udim, uc,
cold     .                 aircraft, wing, propeller, battery, control)
cold	             ystar = za
	             ystar = 0.d0
	             do j = 1, m
	                ystar = ystar + za(j)**2
	             enddo
                  enddo
               else
c                 So we have found a y2star less than yhigh
                  do i = 1,ndim
                     p(ihigh,i) = p2star(i)
                  enddo
                  y(ihigh) = y2star
               endif
            endif
         endif

c        This is the end of the questions.  Now we check the tolerance.
c        The one is in case the function is near zero.
         tol = two*abs(y(ihigh)-y(low))/(abs(y(ihigh))+abs(y(low))+one)
cn         write (6,*) ' tol, tolerance ',tol,tolerance
cn	 write (6,*) ' iteration, itmax ',iterations,itmax
      enddo

      if (iterations .gt. itmax)
     &         write (6,*) ' Iterations exceed itmax'

      return
      end

	subroutine fdcostgen2(m, n, ya, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)

c	Ths subroutines calls the flight dynamics model (fderiv) to determine
c	the derivatives of the state variables to then look for a static solution.
c	James Walker  5 April 2021
c	4/5/21

c       This is what MINPACK wants, so you can compare with what we have
c
c         subroutine fcn(m,n,x,fvec,iflag)
c         integer m,n,iflag
c         double precision x(n),fvec(m)
c         ----------
c         calculate the functions at x and
c         return this vector in fvec.
c         ----------
c         return
c         end

c	The input array of variables is ya.  The output cost is za.
c	Currently, we have

c	Input (for example with n = 3 input variables)
c	   Unorth                 requested north velocity (Veast, Zdown are zero for now)
c	   ya(1) = uc(1) = uc(2)  leading edge control
c	   ya(2) = uc(3) = uc(2)  trailing edge control
c	   ya(3) = theta          pitch angle
c
c	Output (for example with m = 7 output values)
c	   za(1) = weight(1)*xd(1)    Udot in body frame
c	   za(2) = weight(1)*xd(2)    Vdot in body frame
c	   za(3) = weight(1)*xd(3)    Wdot in body frame
c	   za(4) = weight(1)*xd(4)    Pdot
c	   za(5) = weight(2)*xd(5)    Qdot
c	   za(6) = weight(2)*xd(6)    Rdot
c	   za(7) = weight(3)*xo(29)   total power   may not be used right away
c	currently I'm wondering if I can include motor power separately, thus
c	solving my number of equations problems.  I don't think power is degenerate,
c	so if I minimize the list of squareroot(power) it should give total power.


	use iso_c_binding
	implicit none
	include 'parts.h'

	integer xdim, udim

	integer  i, m, n, iflag
	double precision x (xdim), xd(xdim), xo(150), uc(udim)
	double precision time, Unorth, ya(n), za(m), UVWPQR(6)
	double precision weight1, weight2, weight3
	double precision theta, q0, q1, q2, q3
	double precision temp1, temp2, temp3
	double precision phi, Veast, Wdown, Pworld, Qworld, Rworld

	logical :: ip = .false.
	logical loutside

	if (aircraft%debug.gt.0) ip = .true.

c	There is a bunch of set up at this point.

	Unorth = UVWPQR(1)
	Veast  = UVWPQR(2)
	Wdown  = UVWPQR(3)
	Pworld = UVWPQR(4)
	Qworld = UVWPQR(5)
	Rworld = UVWPQR(6)

c	So what are our unknows?  They are all the control variables
c	plus the angles theta and phi.  So n = udim + 2

c	>>> New for simplex
	loutside = .false.
	do i=1, udim
	   uc(i) = ya(i)
	enddo
	theta = ya(n-1)
	phi   = ya(n  )
	do i=1, udim
	   if ((uc(i).gt.1.d0).or.(uc(i).lt.0.d0)) then
              loutside = .true.
	   endif
	enddo
	if ((theta.gt.halfpi).or.(theta.lt.-halfpi)) then
           loutside = .true.
	endif
	if ((phi.gt.halfpi).or.(phi.lt.-halfpi)) then
           loutside = .true.
	endif
	if (loutside) then
           do i=1,6+aircraft%num_propellers
              za(i) = 1.d+07
	   enddo
	   goto 1000
	endif
c	<<< end of new for simplex

	do i=1, udim
	   uc(i) = ya(i)
c	   The fact that 0 <= uc <= 1
	   uc(i) = max(0.d0, uc(i))
	   uc(i) = min(1.d0, uc(i))
	enddo
	theta = max(-halfpi, theta)
	theta = min( halfpi, theta)
	phi   = max(-halfpi, phi  )
	phi   = min( halfpi, phi  )
	   
c       For a given case, we first set up the quaternions, given theta:
	q0 =   cos(theta/2.d0) * cos(phi/2.d0)
	q1 =   cos(theta/2.d0) * sin(phi/2.d0)
	q2 =   sin(theta/2.d0) * cos(phi/2.d0)
	q3 = - sin(theta/2.d0) * sin(phi/2.d0)

c       Some pure geometry (no physics); Direction cosine matrix (dcm) from the quaternions.
c	This rotation takes world frame to body frame.
	xo(11) = q0**2 + q1**2 - q2**2 - q3**2   ! xx
	xo(12) = 2.d0*(q1*q2 + q0*q3)            ! xy
	xo(13) = 2.d0*(q1*q3 - q0*q2)            ! xz
	xo(14) = 2.d0*(q1*q2 - q0*q3)            ! yx
	xo(15) = q0**2 - q1**2 + q2**2 - q3**2   ! yy
	xo(16) = 2.d0*(q2*q3 + q0*q1)            ! yz
	xo(17) = 2.d0*(q1*q3 + q0*q2)            ! zx
	xo(18) = 2.d0*(q2*q3 - q0*q1)            ! zy
	xo(19) = q0**2 - q1**2 - q2**2 + q3**2   ! zz
c
c	We can explicitly compute the matrix, if we want, from theta:
c	xo(11) = cos(theta)   ! xx
c	xo(12) = 0.d0         ! xy
c	xo(13) = - sin(theta) ! xz
c	xo(14) = 0.d0         ! yx
c	xo(15) = 1.d0         ! yy
c	xo(16) = 0.d0         ! yz
c	xo(17) = sin(theta)   ! zx
c	xo(18) = 0.d0         ! zy
c	xo(19) = cos(theta)   ! zz


c	We call fderiv for the sole purpose of getting the rotation matrix (so it is
c	wasteful, but that's the way it is).  The rotation matrix takes world frame
c	to body frame.

c	The picture is
c                                      _ - >  x forward   U = Unorth*cos(theta) > 0
c            +------> x  North      +-    )  theta        W = Unorth*sin(theta) > 0
c            |                       |
c            |                        |
c            V z  Down                 V z down
c
c       Because of this, our multicopter will want to pitch down (i.e., theta < 0)

c	This allows us to compute the expected velocity in body coordinates
	x(1) = xo(11)*Unorth + xo(12)*Veast + xo(13)*Wdown
	x(2) = xo(14)*Unorth + xo(15)*Veast + xo(16)*Wdown
	x(3) = xo(17)*Unorth + xo(18)*Veast + xo(19)*Wdown

c	We don't want any rotational acceleration
	x(4) = xo(11)*Pworld + xo(12)*Qworld + xo(13)*Rworld
	x(5) = xo(14)*Pworld + xo(15)*Qworld + xo(16)*Rworld
	x(6) = xo(17)*Pworld + xo(18)*Qworld + xo(19)*Rworld

c	We store the unit quaternion
	x(7)  = q0
	x(8)  = q1
	x(9)  = q2
	x(10) = q3

c	And the fact there is no displacement
	x(11) = 0.d0
	x(12) = 0.d0	
	x(13) = 0.d0

c	The motor omega terms are set based on the given voltages
	call motor_initialize (time, xdim, x, xd, xo, udim, uc,
     .              aircraft, wing, propeller, battery, control)

c	Now we can call the fderiv
	call fderiv(time, xdim, x, xd, xo, udim, uc,
     .              aircraft, wing, propeller, battery, control)

c	Now with the values, we develop the cost function.  The first three entries
c	are the linear acceleration in the body frame (which we want to be zero) and
c	the second three entries are the rotational acceleration entries, which we
c	multiply by a term to put "radians" on a similar setting to the distance
c	measure.  The xo(29) term is the power usage, where the intent is to get
c	this to be small to avoid a local minimums.

c	The quaternion dots are all zero since we have specified the angular
c	velocity is zero; hence, they do not enter in.

c	weight1 = 100.d0
c	weight2 = 0.d0

c	za      =          xd(1)**2 + xd(2)**2 + xd(3)**2           ! UVW dots
c     .          + weight1*(xd(4)**2 + xd(5)**2 + xd(6)**2)          ! PQR dots
c     .          + weight2*xo(29)             ! total power, to avoid a local min

c	For the outputs, there are at least having to do with the accelerations.
c	The other unknowns are related to power, and we list them separately
c	and then have the minpack software add them up.  I am taking the 
c	square root so that the sume of squares gives the power itself.
	za(1) = aircraft%weight(1)*xd(1)          ! U dot
	za(2) = aircraft%weight(1)*xd(2)          ! V dot
	za(3) = aircraft%weight(1)*xd(3)          ! W dot
	za(4) = aircraft%weight(2)*xd(4)          ! P dot
	za(5) = aircraft%weight(2)*xd(5)          ! Q dot
	za(6) = aircraft%weight(2)*xd(6)          ! R dot
	do i = 1, aircraft%num_propellers   
           za(6+i) = aircraft%weight(3)*sqrt(abs(xo(29+i)))   ! root of power
	enddo
c	Za(7) = aircraft%weight(3)*xo(29)         ! total power, to avoid a local min

	if (ip) then

	   write (6,10) ' UVW world, body ',xd(11),xd(12),xd(13)
     .                                    ,x(1),x(2),x(3)
	   write (6,10) ' xdots ',xd(1:6)
	   write (6,10) ' uc1 - uc4, theta ', uc(1:4), 
     .                    rad_to_deg*theta 
	   write (6,10) ' dots (1 - 6) ', za(1:6)
cc	   write (6,10) ' dots (7 -12) ', za(7:12)
	   write (6,10) ' power 1 -4, tot ',xo(30:33),xo(29)
	   write (6,10) ' amps  1 -4, tot ',xo(34:37),xo(38)
	   write (6,10) ' omega dot 1-4 ',xd(13+1:13+6)
	   write (6,10) ' weighted za 1 - 6 ',za(1:6)
	   write (6,10) ' rest za 7 - 12 ' 
     .          ,za(7:6+max(1,min(aircraft%num_propellers,6)))
	   temp1 = 0.d0
	   temp2 = 0.d0
	   temp3 = 0.d0
	   do i = 1, 3
	      temp1 = temp1 + za(i)**2
	   enddo
	   do i = 4, 6
	      temp2 = temp2 + za(i)**2
	   enddo
	   do i = 7, 6 + aircraft%num_propellers
	      temp3 = temp3 + za(i)**2
	   enddo
	   write (6,10) ' no wgt final costs '
     .                                 ,temp1/aircraft%weight(1)**2
     .                                 ,temp2/aircraft%weight(2)**2
     .                                 ,temp3/aircraft%weight(3)**2
	   write (6,10) ' wgted final costs ',temp1,temp2,temp3
     .                                       ,temp1+temp2+temp3

	   write (6,*)

	endif

 10	format (a20, 10f13.4)

 1000	continue

c	write (6,*) ' za ',za(1:6+aircraft%num_propellers)
	   temp1 = 0.d0
	   temp2 = 0.d0
	   temp3 = 0.d0
	   do i = 1, 3
	      temp1 = temp1 + za(i)**2
	   enddo
	   do i = 4, 6
	      temp2 = temp2 + za(i)**2
	   enddo
	   do i = 7, 6 + aircraft%num_propellers
	      temp3 = temp3 + za(i)**2
	   enddo

c	write (6,10)' Simplex cost',temp1,temp2
c     .                 ,temp3,temp1+temp2+temp3
c	write (6,10) ' Simplex locations ',ya(1:n)

cn	write (40,20) uc(1),uc(2),theta,(za(i),i=1,7)
cn     .                ,temp1+temp2+temp3
 20	format (' uc1,uc2,theta,za1-7',11e12.5)

	return
	end

