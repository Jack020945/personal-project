c	This version the trim uses the Levenberg-Marquardt method from
c	Minpack, by Argonne Naitinoal Laboratory (1980).


	subroutine trim(xdim,x,xd,udim,uc,aircraft,wing,propeller
     .                ,battery,control)
c	This subroutine determines trim states for anything using the
c	flight dynamics model.  
c	James Walker  3 April 2021
c	4/3/21
c
c	2/5/22 Update and improved.  Added a final minpack call for
c              simplex converged solutions.  Combined general_trim 
c              and trim into one piece of code, primarily for code maintenance.

	use iso_c_binding
	implicit none
	include 'parts.h'

	integer xdim, udim

	integer  i, j, k, np
	double precision x (xdim), xd(xdim), xo(1500), uc(udim)

	double precision error

	double precision xd3previous, uc1previous, dxd3

	double precision time, dt, dt_output, time_end

	double precision theta, Unorth, za(75), ya(75), tolerance
	double precision time_of_bat, amps, temp
	double precision UVWPQR(6)
	logical iwarn, ifirst
	double precision speedmax , distancemax
	double precision speedmax0, distancemax0
	double precision speedmax1, distancemax1
	integer itot, jdist, jspeed
	parameter (itot = 50)
	logical sum_iwarn(0:itot), lsolved
	double precision sum_speed (0:itot), sum_pitch(0:itot)
	double precision sum_roll  (0:itot)
	double precision sum_thrust(0:itot), sum_lift (0:itot)
        double precision sum_drag  (0:itot), sum_amps (0:itot)
	double precision sum_power (0:itot), sum_dist (0:itot)
	double precision sum_ftime (0:itot), ratio
	double precision sum_motorampmax(0:itot),sum_motorampmin(0:itot)
	double precision sum_motorpowmax(0:itot),sum_motorpowmin(0:itot)
	double precision sum_batampmax  (0:itot),sum_uc         (0:itot)
	double precision sum_minmaxxd      , sum_minmaxxd_speed
	double precision sum_wl         (0:itot), sum_wl_max
	logical :: ifirst_notrim = .true.
	logical :: ifirst_notrim_flag = .true.
	character*7 sum_csolve(0:itot)
	character*3 sum_ARE(0:itot)

	integer iMINPACK, iSIMPLEX

c	Minpack variables
        integer m,n,info,lwa
	parameter (lwa = 2500)
        integer iwa(75)  ! equals n at least
        double precision tol
        double precision wa(lwa)  ! see min value below
        external fdcost

c	fdcost variables
	integer iflag

c	Variables for the simplex algorithm.
	integer   pdim 
	parameter (pdim = 50)
        double precision pam(pdim,pdim), yam(pdim)
        integer ier
        integer iter
        double precision f, diff(pdim)
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

c	For general trim
	integer imode, irad
	double precision radius, phi

c	More variables
	integer pass_start, ilow, iter_total, jj, jm1, jbegin
c	For storing intermediate solutions (to keep the best one)
	double precision temp1, temp2
	character*1 :: used = ' '
	logical previous_converge

	logical ibetter, lateral, turn1, turn2
	logical itest0, itest1, itest2

	double precision Veast, Wdown, Pworld, Qworld, Rworld

	integer np30, nw
        integer i_high_motor_amps_number , i_high_motor_amps_list (50)
        integer i_high_motor_power_number, i_high_motor_power_list(50)
        integer i_high_wing_load_number  , i_high_wing_load_list  (50)
	character*3 integer_list(50)

	logical compute_A_store

c       Variables for the stability computation
        double precision Clin(12,12)
        integer k1, k2

	time      = aircraft%time
	dt        = aircraft%dt
	dt_output = aircraft%dt_output
	time_end  = aircraft%time_end

	sum_csolve = ''
	sum_ARE = ''

	do i = 1, 50
	   write (integer_list(i),'(i3)') i
	enddo

	if (aircraft%debug_mini) aircraft%debug = 0
	compute_A_store = control%compute_A   

c	What we are looking to do is to set all the xd to zero.  The inputs are all the
c	control variables.  We adjust the uc until all the xd are zero, or at leat 
c	close to zero.

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

c	Let's do our first pass for quadcopters.  Here, assuming lots of symmetry, we
c	have three variables, two motor control variables and one angle variable (pitch).

c	We assume we have an initial guess (provided by the input or by the previous
c	iterate) and so we then look for the solution.  Thee first one is simple, in
c	that we are just looking for hover (we will do this one to see if we know
c	what is going on).  We first find out the motor speed, then we find out
c	the vehavior, and we do it in a loop that has a "no intelligence" convergence.

c	This converges very fast.
c
c	tolerance = 1.d-7
c	error = 1.d0
c	i = 0
c
cc	goto 500  ! as a test, we are skipping this first region
c
c	do while (error.gt.tolerance)
c
c	   i = i + 1
c
c	   call motor_initialize (time, xdim, x, xd, xo, udim, uc,
c     .                 aircraft, wing, propeller, battery, control)
c
cc	   call fderiv (time, xdim, x, xd, xo, udim, uc,
c     .                 aircraft, wing, propeller, battery, control)
c
c	   write (6,*) ' uc ',(uc(j),j=1,4) 
c	   write (6,10) (xd(j),j=1,16)
c
c 10	   format(16f10.3)
c
c	   if (i.eq.1) then
c
c	      uc1previous = uc(1)
c	      if (xd(3).lt.0.d0) uc(1) = uc(1) - 0.01d0
c	      if (xd(3).gt.0.d0) uc(1) = uc(1) + 0.01d0
c	
c	   else
c
cc	      We have enough data to be building a derivative
c
c              dxd3 = (xd(3)-xd3previous)/(uc(1) - uc1previous)
c	      uc1previous = uc(1)
c	      uc(1) = uc(1) - xd(3)/dxd3
c
c	   endif
c
c	   xd3previous = xd(3)
c	   uc(2:4)     = uc(1)
c
c	   error = abs(xd(3))
c
c	enddo

c	Now we do a loop through a number of Unorth velocities.  We also need to consider pitch
c	which we will index with the pitch angle theta, becaause we don't want to deal with
c	all the possibilities of the quaternion.  It seems there is physics here that should
c	be very beneficial to us, but right now I am unclear on how to take advantage of it.
c	So we will try the brute force ameoba routine (sigh).  Let's do a loop on many iterations

c	
c	This is an outer loop to make three passes, one for lateral
c	and two for turning. The indents are not correct for this
c       loop

        tolerance = aircraft%tolerance
	tol = tolerance

c	This is an outer loop to make three passes, one for lateral
c	and two for turning.
	do jj = 0, 2

	 if (jj.eq.0) then
	    lateral = .true.
            turn1   = .false.
            turn2   = .false.
         else if (jj.eq.1) then
            lateral = .false.
            turn1   = .true.
            turn2   = .false.
	 else
            lateral = .false.
            turn1   = .false.
            turn2   = .true.
	 endif

	 if (lateral       ) n = udim + 1
	 if (turn1.or.turn2) n = udim + 2
	 np1 = n + 1

	 theta = 0.d0

	 Write (6,*)

 	 write (6,*) ' This routine finds steady solutions for lateral'
     .        ,' speeds of 0 to 50 m/s (Unorth).'
	 write (6,*) ' (If steady climbing is desired, set the '
     .              ,'appropriate wind speed Wdwind (positive down)).'
	 write (6,*) ' A steady state (trimmed) flight condition is'
     .             ,' achieved when UVWdot and PQRdot are zero.'
	 write (6,*) ' It is assumed that the flight direction is X'
     .             ,' and there is no yaw (psi = 0) or roll (phi = 0).'
	 write (6,*) ' Motor amps also includes amps lost in the ESC.'
         write (6,*) ' aircraft%simplex = '
     .               , integer_list(aircraft%simplex)
     .               ,' (0 = minpack/simplex + previous convergence,'
     .               ,' 1 = simplex + previous convergence,'
     .               ,' 2 = simplex + 0.5 start).'

	 speedmax     = 0.d0
	 distancemax  = 0.d0
  	 speedmax1    = 0.d0
	 distancemax1 = 0.d0

	 if (lateral) control%lateral_num_trim_max = 0  ! the number of trim states in the array.
	 if (turn1)   control%turn_1_num_trim_max  = 0
	 if (turn2)   control%turn_2_num_trim_max  = 0

	 jbegin = 0
	 if (turn1.or.turn2) jbegin = 1

	 iMINPACK = 0
	 iSIMPLEX = 0	
	 iARE     = 0
	 sum_csolve = ''
 	 sum_ARE = ''
         ifirst_notrim = .true.   ! 3/25/22

	 do j= jbegin, 50

	   jm1 = max(0, j - 1)

c	   We sweep up and down   - this didn't make any different for our tailsitter,
c	   so we dropped it.
c	   if (jj.le.50) then
c	      j   = jj
c              jm1 = jj - 1
c	      if (jm1.lt.0) jm1 = 0
c	   else
c	      j    = 101 - jj
c              jm1 = jj + 1
c              if (jm1.gt.50) jm1 = 50
c           endif

c	   Unorth = 5.d0*dfloat(j)  ! from 0 to 50 m/s
	   Unorth = dfloat(j)  ! from 0 to 50 m/s
c        Wdown = -3.d0 ! test case jdw 9/15/22
c           Unorth =  15.d0*cos(dfloat(j)*pi/(100.d0))   ! test case 9/15/22
c           Wdown  = -15.d0*sin(dfloat(j)*pi/(100.d0))
c        Unorth = 1.d0
c        Wdown  = -dfloat(j)
c	   if (j.eq.0) Unorth = 0.001d0  ! this did not seem to stabilize zero state
	   Veast  = 0.d0  ! these being nonzero is not yet implemented
	   Wdown  = 0.d0  ! notice it can be mimicked with wind speeds

	   if (turn1) radius = control%radius_1
	   if (turn2) radius = control%radius_2

	   UVWPQR(1) = Unorth
	   UVWPQR(2) = Veast
	   UVWPQR(3) = Wdown
	   UVWPQR(4) = 0.d0
	   UVWPQR(5) = 0.d0
	   UVWPQR(6) = 0.d0
	   if (turn1.or.turn2) UVWPQR(6) = UVWPQR(1)/radius
	   Unorth = UVWPQR(1)
	   Veast  = UVWPQR(2)
	   Wdown  = UVWPQR(3)
	   Pworld = UVWPQR(4)
	   Qworld = UVWPQR(5)
	   Rworld = UVWPQR(6)

c	   This brings up a very interesting question.  We will later decide
c	   that certain states are not trim states based on variout criteria,
c          and yet we are not using all the criteria during the solution phase
c          - i.e., we may want to come back and consider including all our
c          solution criteria in what we consider acceptable solutions.
c          To be explicit, we may want to include our current, power, and 
c          wing loading criteria to identify unallowable regions in fdcost
c          to push solutions into regions that don't have these problems.  Hmmm.  
c          2/17/22
	   iwarn = .false.
	   ifirst_notrim_flag = .true.

	   write (6,*)

	   if (lateral) then
	      temp = (((100.d0/2.54d0)/12.d0)/5280.d0)*3600.d0
	      write (6,30) Unorth - aircraft%Unwind,
     .                     Veast  - aircraft%Vewind,
     .                     Wdown  - aircraft%Wdwind,
     .                    (Unorth - aircraft%Unwind)*temp,
     .                    (Veast  - aircraft%Vewind)*temp,
     .                    (Wdown  - aircraft%Wdwind)*temp

	   else if (turn1.or.turn2) then

	      write (6,31) Unorth - aircraft%Unwind,
     .                     Veast  - aircraft%Vewind,
     .                     Wdown  - aircraft%Wdwind,
     .                     Pworld, Qworld, Rworld
	      write (6,32) radius, Rworld
	   endif

 30	   format(' Objective steady state speed is UVW world = '
     .             ,3f8.2,' m/s = ',3f8.2, ' miles per hour')

 31	   format(' Objective steady state speed is UVW world = '
     .             ,3f8.2,' m/s; PQR world = ',3f8.2, ' rad/s')

 32        format('  Turning radius ', f10.2, ' m (positive means '
     .                ,'clockwise looking down), Rworld = ', f10.4
     .                , ' radians/s')

c	   We use the previous values as one of the starting points (vertices) and then
c	   make some other guesses for the intial simplex.
c
c	   For this pass, we have three variables
c	   u(1) =  driver for leading edge
c	   u(3) =  driver for trailing edge
c	   theta = pitch angle
c
c          Our initial approach will be to try to set these values to zero through
c          a minimization.
c	   Unorth - Unorth set = 0
c	   Zdown               = 0
c	   Unorthdot           = 0
c	   Zdowndot            = 0
c	   All of these are achieved if UVWdot in the body frame are zero.
c	   

c	   We need initial estimates for the simplex.  For now it is hardwired
c	   As we try to get something of interest (for four variables, five verticies)
c   	   Recall, all control variables run from 0 to 1

	   m = 6 + aircraft%num_propellers  ! 6 accelerations and power for propellers
	   if (lateral)        n = udim + 1  ! all control channels plus the angle theta
	   if (turn1.or.turn2) n = udim + 2  ! all controls plus pitch and roll (theta and phi)
cjdw	   n = udim + 3  ! all control channels plus three independent quaternion components

c	   It turns out that m must be at least as large as n for the routines we are using
c	   in MINPACK.  We will set it to a larger value if necessary by adding some fake
c	   equations.
	   if (n.gt.m) then
	      write (6,*) ' It was necessary to make m = n for MINPACK;'
     .             ,' original n > m = ',n,m
              m = n
	   endif


c	   This if is by request - in the event the user has supplied their own trim states
	   if (.not.control%outside_trim_states) then  ! skip way down - search on variable name in a comment, no indents


c	   The initial condition if there is nothing else.
c	   Due to various issues, we choose to set them all equal to 0.5   8/9/21
	   do i = 1, udim
	      ya(i) = 0.5d0
	   enddo
	   theta = 0.d0 ! an extremenly important initial condition; otherwise the MINPACK
c	                  routines don't find the solution.
	   phi   = 0.d0
	   ya(n) = theta
	   if (turn1.or.turn2) ya(n-1) = phi ! the zero set for phi

c	   If available, we start at the previous speed's trim state   1/24/22
	   if (j.gt.0) then
	     if (lateral.or.(j.eq.1)) then    ! for the two turn states we start with hover
	      if (control%lateral_trim_if_0(jm1)) then
	         do i = 1, udim
                    uc(i) = control%lateral_trim_uc (jm1,i)
		    ya(i) = uc(i)
	         enddo
                 theta = control%lateral_trim_theta (jm1)
	         if (turn1.or.turn2) ya(n-1) = phi
		 ya(n) = theta
	      endif
	     else if (turn1) then
	      if (control%turn_1_trim_if_0(jm1)) then
	         do i = 1, udim
                    uc(i) = control%turn_1_trim_uc (jm1,i)
		    ya(i) = uc(i)
	         enddo
                 theta = control%turn_1_trim_theta (jm1)
		 ya(n) = theta
                 phi = control%turn_1_trim_phi (jm1)
	         ya(n-1) = phi
	      endif
	     else if (turn2) then
	      if (control%turn_2_trim_if_0(jm1)) then
	         do i = 1, udim
                    uc(i) = control%turn_2_trim_uc (jm1,i)
		    ya(i) = uc(i)
	         enddo
                 theta = control%turn_2_trim_theta (jm1)
		 ya(n) = theta
                 phi = control%turn_2_trim_phi (jm1)
	         ya(n-1) = phi
	      endif
	     endif
           endif

c	if (j.eq.51) aircraft%debug = 1
c	if (compute_A_store) control%compute_A = .false.


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
	   if (aircraft%simplex.eq.0) then
              call lmdif1(fdcost,m,n,ya,za,tol,info,iwa,wa,lwa,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)

c	      We save some diagnostics, even if there is no convergence
c	      First is the signed max acceleration
	      temp1 = xd(1)
	      do k = 2, 6
	         if (abs(xd(k)).gt.abs(temp1)) temp1 = xd(k)
	      enddo
c	      Next is the weighted cost that simplex uses
	      temp2 = 0.d0
	      do k = 1, 6 + aircraft%num_propellers
	         temp2 = temp2 + za(k)**2
	      enddo

	      if (lateral) then
	         control%lateral_trim_max_xd(j) = temp1
	         control%lateral_trim_fdcost(j) = temp2
	      else if (turn1) then
	         control%turn_1_trim_max_xd(j) = temp1
	         control%turn_1_trim_fdcost(j) = temp2
	      else if (turn2) then
	         control%turn_2_trim_max_xd(j) = temp1
	         control%turn_2_trim_fdcost(j) = temp2
	      endif
	      ifirst_notrim_flag = .false.

           endif

c          begin nonlinear simplex

c	   This is the nonliner simplex call, as an alternate to the MINPACK (or if there are 
c	   convergence problems)

	   nonlinearsimplex = .false.
	   if (abs(temp1).gt.aircraft%acceleration_tolerance) 
     .                nonlinearsimplex = .true.
           if (aircraft%simplex.ge.1) nonlinearsimplex = .true.
	
	   if (.not.nonlinearsimplex) then  ! MINPACK converged

c	      Since we now may be sweeping twice, we check to see if there is no trim state
c             or an existing that we are better than.

c	      This test is true if there is not a trim state or if the just-computed
c             trim state is better than the previous one.

	      itest0 = ( .not. control%lateral_trim_if_0(j)) .or.
     .             (control%lateral_trim_if_0(j).and.
     .             (temp2.lt.control%lateral_trim_fdcost(j)) )
	      itest1 = ( .not. control%turn_1_trim_if_0 (j)) .or.
     .             (control%turn_1_trim_if_0(j).and.
     .             (temp2.lt.control%turn_1_trim_fdcost (j)) )
	      itest2 = ( .not. control%turn_2_trim_if_0 (j)) .or.
     .             (control%turn_2_trim_if_0(j).and.
     .             (temp2.lt.control%turn_2_trim_fdcost(j)) )

	      if (lateral.and.itest0) then
                 iMINPACK = iMINPACK + 1
	         sum_csolve(j) = 'MINPACK'
	         theta = ya(n)
c	         We now save (though it may be temporary) the trim state
	         control%lateral_trim_if_0  (j)   = .true. ! to say we have a trim state even if no controls  1/24/22
	         do k = 1, udim
                    control%lateral_trim_uc (j,k) = uc(k) ! the lateral trim control settings
	         enddo
	         do k = 1, xdim 
	            control%lateral_trim_x  (j,k) = x(k )
	         enddo
                 control%lateral_trim_theta (j)   = ya(n)   ! the lateral trim pitch angle
                 control%lateral_num_trim_max     = j       ! the number of trim states in the array.
	         control%lateral_trim_speed (j)   = Unorth  ! the world frame north speed
	         used = 'Y'
	      else if (turn1.and.itest1) then
                 iMINPACK = iMINPACK + 1
	         sum_csolve(j) = 'MINPACK'
	         theta = ya(n)
		 phi   = ya(n-1)
	         control%turn_1_trim_if_0   (j)   = .true.
	         do k = 1, udim
                    control%turn_1_trim_uc  (j,k) = uc(k)
	         enddo
	         do k = 1, xdim 
	            control%turn_1_trim_x   (j,k) = x(k )
	         enddo
		 control%turn_1_trim_phi    (j)   = ya(n-1)
                 control%turn_1_trim_theta  (j)   = ya(n)
                 control%turn_1_num_trim_max      = j     
	         control%turn_1_trim_speed  (j)   = Unorth 
	         used = 'Y'
	      else if (turn2.and.itest2) then
                 iMINPACK = iMINPACK + 1
	         sum_csolve(j) = 'MINPACK'
	         theta = ya(n)
		 phi   = ya(n-1)
	         control%turn_2_trim_if_0   (j)   = .true.
	         do k = 1, udim
                    control%turn_2_trim_uc  (j,k) = uc(k)
	         enddo
	         do k = 1, xdim 
	            control%turn_2_trim_x   (j,k) = x(k )
	         enddo
		 control%turn_2_trim_phi    (j)   = ya(n-1)
                 control%turn_2_trim_theta  (j)   = ya(n)
                 control%turn_2_num_trim_max      = j     
	         control%turn_2_trim_speed  (j)   = Unorth 
	         used = 'Y'
	      else
	         used = 'N'
	      endif
	   else
	      used = 'N'
	   endif
           if (aircraft%simplex.eq.0) then
	      if (j.eq.0) then
                 write (6,61) temp1,temp2,used
	      else
	         if (lateral) write (6,62) temp1,temp2
     .                          ,control%lateral_trim_if_0(jm1),used
	         if (turn1) write (6,62) temp1,temp2
     .                          ,control%turn_1_trim_if_0(jm1),used
	         if (turn2) write (6,62) temp1,temp2
     .                       ,control%turn_2_trim_if_0(jm1),used
              endif
	   endif

 61	 format('   MINPACK       max xd, fdcost, previous, used       '
     .           ,3X,2(1pe23.15),1X,1X,1X,A1)
 62	 format('   MINPACK       max xd, fdcost, previous, used       '
     .           ,3X,2(1pe23.15),1X,L1,1X,A1)
 63	 format('   SIMPLEX pass, max xd, fdcost, previous, used, iter '
     .           ,I3,2(1pe23.15),1X,L1,1X,A1,I6)

c	   I want to try to "polish" the solution by running a simplex after
c	   a convergence, to see if that makes any difference.  jdw  1/26/22
c	   Actually, I want to do more than that.  I want to restart the
c	   both the minpack and simplex solution to see if we improve the
c	   solution or if they end up at a better place.  The question is 
c	   how best to do this.  I need to store the various states.

c          After many, many runs, it appears that a MINPACK solutions really
c	   cannot be improved, and certainly not by a simplex iteration.
c	   On the other hand, a simlex solution can be slightly improved
c	   by a minpack run.  Thus, that's what we will do.    jdw  2/4/22

c	   If often takes 3 simplex passes to converge.           

	   iter_total = 0

	   if (nonlinearsimplex.or.(aircraft%simplex.ge.1)) then

c	      Variables for the simplex algorithm.

c	      What we are looking to do is to set all the xd to zero.  The inputs are all the
c	      control variables.  We adjust the uc until all the xd are zero, or at leat 
c	      close to zero.

c	      Every call to fderiv is a new test state - we are not trying to fly.  An important
c	      point we have is that we need to set the motor speed, given there is a motor
c	      control.  This may prove to be quite difficult due to the fact that the propeller
c	      is based on a lookup table rather than a function.  We will use (for now) the Nelder
c	      Mead simplex routine implementation I wrote 30 years ago.
c
c	      Thus, all x variables should be zero for each call
c	
c   	      Recall, all control variables run from 0 to 1

c	      We plan to go through the algorithm twice, if necessary.

cn	write (6,*) ' Here 6 n, udim, np1 ',n,udim,np1

c	      n   = udim + 1
c	      np1 = n    + 1
	      lsolved = .false.
	
	      do pass = 1, 7

	         if ((pass.eq.1).and.(j.gt.0)
     .              .and.(aircraft%simplex.eq.1)) then  ! it will use previous found
c                   state if simplex = 1, otherwise it will not (uses 0.5 state).  jdw  3/9/22
c                   We start at the previous speed's trim state, if available.
c	            The fact that we are here says this start did not work for
c                   MINPACK.
                    previous_converge = .false.
	            if (lateral.and.control%lateral_trim_if_0(jm1)) then

c	               The coding is gone now, but a forward difference led to worse performance... why is that?

	               do i = 1, udim
                          uc(i) = control%lateral_trim_uc (jm1,i)
	               enddo
                       theta = control%lateral_trim_theta (jm1)
	               do i = 1, udim
	                  if (uc(i).lt.0.9d0) then
                             diff(i) =  0.1d0
	                  else	
		             diff(i) = -0.1d0
	                  endif
	               enddo
	               if (theta.lt.halfpi-0.1d0) then
                           diff(n) = 0.1d0
	               else
	                   diff(n) = -0.1d0
	               endif
                       previous_converge = .true.  ! a little minsomer now, but we mean previous speed

	            else if (turn1.and.control%turn_1_trim_if_0(jm1)) then

	               do i = 1, udim
                          uc(i) = control%turn_1_trim_uc (jm1,i)
	               enddo
                       theta = control%turn_1_trim_theta (jm1)
                       phi   = control%turn_1_trim_phi   (jm1)
	               do i = 1, udim
	                  if (uc(i).lt.0.9d0) then
                             diff(i) =  0.1d0
	                  else	
		             diff(i) = -0.1d0
	                  endif
	               enddo
	               if (theta.lt.halfpi-0.1d0) then
                           diff(n) = 0.1d0
	               else
	                   diff(n) = -0.1d0
	               endif
	               if (phi.lt.halfpi-0.1d0) then
                           diff(n-1) = 0.1d0
	               else
	                   diff(n-1) = -0.1d0
	               endif
                       previous_converge = .true.  ! a little minsomer now, but we mean previous speed

	            else if (turn2.and.control%turn_2_trim_if_0(jm1)) then

	               do i = 1, udim
                          uc(i) = control%turn_2_trim_uc (jm1,i)
	               enddo
                       theta = control%turn_2_trim_theta (jm1)
                       phi   = control%turn_2_trim_phi   (jm1)
	               do i = 1, udim
	                  if (uc(i).lt.0.9d0) then
                             diff(i) =  0.1d0
	                  else	
		             diff(i) = -0.1d0
	                  endif
	               enddo
	               if (theta.lt.halfpi-0.1d0) then
                           diff(n) = 0.1d0
	               else
	                   diff(n) = -0.1d0
	               endif
	               if (phi.lt.halfpi-0.1d0) then
                           diff(n-1) = 0.1d0
	               else
	                   diff(n-1) = -0.1d0
	               endif
                       previous_converge = .true.  ! a little minsomer now, but we mean previous speed

	            else 
c	               Otherwise we start here
	               do i = 1, udim
	                  uc(i)   = 0.5d0
		          diff(i) = 0.1d0
	               enddo
	               phi     = 0.d0
		       if (turn1.or.turn2) diff(n-1) = 0.1d0
	               theta   = 0.d0
		       diff(n) = 0.1d0
	            endif

	         else if (pass.eq.1) then
c	            Otherwise we start here

	            do i = 1, udim
	               uc(i)   = 0.5d0
		       diff(i) = 0.1d0
	            enddo
	            phi     = 0.d0
		    if (turn1.or.turn2) diff(n-1) = 0.1d0
	            theta   = 0.d0
		    diff(n) = 0.1d0

	         else if ((pass.eq.2).and.(.not.lsolved)) then
c	            Otherwise we start here

	            do i = 1, udim
	               uc(i)   = 0.3d0
		       diff(i) = 0.4d0
	            enddo
                    phi     = 0.d0
		    if (turn1.or.turn2) diff(n-1) = 0.1d0
	            theta   = 0.d0
		    diff(n) = 0.4d0

		 else if ((pass.eq.3).and.(.not.lsolved)) then
c	            The other start didn't work, so we try here.

	            do i = 1, udim
	               uc(i)   = 0.7d0
		       diff(i) = -0.4d0
	            enddo
	            phi     =  0.d0
		    if (turn1.or.turn2) diff(n-1) = 0.1d0
	            theta   =  0.5d0
		    diff(n) = -0.4d0

c	         else if (pass_start.eq.2) then
c	            We are starting at the MINPACK solution
c	            As stated above, this never seemed to improve anything
c	            do i = 1, udim
c	               if (uc(i).lt.0.9d0) then
c                          diff(i) =  0.1d0
c	               else	
c		          diff(i) = -0.1d0
c	               endif
c	            enddo
c	            if (theta.lt.halfpi-0.1d0) then
c                        diff(n) = 0.1d0
c	            else
c	                diff(n) = -0.1d0
c	            endif

	         else
c	            Otherwise we essentially start with what's in the array from the last try
c	            We set the diffs
	            do i = 1, udim
	               if (uc(i).lt.0.9d0) then
                          diff(i) =  0.1d0
	               else	
		          diff(i) = -0.1d0
	               endif
	            enddo
	            if (theta.lt.halfpi-0.1d0) then
                        diff(n) = 0.1d0
	            else
	                diff(n) = -0.1d0
	            endif
		    if (turn1.or.turn2) then
	               if (phi.lt.halfpi-0.1d0) then
                           diff(n-1) = 0.1d0
	               else
	                   diff(n-1) = -0.1d0
	               endif
		    endif

	         endif

	         do i = 1, np1
	            do k = 1, udim
	   	       pam(i,k) = uc(k)
	            enddo
		    if (turn1.or.turn2) pam(i,n-1) = phi
	            pam(i,n) = theta
	         enddo
	         do i = 1, n
	            pam(i+1,i) = pam(i+1,i) + diff(i)  ! try an offset for everything
	         enddo

c                Initialize these guys
                 do i = 1, np1
		    do k = 1, n
                       ya(k) = pam(i,k)
		    enddo
		    call fdcost(m, n, ya, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
		    yam(i) = 0.d0
                    do k = 1, m
		       yam(i) = yam(i) + za(k)**2
		    enddo
                 enddo

                 call simplex (pdim,pam,yam,np1,n,n,tolerance,iter,
cjdw              call simplex (pam,yam,4,3,3,tolerance,iter,
     .	               m,n,UVWPQR,
     .                 time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)

	        iter_total = iter_total + iter

cn              write (6,*) ' iter, uc1, uc2, theta, rdcost  ',iter
cn     .          ,uc(1), uc(2), xo(6), yam(1)  ! these are most recent call

c	         I don't know if the most recent call is the best, so we are
c	         going to go through the list to find the low
	         ilow = 1
	         do i = 2, n
	            if (yam(i).lt.yam(ilow)) ilow = i
	         enddo
c	         This is the weighted cost
	         temp2 = yam(ilow)
	     
	         do i = 1, udim
	            uc(i) = pam(ilow,i)
		    ya(i) = uc(i)
	         enddo
	         theta = pam(ilow,n)
		 ya(n) = theta
		 if (turn1.or.turn2) then
                    phi     = pam(ilow,n-1)
                    ya(n-1) = phi
	         endif

c                Notice this xd may not correspond to ilow, but it should be close if simplex converged
	         lsolved = .true.
c	         First is the signed max acceleration
         	 temp1 = xd(1)
	         do k = 2, 6
	            if (abs(xd(k)).gt.abs(temp1)) temp1 = xd(k)
	         enddo
	         if (abs(temp1).gt.aircraft%acceleration_tolerance)
     .              lsolved = .false.

	         used = 'N'
                 if (pass.gt.1) 
     .              previous_converge = control%lateral_trim_if_0 (j)
	         if (lsolved) then
		    iSIMPLEX = iSIMPLEX + 1
	            ibetter = .false.

	            if (lateral.and.control%lateral_trim_if_0(j)) then 
c	               There is a previous solution.  Is it better?
                       if (temp2.lt.control%lateral_trim_fdcost(j)) 
     .                    ibetter = .true.
	            else if (turn1.and.control%turn_1_trim_if_0(j)) then
	               if (temp2.lt.control%turn_1_trim_fdcost(j))
     .                    ibetter = .true.
	            else if (turn2.and.control%turn_2_trim_if_0(j)) then
	               if (temp2.lt.control%turn_2_trim_fdcost(j)) 
     .                    ibetter = .true.
	            else
c                      This is the first solution.
                       ibetter = .true.
	            endif

	            if (ibetter) then
	               sum_csolve(j) = 'SIMPLEX'
	               if (lateral) then
	                  control%lateral_trim_if_0  (j)   = .true.  ! to say we have a trim state even if no controls  1/24/22
                          do k = 1, udim
                             control%lateral_trim_uc (j,k) = uc(k)   ! the lateral trim control settings
	                  enddo
	                  do k = 1, xdim 
	                     control%lateral_trim_x  (j,k) = x (k)
	                  enddo
                          control%lateral_trim_theta (j)   = ya(n)   ! the lateral trim pitch angle
                          control%lateral_num_trim_max     = j       ! the number of trim states in the array.
	                  control%lateral_trim_speed (j)   = Unorth  ! the world frame north speed
	                  control%lateral_trim_max_xd(j)   = temp1
	                  control%lateral_trim_fdcost(j)   = temp2
	               else if (turn1) then
	                  control%turn_1_trim_if_0  (j)   = .true.  ! to say we have a trim state even if no controls 
                          do k = 1, udim
                             control%turn_1_trim_uc (j,k) = uc(k)
	                  enddo
	                  do k = 1, xdim 
	                     control%turn_1_trim_x  (j,k) = x (k)
	                  enddo
                          control%turn_1_trim_theta (j)   = ya(n  ) ! the trim pitch angle
		          control%turn_1_trim_phi   (j)   = ya(n-1) ! the trim roll angle
                          control%turn_1_num_trim_max     = j       ! the number of trim states in the array.
	                  control%turn_1_trim_speed (j)   = Unorth  ! the world frame north speed
	                  control%turn_1_trim_max_xd(j)   = temp1
	                  control%turn_1_trim_fdcost(j)   = temp2
	               else if (turn2) then
	                  control%turn_2_trim_if_0  (j)   = .true.  ! to say we have a trim state even if no controls 
                          do k = 1, udim
                             control%turn_2_trim_uc (j,k) = uc(k)
	                  enddo
	                  do k = 1, xdim 
	                     control%turn_2_trim_x  (j,k) = x (k)
	                  enddo
                          control%turn_2_trim_theta (j)   = ya(n  ) ! the trim pitch angle
		          control%turn_2_trim_phi   (j)   = ya(n-1) ! the trim roll angle
                          control%turn_2_num_trim_max     = j       ! the number of trim states in the array.
	                  control%turn_2_trim_speed (j)   = Unorth  ! the world frame north speed
	                  control%turn_2_trim_max_xd(j)   = temp1
	                  control%turn_2_trim_fdcost(j)   = temp2
	               endif
	               used = 'Y'
	            endif

	            write (6,63) pass,temp1,temp2,
     .                           previous_converge,used,iter

		    goto 2000  ! if we have a solution, we stick with it

	         else if (ifirst_notrim_flag) then
	            if (lateral) then
	               control%lateral_trim_max_xd(j) = temp1
	               control%lateral_trim_fdcost(j) = temp2
	            else if (turn1) then
	               control%turn_1_trim_max_xd(j) = temp1
	               control%turn_1_trim_fdcost(j) = temp2
	            else if (turn2) then
	               control%turn_2_trim_max_xd(j) = temp1
	               control%turn_2_trim_fdcost(j) = temp2
	            endif
	            ifirst_notrim_flag = .false.

                 else
c                   This did not converge, but is the cost any better if there is no solution
                    if (lateral.and.
     .                 (.not.control%lateral_trim_if_0 (j))) then 
                       if (abs(temp1).lt.
     .                     abs(control%lateral_trim_max_xd(j))) then
	                  control%lateral_trim_max_xd(j) = temp1
	                  control%lateral_trim_fdcost(j) = temp2
	               endif
                    else if (turn1.and.
     .                 (.not.control%turn_1_trim_if_0 (j))) then 
                       if (abs(temp1).lt.
     .                     abs(control%turn_1_trim_max_xd(j))) then
	                  control%turn_1_trim_max_xd(j) = temp1
	                  control%turn_1_trim_fdcost(j) = temp2
	               endif
                    else if (turn2.and.
     .                 (.not.control%turn_2_trim_if_0 (j))) then 
                       if (abs(temp1).lt.
     .                     abs(control%turn_2_trim_max_xd(j))) then
	                  control%turn_2_trim_max_xd(j) = temp1
	                  control%turn_2_trim_fdcost(j) = temp2
	               endif
	            endif
	            write (6,63) pass,temp1,temp2,
     .                           previous_converge,used,iter

	         endif

	      enddo ! to pass

 2000	      continue

	   endif  ! to nonlinear simplex

c	   end nonlinear simplex
c        write (6,*) ' Here in simplex 1, xd ',xd(1:6)


c	   Now we need to make sure the various variables are their best values
	   if (lateral.and.control%lateral_trim_if_0 (j)) then
              do k = 1, udim
                 uc(k) = control%lateral_trim_uc (j,k)
	         ya(k) = uc(k)
	      enddo
              ya(n) = control%lateral_trim_theta (j)
	      theta = ya(n)
c	      We call fdcost to recover xd and xo (needed below)
	      call fdcost(m, n, ya, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	   else if (turn1.and.control%turn_1_trim_if_0 (j)) then
              do k = 1, udim
                 uc(k) = control%turn_1_trim_uc (j,k)
	         ya(k) = uc(k)
	      enddo
              ya(n-1) = control%turn_1_trim_phi (j)
              ya(n  ) = control%turn_1_trim_theta (j)
	      phi     = ya(n-1)
	      theta   = ya(n  )
c	      We call fdcost to recover xd and xo (needed below)
	      call fdcost(m, n, ya, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	   else if (turn2.and.control%turn_2_trim_if_0 (j)) then
              do k = 1, udim
                 uc(k) = control%turn_2_trim_uc (j,k)
	         ya(k) = uc(k)
	      enddo
              ya(n-1) = control%turn_2_trim_phi (j)
              ya(n  ) = control%turn_2_trim_theta (j)
	      phi     = ya(n-1)
	      theta   = ya(n  )
c	      We call fdcost to recover xd and xo (needed below)
	      call fdcost(m, n, ya, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	   endif

c	   We see what happens with a second minpack call.  Simplex never actually
c	   improved the minpack solution in my test case.  I wonder if a second
c	   call to minpack could potentially improve a simplex result.

	   if (nonlinearsimplex.and.(aircraft%simplex.eq.0)) then  ! i.e., we called the simplex routine
              call lmdif1(fdcost,m,n,ya,za,tol,info,iwa,wa,lwa,
     .              UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .              aircraft, wing, propeller, battery, control)
	      lsolved = .true.
c	      First is the signed max acceleration
              temp1 = xd(1)
	      do k = 2, 6
	         if (abs(xd(k)).gt.abs(temp1)) temp1 = xd(k)
	      enddo
	      if (abs(temp1).gt.aircraft%acceleration_tolerance)
     .           lsolved = .false.
	      temp2 = 0.d0
	      do k = 1, 6 + aircraft%num_propellers
	         temp2 = temp2 + za(k)**2
	      enddo

	      if (lateral) previous_converge 
     .           = control%lateral_trim_if_0 (j)
	      if (turn1) previous_converge = control%turn_1_trim_if_0(j)
	      if (turn2) previous_converge = control%turn_2_trim_if_0(j)

	      ibetter = .false.
	      if (lsolved) then
c	         A solution was found.  Now the question arises as to whether it is better
	         if (lateral.and.control%lateral_trim_if_0(j)) then 
c	            There is a previous solution.  Is it better?
	            if (temp2.lt.control%lateral_trim_fdcost(j)) then
                       ibetter = .true.
	               sum_csolve(j) = 'SIM/MIN'
	            endif
	         else if (turn1.and.control%turn_1_trim_if_0(j)) then
	            if (temp2.lt.control%turn_1_trim_fdcost(j)) then
                       ibetter = .true.
	               sum_csolve(j) = 'SIM/MIN'
	            endif
	         else if (turn2.and.control%turn_2_trim_if_0(j)) then
	            if (temp2.lt.control%turn_2_trim_fdcost(j)) then
                       ibetter = .true.
	               sum_csolve(j) = 'SIM/MIN'
	            endif
	         else
c                   This is the first solution.
                    ibetter = .true.
		    sum_csolve(j) = 'MINPACK'
		    iMINPACK = iMINPACK + 1
	         endif
	      endif

	      if (ibetter) then 
c	         A better solution was found.

		 if (lateral) then
	            control%lateral_trim_if_0  (j)   = .true.  ! to say we have a trim state even if no controls  1/24/22
                    do k = 1, udim
                       control%lateral_trim_uc (j,k) = uc(k)   ! the lateral trim control settings
	            enddo
	            do k = 1, xdim 
	               control%lateral_trim_x  (j,k) = x (k)
	            enddo
                    control%lateral_trim_theta (j)   = ya(n)   ! the lateral trim pitch angle
                    control%lateral_num_trim_max     = j       ! the number of trim states in the array.
	            control%lateral_trim_speed (j)   = Unorth  ! the world frame north speed
	            control%lateral_trim_max_xd(j)   = temp1
	            control%lateral_trim_fdcost(j)   = temp2
		 else if (turn1) then
	            control%turn_1_trim_if_0  (j)   = .true.  ! to say we have a trim state even if no controls  1/24/22
                    do k = 1, udim
                       control%turn_1_trim_uc (j,k) = uc(k)   ! the trim control settings
	            enddo
	            do k = 1, xdim 
	               control%turn_1_trim_x  (j,k) = x (k)
	            enddo
                    control%turn_1_trim_theta (j)   = ya(n  ) ! the trim pitch angle
                    control%turn_1_trim_phi   (j)   = ya(n-1) ! the trim roll angle
                    control%turn_1_num_trim_max     = j       ! the number of trim states in the array.
	            control%turn_1_trim_speed (j)   = Unorth  ! the world frame north speed
	            control%turn_1_trim_max_xd(j)   = temp1
	            control%turn_1_trim_fdcost(j)   = temp2
		 else if (turn2) then
	            control%turn_2_trim_if_0  (j)   = .true.  ! to say we have a trim state even if no controls  1/24/22
                    do k = 1, udim
                       control%turn_2_trim_uc (j,k) = uc(k)   ! the trim control settings
	            enddo
	            do k = 1, xdim 
	               control%turn_2_trim_x  (j,k) = x (k)
	            enddo
                    control%turn_2_trim_theta (j)   = ya(n  ) ! the trim pitch angle
                    control%turn_2_trim_phi   (j)   = ya(n-1) ! the trim roll angle
                    control%turn_2_num_trim_max     = j       ! the number of trim states in the array.
	            control%turn_2_trim_speed (j)   = Unorth  ! the world frame north speed
	            control%turn_2_trim_max_xd(j)   = temp1
	            control%turn_2_trim_fdcost(j)   = temp2
	         endif
	         used = 'Y'
	      else
c	         If MINPACK does not converge to a solution, we restore to a previous trim state, if available
	         if (lateral.and.control%lateral_trim_if_0 (j)) then
                    do k = 1, udim
                       uc(k) = control%lateral_trim_uc (j,k)
	               ya(k) = uc(k)
	            enddo
                    ya(n) = control%lateral_trim_theta (j)
	            theta = ya(n)
c	            We call fdcost to recover xd and xo (needed below)
	            call fdcost(m, n, ya, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	            used = 'N'
	         endif
	         if (turn1.and.control%turn_1_trim_if_0 (j)) then
                    do k = 1, udim
                       uc(k) = control%turn_1_trim_uc (j,k)
	               ya(k) = uc(k)
	            enddo
	            ya(n-1) = control%turn_1_trim_phi   (j)
                    ya(n  ) = control%turn_1_trim_theta (j)
	            phi     = ya(n-1)
	            theta   = ya(n  )
c	            We call fdcost to recover xd and xo (needed below)
	            call fdcost(m, n, ya, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	            used = 'N'
	         endif
	         if (turn2.and.control%turn_2_trim_if_0 (j)) then
                    do k = 1, udim
                       uc(k) = control%turn_2_trim_uc (j,k)
	               ya(k) = uc(k)
	            enddo
	            ya(n-1) = control%turn_2_trim_phi   (j)
                    ya(n  ) = control%turn_2_trim_theta (j)
	            phi     = ya(n-1)
	            theta   = ya(n  )
c	            We call fdcost to recover xd and xo (needed below)
	            call fdcost(m, n, ya, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	            used = 'N'
	         endif
              endif
	      write (6,62) temp1,temp2
     .                          ,previous_converge,used
	   endif
c        write (6,*) ' Here in simplex 2, xd ',xd(1:6)

c	   To simplify other if statements, we store the hover state in the 0 turn trim states.
           if ((j.eq.0).and.control%lateral_trim_if_0(j)) then
	      control%turn_1_trim_if_0 (j) = .true. ! to say we have a trim state even if no controls  1/24/22
              do k = 1, udim
                 control%turn_1_trim_uc(j,k) 
     .                  = control%lateral_trim_uc (j,k)
	      enddo
	      do k = 1, xdim 
	         control%turn_1_trim_x(j,k) 
     .                  = control%lateral_trim_x(j,k)
	      enddo
              control%turn_1_trim_theta(j) 
     .                  = control%lateral_trim_theta (j)
              control%turn_1_trim_phi  (j) = 0.d0
              control%turn_1_num_trim_max      
     .                  = control%lateral_num_trim_max
	      control%turn_1_trim_speed (j) 
     .                  = control%lateral_trim_speed (j)
	      control%turn_1_trim_max_xd(j) 
     .                  = control%lateral_trim_max_xd(j)
	      control%turn_1_trim_fdcost(j) 
     .                  = control%lateral_trim_fdcost(j)

	      control%turn_2_trim_if_0 (j) = .true. ! to say we have a trim state even if no controls  1/24/22
              do k = 1, udim
                 control%turn_2_trim_uc(j,k) 
     .                  = control%lateral_trim_uc (j,k)
	      enddo
	      do k = 1, xdim 
	         control%turn_2_trim_x(j,k) 
     .                  = control%lateral_trim_x(j,k)
	      enddo
              control%turn_2_trim_theta(j) 
     .                  = control%lateral_trim_theta (j)
              control%turn_2_trim_phi  (j) = 0.d0
              control%turn_2_num_trim_max      
     .                  = control%lateral_num_trim_max
	      control%turn_2_trim_speed (j) 
     .                  = control%lateral_trim_speed (j)
	      control%turn_2_trim_max_xd(j) 
     .                  = control%lateral_trim_max_xd(j)
	      control%turn_2_trim_fdcost(j) 
     .                  = control%lateral_trim_fdcost(j)
	   endif

	   write (6,21) info
 21	   format('   Finished  MINPACK lmdif1 call; info = ',i2, 
     .            ' (should be 1, 2 or 3; see MINPACK documentation)')
	   if (nonlinearsimplex) write (6,19) pass, iter_total
 19	   format('   Nonlinear SIMPLEX invoked ',i1
     .            ,' time(s): total iterations = ',i5)
	   write (6,20) ' UVW world, body   (m/s)',xd(11),xd(12),xd(13)
     .                                    ,x(1),x(2),x(3)
	   write (6,20) ' UVWdot,PQRdot (m|r/s^2)',xd(1:6)
	   do k = 1, 6
	      if (abs(xd(k)).gt.aircraft%acceleration_tolerance) then
                 write (6,*) '   Warning: at least one value of '
     .                      ,'UVWdot or PQRdot is large enough '
     .                      ,'that steady flight is unlikely'
     .                      ,' (not a trim state).'
	         iwarn = .true.
	         goto 96
	      endif
	   enddo
 96	   continue

	   else  ! this is to the user supplying their own trim state
c	      means control%outside_trim_states is true
c	      This is in case the user provides their own trim states -
c	      we still perform an fdcost to evaluate the state
	      if (lateral.and.control%lateral_trim_if_0 (j)) then
                 do k = 1, udim
                    uc(k) = control%lateral_trim_uc (j,k)
	            ya(k) = uc(k)
	         enddo
                 ya(n) = control%lateral_trim_theta (j)
	         theta = ya(n)
c	         We call fdcost to recover xd and xo (needed below)
	         call fdcost(m, n, ya, za, iflag,
     .               UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	         write (6,*) '  User specified trim state '
c                We fill out the rest of the trim states
	         do k = 1, xdim 
	            control%lateral_trim_x  (j,k) = x (k)
	         enddo
                 control%lateral_trim_theta (j)   = ya(n)   ! the lateral trim pitch angle
                 control%lateral_num_trim_max     = j       ! the number of trim states in the array.
	         if (control%lateral_trim_speed (j).ne.Unorth)
     .              write (6,*) '  Warning: Unorth .ne. trim speed '
     .                        ,Unorth, control%lateral_trim_speed (j)
	      endif
	      if (turn1.and.control%turn_1_trim_if_0 (j)) then
                 do k = 1, udim
                    uc(k) = control%turn_1_trim_uc (j,k)
	            ya(k) = uc(k)
	         enddo
                 ya(n-1) = control%turn_1_trim_phi   (j)
                 ya(n  ) = control%turn_1_trim_theta (j)
	         phi     = ya(n-1)
	         theta   = ya(n  )
c	         We call fdcost to recover xd and xo (needed below)
	         call fdcost(m, n, ya, za, iflag,
     .               UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	         write (6,*) '  User specified trim state '
c                We fill out the rest of the trim states
	         do k = 1, xdim 
	            control%turn_1_trim_x  (j,k) = x (k)
	         enddo
                 control%turn_1_trim_theta (j)   = ya(n)   ! the lateral trim pitch angle
                 control%turn_1_num_trim_max     = j       ! the number of trim states in the array.
	         if (control%turn_1_trim_speed (j).ne.Unorth)
     .              write (6,*) '  Warning: Unorth .ne. trim speed '
     .                        ,Unorth, control%turn_1_trim_speed (j)
	      endif
	      if (turn2.and.control%turn_2_trim_if_0 (j)) then
                 do k = 1, udim
                    uc(k) = control%turn_2_trim_uc (j,k)
	            ya(k) = uc(k)
	         enddo
                 ya(n-1) = control%turn_2_trim_phi   (j)
                 ya(n  ) = control%turn_2_trim_theta (j)
	         phi     = ya(n-1)
	         theta   = ya(n  )
c	         We call fdcost to recover xd and xo (needed below)
	         call fdcost(m, n, ya, za, iflag,
     .               UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	         write (6,*) '  User specified trim state '
c                We fill out the rest of the trim states
	         do k = 1, xdim 
	            control%turn_2_trim_x  (j,k) = x (k)
	         enddo
                 control%turn_2_trim_theta (j)   = ya(n)   ! the lateral trim pitch angle
                 control%turn_2_num_trim_max     = j       ! the number of trim states in the array.
	         if (control%turn_2_trim_speed (j).ne.Unorth)
     .              write (6,*) '  Warning: Unorth .ne. trim speed '
     .                        ,Unorth, control%turn_2_trim_speed (j)
	      endif
	   endif  ! to the user supplying their own trim state


c	   write (6,20) ' Pitch angle theta (deg)',rad_to_deg*ya(n)

	   if (turn1.or.turn2) 
     .        write (6,20) ' Roll  angle phi   (deg)',rad_to_deg*phi
	   write (6,20) ' Pitch angle theta (deg)',rad_to_deg*theta
c	   write (6,20) ' Yaw   angle psi   (deg)',rad_to_deg*xo(7)


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

c	   Now we print out wing load information for stuctural considerations
	   np = aircraft%num_propellers
	   nw = aircraft%num_wings
           np30 = 30 + 5*np
	   if (nw.le.6) then
	      write (6,22) ' Wing Ld(N) ', 1, nw, xo(np30+1:np30+nw)
	   else if (nw.le.12) then
	      write (6,20) ' Wing Ld(N)  1 -  6     ', xo(np30+1:np30+6)
	      write (6,22) ' Wing Ld(N) ', 7, nw, xo(np30+7:np30+nw)
	   else if (nw.le.18) then
	      write (6,20) ' Wing Ld(N)  1 -  6     ', xo(np30+1:np30+6)
	      write (6,20) ' Wing Ld(N)  7 - 12     ',xo(np30+7:np30+12)
	      write (6,22) ' Wing Ld(N) ',13, nw, xo(np30+13:np30+nw)
	   else if (nw.le.24) then
	      write (6,20) ' Wing Ld(N)  1 -  6     ', xo(np30+1:np30+6)
	      write (6,20) ' Wing Ld(N)  7 - 12     ',xo(np30+7:np30+12)
	      write (6,20)' Wing Ld(N) 13 - 18     ',xo(np30+13:np30+18)
	      write (6,22) ' Wing Ld(N) ',19, nw, xo(np30+19:np30+nw)
	   else  ! lots of controls - if they have more than 30 they can ask for help
	      write (6,20) ' Wing Ld(N)  1 -  6     ', xo(np30+1:np30+6)
	      write (6,20) ' Wing Ld(N)  7 - 12     ',xo(np30+7:np30+12)
	      write (6,20)' Wing Ld(N) 13 - 18     ',xo(np30+13:np30+18)
	      write (6,20)' Wing Ld(N) 19 - 24     ',xo(np30+19:np30+24)
	      write (6,22) ' Wing Ld(N) ',25, min(nw,30),
     .                        xo(np30+25:np30+min(nw,30))
	   endif
	   sum_wl(j)  = 0.d0
	   sum_wl_max = 0.d0
           i_high_wing_load_number = 0
	   do k = 1, nw
              temp = xo(np30+k)/wing(k)%max_load
	      if (wing(k)%max_load.gt.0.d0)
     .           sum_wl(j) = max(sum_wl(j),temp)
	      sum_wl_max = max(sum_wl_max,sum_wl(j))
	      if (temp.gt.1.d0) then
                 i_high_wing_load_number = i_high_wing_load_number + 1
	         i_high_wing_load_list (i_high_wing_load_number) = k
	      endif
	   enddo
	   if (sum_wl(j).gt.1.d0) then
              write (6,*) '   Warning: at least one wing is loaded'
     .                      ,' beyond max load,'
     .                ,' hence structural failure likely.  Wings:'
     .            ,integer_list(
     .              i_high_wing_load_list(1:i_high_wing_load_number))
              iwarn = .true.
              if (aircraft%ignore_wingload) iwarn = .false.  ! still allow the trim state  3/25/22
	   endif

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
           np30 = 30 + 4*np
	   if (np.le.6) then
	      write (6,22) ' Motor RPM  ', 1, np, x(13+ 1:13+np)
     .                             * 60.d0/twopi
	      write (6,22) ' Thrust (N) ', 1, np, xo(np30+1:np30+np)
	   else if (np.le.12) then
	      write (6,20) ' Motor RPM   1 -  6     ', x(13+ 1:13+ 6) 
     .                             * 60.d0/twopi
	      write (6,20) ' Thrust (N)  1 -  6     ', xo(np30+1:np30+6)
	      write (6,22) ' Motor RPM  ', 7, np, x(13+ 7:13+np)
     .                             * 60.d0/twopi
	      write (6,22) ' Thrust (N) ', 7, np, xo(np30+7:np30+np)
	   else if (np.le.18) then
	      write (6,20) ' Motor RPM   1 -  6     ', x(13+ 1:13+ 6) 
     .                             * 60.d0/twopi
	      write (6,20) ' Thrust (N)  1 -  6     ', xo(np30+1:np30+6)
	      write (6,20) ' Motor RPM   7 - 12     ', x(13+ 7:13+12) 
     .                             * 60.d0/twopi
	      write (6,20) ' Thrust (N)  7 - 12     ',xo(np30+7:np30+12)
	      write (6,22) ' Motor RPM  ',13, np, x(13+13:13+np)
     .                             * 60.d0/twopi
	      write (6,22) ' Thrust (N) ',13, np, xo(np30+13:np30+np)
	   else if (np.le.24) then
	      write (6,20) ' Motor RPM   1 -  6     ', x(13+ 1:13+ 6) 
     .                             * 60.d0/twopi
	      write (6,20) ' Thrust (N)  1 -  6     ', xo(np30+1:np30+6)
	      write (6,20) ' Motor RPM   7 - 12     ', x(13+ 7:13+12) 
     .                             * 60.d0/twopi
	      write (6,20) ' Thrust (N)  7 - 12     ',xo(np30+7:np30+12)
	      write (6,20) ' Motor RPM  13 - 18     ', x(13+13:13+18) 
     .                             * 60.d0/twopi
	      write (6,20)' Thrust (N) 13 - 18     ',xo(np30+13:np30+18)
	      write (6,22) ' Motor RPM  ',19, np, x(13+19:13+np)
     .                             * 60.d0/twopi
	      write (6,22) ' Thrust (N) ',19, np, xo(np30+19:np30+np)
	   else  ! lots of controls - if they have more than 30 they can ask for help
	      write (6,20) ' Motor RPM   1 -  6     ', x(13+ 1:13+ 6) 
     .                             * 60.d0/twopi
	      write (6,20) ' Thrust (N)  1 -  6     ', xo(np30+1:np30+6)
	      write (6,20) ' Motor RPM   7 - 12     ', x(13+ 7:13+12) 
     .                             * 60.d0/twopi
	      write (6,20) ' Thrust (N)  7 - 12     ',xo(np30+7:np30+12)
	      write (6,20) ' Motor RPM  13 - 18     ', x(13+13:13+18) 
     .                             * 60.d0/twopi
	      write (6,20)' Thrust (N) 13 - 18     ',xo(np30+13:np30+18)
	      write (6,20) ' Motor RPM  19 - 24     ', x(13+19:13+24) 
     .                             * 60.d0/twopi
	      write (6,20)' Thrust (N) 19 - 24     ',xo(np30+19:np30+24)
	      write (6,22) ' Motor RPM  ',25, min(np,30),
     .                         x(13+25:min(13+np,13+30)) 
     .                             * 60.d0/twopi
	      write (6,22) ' Thrust (N) ',25, min(np,30),
     .                        xo(np30+25:np30+min(np,30))
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
	      write (6,22) ' Motor Amps ',25, min(29+np+np,29+np+30),  !  fixed format number = 22 (was 20)  4/7/22
     .                            xo(29+np+25:min(29+np+np,29+np+30)) 
	   endif

c	   Now we look at whether we are exceeding the motor's allowed amperage
           i_high_motor_amps_number  = 0
           i_high_motor_power_number = 0
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
	      if (ratio.gt.1.d0) then
                 i_high_motor_amps_number = i_high_motor_amps_number + 1
	         i_high_motor_amps_list (i_high_motor_amps_number) = k
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
	      if (ratio.gt.1.d0) then
                 i_high_motor_power_number = i_high_motor_power_number+1
	         i_high_motor_power_list (i_high_motor_power_number) = k
	      endif
	   enddo
	   if (sum_motorampmax(j).gt.1.d0) then
c              write (6,*) '   Caution: at least one motor'
              write (6,*) '   Warning: at least one motor'
     .                   ,' maximum current exceeded.  Motors:'
     .            ,integer_list(
     .              i_high_motor_amps_list(1:i_high_motor_amps_number))
	      iwarn = .true.
	   endif
	   if (sum_motorpowmax(j).gt.1.d0) then
c              write (6,*) '   Caution: at least one motor'
              write (6,*) '   Warning: at least one motor'
     .                   ,' maximum power exceeded.    Motors:'
     .            ,integer_list(
     .             i_high_motor_power_list(1:i_high_motor_power_number))
	      iwarn = .true.
	   endif

c	   write (6,20) ' weighted dots 1 - 6', za(1:6)
cc	   write (6,20) ' weighted dots 7 -12', za(7:12)
c	   write (6,20) ' power 1 - 4, tot   ',xo(30:33),xo(29)
c	   write (6,20) ' amps  1 - 4, tot   ',xo(34:37),xo(38)
	   sum_amps(j)  = 0.d0
	   distancemax0 = 0.d0
	   speedmax0    = 0.d0
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
	         speedmax0    = Unorth
	         sum_ftime(j) = time_of_bat
	         ifirst = .false.
	      else
                 write (6,40)  k, amps, time_of_bat, xd(11)*time_of_bat
		 if ((xd(11)*time_of_bat).lt.distancemax0) then
		    distancemax0 = xd(11)*time_of_bat
	            speedmax0    = Unorth
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
	   temp1 = 0.d0
	   write (6,51) xo(29+4*aircraft%num_propellers+1),xo(29)
	   if (distancemax0.gt.distancemax) then
	      speedmax     = speedmax0
	      distancemax  = distancemax0
	      jdist        = j
	   endif 
	   if (speedmax0.gt.speedmax1) then
	      speedmax1    = speedmax0
	      distancemax1 = distancemax0
	      jspeed       = j
	   endif

	   sum_iwarn (j) = iwarn
	   sum_speed (j) = xd(11)
	   sum_pitch (j) = rad_to_deg*ya(n)
	   if (turn1.or.turn2) sum_roll (j) = rad_to_deg*ya(n-1)
	   sum_power (j) = xo(29)
	   sum_thrust(j) = sqrt(xo(26)**2 + xo(27)**2 + xo(28)**2)
	   sum_lift  (j) = sqrt(xo(20)**2 + xo(21)**2 + xo(22)**2)
	   sum_drag  (j) = sqrt(xo(23)**2 + xo(24)**2 + xo(25)**2)
	   sum_dist  (j) = distancemax0

c	   New metrics for stuff that does not converge   jdw  2/3/22
	   if (sum_iwarn(j)) then  ! it did not converge
              if (ifirst_notrim) then  ! i.e., the first no trim state
		 if (lateral) sum_minmaxxd 
     .               = control%lateral_trim_max_xd(j)
		 if (turn1) sum_minmaxxd = control%turn_1_trim_max_xd(j)
		 if (turn2) sum_minmaxxd = control%turn_2_trim_max_xd(j)
	         sum_minmaxxd_speed = xd(11)
	         ifirst_notrim = .false.
	      else
	         if (lateral) then
	            if (abs(control%lateral_trim_max_xd(j)).lt.
     .                  abs(sum_minmaxxd)) then
	               sum_minmaxxd = control%lateral_trim_max_xd(j)
	               sum_minmaxxd_speed = xd(11)
	            endif
                 else if (turn1) then
	            if (abs(control%turn_1_trim_max_xd(j)).lt.
     .                  abs(sum_minmaxxd)) then
	               sum_minmaxxd = control%turn_1_trim_max_xd(j)
	               sum_minmaxxd_speed = xd(11)
	            endif
                 else if (turn2) then
	            if (abs(control%turn_2_trim_max_xd(j)).lt.
     .                  abs(sum_minmaxxd)) then
	               sum_minmaxxd = control%turn_2_trim_max_xd(j)
	               sum_minmaxxd_speed = xd(11)
	             endif
	         endif
	      endif
	   endif

c	   Saving information for the control algorithm (potentially as a fallback).  this should already be done
c	   control%lateral_trim_if (j) = .not. sum_iwarn (j)  ! is this a legitimate state
c	   control%lateral_trim_if_0 (j) = .not. sum_iwarn (j)  ! to say we have a trim state even if no controls  1/24/22
c	   if (.not.sum_iwarn(j)) then
c	      do k = 1, udim
c                 control%lateral_trim_uc (j,k) = uc(k) ! the lateral trim control settings
c	      enddo
c	      do k = 1, xdim 
c	         control%lateral_trim_x(j,k) = x(k )
c	      enddo
c              control%lateral_trim_theta (j) = ya(n) ! the lateral trim pitch angle
c              control%lateral_num_trim_max   = j     ! the number of trim states in the array.
c	      control%lateral_trim_speed (j) = Unorth  ! the world frame north speed
c	   endif

c	   This is potentially wasteful, but it does insure that Alin and Blin are correct.
c	   It also allows for the mini debug.
c          it this way.
c	   if (compute_A_store) control%compute_A = .true.
c           if (aircraft%debug_mini) aircraft%debug = 1
c	   call fdcost(m, n, ya, za, iflag,
c     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
c     .                 aircraft, wing, propeller, battery, control)
c           if (aircraft%debug_mini) aircraft%debug = 0

c	   We now say that only states which satisfy all the conditions are trim states
c	    - it turns out this is a bad idea, because we get finding states based on 
c          previously found state benefits that this wipes out.  I'm leaving this in
c          here as a reminder that now trim_if_0 means if found a UVWPQRdot solution but 
c          it is not aware of other warnings.  Thus, trim_if is the correct storage of 
c          whether an acceptable trim state was found.
c	   if (lateral) control%lateral_trim_if_0 (j) = .not. iwarn
c	   if (turn1  ) control%turn_1_trim_if_0  (j) = .not. iwarn
c	   if (turn2  ) control%turn_2_trim_if_0  (j) = .not. iwarn


c	   Now, if requested, we determine the controls for the trim states, and store them.

	   if (control%compute_A.and.(.not.iwarn).and.
     .           (.not.control%outside_control_states)) then

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

	      if (lateral) control%lateral_trim_if (j) = isuccess
	      if (turn1  ) control%turn_1_trim_if  (j) = isuccess
	      if (turn2  ) control%turn_2_trim_if  (j) = isuccess
	      if (isuccess) then
	         iARE = iARE + 1
	         if (lateral) then
	            control%lateral_trim_qskip(j) = control%qskip
	            do k = 1, udim
	               do nA = 1, 12
		          control%lateral_trim_K(j,k,nA) = KK(k,nA)
		       enddo
	            enddo
	         endif
	         if (turn1) then
	            control%turn_1_trim_qskip(j) = control%qskip
	            do k = 1, udim
	               do nA = 1, 12
		          control%turn_1_trim_K(j,k,nA) = KK(k,nA)
		       enddo
	            enddo
	         endif
	         if (turn2) then
	            control%turn_2_trim_qskip(j) = control%qskip
	            do k = 1, udim
	               do nA = 1, 12
		          control%turn_2_trim_K(j,k,nA) = KK(k,nA)
		       enddo
	            enddo
	         endif

	         write (6,*) ' Controls K where uc = uc_trim'
     .                      ,' + K(x - x_trim)'
	         do k = 1, udim
c		    write (6,56)(control%lateral_trim_K(j,k,nA),nA=1,12)
		    write (6,56)(KK(k,nA),nA=1,12)
	         enddo
 56	         format(12f12.5)
        write (6,*) ' xtrim, qskip ',control%lateral_trim_qskip(j)
        write (6,56) (control%lateral_trim_x(j,k),k=1,12)
c	         do k = 1, xdim 
c	   	    control%lateral_trim_x(j,k) = x(k )
c	         enddo
c	         do k = 1, udim
c		    control%lateral_trim_uc(j,k) = uc(k)
c	         enddo
		 sum_ARE(j) = 'ARE'
c	      write (68,1010) Unorth,((KK(k,nA),k=1,udim),nA=1,12)

c                We had a successful computation of controls.  We now
c                ask if it is stable, based on a computation as follows:
c                We've linearized the FDM: x dot = Alin x + Blin u + Glin
c                Our controls are: u = u_trim + K(x - x_trim).
c                Thus, our overall equation is
c                xdot = Alin x + Blin (u_trim + K(x - x_trim)) + Glin
c                     = (Alin + Blin K)x + Blin(u_trim-Kx_trim) +  Glin
c                So for stability we want to know the eigenvalues of
c                Alin + Blin K.  If these have positive real part, we
c                presumably have a problem when we fly it.
                 do k1 = 1, 12
                    do k2 = 1, 12
                       Clin(k1,k2) = Alin(k1,k2)
                       do k = 1, udim
                         Clin(k1,k2) = Clin(k1,k2) + Blin(k1,k)*KK(k,k2)
                       enddo
                    enddo
                 enddo
                          
	      endif

	   endif  ! to compute_A

	enddo  ! to the loop over the speeds

c	if (control%compute_A) then
c	   In the event a lateral speed has not been requested, we choose the distance best
c	   if (abs(control%requested_lateral_speed).lt.0.05d0) then
c                   control%requested_lateral_speed = speedmax
c	       write (6,*)
c	       write (6,*) 'A positive lateral speed was'
c     .                 ,' not requested and is set to '
c     .                ,control%requested_lateral_speed
c	   endif
c	endif

	write (6,*)
	write (6,27) iMINPACK, iSIMPLEX
 27	format(' Trim states: ',i2,' found by MINPACK, ',i2
     .                          ,' found by nonlinear simplex.'
     .        ,'  SIM/MIN means simplex found a solution and'
     .        ,' minpack successfully polished it.')
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
        write (6,*) 'If no trim state is found, at the far right'
     .     ,' is shown the maximum of the accelerations' 
     .     ,' and the weighted cost used by simplex.'
        write (6,*) 'I.e., if all UVWPQR derivatives are'
     .     ,' less than the acceleration tolerance (in absolute value)'
     .     ,' a trim state is found.'
	write (6,*) 'The acceleration tolerance is'
     .     ,aircraft%acceleration_tolerance,'m|r/s^2'
     .     ,' (default 0.01 m/s^2).'
	write (6,*)

	if (lateral) then

	write (6,*) ' Lateral speed  Distance '
     .     ,' Flight time  Pitch angle  Max uc    Thrust '
     .     ,'     Lift       Drag    Current  Total power'
     .     ,' Frac amp  Frac pow  Frac current'
	write (6,*) '     (m/s)        (m)        '
     .     ,' (s)        (deg)        (-)       (N)   '
     .     ,'     (N)        (N)       (amp)     (watt)'
     .     ,'      (-)       (-)       (-)'
	iwarn = .true.
	do i = 0, 50
	   if (.not.sum_iwarn(i)) then
              write (6,50) sum_speed (i), sum_dist (i)
     .                    ,sum_ftime (i), sum_pitch(i), sum_uc   (i)
     .                    ,sum_thrust(i), sum_lift (i), sum_drag (i) 
     .                    ,sum_amps  (i), sum_power(i)
     .                    ,sum_motorampmax(i), sum_motorpowmax(i)
     .                    ,sum_batampmax(i),sum_csolve(i),sum_ARE(i)
	      iwarn = .false.
	   else
	      write (6,52) sum_speed (i), control%lateral_trim_max_xd(i)
     .                     ,control%lateral_trim_fdcost(i)
	   endif
	enddo

	else if (turn1.or.turn2) then

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
	do i = 1, 50
	   if (.not.sum_iwarn(i)) then
              write (6,53) sum_speed (i), sum_dist (i)
     .                    ,sum_ftime (i), sum_pitch(i)
     .                    ,sum_roll  (i), sum_uc   (i)
     .                    ,sum_thrust(i), sum_lift (i), sum_drag (i) 
     .                    ,sum_amps  (i), sum_power(i)
     .                    ,sum_motorampmax(i), sum_motorpowmax(i)
     .                    ,sum_batampmax(i),sum_csolve(i),sum_ARE(i)
	      iwarn = .false.
	   else
	      if (turn1) write (6,52) sum_speed (i)
     .                    , control%turn_1_trim_max_xd(i)
     .                     ,control%turn_1_trim_fdcost(i)
	      if (turn2) write (6,52) sum_speed (i)
     .                    , control%turn_2_trim_max_xd(i)
     .                     ,control%turn_2_trim_fdcost(i)
	   endif
	enddo


	endif


	write (6,*)
	write (6,45) speedmax ,distancemax
	write (6,46) speedmax1,distancemax1

c	Some identified metrics for plots

	do i=1, 2
	   if (i.eq.1) k = 6
	   if (i.eq.2) k = aircraft%i_out_metrics

	   write (k,*)
	   write (k,*) '#Metrics'
	   if (iwarn) then
	      write (k,*) ' No trim conditions were found'
	write (k,*) 'Min_max_nontrim_signed_acceleration_error'
     .              ,sngl(sum_minmaxxd)
	write (k,*) 'Min_max_nontrim_signed_acceleration_error_Speed'
     .              ,sngl(sum_minmaxxd_speed)
    	   else

	write (k,*) 'Max_Hover_Time_(s) ',sngl(sum_ftime(0))
	write (k,*) 'Max_Lateral_Speed_(m/s) ',sngl(speedmax1)
	write (k,*) 'Max_Flight_Distance_(m) ',sngl(distancemax)
	write (k,*) 'Speed_at_Max_Flight_Distance_(m/s) ',sngl(speedmax)
	write (k,*) 'Max_uc_at_Max_Flight_Distance ',sngl(sum_uc(jdist))
	write (k,*) 'Power_at_Max_Flight_Distance_(W) '
     .              ,sngl(sum_power(jdist))
	write (k,*) 'Motor_amps_to_max_amps_ratio_at_Max_'
     .              ,'Flight_Distance ',sngl(sum_motorampmax(jdist))
	write (k,*) 'Motor_power_to_max_power_ratio_at_Max_'
     .              ,'Flight_Distance ',sngl(sum_motorpowmax(jdist))
	write (k,*) 'Battery_amps_to_max_amps_ratio_at_Max_'
     .              ,'Flight_Distance ',sngl(sum_batampmax(jdist))
	write (k,*) 'Distance_at_Max_Speed_(m) ',sngl(distancemax1)
	write (k,*) 'Power_at_Max_Speed_(W) '
     .              ,sngl(sum_power(jspeed))
	write (k,*) 'Motor_amps_to_max_amps_ratio_at_Max_'
     .              ,'Speed ',sngl(sum_motorampmax(jspeed))
	write (k,*) 'Motor_power_to_max_power_ratio_at_Max_'
     .              ,'Speed ',sngl(sum_motorpowmax(jspeed))
	write (k,*) 'Battery_amps_to_max_amps_ratio_at_Max_'
     .              ,'Speed ',sngl(sum_batampmax(jspeed))
        write (k,*) 'Max_wing_segement_load_ratio',sngl(sum_wl_max)
	write (k,*) 'Min_max_nontrim_signed_acceleration_error'
     .              ,sngl(sum_minmaxxd)
	write (k,*) 'Min_max_nontrim_signed_acceleration_error_Speed'
     .              ,sngl(sum_minmaxxd_speed)

	   endif

	 enddo ! this is the j loop on forward speeds

	enddo  ! this is the jj loop on lateral, trim1, trim2

 20	format (a25, 6f17.8)  ! changed from 15.8 for more space 3/25/22
 22	format (a13,i2,' - ',i2,'     ',6f17.8)  ! changed from 15.8 for more space 3/25/22
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
 50	format (f11.2,3f12.2,6f11.2,3f10.3,5x,a7,2x,a3)
 53	format (f11.2,3f12.2,7f10.2,3f10.3,5x,a7,2x,a3)
 51     format(' Total power from batteries ',f11.3, ' watts; ',
     .         ' total power in motors ',f11.3, ' watts')
c 52	format (f11.2,36x,66x,30x,5x,f7.4,2x,1pe11.3)  old format changed by request  jdw  9/15/22
 52	format (f11.2,36x,66x,25x,1pe15.7,2x,1pe15.7)


c	Now we do the general trim for turning states
c	This is the old approach, no superseded with the above.
c
c	write (6,*) ' Here in fly, right before general_trim call'
c
c	if (control%radius_1.ne.0.d0) then
c
c	   imode  = 2
c	   imode  = 1  ! jdw debug 1/27/22
c	   irad   = 1
c	   radius = control%radius_1
c
c	   call general_trim(xdim,x,xd,udim,uc,aircraft,wing,propeller
c     .                ,battery,control,UVWPQR, KK, imode, radius, irad)
c
c	endif
c
c	if (control%radius_2.ne.0.d0) then
c
c	   imode  = 2
c	   imode  = 1  ! jdw debug 1/27/22
c	   irad   = 2
c	   radius = control%radius_2
c
c	   call general_trim(xdim,x,xd,udim,uc,aircraft,wing,propeller
c     .                ,battery,control,UVWPQR, KK, imode, radius, irad)
c
c	endif
c
c	write (6,*) ' Here in fly, right after general_trim call'


	return
	end



	subroutine fdcost(m, n, ya, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)

c	This subroutines calls the flight dynamics model (fderiv) to determine
c	the derivatives of the state variables to then look for a static solution.
c	James Walker  5 April 2021
c	4/5/21
c	
c	2/5/22  We are going to combine this with the general case so that
c               we only have one routine.  The internal discriminator is the
c	        difference between n and udim.  If 1, its lateral, if 2,
c               it includes a turn.

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
c	   Unorth                 requested north velocity (Veast, Zdown are zero for now)
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

	integer  i, m, n, iflag
	double precision x (xdim), xd(xdim), xo(1500), uc(udim)
	double precision time, Unorth, ya(n), za(m), UVWPQR(6)
	double precision Veast, Wdown, Pworld, Qworld, Rworld
	double precision weight1, weight2, weight3
	double precision theta, q0, q1, q2, q3, phi
	double precision temp1, temp2, temp3

	logical :: ip = .false.
	logical loutside
	logical :: lateral = .false.
	logical :: turning = .false.
	logical lwarn

	if (n.eq.udim+1) then
           lateral = .true.
	   turning = .false.
	else if (n.eq.udim+2) then
	   lateral = .false.
           turning = .true.
        else
           write (6,*) ' Incorrect setup in fdcost: n,udim ',n,udim
           stop
	endif

	if (aircraft%debug.eq.0) then
           ip = .false.
        else 
           ip = .true.
	endif

	lwarn = .false.

c	There is a bunch of set up at this point.

c	So what are our unknows?  They are all the control variables
c	plus the angle theta.  So n = udim + 1

	Unorth = UVWPQR(1)
	Veast  = UVWPQR(2)
	Wdown  = UVWPQR(3)
	Pworld = UVWPQR(4)
	Qworld = UVWPQR(5)
	Rworld = UVWPQR(6)

c	write (6,*) ' UVWPQR 3 ',UVWPQR

	if (ip) write (60,60) Unorth,(ya(i),i=1,n)
 60	format (' Unorth, ya(1...n)      ',12e13.5)
c	write (6,10) ' m no wgt 3 costs,            '   ! jdw debug
c     .                      ,0.,0.,0.,0.
c     .                   ,rad_to_deg*ya(n),ya(1),ya(2),ya(3),ya(4),
c     .                    ya(5),ya(6),ya(7),ya(8)

c	>>> New from simplex
	loutside = .false.
	do i=1, udim
	   uc(i) = ya(i)
	enddo
        theta = ya(n)
	if (turning) phi = ya(n - 1)
	do i=1, udim
	   if ((uc(i).gt.1.d0).or.(uc(i).lt.0.d0)) then
              loutside = .true.
	   endif
	enddo
	if ((theta.gt.halfpi).or.(theta.lt.-halfpi)) then
           loutside = .true.
	endif
	if (turning) then
           if ((phi.gt.halfpi).or.(phi.lt.-halfpi)) then
              loutside = .true.
	   endif
	endif
c	if (loutside) goto 1000  good for simplex, not for minpack, so now
c	                         we are going with the minpack version but
c                                modifying simplex code so it doesn't matter
c                                if the state vector is changed.
c	<<< end of new from simplex

	do i=1, udim
c	   The fact that 0 <= uc <= 1
	   uc(i) = max(0.d0, uc(i))
	   uc(i) = min(1.d0, uc(i))
	enddo
	theta    = max(-halfpi, theta)
	theta    = min( halfpi, theta)
	if (turning) then
	   phi   = max(-halfpi, phi  )
	   phi   = min( halfpi, phi  )
	endif

	if (loutside) goto 1000  ! it makes a difference for minpack where this call is
	   
c       For a given case, we first set up the quaternions, given theta:
	if (lateral) then
	   q0 = cos(theta/2.d0)
	   q1 = 0.d0
	   q2 = sin(theta/2.d0)
	   q3 = 0.d0
	endif
	if (turning) then
	   q0 =   cos(theta/2.d0) * cos(phi/2.d0)
	   q1 =   cos(theta/2.d0) * sin(phi/2.d0)
	   q2 =   sin(theta/2.d0) * cos(phi/2.d0)
	   q3 = - sin(theta/2.d0) * sin(phi/2.d0)
	endif


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
	if (lateral) then
	   x(1) = xo(11)*Unorth        ! U in body frame  = Unorth*cos(theta)
	   x(2) = xo(14)*Unorth        ! V in body frame  = 0
	   x(3) = xo(17)*Unorth        ! W in body frame  = Unorth*sin(theta) 

c          New form due to including Wdown component  9/15/22
	   x(1) = xo(11)*Unorth + xo(12)*Veast + xo(13)*Wdown
	   x(2) = xo(14)*Unorth + xo(15)*Veast + xo(16)*Wdown
	   x(3) = xo(17)*Unorth + xo(18)*Veast + xo(19)*Wdown

c	   We don't want any rotation
	   x(4) = 0.d0
	   x(5) = 0.d0
	   x(6) = 0.d0
	endif

	if (turning) then
	   x(1) = xo(11)*Unorth + xo(12)*Veast + xo(13)*Wdown
	   x(2) = xo(14)*Unorth + xo(15)*Veast + xo(16)*Wdown
	   x(3) = xo(17)*Unorth + xo(18)*Veast + xo(19)*Wdown

c	   We don't want any rotational acceleration
	   x(4) = xo(11)*Pworld + xo(12)*Qworld + xo(13)*Rworld
	   x(5) = xo(14)*Pworld + xo(15)*Qworld + xo(16)*Rworld
	   x(6) = xo(17)*Pworld + xo(18)*Qworld + xo(19)*Rworld
	endif

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
c	write (6,*) ' theta cost ',theta
c	write (6,*) ' uc cost ',uc(1:udim)
c	write (6,*) ' xd cost ',xd(1:6)

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
c           za(6+i) = aircraft%weight(3)*sqrt(abs(xo(29+i)))   ! root of power
           za(6+i) = aircraft%weight(3)
     .       *sqrt(abs(xo(29+3*aircraft%num_propellers+i)))   ! root of power  2/22
	enddo
c	Za(7) = aircraft%weight(3)*xo(29)         ! total power, to avoid a local min
c	It is possible that we had to include some extra equations for MINPACK 2/9/22
	if (m.gt.(6 + aircraft%num_propellers)) then
           do i = 6 + aircraft%num_propellers + 1, m    ! added +1 to index, jdw, 3/9/22
              za(i) = 0.d0
	   enddo
	endif

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

c	   write (6,*)

	endif

c	This is new code to trigger a large fdcost return
c       in the event that the warning situations arise, in part
c       to see if that drives finding other trim states rather
c       than ones that we later discard

        if (control%strong_fdcost) then

	   write (6,*) ' strong_fdcost is not yet coded'
	   stop

c	   The idea is to set lwarn = .true. if the state will 
c          trip the warning categories in trim.
c          I'm concerned these constraints will prevent convergence,
c	   hence I haven't coded it up.  The big issue is whether
c          we can find a starting location given the extent
c          of the contstraints.

        endif


c	   temp1 = 0.d0
c	   temp2 = 0.d0
c	   temp3 = 0.d0
c	   do i = 1, 3
c	      temp1 = temp1 + za(i)**2
c	   enddo
c	   do i = 4, 6
c	      temp2 = temp2 + za(i)**2
c	   enddo
c	   do i = 7, 6 + aircraft%num_propellers
c	      temp3 = temp3 + za(i)**2
c	   enddo
c	write (6,10) ' m no wgt 3 costs, final cost '   ! jdw debug
c     .                                 ,temp1/aircraft%weight(1)**2
c     .                                 ,temp2/aircraft%weight(2)**2
c     .                                 ,temp3/aircraft%weight(3)**2
c     .                                       ,temp1+temp2+temp3
c     .                   ,rad_to_deg*theta,uc(1),uc(2),uc(3),uc(4),
c     .                    uc(5),uc(6),uc(7),uc(8)

c 10	format (a20, 6f15.8)
c 10	format (a20, 15(1pe11.4))
 10	format (a20, 15(1pe20.13))
c	write (6,*) ' x motors ',x(14:17)
c	write (6,*) ' ya  cost ',u (1:udim)
c	write (6,*) ' xd cost ',xd(1:6)

 1000   continue
c	>>> new from simplex
	if (loutside.or.lwarn) then
           do i = 1, 6 + aircraft%num_propellers 
              za(i) = 1.d+07
	   enddo
	   if (m.gt.(6 + aircraft%num_propellers)) then
              do i = 6 + aircraft%num_propellers, m
	         za(i) = 0.d0
	      enddo
	   endif
	endif
c	<<< end of new for simplex

c	We are going to change ya to what we are really using, to see if
c	that makes any difference (or if it is just passed in)
	do i=1, udim
	   ya(i) = uc(i)
	enddo
	ya(n) = theta
	if (turning) ya(n-1) = phi

c	write (60,61) Unorth,(ya(i),i=1,n),temp1+temp2+temp3 ! jdw debug
	if (ip) write (60,61) Unorth,(ya(i),i=1,n),temp1+temp2+temp3
 61	format (' Unorth, ya(1...n),cost ',12e13.5)

	return
	end


c  The following are modified routines from MINPACK.  The primary modification
c  is to the subroutine calls to pass all the variables required by our application.
c  James Walker  8 April 2021  Southwest Research Institute

      double precision function dpmpar(i)
      integer i
c     **********
c
c     Function dpmpar
c
c     This function provides double precision machine parameters
c     when the appropriate set of data statements is activated (by
c     removing the c from column 1) and all other data statements are
c     rendered inactive. Most of the parameter values were obtained
c     from the corresponding Bell Laboratories Port Library function.
c
c     The function statement is
c
c       double precision function dpmpar(i)
c
c     where
c
c       i is an integer input variable set to 1, 2, or 3 which
c         selects the desired machine parameter. If the machine has
c         t base b digits and its smallest and largest exponents are
c         emin and emax, respectively, then these parameters are
c
c         dpmpar(1) = b**(1 - t), the machine precision,
c
c         dpmpar(2) = b**(emin - 1), the smallest magnitude,
c
c         dpmpar(3) = b**emax*(1 - b**(-t)), the largest magnitude.
c
c     Argonne National Laboratory. MINPACK Project. November 1996.
c     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More'
c
c     **********
      integer mcheps(4)
      integer minmag(4)
      integer maxmag(4)
      double precision dmach(3)
      equivalence (dmach(1),mcheps(1))
      equivalence (dmach(2),minmag(1))
      equivalence (dmach(3),maxmag(1))
c
c     Machine constants for the IBM 360/370 series,
c     the Amdahl 470/V6, the ICL 2900, the Itel AS/6,
c     the Xerox Sigma 5/7/9 and the Sel systems 85/86.
c
c     data mcheps(1),mcheps(2) / z34100000, z00000000 /
c     data minmag(1),minmag(2) / z00100000, z00000000 /
c     data maxmag(1),maxmag(2) / z7fffffff, zffffffff /
c
c     Machine constants for the Honeywell 600/6000 series.
c
c     data mcheps(1),mcheps(2) / o606400000000, o000000000000 /
c     data minmag(1),minmag(2) / o402400000000, o000000000000 /
c     data maxmag(1),maxmag(2) / o376777777777, o777777777777 /
c
c     Machine constants for the CDC 6000/7000 series.
c
c     data mcheps(1) / 15614000000000000000b /
c     data mcheps(2) / 15010000000000000000b /
c
c     data minmag(1) / 00604000000000000000b /
c     data minmag(2) / 00000000000000000000b /
c
c     data maxmag(1) / 37767777777777777777b /
c     data maxmag(2) / 37167777777777777777b /
c
c     Machine constants for the PDP-10 (KA processor).
c
c     data mcheps(1),mcheps(2) / "114400000000, "000000000000 /
c     data minmag(1),minmag(2) / "033400000000, "000000000000 /
c     data maxmag(1),maxmag(2) / "377777777777, "344777777777 /
c
c     Machine constants for the PDP-10 (KI processor).
c
c     data mcheps(1),mcheps(2) / "104400000000, "000000000000 /
c     data minmag(1),minmag(2) / "000400000000, "000000000000 /
c     data maxmag(1),maxmag(2) / "377777777777, "377777777777 /
c
c     Machine constants for the PDP-11. 
c
c     data mcheps(1),mcheps(2) /   9472,      0 /
c     data mcheps(3),mcheps(4) /      0,      0 /
c
c     data minmag(1),minmag(2) /    128,      0 /
c     data minmag(3),minmag(4) /      0,      0 /
c
c     data maxmag(1),maxmag(2) /  32767,     -1 /
c     data maxmag(3),maxmag(4) /     -1,     -1 /
c
c     Machine constants for the Burroughs 6700/7700 systems.
c
c     data mcheps(1) / o1451000000000000 /
c     data mcheps(2) / o0000000000000000 /
c
c     data minmag(1) / o1771000000000000 /
c     data minmag(2) / o7770000000000000 /
c
c     data maxmag(1) / o0777777777777777 /
c     data maxmag(2) / o7777777777777777 /
c
c     Machine constants for the Burroughs 5700 system.
c
c     data mcheps(1) / o1451000000000000 /
c     data mcheps(2) / o0000000000000000 /
c
c     data minmag(1) / o1771000000000000 /
c     data minmag(2) / o0000000000000000 /
c
c     data maxmag(1) / o0777777777777777 /
c     data maxmag(2) / o0007777777777777 /
c
c     Machine constants for the Burroughs 1700 system.
c
c     data mcheps(1) / zcc6800000 /
c     data mcheps(2) / z000000000 /
c
c     data minmag(1) / zc00800000 /
c     data minmag(2) / z000000000 /
c
c     data maxmag(1) / zdffffffff /
c     data maxmag(2) / zfffffffff /
c
c     Machine constants for the Univac 1100 series.
c
c     data mcheps(1),mcheps(2) / o170640000000, o000000000000 /
c     data minmag(1),minmag(2) / o000040000000, o000000000000 /
c     data maxmag(1),maxmag(2) / o377777777777, o777777777777 /
c
c     Machine constants for the Data General Eclipse S/200.
c
c     Note - it may be appropriate to include the following card -
c     static dmach(3)
c
c     data minmag/20k,3*0/,maxmag/77777k,3*177777k/
c     data mcheps/32020k,3*0/
c
c     Machine constants for the Harris 220.
c
c     data mcheps(1),mcheps(2) / '20000000, '00000334 /
c     data minmag(1),minmag(2) / '20000000, '00000201 /
c     data maxmag(1),maxmag(2) / '37777777, '37777577 /
c
c     Machine constants for the Cray-1.
c
c     data mcheps(1) / 0376424000000000000000b /
c     data mcheps(2) / 0000000000000000000000b /
c
c     data minmag(1) / 0200034000000000000000b /
c     data minmag(2) / 0000000000000000000000b /
c
c     data maxmag(1) / 0577777777777777777777b /
c     data maxmag(2) / 0000007777777777777776b /
c
c     Machine constants for the Prime 400.
c
c     data mcheps(1),mcheps(2) / :10000000000, :00000000123 /
c     data minmag(1),minmag(2) / :10000000000, :00000100000 /
c     data maxmag(1),maxmag(2) / :17777777777, :37777677776 /
c
c     Machine constants for the VAX-11.
c
c     data mcheps(1),mcheps(2) /   9472,  0 /
c     data minmag(1),minmag(2) /    128,  0 /
c     data maxmag(1),maxmag(2) / -32769, -1 /
c
c     Machine constants for IEEE machines.
c
      data dmach(1) /2.22044604926d-16/
      data dmach(2) /2.22507385852d-308/
      data dmach(3) /1.79769313485d+308/
c
      dpmpar = dmach(i)
      return
c
c     Last card of function dpmpar.
c
      end
      double precision function enorm(n,x)
      integer n
      double precision x(n)
c     **********
c
c     function enorm
c
c     given an n-vector x, this function calculates the
c     euclidean norm of x.
c
c     the euclidean norm is computed by accumulating the sum of
c     squares in three different sums. the sums of squares for the
c     small and large components are scaled so that no overflows
c     occur. non-destructive underflows are permitted. underflows
c     and overflows do not occur in the computation of the unscaled
c     sum of squares for the intermediate components.
c     the definitions of small, intermediate and large components
c     depend on two constants, rdwarf and rgiant. the main
c     restrictions on these constants are that rdwarf**2 not
c     underflow and rgiant**2 not overflow. the constants
c     given here are suitable for every known computer.
c
c     the function statement is
c
c       double precision function enorm(n,x)
c
c     where
c
c       n is a positive integer input variable.
c
c       x is an input array of length n.
c
c     subprograms called
c
c       fortran-supplied ... dabs,dsqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i
      double precision agiant,floatn,one,rdwarf,rgiant,s1,s2,s3,xabs,
     *                 x1max,x3max,zero
      data one,zero,rdwarf,rgiant /1.0d0,0.0d0,3.834d-20,1.304d19/
      s1 = zero
      s2 = zero
      s3 = zero
      x1max = zero
      x3max = zero
      floatn = n
      agiant = rgiant/floatn
      do 90 i = 1, n
         xabs = dabs(x(i))
         if (xabs .gt. rdwarf .and. xabs .lt. agiant) go to 70
            if (xabs .le. rdwarf) go to 30
c
c              sum for large components.
c
               if (xabs .le. x1max) go to 10
                  s1 = one + s1*(x1max/xabs)**2
                  x1max = xabs
                  go to 20
   10          continue
                  s1 = s1 + (xabs/x1max)**2
   20          continue
               go to 60
   30       continue
c
c              sum for small components.
c
               if (xabs .le. x3max) go to 40
                  s3 = one + s3*(x3max/xabs)**2
                  x3max = xabs
                  go to 50
   40          continue
                  if (xabs .ne. zero) s3 = s3 + (xabs/x3max)**2
   50          continue
   60       continue
            go to 80
   70    continue
c
c           sum for intermediate components.
c
            s2 = s2 + xabs**2
   80    continue
   90    continue
c
c     calculation of norm.
c
      if (s1 .eq. zero) go to 100
         enorm = x1max*dsqrt(s1+(s2/x1max)/x1max)
         go to 130
  100 continue
         if (s2 .eq. zero) go to 110
            if (s2 .ge. x3max)
     *         enorm = dsqrt(s2*(one+(x3max/s2)*(x3max*s3)))
            if (s2 .lt. x3max)
     *         enorm = dsqrt(x3max*((s2/x3max)+(x3max*s3)))
            go to 120
  110    continue
            enorm = x3max*dsqrt(s3)
  120    continue
  130 continue
      return
c
c     last card of function enorm.
c
      end
      subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa,
     .                 UVWPQR, time, xdim, xa, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	use iso_c_binding
	implicit none
	include 'parts.h'
	integer xdim, udim
	double precision xa(xdim), xd(xdim), xo(1500), uc(udim)
	double precision UVWPQR(6), time
      integer m,n,ldfjac,iflag
      double precision epsfcn
      double precision x(n),fvec(m),fjac(ldfjac,n),wa(m)
c     **********
c
c     subroutine fdjac2
c
c     this subroutine computes a forward-difference approximation
c     to the m by n jacobian matrix associated with a specified
c     problem of m functions in n variables.
c
c     the subroutine statement is
c
c       subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa)
c
c     where
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions. fcn must be declared
c         in an external statement in the user calling
c         program, and should be written as follows.
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
c
c         the value of iflag should not be changed by fcn unless
c         the user wants to terminate execution of fdjac2.
c         in this case set iflag to a negative integer.
c
c       m is a positive integer input variable set to the number
c         of functions.
c
c       n is a positive integer input variable set to the number
c         of variables. n must not exceed m.
c
c       x is an input array of length n.
c
c       fvec is an input array of length m which must contain the
c         functions evaluated at x.
c
c       fjac is an output m by n array which contains the
c         approximation to the jacobian matrix evaluated at x.
c
c       ldfjac is a positive integer input variable not less than m
c         which specifies the leading dimension of the array fjac.
c
c       iflag is an integer variable which can be used to terminate
c         the execution of fdjac2. see description of fcn.
c
c       epsfcn is an input variable used in determining a suitable
c         step length for the forward-difference approximation. this
c         approximation assumes that the relative errors in the
c         functions are of the order of epsfcn. if epsfcn is less
c         than the machine precision, it is assumed that the relative
c         errors in the functions are of the order of the machine
c         precision.
c
c       wa is a work array of length m.
c
c     subprograms called
c
c       user-supplied ...... fcn
c
c       minpack-supplied ... dpmpar
c
c       fortran-supplied ... dabs,dmax1,dsqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,j
      double precision eps,epsmch,h,temp,zero
      double precision dpmpar
      data zero /0.0d0/
c
c     epsmch is the machine precision.
c
      epsmch = dpmpar(1)
c
      eps = dsqrt(dmax1(epsfcn,epsmch))
      do 20 j = 1, n
         temp = x(j)
         h = eps*dabs(temp)
         if (h .eq. zero) h = eps
         x(j) = temp + h
         call fcn(m,n,x,wa,iflag,
     .                 UVWPQR, time, xdim, xa, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
         if (iflag .lt. 0) go to 30
         x(j) = temp
         do 10 i = 1, m
            fjac(i,j) = (wa(i) - fvec(i))/h
   10       continue
   20    continue
   30 continue
      return
c
c     last card of subroutine fdjac2.
c
      end
      subroutine lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,
     *                 diag,mode,factor,nprint,info,nfev,fjac,ldfjac,
     *                 ipvt,qtf,wa1,wa2,wa3,wa4,
     .                 UVWPQR, time, xdim, xa, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	use iso_c_binding
	implicit none
	include 'parts.h'
	integer xdim, udim
	double precision xa(xdim), xd(xdim), xo(1500), uc(udim)
	double precision UVWPQR(6), time
      integer m,n,maxfev,mode,nprint,info,nfev,ldfjac
      integer ipvt(n)
      double precision ftol,xtol,gtol,epsfcn,factor
      double precision x(n),fvec(m),diag(n),fjac(ldfjac,n),qtf(n),
     *                 wa1(n),wa2(n),wa3(n),wa4(m)
      external fcn
c     **********
c
c     subroutine lmdif
c
c     the purpose of lmdif is to minimize the sum of the squares of
c     m nonlinear functions in n variables by a modification of
c     the levenberg-marquardt algorithm. the user must provide a
c     subroutine which calculates the functions. the jacobian is
c     then calculated by a forward-difference approximation.
c
c     the subroutine statement is
c
c       subroutine lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,
c                        diag,mode,factor,nprint,info,nfev,fjac,
c                        ldfjac,ipvt,qtf,wa1,wa2,wa3,wa4)
c
c     where
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions. fcn must be declared
c         in an external statement in the user calling
c         program, and should be written as follows.
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
c
c         the value of iflag should not be changed by fcn unless
c         the user wants to terminate execution of lmdif.
c         in this case set iflag to a negative integer.
c
c       m is a positive integer input variable set to the number
c         of functions.
c
c       n is a positive integer input variable set to the number
c         of variables. n must not exceed m.
c
c       x is an array of length n. on input x must contain
c         an initial estimate of the solution vector. on output x
c         contains the final estimate of the solution vector.
c
c       fvec is an output array of length m which contains
c         the functions evaluated at the output x.
c
c       ftol is a nonnegative input variable. termination
c         occurs when both the actual and predicted relative
c         reductions in the sum of squares are at most ftol.
c         therefore, ftol measures the relative error desired
c         in the sum of squares.
c
c       xtol is a nonnegative input variable. termination
c         occurs when the relative error between two consecutive
c         iterates is at most xtol. therefore, xtol measures the
c         relative error desired in the approximate solution.
c
c       gtol is a nonnegative input variable. termination
c         occurs when the cosine of the angle between fvec and
c         any column of the jacobian is at most gtol in absolute
c         value. therefore, gtol measures the orthogonality
c         desired between the function vector and the columns
c         of the jacobian.
c
c       maxfev is a positive integer input variable. termination
c         occurs when the number of calls to fcn is at least
c         maxfev by the end of an iteration.
c
c       epsfcn is an input variable used in determining a suitable
c         step length for the forward-difference approximation. this
c         approximation assumes that the relative errors in the
c         functions are of the order of epsfcn. if epsfcn is less
c         than the machine precision, it is assumed that the relative
c         errors in the functions are of the order of the machine
c         precision.
c
c       diag is an array of length n. if mode = 1 (see
c         below), diag is internally set. if mode = 2, diag
c         must contain positive entries that serve as
c         multiplicative scale factors for the variables.
c
c       mode is an integer input variable. if mode = 1, the
c         variables will be scaled internally. if mode = 2,
c         the scaling is specified by the input diag. other
c         values of mode are equivalent to mode = 1.
c
c       factor is a positive input variable used in determining the
c         initial step bound. this bound is set to the product of
c         factor and the euclidean norm of diag*x if nonzero, or else
c         to factor itself. in most cases factor should lie in the
c         interval (.1,100.). 100. is a generally recommended value.
c
c       nprint is an integer input variable that enables controlled
c         printing of iterates if it is positive. in this case,
c         fcn is called with iflag = 0 at the beginning of the first
c         iteration and every nprint iterations thereafter and
c         immediately prior to return, with x and fvec available
c         for printing. if nprint is not positive, no special calls
c         of fcn with iflag = 0 are made.
c
c       info is an integer output variable. if the user has
c         terminated execution, info is set to the (negative)
c         value of iflag. see description of fcn. otherwise,
c         info is set as follows.
c
c         info = 0  improper input parameters.
c
c         info = 1  both actual and predicted relative reductions
c                   in the sum of squares are at most ftol.
c
c         info = 2  relative error between two consecutive iterates
c                   is at most xtol.
c
c         info = 3  conditions for info = 1 and info = 2 both hold.
c
c         info = 4  the cosine of the angle between fvec and any
c                   column of the jacobian is at most gtol in
c                   absolute value.
c
c         info = 5  number of calls to fcn has reached or
c                   exceeded maxfev.
c
c         info = 6  ftol is too small. no further reduction in
c                   the sum of squares is possible.
c
c         info = 7  xtol is too small. no further improvement in
c                   the approximate solution x is possible.
c
c         info = 8  gtol is too small. fvec is orthogonal to the
c                   columns of the jacobian to machine precision.
c
c       nfev is an integer output variable set to the number of
c         calls to fcn.
c
c       fjac is an output m by n array. the upper n by n submatrix
c         of fjac contains an upper triangular matrix r with
c         diagonal elements of nonincreasing magnitude such that
c
c                t     t           t
c               p *(jac *jac)*p = r *r,
c
c         where p is a permutation matrix and jac is the final
c         calculated jacobian. column j of p is column ipvt(j)
c         (see below) of the identity matrix. the lower trapezoidal
c         part of fjac contains information generated during
c         the computation of r.
c
c       ldfjac is a positive integer input variable not less than m
c         which specifies the leading dimension of the array fjac.
c
c       ipvt is an integer output array of length n. ipvt
c         defines a permutation matrix p such that jac*p = q*r,
c         where jac is the final calculated jacobian, q is
c         orthogonal (not stored), and r is upper triangular
c         with diagonal elements of nonincreasing magnitude.
c         column j of p is column ipvt(j) of the identity matrix.
c
c       qtf is an output array of length n which contains
c         the first n elements of the vector (q transpose)*fvec.
c
c       wa1, wa2, and wa3 are work arrays of length n.
c
c       wa4 is a work array of length m.
c
c     subprograms called
c
c       user-supplied ...... fcn
c
c       minpack-supplied ... dpmpar,enorm,fdjac2,lmpar,qrfac
c
c       fortran-supplied ... dabs,dmax1,dmin1,dsqrt,mod
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,iflag,iter,j,l
      double precision actred,delta,dirder,epsmch,fnorm,fnorm1,gnorm,
     *                 one,par,pnorm,prered,p1,p5,p25,p75,p0001,ratio,
     *                 sum,temp,temp1,temp2,xnorm,zero
      double precision dpmpar,enorm
      data one,p1,p5,p25,p75,p0001,zero
     *     /1.0d0,1.0d-1,5.0d-1,2.5d-1,7.5d-1,1.0d-4,0.0d0/
c
c     epsmch is the machine precision.
c
      epsmch = dpmpar(1)
c
      info = 0
      iflag = 0
      nfev = 0
c
c     check the input parameters for errors.
c
      if (n .le. 0 .or. m .lt. n .or. ldfjac .lt. m
     *    .or. ftol .lt. zero .or. xtol .lt. zero .or. gtol .lt. zero
     *    .or. maxfev .le. 0 .or. factor .le. zero) go to 300
      if (mode .ne. 2) go to 20
      do 10 j = 1, n
         if (diag(j) .le. zero) go to 300
   10    continue
   20 continue
c
c     evaluate the function at the starting point
c     and calculate its norm.
c
      iflag = 1
      call fcn(m,n,x,fvec,iflag,
     .                 UVWPQR, time, xdim, xa, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
      nfev = 1
      if (iflag .lt. 0) go to 300
      fnorm = enorm(m,fvec)
c
c     initialize levenberg-marquardt parameter and iteration counter.
c
      par = zero
      iter = 1
c
c     beginning of the outer loop.
c
   30 continue
c
c        calculate the jacobian matrix.
c
         iflag = 2
         call fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa4,
     .                 UVWPQR, time, xdim, xa, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
         nfev = nfev + n
         if (iflag .lt. 0) go to 300
c
c        if requested, call fcn to enable printing of iterates.
c
         if (nprint .le. 0) go to 40
         iflag = 0
         if (mod(iter-1,nprint) .eq. 0) call fcn(m,n,x,fvec,iflag,
     .                 UVWPQR, time, xdim, xa, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
         if (iflag .lt. 0) go to 300
   40    continue
c
c        compute the qr factorization of the jacobian.
c
         call qrfac(m,n,fjac,ldfjac,.true.,ipvt,n,wa1,wa2,wa3)
c
c        on the first iteration and if mode is 1, scale according
c        to the norms of the columns of the initial jacobian.
c
         if (iter .ne. 1) go to 80
         if (mode .eq. 2) go to 60
         do 50 j = 1, n
            diag(j) = wa2(j)
            if (wa2(j) .eq. zero) diag(j) = one
   50       continue
   60    continue
c
c        on the first iteration, calculate the norm of the scaled x
c        and initialize the step bound delta.
c
         do 70 j = 1, n
            wa3(j) = diag(j)*x(j)
   70       continue
         xnorm = enorm(n,wa3)
         delta = factor*xnorm
         if (delta .eq. zero) delta = factor
   80    continue
c
c        form (q transpose)*fvec and store the first n components in
c        qtf.
c
         do 90 i = 1, m
            wa4(i) = fvec(i)
   90       continue
         do 130 j = 1, n
            if (fjac(j,j) .eq. zero) go to 120
            sum = zero
            do 100 i = j, m
               sum = sum + fjac(i,j)*wa4(i)
  100          continue
            temp = -sum/fjac(j,j)
            do 110 i = j, m
               wa4(i) = wa4(i) + fjac(i,j)*temp
  110          continue
  120       continue
            fjac(j,j) = wa1(j)
            qtf(j) = wa4(j)
  130       continue
c
c        compute the norm of the scaled gradient.
c
         gnorm = zero
         if (fnorm .eq. zero) go to 170
         do 160 j = 1, n
            l = ipvt(j)
            if (wa2(l) .eq. zero) go to 150
            sum = zero
            do 140 i = 1, j
               sum = sum + fjac(i,j)*(qtf(i)/fnorm)
  140          continue
            gnorm = dmax1(gnorm,dabs(sum/wa2(l)))
  150       continue
  160       continue
  170    continue
c
c        test for convergence of the gradient norm.
c
         if (gnorm .le. gtol) info = 4
         if (info .ne. 0) go to 300
c
c        rescale if necessary.
c
         if (mode .eq. 2) go to 190
         do 180 j = 1, n
            diag(j) = dmax1(diag(j),wa2(j))
  180       continue
  190    continue
c
c        beginning of the inner loop.
c
  200    continue
c
c           determine the levenberg-marquardt parameter.
c
            call lmpar(n,fjac,ldfjac,ipvt,diag,qtf,delta,par,wa1,wa2,
     *                 wa3,wa4)
c
c           store the direction p and x + p. calculate the norm of p.
c
            do 210 j = 1, n
               wa1(j) = -wa1(j)
               wa2(j) = x(j) + wa1(j)
               wa3(j) = diag(j)*wa1(j)
  210          continue
            pnorm = enorm(n,wa3)
c
c           on the first iteration, adjust the initial step bound.
c
            if (iter .eq. 1) delta = dmin1(delta,pnorm)
c
c           evaluate the function at x + p and calculate its norm.
c
            iflag = 1
            call fcn(m,n,wa2,wa4,iflag,
     .                 UVWPQR, time, xdim, xa, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
            nfev = nfev + 1
            if (iflag .lt. 0) go to 300
            fnorm1 = enorm(m,wa4)
c
c           compute the scaled actual reduction.
c
            actred = -one
            if (p1*fnorm1 .lt. fnorm) actred = one - (fnorm1/fnorm)**2
c
c           compute the scaled predicted reduction and
c           the scaled directional derivative.
c
            do 230 j = 1, n
               wa3(j) = zero
               l = ipvt(j)
               temp = wa1(l)
               do 220 i = 1, j
                  wa3(i) = wa3(i) + fjac(i,j)*temp
  220             continue
  230          continue
            temp1 = enorm(n,wa3)/fnorm
            temp2 = (dsqrt(par)*pnorm)/fnorm
            prered = temp1**2 + temp2**2/p5
            dirder = -(temp1**2 + temp2**2)
c
c           compute the ratio of the actual to the predicted
c           reduction.
c
            ratio = zero
            if (prered .ne. zero) ratio = actred/prered
c
c           update the step bound.
c
            if (ratio .gt. p25) go to 240
               if (actred .ge. zero) temp = p5
               if (actred .lt. zero)
     *            temp = p5*dirder/(dirder + p5*actred)
               if (p1*fnorm1 .ge. fnorm .or. temp .lt. p1) temp = p1
               delta = temp*dmin1(delta,pnorm/p1)
               par = par/temp
               go to 260
  240       continue
               if (par .ne. zero .and. ratio .lt. p75) go to 250
               delta = pnorm/p5
               par = p5*par
  250          continue
  260       continue
c
c           test for successful iteration.
c
            if (ratio .lt. p0001) go to 290
c
c           successful iteration. update x, fvec, and their norms.
c
            do 270 j = 1, n
               x(j) = wa2(j)
               wa2(j) = diag(j)*x(j)
  270          continue
            do 280 i = 1, m
               fvec(i) = wa4(i)
  280          continue
            xnorm = enorm(n,wa2)
            fnorm = fnorm1
            iter = iter + 1
  290       continue
c
c           tests for convergence.
c
            if (dabs(actred) .le. ftol .and. prered .le. ftol
     *          .and. p5*ratio .le. one) info = 1
            if (delta .le. xtol*xnorm) info = 2
            if (dabs(actred) .le. ftol .and. prered .le. ftol
     *          .and. p5*ratio .le. one .and. info .eq. 2) info = 3
            if (info .ne. 0) go to 300
c
c           tests for termination and stringent tolerances.
c
            if (nfev .ge. maxfev) info = 5
            if (dabs(actred) .le. epsmch .and. prered .le. epsmch
     *          .and. p5*ratio .le. one) info = 6
            if (delta .le. epsmch*xnorm) info = 7
            if (gnorm .le. epsmch) info = 8
            if (info .ne. 0) go to 300
c
c           end of the inner loop. repeat if iteration unsuccessful.
c
            if (ratio .lt. p0001) go to 200
c
c        end of the outer loop.
c
         go to 30
  300 continue
c
c     termination, either normal or user imposed.
c
      if (iflag .lt. 0) info = iflag
      iflag = 0
      if (nprint .gt. 0) call fcn(m,n,x,fvec,iflag,
     .                 UVWPQR, time, xdim, xa, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
      return
c
c     last card of subroutine lmdif.
c
      end
      subroutine lmdif1(fcn,m,n,x,fvec,tol,info,iwa,wa,lwa,
     .                 UVWPQR, time, xdim, xa, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
	use iso_c_binding
	implicit none
	include 'parts.h'
	integer xdim, udim
	double precision xa(xdim), xd(xdim), xo(1500), uc(udim)
	double precision UVWPQR(6), time
      integer m,n,info,lwa
      integer iwa(n)
      double precision tol
      double precision x(n),fvec(m),wa(lwa)
      external fcn
c     **********
c
c     subroutine lmdif1
c
c     the purpose of lmdif1 is to minimize the sum of the squares of
c     m nonlinear functions in n variables by a modification of the
c     levenberg-marquardt algorithm. this is done by using the more
c     general least-squares solver lmdif. the user must provide a
c     subroutine which calculates the functions. the jacobian is
c     then calculated by a forward-difference approximation.
c
c     the subroutine statement is
c
c       subroutine lmdif1(fcn,m,n,x,fvec,tol,info,iwa,wa,lwa)
c
c     where
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions. fcn must be declared
c         in an external statement in the user calling
c         program, and should be written as follows.
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
c
c         the value of iflag should not be changed by fcn unless
c         the user wants to terminate execution of lmdif1.
c         in this case set iflag to a negative integer.
c
c       m is a positive integer input variable set to the number
c         of functions.
c
c       n is a positive integer input variable set to the number
c         of variables. n must not exceed m.
c
c       x is an array of length n. on input x must contain
c         an initial estimate of the solution vector. on output x
c         contains the final estimate of the solution vector.
c
c       fvec is an output array of length m which contains
c         the functions evaluated at the output x.
c
c       tol is a nonnegative input variable. termination occurs
c         when the algorithm estimates either that the relative
c         error in the sum of squares is at most tol or that
c         the relative error between x and the solution is at
c         most tol.
c
c       info is an integer output variable. if the user has
c         terminated execution, info is set to the (negative)
c         value of iflag. see description of fcn. otherwise,
c         info is set as follows.
c
c         info = 0  improper input parameters.
c
c         info = 1  algorithm estimates that the relative error
c                   in the sum of squares is at most tol.
c
c         info = 2  algorithm estimates that the relative error
c                   between x and the solution is at most tol.
c
c         info = 3  conditions for info = 1 and info = 2 both hold.
c
c         info = 4  fvec is orthogonal to the columns of the
c                   jacobian to machine precision.
c
c         info = 5  number of calls to fcn has reached or
c                   exceeded 200*(n+1).
c
c         info = 6  tol is too small. no further reduction in
c                   the sum of squares is possible.
c
c         info = 7  tol is too small. no further improvement in
c                   the approximate solution x is possible.
c
c       iwa is an integer work array of length n.
c
c       wa is a work array of length lwa.
c
c       lwa is a positive integer input variable not less than
c         m*n+5*n+m.
c
c     subprograms called
c
c       user-supplied ...... fcn
c
c       minpack-supplied ... lmdif
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer maxfev,mode,mp5n,nfev,nprint
      double precision epsfcn,factor,ftol,gtol,xtol,zero
      data factor,zero /1.0d2,0.0d0/
      info = 0
c
c     check the input parameters for errors.
c
      if (n .le. 0 .or. m .lt. n .or. tol .lt. zero
     *    .or. lwa .lt. m*n + 5*n + m) go to 10
c
c     call lmdif.
c
      maxfev = 200*(n + 1)
      ftol = tol
      xtol = tol
      gtol = zero
      epsfcn = zero
      mode = 1
      nprint = 0
      mp5n = m + 5*n
      call lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,wa(1),
     *           mode,factor,nprint,info,nfev,wa(mp5n+1),m,iwa,
     *           wa(n+1),wa(2*n+1),wa(3*n+1),wa(4*n+1),wa(5*n+1),
     .                 UVWPQR, time, xdim, xa, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)
      if (info .eq. 8) info = 4
   10 continue
      return
c
c     last card of subroutine lmdif1.
c
      end
      subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,wa1,
     *                 wa2)
      integer n,ldr
      integer ipvt(n)
      double precision delta,par
      double precision r(ldr,n),diag(n),qtb(n),x(n),sdiag(n),wa1(n),
     *                 wa2(n)
c     **********
c
c     subroutine lmpar
c
c     given an m by n matrix a, an n by n nonsingular diagonal
c     matrix d, an m-vector b, and a positive number delta,
c     the problem is to determine a value for the parameter
c     par such that if x solves the system
c
c           a*x = b ,     sqrt(par)*d*x = 0 ,
c
c     in the least squares sense, and dxnorm is the euclidean
c     norm of d*x, then either par is zero and
c
c           (dxnorm-delta) .le. 0.1*delta ,
c
c     or par is positive and
c
c           abs(dxnorm-delta) .le. 0.1*delta .
c
c     this subroutine completes the solution of the problem
c     if it is provided with the necessary information from the
c     qr factorization, with column pivoting, of a. that is, if
c     a*p = q*r, where p is a permutation matrix, q has orthogonal
c     columns, and r is an upper triangular matrix with diagonal
c     elements of nonincreasing magnitude, then lmpar expects
c     the full upper triangle of r, the permutation matrix p,
c     and the first n components of (q transpose)*b. on output
c     lmpar also provides an upper triangular matrix s such that
c
c            t   t                   t
c           p *(a *a + par*d*d)*p = s *s .
c
c     s is employed within lmpar and may be of separate interest.
c
c     only a few iterations are generally needed for convergence
c     of the algorithm. if, however, the limit of 10 iterations
c     is reached, then the output par will contain the best
c     value obtained so far.
c
c     the subroutine statement is
c
c       subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,
c                        wa1,wa2)
c
c     where
c
c       n is a positive integer input variable set to the order of r.
c
c       r is an n by n array. on input the full upper triangle
c         must contain the full upper triangle of the matrix r.
c         on output the full upper triangle is unaltered, and the
c         strict lower triangle contains the strict upper triangle
c         (transposed) of the upper triangular matrix s.
c
c       ldr is a positive integer input variable not less than n
c         which specifies the leading dimension of the array r.
c
c       ipvt is an integer input array of length n which defines the
c         permutation matrix p such that a*p = q*r. column j of p
c         is column ipvt(j) of the identity matrix.
c
c       diag is an input array of length n which must contain the
c         diagonal elements of the matrix d.
c
c       qtb is an input array of length n which must contain the first
c         n elements of the vector (q transpose)*b.
c
c       delta is a positive input variable which specifies an upper
c         bound on the euclidean norm of d*x.
c
c       par is a nonnegative variable. on input par contains an
c         initial estimate of the levenberg-marquardt parameter.
c         on output par contains the final estimate.
c
c       x is an output array of length n which contains the least
c         squares solution of the system a*x = b, sqrt(par)*d*x = 0,
c         for the output par.
c
c       sdiag is an output array of length n which contains the
c         diagonal elements of the upper triangular matrix s.
c
c       wa1 and wa2 are work arrays of length n.
c
c     subprograms called
c
c       minpack-supplied ... dpmpar,enorm,qrsolv
c
c       fortran-supplied ... dabs,dmax1,dmin1,dsqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,iter,j,jm1,jp1,k,l,nsing
      double precision dxnorm,dwarf,fp,gnorm,parc,parl,paru,p1,p001,
     *                 sum,temp,zero
      double precision dpmpar,enorm
      data p1,p001,zero /1.0d-1,1.0d-3,0.0d0/
c
c     dwarf is the smallest positive magnitude.
c
      dwarf = dpmpar(2)
c
c     compute and store in x the gauss-newton direction. if the
c     jacobian is rank-deficient, obtain a least squares solution.
c
      nsing = n
      do 10 j = 1, n
         wa1(j) = qtb(j)
         if (r(j,j) .eq. zero .and. nsing .eq. n) nsing = j - 1
         if (nsing .lt. n) wa1(j) = zero
   10    continue
      if (nsing .lt. 1) go to 50
      do 40 k = 1, nsing
         j = nsing - k + 1
         wa1(j) = wa1(j)/r(j,j)
         temp = wa1(j)
         jm1 = j - 1
         if (jm1 .lt. 1) go to 30
         do 20 i = 1, jm1
            wa1(i) = wa1(i) - r(i,j)*temp
   20       continue
   30    continue
   40    continue
   50 continue
      do 60 j = 1, n
         l = ipvt(j)
         x(l) = wa1(j)
   60    continue
c
c     initialize the iteration counter.
c     evaluate the function at the origin, and test
c     for acceptance of the gauss-newton direction.
c
      iter = 0
      do 70 j = 1, n
         wa2(j) = diag(j)*x(j)
   70    continue
      dxnorm = enorm(n,wa2)
      fp = dxnorm - delta
      if (fp .le. p1*delta) go to 220
c
c     if the jacobian is not rank deficient, the newton
c     step provides a lower bound, parl, for the zero of
c     the function. otherwise set this bound to zero.
c
      parl = zero
      if (nsing .lt. n) go to 120
      do 80 j = 1, n
         l = ipvt(j)
         wa1(j) = diag(l)*(wa2(l)/dxnorm)
   80    continue
      do 110 j = 1, n
         sum = zero
         jm1 = j - 1
         if (jm1 .lt. 1) go to 100
         do 90 i = 1, jm1
            sum = sum + r(i,j)*wa1(i)
   90       continue
  100    continue
         wa1(j) = (wa1(j) - sum)/r(j,j)
  110    continue
      temp = enorm(n,wa1)
      parl = ((fp/delta)/temp)/temp
  120 continue
c
c     calculate an upper bound, paru, for the zero of the function.
c
      do 140 j = 1, n
         sum = zero
         do 130 i = 1, j
            sum = sum + r(i,j)*qtb(i)
  130       continue
         l = ipvt(j)
         wa1(j) = sum/diag(l)
  140    continue
      gnorm = enorm(n,wa1)
      paru = gnorm/delta
      if (paru .eq. zero) paru = dwarf/dmin1(delta,p1)
c
c     if the input par lies outside of the interval (parl,paru),
c     set par to the closer endpoint.
c
      par = dmax1(par,parl)
      par = dmin1(par,paru)
      if (par .eq. zero) par = gnorm/dxnorm
c
c     beginning of an iteration.
c
  150 continue
         iter = iter + 1
c
c        evaluate the function at the current value of par.
c
         if (par .eq. zero) par = dmax1(dwarf,p001*paru)
         temp = dsqrt(par)
         do 160 j = 1, n
            wa1(j) = temp*diag(j)
  160       continue
         call qrsolv(n,r,ldr,ipvt,wa1,qtb,x,sdiag,wa2)
         do 170 j = 1, n
            wa2(j) = diag(j)*x(j)
  170       continue
         dxnorm = enorm(n,wa2)
         temp = fp
         fp = dxnorm - delta
c
c        if the function is small enough, accept the current value
c        of par. also test for the exceptional cases where parl
c        is zero or the number of iterations has reached 10.
c
         if (dabs(fp) .le. p1*delta
     *       .or. parl .eq. zero .and. fp .le. temp
     *            .and. temp .lt. zero .or. iter .eq. 10) go to 220
c
c        compute the newton correction.
c
         do 180 j = 1, n
            l = ipvt(j)
            wa1(j) = diag(l)*(wa2(l)/dxnorm)
  180       continue
         do 210 j = 1, n
            wa1(j) = wa1(j)/sdiag(j)
            temp = wa1(j)
            jp1 = j + 1
            if (n .lt. jp1) go to 200
            do 190 i = jp1, n
               wa1(i) = wa1(i) - r(i,j)*temp
  190          continue
  200       continue
  210       continue
         temp = enorm(n,wa1)
         parc = ((fp/delta)/temp)/temp
c
c        depending on the sign of the function, update parl or paru.
c
         if (fp .gt. zero) parl = dmax1(parl,par)
         if (fp .lt. zero) paru = dmin1(paru,par)
c
c        compute an improved estimate for par.
c
         par = dmax1(parl,par+parc)
c
c        end of an iteration.
c
         go to 150
  220 continue
c
c     termination.
c
      if (iter .eq. 0) par = zero
      return
c
c     last card of subroutine lmpar.
c
      end
      subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
      integer m,n,lda,lipvt
      integer ipvt(lipvt)
      logical pivot
      double precision a(lda,n),rdiag(n),acnorm(n),wa(n)
c     **********
c
c     subroutine qrfac
c
c     this subroutine uses householder transformations with column
c     pivoting (optional) to compute a qr factorization of the
c     m by n matrix a. that is, qrfac determines an orthogonal
c     matrix q, a permutation matrix p, and an upper trapezoidal
c     matrix r with diagonal elements of nonincreasing magnitude,
c     such that a*p = q*r. the householder transformation for
c     column k, k = 1,2,...,min(m,n), is of the form
c
c                           t
c           i - (1/u(k))*u*u
c
c     where u has zeros in the first k-1 positions. the form of
c     this transformation and the method of pivoting first
c     appeared in the corresponding linpack subroutine.
c
c     the subroutine statement is
c
c       subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
c
c     where
c
c       m is a positive integer input variable set to the number
c         of rows of a.
c
c       n is a positive integer input variable set to the number
c         of columns of a.
c
c       a is an m by n array. on input a contains the matrix for
c         which the qr factorization is to be computed. on output
c         the strict upper trapezoidal part of a contains the strict
c         upper trapezoidal part of r, and the lower trapezoidal
c         part of a contains a factored form of q (the non-trivial
c         elements of the u vectors described above).
c
c       lda is a positive integer input variable not less than m
c         which specifies the leading dimension of the array a.
c
c       pivot is a logical input variable. if pivot is set true,
c         then column pivoting is enforced. if pivot is set false,
c         then no column pivoting is done.
c
c       ipvt is an integer output array of length lipvt. ipvt
c         defines the permutation matrix p such that a*p = q*r.
c         column j of p is column ipvt(j) of the identity matrix.
c         if pivot is false, ipvt is not referenced.
c
c       lipvt is a positive integer input variable. if pivot is false,
c         then lipvt may be as small as 1. if pivot is true, then
c         lipvt must be at least n.
c
c       rdiag is an output array of length n which contains the
c         diagonal elements of r.
c
c       acnorm is an output array of length n which contains the
c         norms of the corresponding columns of the input matrix a.
c         if this information is not needed, then acnorm can coincide
c         with rdiag.
c
c       wa is a work array of length n. if pivot is false, then wa
c         can coincide with rdiag.
c
c     subprograms called
c
c       minpack-supplied ... dpmpar,enorm
c
c       fortran-supplied ... dmax1,dsqrt,min0
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,j,jp1,k,kmax,minmn
      double precision ajnorm,epsmch,one,p05,sum,temp,zero
      double precision dpmpar,enorm
      data one,p05,zero /1.0d0,5.0d-2,0.0d0/
c
c     epsmch is the machine precision.
c
      epsmch = dpmpar(1)
c
c     compute the initial column norms and initialize several arrays.
c
      do 10 j = 1, n
         acnorm(j) = enorm(m,a(1,j))
         rdiag(j) = acnorm(j)
         wa(j) = rdiag(j)
         if (pivot) ipvt(j) = j
   10    continue
c
c     reduce a to r with householder transformations.
c
      minmn = min0(m,n)
      do 110 j = 1, minmn
         if (.not.pivot) go to 40
c
c        bring the column of largest norm into the pivot position.
c
         kmax = j
         do 20 k = j, n
            if (rdiag(k) .gt. rdiag(kmax)) kmax = k
   20       continue
         if (kmax .eq. j) go to 40
         do 30 i = 1, m
            temp = a(i,j)
            a(i,j) = a(i,kmax)
            a(i,kmax) = temp
   30       continue
         rdiag(kmax) = rdiag(j)
         wa(kmax) = wa(j)
         k = ipvt(j)
         ipvt(j) = ipvt(kmax)
         ipvt(kmax) = k
   40    continue
c
c        compute the householder transformation to reduce the
c        j-th column of a to a multiple of the j-th unit vector.
c
         ajnorm = enorm(m-j+1,a(j,j))
         if (ajnorm .eq. zero) go to 100
         if (a(j,j) .lt. zero) ajnorm = -ajnorm
         do 50 i = j, m
            a(i,j) = a(i,j)/ajnorm
   50       continue
         a(j,j) = a(j,j) + one
c
c        apply the transformation to the remaining columns
c        and update the norms.
c
         jp1 = j + 1
         if (n .lt. jp1) go to 100
         do 90 k = jp1, n
            sum = zero
            do 60 i = j, m
               sum = sum + a(i,j)*a(i,k)
   60          continue
            temp = sum/a(j,j)
            do 70 i = j, m
               a(i,k) = a(i,k) - temp*a(i,j)
   70          continue
            if (.not.pivot .or. rdiag(k) .eq. zero) go to 80
            temp = a(j,k)/rdiag(k)
            rdiag(k) = rdiag(k)*dsqrt(dmax1(zero,one-temp**2))
            if (p05*(rdiag(k)/wa(k))**2 .gt. epsmch) go to 80
            rdiag(k) = enorm(m-j,a(jp1,k))
            wa(k) = rdiag(k)
   80       continue
   90       continue
  100    continue
         rdiag(j) = -ajnorm
  110    continue
      return
c
c     last card of subroutine qrfac.
c
      end
      subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)
      integer n,ldr
      integer ipvt(n)
      double precision r(ldr,n),diag(n),qtb(n),x(n),sdiag(n),wa(n)
c     **********
c
c     subroutine qrsolv
c
c     given an m by n matrix a, an n by n diagonal matrix d,
c     and an m-vector b, the problem is to determine an x which
c     solves the system
c
c           a*x = b ,     d*x = 0 ,
c
c     in the least squares sense.
c
c     this subroutine completes the solution of the problem
c     if it is provided with the necessary information from the
c     qr factorization, with column pivoting, of a. that is, if
c     a*p = q*r, where p is a permutation matrix, q has orthogonal
c     columns, and r is an upper triangular matrix with diagonal
c     elements of nonincreasing magnitude, then qrsolv expects
c     the full upper triangle of r, the permutation matrix p,
c     and the first n components of (q transpose)*b. the system
c     a*x = b, d*x = 0, is then equivalent to
c
c                  t       t
c           r*z = q *b ,  p *d*p*z = 0 ,
c
c     where x = p*z. if this system does not have full rank,
c     then a least squares solution is obtained. on output qrsolv
c     also provides an upper triangular matrix s such that
c
c            t   t               t
c           p *(a *a + d*d)*p = s *s .
c
c     s is computed within qrsolv and may be of separate interest.
c
c     the subroutine statement is
c
c       subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)
c
c     where
c
c       n is a positive integer input variable set to the order of r.
c
c       r is an n by n array. on input the full upper triangle
c         must contain the full upper triangle of the matrix r.
c         on output the full upper triangle is unaltered, and the
c         strict lower triangle contains the strict upper triangle
c         (transposed) of the upper triangular matrix s.
c
c       ldr is a positive integer input variable not less than n
c         which specifies the leading dimension of the array r.
c
c       ipvt is an integer input array of length n which defines the
c         permutation matrix p such that a*p = q*r. column j of p
c         is column ipvt(j) of the identity matrix.
c
c       diag is an input array of length n which must contain the
c         diagonal elements of the matrix d.
c
c       qtb is an input array of length n which must contain the first
c         n elements of the vector (q transpose)*b.
c
c       x is an output array of length n which contains the least
c         squares solution of the system a*x = b, d*x = 0.
c
c       sdiag is an output array of length n which contains the
c         diagonal elements of the upper triangular matrix s.
c
c       wa is a work array of length n.
c
c     subprograms called
c
c       fortran-supplied ... dabs,dsqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,j,jp1,k,kp1,l,nsing
      double precision cos,cotan,p5,p25,qtbpj,sin,sum,tan,temp,zero
      data p5,p25,zero /5.0d-1,2.5d-1,0.0d0/
c
c     copy r and (q transpose)*b to preserve input and initialize s.
c     in particular, save the diagonal elements of r in x.
c
      do 20 j = 1, n
         do 10 i = j, n
            r(i,j) = r(j,i)
   10       continue
         x(j) = r(j,j)
         wa(j) = qtb(j)
   20    continue
c
c     eliminate the diagonal matrix d using a givens rotation.
c
      do 100 j = 1, n
c
c        prepare the row of d to be eliminated, locating the
c        diagonal element using p from the qr factorization.
c
         l = ipvt(j)
         if (diag(l) .eq. zero) go to 90
         do 30 k = j, n
            sdiag(k) = zero
   30       continue
         sdiag(j) = diag(l)
c
c        the transformations to eliminate the row of d
c        modify only a single element of (q transpose)*b
c        beyond the first n, which is initially zero.
c
         qtbpj = zero
         do 80 k = j, n
c
c           determine a givens rotation which eliminates the
c           appropriate element in the current row of d.
c
            if (sdiag(k) .eq. zero) go to 70
            if (dabs(r(k,k)) .ge. dabs(sdiag(k))) go to 40
               cotan = r(k,k)/sdiag(k)
               sin = p5/dsqrt(p25+p25*cotan**2)
               cos = sin*cotan
               go to 50
   40       continue
               tan = sdiag(k)/r(k,k)
               cos = p5/dsqrt(p25+p25*tan**2)
               sin = cos*tan
   50       continue
c
c           compute the modified diagonal element of r and
c           the modified element of ((q transpose)*b,0).
c
            r(k,k) = cos*r(k,k) + sin*sdiag(k)
            temp = cos*wa(k) + sin*qtbpj
            qtbpj = -sin*wa(k) + cos*qtbpj
            wa(k) = temp
c
c           accumulate the tranformation in the row of s.
c
            kp1 = k + 1
            if (n .lt. kp1) go to 70
            do 60 i = kp1, n
               temp = cos*r(i,k) + sin*sdiag(i)
               sdiag(i) = -sin*r(i,k) + cos*sdiag(i)
               r(i,k) = temp
   60          continue
   70       continue
   80       continue
   90    continue
c
c        store the diagonal element of s and restore
c        the corresponding diagonal element of r.
c
         sdiag(j) = r(j,j)
         r(j,j) = x(j)
  100    continue
c
c     solve the triangular system for z. if the system is
c     singular, then obtain a least squares solution.
c
      nsing = n
      do 110 j = 1, n
         if (sdiag(j) .eq. zero .and. nsing .eq. n) nsing = j - 1
         if (nsing .lt. n) wa(j) = zero
  110    continue
      if (nsing .lt. 1) go to 150
      do 140 k = 1, nsing
         j = nsing - k + 1
         sum = zero
         jp1 = j + 1
         if (nsing .lt. jp1) go to 130
         do 120 i = jp1, nsing
            sum = sum + r(i,j)*wa(i)
  120       continue
  130    continue
         wa(j) = (wa(j) - sum)/sdiag(j)
  140    continue
  150 continue
c
c     permute the components of z back to components of x.
c
      do 160 j = 1, n
         l = ipvt(j)
         x(l) = wa(j)
  160    continue
      return
c
c     last card of subroutine qrsolv.
c
      end
