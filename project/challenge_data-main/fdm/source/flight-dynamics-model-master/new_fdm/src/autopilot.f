	subroutine autopilot(time, xdim, x, xd, xo, udim, uc,
     .       aircraft, wing, propeller, battery, control
     .     , Xpath, Xpathdot, lstraight, radius, ipath)
c	This is the first autopilot.  We use an LQR solution
c	and the trim states to provide the requested controls udim.
c	James Davidson Walker   13 July 2021

c	7/13/21  Coding from twodtestric.f

c	Input that is used
c	xdim           state space dimension (though we only use 12)
c	x              current state
c	udim           number of controls
c	control        the control K is included in this 
c	Xpath          X Y Z in world frame of requested flight path
c	Xpathdot       U V W in world frame of requested flight path
c	lstraight      = .true. if flight path is straight
c	radius         the turning radius
c	ipath          the path - used for some temporary ifs
c
c	xtilde         current state (to our knowledge) on the requested path
c
c	Output
c	uc             Main output, the control settings



c	Can be a user created subroutine.  Various variables are
c	described in subroutine fderiv and parts.h
c	The intent is that everything on this list is input,
c	and the only output changes are the control array uc
c	and (perhaps) the internal information in the type control.

	use iso_c_binding
	implicit none

	include 'parts.h'

	integer xdim,udim

	double precision time, x(xdim), xd(xdim), xo(1500), uc(udim)

	double precision xtilde(13), xprime(13), xtrim(75)  ! changed from xtrim(13)  jdw  2/22/22
	double precision xtildeprime(13), xdiff(12)
	double precision Xpath(3), Xpathdot(3), radius
        double precision path_data(1000)
	double precision K(50,12), uctrim(50)
	double precision cospsi, sinpsi, coshalfpsi, sinhalfpsi
	double precision temp
	double precision xprime7orig, deltauc(50)
	double precision g_psi, l_psi, g_psi_old, l_psi_old
	
	integer i, j, m, qskip, ipath

	logical ltrimset, lstraight

	double precision tblend, alpha, alpham1
        integer          i_speed_path_low, i_speed_path_high
        double precision speed_path, speed_path_low, speed_path_high
        integer, save :: i_trim_state, i_trim_state_last

c	Coding to update uc, etc., follows.

c	This initial coding using trim states.  We load the relevant trim
c	state (or try to interpolate to find one).

c	As long as things are working, we will not be updating the controller.

	control%compute_A = .false.

	ltrimset = .false.

	tblend = control%tblend

c	Now we have trimstates related to speed and radii of curvature.  We look
c	at the turning states first, since we will go with the lateral states if
c	the there are no appropriate turning states.

	if ((.not.lstraight).and.(ipath.eq.3)) then

c	   For now, we look to see if the turning radius is within 100 meters of
c	   our trim state turning radii.
c	write (6,*) ' Here with lstraight,radii = ',lstraight,radius,
c     .         control%radius_1, control%radius_2

	   if ((control%radius_1.ne.0.d0).and.
     .         (abs(radius - control%radius_1).lt.100.d0)) then

	      do i = control%turn_1_num_trim_min
     .             , control%turn_1_num_trim_max
	         if (control%turn_1_trim_if(i)) then
	            if (abs(control%turn_1_trim_speed(i)
     .                 -control%requested_lateral_speed).lt.0.05d0) then
c	               We are close enough to a trim state that we will use it

c	write (6,*) ' Here 1 in autopilot radius radius_1'
c     .              ,radius,control%radius_1

	               do j = 1, xdim
	                  xtrim(j) = control%turn_1_trim_x (i,j)
	               enddo
	               do j = 1, udim
		          do m = 1, 12
		             K(j,m) = control%turn_1_trim_K(i,j,m)
		          enddo
	               enddo
	               qskip = control%turn_1_trim_qskip(i)
	               do j = 1, udim
		          uctrim(j) = control%turn_1_trim_uc(i,j)
	               enddo


c	Here we will try a blend, to see if this approach can do something
c	for us

	if (time.lt.tblend) then

	   alpha = 1.d0 - time/tblend
	   alpha = max(0.d0, alpha)
	   alpha = min(1.d0, alpha)
	   alpham1 = 1.d0 - alpha

	   do j = 1, xdim
	      xtrim(j) = alpham1 * control%turn_1_trim_x  (i,j)
     .                 + alpha   * control%lateral_trim_x (i,j)
	   enddo
	   do j = 1, udim
	      do m = 1, 12
	         K(j,m) = alpham1 * control%turn_1_trim_K (i,j,m)
     .                  + alpha   * control%lateral_trim_K(i,j,m)
	      enddo
	   enddo
	   qskip = control%turn_1_trim_qskip(i)
	   do j = 1, udim
	      uctrim(j) = alpham1 * control%turn_1_trim_uc (i,j)
     .                  + alpha   * control%lateral_trim_uc(i,j)
	   enddo
	
	endif




		       ltrimset = .true.
	            endif
	         endif
	      enddo

	   endif

	   if (ltrimset) goto 200


	   if ((control%radius_2.ne.0.d0).and.
     .         (abs(radius - control%radius_2).lt.100.d0)) then 

	      do i = control%turn_2_num_trim_min
     .             , control%turn_2_num_trim_max
	         if (control%turn_2_trim_if(i)) then
	            if (abs(control%turn_2_trim_speed(i)
     .                 -control%requested_lateral_speed).lt.0.05d0) then
c	               We are close enough to a trim state that we will use it
c	write (6,*) ' Here 2 in autopilot radius radius_2'
c     .              ,radius,control%radius_2
	               do j = 1, xdim
	                  xtrim(j) = control%turn_2_trim_x (i,j)
	               enddo
	               do j = 1, udim
		          do m = 1, 12
		             K(j,m) = control%turn_2_trim_K(i,j,m)
		          enddo
	               enddo
	               qskip = control%turn_2_trim_qskip(i)
	               do j = 1, udim
		          uctrim(j) = control%turn_2_trim_uc(i,j)
	               enddo
		       ltrimset = .true.
	            endif
	         endif
	      enddo
	
	   endif

	   if (ltrimset) goto 200

	endif

c        speed_path = Xpathdot(1)**2 + Xpathdot(2)**2 + Xpathdot(3)**2
c       For right now, we are separating out vertical and horizontal motion
        speed_path = Xpathdot(1)**2 + Xpathdot(2)**2
        speed_path = sqrt(speed_path)
        i_speed_path_low  = -1
        i_speed_path_high = -1
	do i =control%lateral_num_trim_min, control%lateral_num_trim_max
	   if (control%lateral_trim_if(i)) then
c
c             Given lots of trim states, it worked to just look for one very nearby (within 0.5 m/s).
c             Now we want to see if we can fly with sparser trim states.  The question is how to choose one.
c             Our first choice is to pick the nearest one that is larger, but the problem with that
c             is that there may not be a larger one.
c
              if (control%lateral_trim_speed(i).lt.speed_path) then
                 if (i_speed_path_low.eq.-1) then
                   i_speed_path_low = i
                   speed_path_low = control%lateral_trim_speed(i)
                 else
                   if (control%lateral_trim_speed(i).gt.speed_path_low)
     .                then
                     i_speed_path_low = i
                     speed_path_low = control%lateral_trim_speed(i)
                   endif
                 endif
              else   ! this means that lateral trim speed is ge speed path
                 if (i_speed_path_high.eq.-1) then
                   i_speed_path_high = i
                   speed_path_high = control%lateral_trim_speed(i)
c                  If it is assumed the trim state speeds are  in order, then
c                  we can jump out of this loop at this point.
                   goto 840
                 else
                   if(control%lateral_trim_speed(i).lt.speed_path_high)
     .                then
                     i_speed_path_high = i
                     speed_path_high = control%lateral_trim_speed(i)
                   endif
                 endif
              endif
           endif
        enddo
 840    continue

c       Now we should have a bracket of trim states on the current speed,
c       or hopefully at least one trim state.
        if ((i_speed_path_low.ne.-1).and.(i_speed_path_high.ne.-1)) then
           if ((speed_path - speed_path_low)
     .        .lt.(speed_path_high-speed_path)) then
              i = i_speed_path_low
           else
              i = i_speed_path_high
           endif
        else if ((i_speed_path_low.eq.-1).and.(i_speed_path_high.ne.-1))
     .      then
           i = i_speed_path_high
        else if ((i_speed_path_low.ne.-1).and.(i_speed_path_high.eq.-1))
     .      then
           i = i_speed_path_low
        else
           write (6,*) '  Did not find a trim state.'
           stop
        endif
c
c       ***** This is a test
        if (i.eq.0) i = 1   ! though labelled as a test, for right now we always use the lateral 1 m/s, even for vertical ascent  jdw 9/27/22
c        if (i.eq.45) i = 46  ! this solved a particular problem with the Axe - so commented out that problem is not fixed.  jdw  9/27/22

c        i = 1  ! hard wire for vertical ascent tests  9/15/22  !!  is this truly what's been happening?  jdw  9/27/22
c        if (i.gt.6) i = i_speed_path_high  didn't seem to work, so we will need to try to look at this again sometime.
c       ***** end of test

        i_trim_state = i
        control%i_trim_state = i

        if (i_trim_state.ne.i_trim_state_last) then
c           write (6,*) '  Reference trim state change, time, old, new '
c     .                   ,time,i_trim_state_last,i
           i_trim_state_last = i
        endif
c
c	      if (abs(control%lateral_trim_speed(i)-speed_path)
c     .                      .le.0.5d0) then ! we need some control settings
cold     .               -control%requested_lateral_speed).lt.0.05d0) then
c	         We are close enough to a trim state that we will use it

	         do j = 1, xdim
	            xtrim(j) = control%lateral_trim_x (i,j)
	         enddo
	         do j = 1, udim
		    do m = 1, 12
		       K(j,m) = control%lateral_trim_K(i,j,m)
		    enddo
	         enddo
	         qskip = control%lateral_trim_qskip(i)
	         do j = 1, udim
		    uctrim(j) = control%lateral_trim_uc(i,j)
	         enddo
		 ltrimset = .true.
c        write (6,*) ' Here 2 in autopilot : control i = ',i,Xpathdot(1)
c	      endif
c	   endif
c	enddo

 200	continue

c	do i=1,udim
c	   write (6,100) ' 1st K = ',(K(i,j),j=1,12)
c	enddo


c	In this region we look around and interpolate, or perhaps we
c	try to solve for a specific case.
	
	if (.not.ltrimset) then

c	   This is not yet written

	endif

c	We build a pseudo xtilde state vector - there is no PQR or  q0

	xtilde( 1) = Xpathdot( 1)
	xtilde( 2) = Xpathdot( 2)
	xtilde( 3) = Xpathdot( 3)
	xtilde( 4) = 0.d0
	xtilde( 5) = 0.d0
	xtilde( 6) = 0.d0
	xtilde( 7) = 1.d0
	xtilde( 8) = 0.d0
	xtilde( 9) = 0.d0
	xtilde(10) = 0.d0
	xtilde(11) = Xpath( 1)
	xtilde(12) = Xpath( 2)
	xtilde(13) = Xpath( 3)

c	Our first pass through this to fly a planar path relying on the
c	controls as determined during the trim state computations.
c	Given that plan, we need to think through what we mean here.
c	In particular, we expect to just rotate in the plane, so
c	what does that mean in terms of the quaternions and the location.
c	As I see it, it means that we look at the required rotation 
c	based on the north direction (trim state) vs. the xtilde direction,
c	again, just looking at the X-Y plane.  Thus, we compute the
c	angle in the X-Y plane:

c	The dot product of (1,0) and (xtilde,ytilde) for the yaw psi.
	temp   = sqrt(xtilde(1)**2+xtilde(2)**2)
	if (temp.gt.0.d0) then
	   cospsi = xtilde(1)/temp  ! not consider Z direction
	   sinpsi = xtilde(2)/temp
	else
c	   Assume no rotation
	   cospsi = 1.d0
	   sinpsi = 0.d0
	endif

	cospsi = max(cospsi,-1.d0)
	cospsi = min(cospsi, 1.d0)

c	We are looking at how to address the double wrap problem with the half
c	angle (i.e., with the quaternion).  The atan2(sin,cos) returns values
c	from - pi to + pi, with the disontinuity at +- pi.  If I am going to
c	track the double wrap (not sure that will solve the problem, though)
c	I need to let psi range from - 2 pi to + 2 pi.  So I need to pay attention
c	to what happens at +- pi.  I'm assuming some smoothness here.
c	The thinking is that if g_psi > pi/2, say, then we expect the new
c	g_psi to be in the range of 0 to  2 pi, and so if g_psi_temp < 0 
c	g_psi_temp = atan2(sinpsi,cospsi)
c	d_g_psi = 
c	So - pi < g_psi_temp <= pi
c	if (g_psi_temp.gt.0.d0) then
c           if (
c        if (g_psi.ge.twopi) g_psi = g_psi_temp + twopi
c	if (g_psi.le.-twopi) g_psi = g
c	Okay - so what is the problem.  We want to go to - 2 pi so that the
c	wrap comes back to pi.  We want to go to 2 pi so that the wrap comes
c	back to pi.
c                 X
c                 ^
c	      II  |   I      if psi is computed in I then it can be a large 
c           -------------> Y negative psi or a small positive psi.
c             III |  IV      We need to know the previous angle.
c
c	if ((cospsi.ge.0.d0).and.(sinpsi.ge.0.d0)) then
c	   We are in quadrant I
c	   if (g_psi.gt.-0.25d0*pi).and.(g_psi.lt.


c	This rotation which needs to act in two ways - to
c	rotate xtilde to x, rotate xtilde dot to xdot, and rotate the quaternion.
c	Then the resulting uc should be fine as is, not requiring any adjustments.
c	The matrix (c s , -s c) rotates xtildedot,ytildedot to (1,0) times
c	sqrt(tildedot**2+ytildedot**2).

c	So xprime is intended to be the desired trim state.  Given that, we
c	need to set it up.  Since we are staying in the plane for this, the
c	speed is given by xprime(1) and it should be close to the requested
c	speed.  Hence, we only have a yaw.

	xprime( 1) =   x(1)
	xprime( 2) =   x(2)
	xprime( 3) =   x(3)
	xprime( 4) =   x(4)
	xprime( 5) =   x(5)
	xprime( 6) =   x(6)

c	We need to move the quaternion for x into the correct orientation.
c	To do the quaternion multiplication, we need the appropriate half angles.

	if (temp.gt.0.) then

	   coshalfpsi = sqrt((1.d0 + cospsi)/2.d0)        ! this is always positive
	   sinhalfpsi = sqrt((1.d0 - cospsi)/2.d0)
	   if (sinpsi.lt.0.d0) sinhalfpsi = - sinhalfpsi  ! this preserves the sign of sinpsi

	else
	
	   coshalfpsi = 1.d0
	   sinhalfpsi  = 0.d0

	endif

c	We reverse the sign so that we have -psi/2, as requested below (but we do not 
c	reverse the sign of psi because that is the correct rotation for the location).
	sinhalfpsi = - sinhalfpsi  ! this reverses the sign of psi 

c	write (6,*) ' Same from autopilot '
c     .              ,sngl(cospsi),sngl(sinpsi),sngl(coshalfpsi)
c     .              ,sngl(sinhalfpsi)
c     .              ,sngl(rad_to_deg*atan2(sinpsi,cospsi))
c     .              ,sngl(rad_to_deg*atan2(sinhalfpsi,coshalfpsi))


c	Now, what is the order of multiplication.  We have the current q.  If we came
c	from the initializtion, then we have q = q_psi * q_trim.  This argues that
c	the correct multiplication is q_uc eval = q_(-psi)*q = q_(-psi)*q_psi*q_trim = q_trim.

c	This is q_(-psi)*q_x
	xprime( 7) = coshalfpsi*x( 7) - sinhalfpsi*x(10)
	xprime( 8) = coshalfpsi*x( 8) - sinhalfpsi*x( 9)
	xprime( 9) = coshalfpsi*x( 9) + sinhalfpsi*x( 8)
	xprime(10) = coshalfpsi*x(10) + sinhalfpsi*x( 7)

c	Okay, there is another way to deal with the double wrap problem, though it
c	essentially assumes some things - probably in particular that the vehicle
c	is going in roughly the intended direction.  For our trim state
c	we have that q0 is around +1.  Thus, if our half angle rotation is not
c	correct, it will likely flip the sign of the quaternion.  Thus, our
c	attempt at a simple fix is as follows:
	xprime7orig = xprime(7)
	if (xprime(7).lt.0.d0) then
	   do i = 7, 10
	      xprime(i) = - xprime(i)
	   enddo
	endif


c	This is rotating the X location into the trim state
cold	This coding worked for straight lines.
cold	xprime(11) =   cospsi * x(11) + sinpsi * x(12)
cold	xprime(12) = - sinpsi * x(11) + cospsi * x(12)
cold	xprime(13) =   x(13)
c	However, for curves we need a translation followed by
c	a rotation.  Our choice is to move Xpath to the origin for the 
c	trim state.  Thus, we move x the same before performing the
c	same rotation as with the straight line paths.
	xprime(11) =   cospsi * (x(11) - Xpath(1)) 
     .               + sinpsi * (x(12) - Xpath(2))
	xprime(12) = - sinpsi * (x(11) - Xpath(1))
     .               + cospsi * (x(12) - Xpath(2))
	xprime(13) =   x(13) - Xpath(3)


c       **** this is a test trial
c       What if we remove the X dependence here
c	xprime(11) =   sinpsi * (x(12) - Xpath(2))
c	xprime(12) =   cospsi * (x(12) - Xpath(2))
c	xprime(13) =   x(13) - Xpath(3)
c       This showed that you do not want to remove
c       the x11 error from the flight.  In this case
c       the vehicle flew much faster than requested.
c       **** end of test trial


c	We move the Xpath trim state.  tilde prime is the trim state except
c	for the location information which comes from the flight path.

	do i = 1, 10
	   xtildeprime(i) = xtrim(i)
	enddo
c	This I think just rotates in the plane, nothing out of plane.
cold	xtildeprime(11) =   cospsi * xtilde(11) + sinpsi * xtilde(12)
cold	xtildeprime(12) = - sinpsi * xtilde(11) + cospsi * xtilde(12)
cold	xtildeprime(13) =   xtilde(13)
c	The translation to xtilde takes us to the origin.  the trim state
c	should be at the origin, but to be sure we pin it down.
c	This is xtilde - Xpath
	xtildeprime(11) = 0.d0
	xtildeprime(12) = 0.d0
	xtildeprime(13) = 0.d0

c	We now construct the difference that will be used in the control law.
c	This also includes the qskip contraction.
	do i = 1, 6
	   xdiff(i) = xprime( i) - xtildeprime( i)
	enddo
	if (qskip.eq.0) then
	   xdiff(7) = xprime( 8) - xtildeprime( 8)
	   xdiff(8) = xprime( 9) - xtildeprime( 9)
	   xdiff(9) = xprime(10) - xtildeprime(10)
	else if (qskip.eq.1) then
	   xdiff(7) = xprime( 7) - xtildeprime( 7)
	   xdiff(8) = xprime( 9) - xtildeprime( 9)
	   xdiff(9) = xprime(10) - xtildeprime(10)
	else if (qskip.eq.2) then
	   xdiff(7) = xprime( 7) - xtildeprime( 7)
	   xdiff(8) = xprime( 8) - xtildeprime( 8)
	   xdiff(9) = xprime(10) - xtildeprime(10)
	else
	   xdiff(7) = xprime( 7) - xtildeprime( 7)
	   xdiff(8) = xprime( 8) - xtildeprime( 8)
	   xdiff(9) = xprime( 9) - xtildeprime( 9)
	endif
	do i = 10, 12
	   xdiff(i) = xprime(i+1) - xtildeprime(i+1)
	enddo
	do i = 1, udim
	   uc(i) = uctrim(i)
	enddo

c	We are now ready to compute the updated control values
c	uc = uctrim + K*(x - xtilde)
	do i= 1, udim
	   uc(i) = uctrim(i)
	   do j = 1, 12
	      uc(i) = uc(i) + K(i,j)*xdiff(j)
	   enddo
	   deltauc(i) = uc(i) - uctrim(i)
	   uc(i) = max(uc(i),0.d0)
	   uc(i) = min(uc(i),1.d0)
	enddo

c	write (55,100) ' xtildeprime ',xtildeprime(1),xtildeprime(2)
c     .               ,xtildeprime(3)
c     .               ,xtildeprime(11),xtildeprime(12),xtildeprime(13)

c	write (6,100) ' Time = ',time
c	write (6,100) ' xtrim = ',xtrim(1:13)
c	write (6,100) ' x = ',x(1:13)
c	write (6,100) ' xprime = ',xprime(1:13)
c	write (6,100) ' xtprime = ',xtildeprime(1:13)
c	write (6,100) ' xdiff = ',xdiff(1:12)
c	if (udim.lt.14) then
c	   write (6,100) ' utrim ',uctrim(1:udim)
c	   write (6,100) ' uc    ',uc(1:udim)
c	   write (6,100) ' delta uc ',deltauc(1:udim)
c	else
c	   write (6,100) ' utrim ',uctrim(1:13)
c	   write (6,100) ' utrim 2 ',uctrim(14:udim)
c	   write (6,100) ' uc    ',uc(1:13)
c	   write (6,100) ' uc    2 ',uc(14:udim)
c	   write (6,100) ' delta uc ',deltauc(1:13)
c	   write (6,100) ' delta uc 2 ',deltauc(14:udim)
c	endif
c	do i = 1, udim
c	   write (6,100) ' K = ',(K(i,j),j=1,12)
c	   write (6,100) ' K*xdiff = ',(K(i,j)*xdiff(j),j=1,12)
c	enddo
c	write (6,*)

c	if (time.gt.0.d0) 
c     .   write (57,57) time,cospsi,sinpsi,coshalfpsi,sinhalfpsi,
c     .         xdiff(7),xdiff(8),xdiff(9),xdiff(10),
c     .         xprime(7),xprime(8),xprime(9),xprime(10)
c     .        ,xprime7orig
 57	format(15e12.4)
c	if (time.gt.0.d0) 
c     .    write (58,57) time, (uctrim(i),i=1,udim),(uc(i),i=1,udim)
c     .      ,(deltauc(i),i=1,udim)
c	
 100	format(a14,13e12.4)
c        write (57,101) time,(uc(i),i=1,udim),speed_path
c     .      ,float(i_trim_state)
 101    format(f12.5,13(1pe12.5))

	return
	end


	subroutine path_error(time, dt, speed, xdim, x, Xpath, Xpathdot
     .       , l1_error, l_max_error, l1_error_int, longitudinal_error
     .       , path_distance
     .       , max_error_time, max_error_location, max_error_velocity)

c	This determines the approximate error off the flight path.
c	We use the tangent line to Xpath at the time of interest
c	as where we measure the distance away.

c	Input: time      (to save)
c	       dt        time step to compute ddistance
c	       speed     to compute d distance = speed * dt
c	       xdim      dimension of x
c	       x         the state vector
c	       Xpath     (X Y Z) of requested path
c	       Xpathdot  (U V W) in world frame of path
c	       path_distance   distance so far traveled along flight path
c
c	Input/output
c	       l1_error            distance between x and Xpath tangent line
c	       l_max_error         maximum l1_error over the flight
c	       l1_error_int        spatial integral of abs(l1_error)
c	       max_error_time      time of l_max_error
c	       max_error_location  spatial location of l_max_error
c	       max_error_velocity  vehicle velocity at time of l_max_error

	implicit none

	integer xdim
	double precision x(xdim)
	double precision time, dt, speed, Xpath(3), Xpathdot(3)
	double precision l1_error, l_max_error, l1_error_int
	double precision max_error_time, max_error_location(3)
	double precision path_distance , max_error_velocity(3)
	double precision longitudinal_error

	double precision temp, x_diff, y_diff, z_diff
	double precision t_x, t_y, t_z

c	We want to determine the distance from the tangent line to
c	Xpath, since we are trying to approximate distance away from
c	the path and not how off the timing may be.  There could be
c	errors on curved regions of the path.

	x_diff = x(11) - Xpath(1)
	y_diff = x(12) - Xpath(2)
	z_diff = x(13) - Xpath(3)

	temp   = sqrt(Xpathdot(1)**2 + Xpathdot(2)**2 + Xpathdot(3)**2)
	if (temp.gt.0.d0) then
	   t_x = Xpathdot(1)/temp
	   t_y = Xpathdot(2)/temp
	   t_z = Xpathdot(3)/temp
	else
	   t_x = 0.d0
	   t_y = 0.d0
	   t_z = 0.d0
	endif

c	We remove the tangential displacement
	temp   = t_x * x_diff + t_y * y_diff + t_z * z_diff
	x_diff = x_diff - temp * t_x
	y_diff = y_diff - temp * t_y
	z_diff = z_diff - temp * t_z

	longitudinal_error = temp

c	What remains is the error
	l1_error = sqrt(x_diff**2 + y_diff**2 + z_diff**2)
	if (l1_error.gt.l_max_error) then
	   l_max_error        = l1_error
	   max_error_time     = time
	   max_error_location = Xpath
	   max_error_velocity = Xpathdot
	endif
c	The l1_error_int is normalized by flight distance.
	l1_error_int = (l1_error_int * (path_distance - speed * dt)
     .                 + l1_error * speed * dt )/ path_distance
	
	return
	end


	subroutine flightpath(iHackathon, ipath, time, timeslip, speed
     .             , Xpath, Xpathdot, lstraight, radius, icomplete
     .             , path_data)
c	This is the contoller subroutine for calling various flight paths.
c	James Walker  14 July 2021

c	Input: iHackathon - index to which paths to call
c	       ipath - which flightpath_i() subroutine to call
c	       time  - current time
c	       tslip - intentional slip on the time (I can envision someone falling behind but
c		       not wanting to always be trying to catch up)
c	Output:
c	       Xpath(3)    - the XYZ of the flight path in the world frame.
c	       Xpathdot(3) - the UVW of the flight path in the world frame.
c	       radius      - the turning radius
c	       icomplete   - some paths are completed, and this is true of so.

	use iso_c_binding
	implicit none

	include 'parts.h'

	integer ipath, iHackathon
	double precision time, timeslip, speed, Xpath(3), Xpathdot(3)
        double precision path_data(1000)
	double precision radius
	logical icomplete, lstraight

c	This is simply a clearinghouse to call the various flight paths, to simplify and
c	distribute how they are written.  Right now we go up to 10, but this can clearly
c	be expanded.

	if (iHackathon.eq.0) then
	
	   if (ipath.eq.      1) then
	      call flightpath_1  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete)
	   else if (ipath.eq. 2) then
	      call flightpath_2  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete)
	   else if (ipath.eq. 3) then
	      call flightpath_3  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete)
	   else if (ipath.eq. 4) then
	      call flightpath_4  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete)
	   else if (ipath.eq. 5) then
	      call flightpath_5  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete)
c	   else if (ipath.eq. 6) then
c	      call flightpath_6  (time, timeslip, speed, Xpath, Xpathdot
c     .                , lstraight, radius, icomplete)
c	   else if (ipath.eq. 7) then
c	      call flightpath_7  (time, timeslip, speed, Xpath, Xpathdot
c     .                , lstraight, radius, icomplete)
c	   else if (ipath.eq. 8) then
c	      call flightpath_8  (time, timeslip, speed, Xpath, Xpathdot
c     .                , lstraight, radius, icomplete)
c	   else if (ipath.eq. 9) then
c	      call flightpath_9  (time, timeslip, speed, Xpath, Xpathdot
c     .                , lstraight, radius, icomplete)
c	   else if (ipath.eq.10) then
c	      call flightpath_10 (time, timeslip, speed, Xpath, Xpathdot
c     .                , lstraight, radius, icomplete)
	   else
	      write (6,*) ' No such flight path number ',ipath
	   endif

	else if (iHackathon.eq.1) then

	   if (ipath.eq.      1) then
	      call flightpath_1H1  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete)
	   else if (ipath.eq. 2) then
	      call flightpath_1H1  (time, timeslip, speed, Xpath, Xpathdot  ! this is on purpose: 1 = 2 here
     .                , lstraight, radius, icomplete)
	   else if (ipath.eq. 3) then
	      call flightpath_3H1  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete)
	   else if (ipath.eq. 4) then
	      call flightpath_4H1  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete)
	   else if (ipath.eq. 5) then
	      call flightpath_5H1  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete)
	   else if (ipath.eq. 6) then  ! 9/15/22  Moved new paths back to Hackathon 1, as requested by Vanderbilt
	      call flightpath_6H1  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete, path_data)
	   else if (ipath.eq. 7) then
	      call flightpath_7H1  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete, path_data)
	   else if (ipath.eq. 8) then
	      call flightpath_8H1  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete, path_data)
	   else if (ipath.eq. 9) then
	      call flightpath_9H1  (time, timeslip, speed, Xpath, Xpathdot
     .                , lstraight, radius, icomplete, path_data)
	   else
	      write (6,*) ' No such flight path number ',ipath
	   endif

c	else if (iHackathon.eq.2) then      !  8/17/22  New variable speed flight paths  JDW
c
c	   if (ipath.eq.      1) then
c	      call flightpath_1H2  (time, timeslip, speed, Xpath, Xpathdot
c     .                , lstraight, radius, icomplete, path_data)
c	   else
c	      write (6,*) ' No such flight path number ',ipath
c	   endif
 
 	else
 	
	   write (6,*) ' No such Hackathon ',iHackathon
	
	endif

	return
	end


	subroutine flightpath_1 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete)

c	This is an example of the simplest flight path possible - a stright line.

	implicit none

	logical icomplete, lstraight

	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)
	double precision radius

c	speed = 20.d0  ! meters per second

	icomplete = .false.  ! this goes on forever

	Xpath( 1) =  speed*(time - timeslip) ! X
	Xpath( 2) =  0.d0                    ! Y
	Xpath( 3) =  0.d0                    ! Z

	Xpathdot( 1) =  speed    ! U   this is in the world frame
	Xpathdot( 2) =  0.d0     ! V
	Xpathdot( 3) =  0.d0     ! W

	lstraight = .true.
	radius = 0.d0

	return
	end
	

	subroutine flightpath_2 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete)

c	This is an example of the simplest flight path possible - a stright line.

	use iso_c_binding
	implicit none

	include 'pi.h'
	
	logical icomplete, lstraight

	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)
	double precision radius

	double precision theta

	icomplete = .false.  ! this goes on forever

c	theta = 179.99d0*deg_to_rad
	theta = 180.d0*deg_to_rad

c	speed = 20.d0  ! meters per second

	Xpath( 1) =  speed*(time - timeslip)*cos(theta) ! X
	Xpath( 2) =  speed*(time - timeslip)*sin(theta) ! Y
	Xpath( 3) =  0.d0                    ! Z

	Xpathdot( 1) =  speed*cos(theta)    ! U   this is in the world frame
	Xpathdot( 2) =  speed*sin(theta)    ! V   this is in the world frame
	Xpathdot( 3) =  0.d0     ! W

	lstraight = .true.
	radius = 0.d0

	return
	end

	subroutine flightpath_3 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete)

c	This is a big circle

	use iso_c_binding
	implicit none

	include 'pi.h'

	logical icomplete, lstraight

	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)

	double precision theta, radius, a

	icomplete = .false.

	radius = 500.d0  ! in meters

	a = speed/radius

	theta = a*(time - timeslip)

	Xpath( 1) =  radius*cos(theta) ! X
	Xpath( 2) =  radius*sin(theta) ! Y
	Xpath( 3) =  0.d0              ! Z

	Xpathdot( 1) =  - speed*sin(theta)    ! U   this is in the world frame
	Xpathdot( 2) =    speed*cos(theta)    ! V   this is in the world frame
	Xpathdot( 3) =    0.d0                ! W

	if (theta.gt.twopi) icomplete = .true.

	lstraight = .false.
	radius = radius ! the sign for the orientation of clockwise from above

	return
	end


	subroutine flightpath_4 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete)

c	This is a vertical ascent and landing

	implicit none

	logical icomplete, lstraight

	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)
	double precision radius

	double precision height

	icomplete = .false.

	height = -200.d0  ! in meters, negative for up

	Xpath( 1) =  0.d0 ! X
	Xpath( 2) =  0.d0 ! Y
	Xpath( 3) =  speed*(time - timeslip)     ! Z
	if (Xpath( 3).lt.height) Xpath (3) = height   ! Z

	Xpathdot( 1) =  0.d0    ! U   this is in the world frame
	Xpathdot( 2) =  0.d0    ! V   this is in the world frame
	Xpathdot( 3) =  speed                ! W
	if (Xpath( 3).lt.height) Xpathdot( 3) =  0.d0        ! W

	lstraight = .true.
	radius = 0.d0

	return
	end


	subroutine flightpath_5 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete)

c	This is a racing oval

	use iso_c_binding
	implicit none

	include 'pi.h'

	logical icomplete, lstraight

	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)

	double precision theta, radius, a
	double precision side, arc, distance
	double precision height

	icomplete = .false.

	side   =  1000.d0
	radius =   250.d0  ! in meters
	height = - 200.d0

	a = speed/radius
	
	arc = pi*radius

	distance = speed*(time - timeslip)

	if (distance.lt.side) then

	   Xpath( 1) =  distance ! X
	   Xpath( 2) =    0.d0                    ! Y
	   Xpath( 3) =  height                 ! Z

	   Xpathdot( 1) =  speed    ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V
	   Xpathdot( 3) =  0.d0     ! W

	   lstraight = .true.
	   radius = 0.d0

	else if (distance.lt.(side+arc)) then

	   theta = (distance - side)/radius

	   Xpath( 1) =  side + radius*sin(theta)          ! X
	   Xpath( 2) =         radius*(cos(theta) - 1.d0) ! Y
	   Xpath( 3) =  height                            ! Z

	   Xpathdot( 1) =    speed*cos(theta)    ! U   this is in the world frame
	   Xpathdot( 2) =  - speed*sin(theta)    ! V   this is in the world frame
	   Xpathdot( 3) =    0.d0                ! W

	   lstraight = .false.
	   radius = - radius

	else if (distance.lt.(2.d0*side+arc)) then

	   Xpath( 1) =  side - (distance - arc - side) ! X
	   Xpath( 2) =  -2.d0 * radius                 ! Y
	   Xpath( 3) =  height                         ! Z

	   Xpathdot( 1) =  - speed    ! U   this is in the world frame
	   Xpathdot( 2) =    0.d0     ! V
	   Xpathdot( 3) =    0.d0     ! W

	   lstraight = .true.
	   radius = 0.d0

	else 

	   theta = (distance - 2.d0*side - arc)/radius

	   Xpath( 1) =       - radius*sin(theta)          ! X
	   Xpath( 2) =       - radius*(cos(theta) + 1.d0) ! Y
	   Xpath( 3) =       - 200.d0                     ! Z

	   Xpathdot( 1) =  - speed*cos(theta)    ! U   this is in the world frame
	   Xpathdot( 2) =    speed*sin(theta)    ! V   this is in the world frame
	   Xpathdot( 3) =    0.d0                ! W

	   if (theta.gt.pi) icomplete = .true.

	   lstraight = .false.
	   radius = - radius

	endif

	return
	end


	subroutine flightpath_1H1 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete)

c	This is an example of the simplest flight path possible - a stright line.

	implicit none

	logical icomplete, lstraight

	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)
	double precision radius, height

	height = -200.d0  ! in meters

c	speed = 20.d0  ! meters per second

	icomplete = .false.  ! this goes on forever

	Xpath( 1) =  speed*(time - timeslip) ! X
	Xpath( 2) =  0.d0                    ! Y
	Xpath( 3) =  height                  ! Z

	Xpathdot( 1) =  speed    ! U   this is in the world frame
	Xpathdot( 2) =  0.d0     ! V
	Xpathdot( 3) =  0.d0     ! W

	lstraight = .true.
	radius = 0.d0

	return
	end
	

	subroutine flightpath_3H1 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete)

c	This is a big circle

	use iso_c_binding
	implicit none

	include 'pi.h'

	logical icomplete, lstraight

	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)
	double precision height

	double precision theta, radius, a

	icomplete = .false.

c	radius = 500.d0  ! in meters
c	height =   0.d0  

	radius =   500.d0  ! in meters
	height = - 250.d0  

	a = speed/radius

	theta = a*(time - timeslip)

	Xpath( 1) =  radius*cos(theta) ! X
	Xpath( 2) =  radius*sin(theta) ! Y
	Xpath( 3) =  height            ! Z

	Xpathdot( 1) =  - speed*sin(theta)    ! U   this is in the world frame
	Xpathdot( 2) =    speed*cos(theta)    ! V   this is in the world frame
	Xpathdot( 3) =    0.d0                ! W

	if (theta.gt.twopi) icomplete = .true.

	lstraight = .false.
	radius = radius ! the sign for the orientation of clockwise from above

	return
	end


	subroutine flightpath_4H1 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete)

c	This is a vertical ascent and landing

	implicit none

	logical icomplete, lstraight

	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)
	double precision radius

	double precision height

	icomplete = .false.

c	height = -200.d0  ! in meters, negative for up

	height = -150.d0  ! in meters, negative for up

	Xpath( 1) =  0.d0 ! X
	Xpath( 2) =  0.d0 ! Y
	Xpath( 3) =  speed*(time - timeslip)     ! Z
	if (Xpath( 3).lt.height) Xpath (3) = height   ! Z

	Xpathdot( 1) =  0.d0    ! U   this is in the world frame
	Xpathdot( 2) =  0.d0    ! V   this is in the world frame
	Xpathdot( 3) =  speed                ! W
	if (Xpath( 3).lt.height) Xpathdot( 3) =  0.d0        ! W

	lstraight = .true.
	radius = 0.d0

	return
	end


	subroutine flightpath_5H1 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete)

c	This is a racing oval

	use iso_c_binding
	implicit none

	include 'pi.h'

	logical icomplete, lstraight

	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)

	double precision theta, radius, a
	double precision side, arc, distance
	double precision height

	icomplete = .false.

c	side   =  1000.d0
c	radius =   250.d0  ! in meters
c	height = - 200.d0

	side   =   750.d0
	radius =   300.d0  ! in meters
	height = - 500.d0

	a = speed/radius
	
	arc = pi*radius

	distance = speed*(time - timeslip)

	if (distance.lt.side) then

	   Xpath( 1) =  distance ! X
	   Xpath( 2) =    0.d0                    ! Y
	   Xpath( 3) =   height                 ! Z

	   Xpathdot( 1) =  speed    ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V
	   Xpathdot( 3) =  0.d0     ! W

	   lstraight = .true.
	   radius = 0.d0

	else if (distance.lt.(side+arc)) then

	   theta = (distance - side)/radius

	   Xpath( 1) =  side + radius*sin(theta)          ! X
	   Xpath( 2) =         radius*(cos(theta) - 1.d0) ! Y
	   Xpath( 3) =  height                            ! Z

	   Xpathdot( 1) =    speed*cos(theta)    ! U   this is in the world frame
	   Xpathdot( 2) =  - speed*sin(theta)    ! V   this is in the world frame
	   Xpathdot( 3) =    0.d0                ! W

	   lstraight = .false.
	   radius = - radius

	else if (distance.lt.(2.d0*side+arc)) then

	   Xpath( 1) =  side - (distance - arc - side)               ! X
	   Xpath( 2) =  -2.d0 * radius                 ! Y
	   Xpath( 3) =  height                         ! Z

	   Xpathdot( 1) =  - speed    ! U   this is in the world frame
	   Xpathdot( 2) =    0.d0     ! V
	   Xpathdot( 3) =    0.d0     ! W

	   lstraight = .true.
	   radius = 0.d0

	else 

	   theta = (distance - 2.d0*side - arc)/radius

	   Xpath( 1) =       - radius*sin(theta)          ! X
	   Xpath( 2) =       - radius*(cos(theta) + 1.d0) ! Y
	   Xpath( 3) =       height                       ! Z

	   Xpathdot( 1) =  - speed*cos(theta)    ! U   this is in the world frame
	   Xpathdot( 2) =    speed*sin(theta)    ! V   this is in the world frame
	   Xpathdot( 3) =    0.d0                ! W

	   if (theta.gt.pi) icomplete = .true.

	   lstraight = .false.
	   radius = - radius

	endif

	return
	end

	subroutine flightpath_6H1 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete, path_data)

c	This is an example of the simplest flight path possible - a stright line.
c       8/17/22  Now it has a variable speed, so it is more complicated.
c
c       10/19/22  This to do standalone checks of lateral acceleration.

	implicit none

	logical icomplete, lstraight

	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)
	double precision radius, height
        double precision path_data(1000)
        double precision slope
        double precision start_speed, accel
        double precision adjusted_time, distance, delta_t

	height = -200.d0  ! in meters

c       Now we move all these terms for the requested accelerations
c       path_data( 3) = control%requested_lateral_speed
c       path_data( 9) = control%requested_lateral_acceleration
c       path_data(10) = control%requested_lateral_deceleration
c       path_data(11) = dfloat(control%start_trim_state)

        speed       = path_data(3)
        start_speed = max(0.d0, path_data(11))
        if (start_speed.lt.speed) then
           accel = path_data( 9)
        else
           accel = path_data(10)
        endif

        if (accel.eq.0.d0) then
           delta_t = 1000.d0  ! a max flight time
        else
           delta_t = (speed - start_speed)/accel
        endif
        distance = start_speed * delta_t + 0.5d0 * accel * delta_t**2

c	speed = 20.d0  ! meters per second, hardwired for now.
c        slope = 0.005d0  ! so at 100 seconds we are going twice as fast

c	speed = 0.d0  ! meters per second, hardwired for now.
c        slope = 0.5d0

        adjusted_time = time - timeslip

	icomplete = .false. 

	Xpath( 1) =  start_speed * adjusted_time
     .             + 0.5d0 * accel * adjusted_time**2 ! X
	Xpath( 2) =  0.d0                    ! Y
	Xpath( 3) =  height                  ! Z

	Xpathdot( 1) =  start_speed + accel * adjusted_time  ! U   this is in the world frame
	Xpathdot( 2) =  0.d0     ! V
	Xpathdot( 3) =  0.d0     ! W

	lstraight = .true.
	radius = 0.d0


c	Xpath( 1) =  speed*(time + slope*time - timeslip) ! X
c	Xpath( 1) =  slope*(time + 0.5d0*time**2 - timeslip) ! X
c	Xpath( 2) =  0.d0                    ! Y
c	Xpath( 3) =  height                  ! Z

c	Xpathdot( 1) =  speed  + 2.d0*speed*slope*time  ! U   this is in the world frame
c	Xpathdot( 1) =  slope*(1.d0+time)  ! U   this is in the world frame
c	Xpathdot( 2) =  0.d0     ! V
c	Xpathdot( 3) =  0.d0     ! W


        if (Xpath(1)      .gt. distance) icomplete = .true.  ! we stop at this distance
        if (adjusted_time .gt. delta_t ) icomplete = .true.  ! we stop at this time

        path_data(12) = distance
        path_data(13) = delta_t
        path_data(14) = accel

	return
	end
	



	subroutine flightpath_7H1 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete, path_data)

c	This is an example of the simplest flight path possible - a stright line.
c       8/17/22  Now it has a variable speed, so it is more complicated.

	implicit none

	logical icomplete, lstraight

	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)
	double precision radius, height
        double precision path_data(1000)
        double precision slope
        double precision tswitch

	speed = 2.d0  ! meters per second, hardwired for now.
        slope = 0.02d0

        tswitch = speed/slope - 1.d0

	icomplete = .false. 

	Xpath( 1) =  0.d0                    ! X
	Xpath( 2) =  0.d0                    ! Y
	Xpath( 3) =  - slope*(time + 0.5d0*time**2 - timeslip) ! W
        if (time.gt.tswitch) Xpath(3) = 
     .          - slope*(tswitch + 0.5d0*tswitch**2 - timeslip)
     .          - speed*(time - tswitch)

	Xpathdot( 1) =  0.d0     ! U dot
	Xpathdot( 2) =  0.d0     ! V dot
	Xpathdot( 3) =  - slope*(1.d0+time)  ! W dot
	if (time.gt.tswitch) Xpathdot( 3) =  - speed  ! W dot

	lstraight = .true.
	radius = 0.d0

        if (Xpathdot(3) .lt. -50.d0) icomplete = .true.  ! we stop at this speed
c        if (time .gt. 120.d0) icomplete = .true.  ! we stop at this speed

	return
	end
	



	subroutine flightpath_8H1 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete, path_data)

c	This is an example of the simplest flight path possible - a stright line.
c       8/17/22  Now it has a variable speed, so it is more complicated.
c       9/19/22  An up from zero to hover to lateral motion path.

	implicit none

	logical icomplete, lstraight

	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)
	double precision radius, height
        double precision path_data(1000)
        double precision slope
        double precision tswitch

	icomplete = .false. 

c	speed = 20.d0  ! meters per second, hardwired for now.
c        slope = 0.005d0  ! so at 100 seconds we are going twice as fast

	speed = 2.d0  ! meters per second, hardwired for now.
        slope = 0.02d0

c       Initially a vertical ascent, like path 4.
	height = -200.d0  ! in meters, negative for up

        if (time.lt.100.d0) then

	   Xpath( 1) =  0.d0 ! X
	   Xpath( 2) =  0.d0 ! Y
	   Xpath( 3) = - speed*(time - timeslip)     ! Z
	   if (Xpath( 3).lt.height) Xpath (3) = height   ! Z

	   Xpathdot( 1) =  0.d0    ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0    ! V   this is in the world frame
	   Xpathdot( 3) =  - speed                ! W

        else  ! we are now on the horizontal path

	   Xpath( 1) =  speed*(time - timeslip - 100.d0)                    ! X
	   Xpath( 2) =  0.d0                    ! Y
	   Xpath( 3) =  height ! W

	   Xpathdot( 1) =  speed    ! U dot
	   Xpathdot( 2) =  0.d0     ! V dot
	   Xpathdot( 3) =  0.d0  ! W dot

        endif

	lstraight = .true.
	radius = 0.d0

        if (Xpathdot(3) .lt. -50.d0) icomplete = .true.  ! we stop at this speed
c        if (time .gt. 120.d0) icomplete = .true.  ! we stop at this speed

	return
	end
	

        subroutine initialize_9H1 (path_data)
c       There is lots of work involved in initializing the flight path for path 9.

c       9/30/22  Moving items from the flight path into this intialization routine,
c                and including Alex's initializtion.  Uses equivalence to pass data in path_data.

        use iso_c_binding
	implicit none

	include 'pi.h'
	
        double precision path_data  (1000)
        double precision path_data_2(1000)

	double precision radius, height
c       These should all be set to zero when path_data is set to zero
c       when initialized in subroutine fly.
        double precision Xstart      (20)
        double precision Ystart      (20)
        double precision Zstart      (20)
        double precision VXstart     (20)
        double precision VYstart     (20)
        double precision VZstart     (20)
        double precision delta_t_list(20)
        double precision delta_v_list(20)
        double precision time_list   (20)

        equivalence ( path_data_2(101), Xstart      (1) )
        equivalence ( path_data_2(121), Ystart      (1) )
        equivalence ( path_data_2(141), Zstart      (1) )
        equivalence ( path_data_2(161), VXstart     (1) )
        equivalence ( path_data_2(181), VYstart     (1) )
        equivalence ( path_data_2(201), VZstart     (1) )
        equivalence ( path_data_2(221), delta_t_list(1) )
        equivalence ( path_data_2(241), delta_v_list(1) )
        equivalence ( path_data_2(261), time_list   (1) )


c       This is perhaps an awkward way to do this, but it ensures the
c       variable is passed back in path_data in the event we change it in our
c       error bound checks.


        double precision alength_start  ! and end, due to symmetry
        double precision alength_mid

        double precision up_acceleration_dist
        double precision up_deceleration_dist

        double precision horizontal_acceleration_dist
        double precision horizontal_deceleration_dist

        double precision down_acceleration_dist
        double precision down_deceleration_dist_1  !  perhaps most important, to have a soft landing.
        double precision down_deceleration_dist_2  !  perhaps most important, to have a soft landing.

        double precision up_speed
        double precision horizontal_speed
        double precision down_speed
        double precision landing_speed, landing_speed_at_ground
        double precision landing_approach_height
        double precision lateral_acceleration,  lateral_deceleration
        double precision vertical_acceleration, vertical_deceleration

c       Now we get all these terms from the inputs (from control common,
c       stored in path_data in subroutine fly)
        equivalence( up_speed                , path_data_2( 1) )
        equivalence( down_speed              , path_data_2( 2) )
        equivalence( horizontal_speed        , path_data_2( 3) )
        equivalence( landing_speed           , path_data_2( 4) )
        equivalence( landing_speed_at_ground , path_data_2( 5) )
        equivalence( vertical_acceleration   , path_data_2( 6) )
        equivalence( vertical_deceleration   , path_data_2( 7) )
        equivalence( landing_approach_height , path_data_2( 8) )
        equivalence( lateral_acceleration    , path_data_2( 9) )
        equivalence( lateral_deceleration    , path_data_2(10) )


        equivalence( alength_start                , path_data_2(21) )
        equivalence( alength_mid                  , path_data_2(22) )
        equivalence( height                       , path_data_2(23) )
        equivalence( radius                       , path_data_2(24) )
        equivalence( up_acceleration_dist         , path_data_2(25) )
        equivalence( up_deceleration_dist         , path_data_2(26) )
        equivalence( horizontal_acceleration_dist , path_data_2(27) )
        equivalence( horizontal_deceleration_dist , path_data_2(28) )
        equivalence( down_acceleration_dist       , path_data_2(29) )
        equivalence( down_deceleration_dist_1     , path_data_2(30) )
        equivalence( down_deceleration_dist_2     , path_data_2(31) )


        logical :: laccelerate = .false.
        logical :: ldecelerate = .false.

        double precision delta_t, delta_v, delta_s, accel, ct, st

        double precision :: t_end_buffer = 20.d0  ! twenty seconds at the end to recover.

c       These are variables from the scoring routine
        integer index_score
        parameter (index_score = 10)
	double precision path(index_score, 3),thirdpoint(index_score, 3)
	double precision allowerr(index_score), pointrate(index_score)
	double precision point(3)
	double precision flightdist
	integer pathsize, flightscore
	double precision inf

        equivalence( path      (1,1) , path_data_2(501)               )
        equivalence( thirdpoint(1,1) , path_data_2(501+3*index_score) )
        equivalence( allowerr  (1)   , path_data_2(501+6*index_score) )
        equivalence( pointrate (1)   , path_data_2(501+7*index_score) )

        integer i

        double precision temp


c       We move all the data to the copy
        do i = 1, 1000
           path_data_2(i) = path_data(i)
        enddo

c       These values define the flight path.  
c       Initially a vertical ascent, like path 4, to Height.  
c       Then a straight segement in the X direction of alength_start.
c       Then a curve to the right of radius radius.
c       Then a straight segment in the Y direction of length alength_mid.
c       Then a turn to the left of radius radius.
c       Then a straight segment in the +X direction of length alength_start.
c       Then a descent from Height to Z = 0.
	height = -100.d0  ! in meters, negative for up
        alength_start = 700.d0
        alength_mid   = 300.d0
        radius        = 500.d0



c       Originally we set all these distances for testing.
c       Now we have a variety of control to give to the autopilot
c       regarding the flight.  These are really autopilot values
c       and not path values, but the path needs to know about them
c       since we have variable speed, and not just a set speed.
c        up_acceleration_dist = 10.d0
c        up_deceleration_dist = 10.d0  ! = height + 10 m
c        down_acceleration_dist = 10.d0 ! = height + 10 m (remember down is plus)
c        down_deceleration_dist_1 = 10.d0
c        down_deceleration_dist_2 = 10.d0
c        up_speed = -2.d0 ! m/s
c        down_speed = 1.d0 
c        landing_speed = 1.d0
c        horizontal_speed = 5.d0

c       Kind of awkward way to do things but...
c        horizontal_acceleration_dist = 100.d0
c        horizontal_deceleration_dist = 100.d0


c        up_speed   =   control%requested_vertical_speed
c        down_speed =   control%requested_vertical_down_speed
c        horizontal_speed = control%requested_lateral_speed
c        landing_speed = control%vertical_landing_approach_speed
c        landing_speed_at_ground 
c     .             = control%vertical_landing_speed_at_ground

c        up_acceleration_dist = 0.5d0 * (-up_speed**2)
c     .      / control%requested_vertical_acceleration
c        up_deceleration_dist = 0.5d0 * (up_speed**2)
c     .      / control%requested_vertical_deceleration
c        down_acceleration_dist = 0.5d0 * (down_speed**2)
c     .      / control%requested_vertical_deceleration
c        down_deceleration_dist_1 = 0.5d0 * 
c     .        (down_speed**2 - landing_speed**2)
c     .      / control%requested_vertical_deceleration
c        down_deceleration_dist_2 = control%landing_approach_height
c        horizontal_acceleration_dist = 0.5d0 * horizontal_speed**2
c     .      / control%requested_lateral_acceleration
c        horizontal_deceleration_dist = 0.5d0 * (-horizontal_speed**2)
c     .      / control%requested_lateral_deceleration


        up_acceleration_dist = 0.5d0 * (-up_speed**2)
     .      / vertical_acceleration
        up_deceleration_dist = 0.5d0 * (up_speed**2)
     .      / vertical_deceleration
        down_acceleration_dist = 0.5d0 * (down_speed**2)
     .      / vertical_deceleration
        down_deceleration_dist_1 = 0.5d0 * 
     .        (down_speed**2 - landing_speed**2)
     .      / vertical_deceleration
        down_deceleration_dist_2 = landing_approach_height
        horizontal_acceleration_dist = 0.5d0 * horizontal_speed**2
     .      / lateral_acceleration
        horizontal_deceleration_dist = 0.5d0 * (-horizontal_speed**2)
     .      / lateral_deceleration

c        write (6,*) ' up accel, up decel ',up_acceleration_dist
c     .                                    ,up_deceleration_dist

c       We write out some details of the set up

           write (6,*) ' Details of Path 9 '
           write (6,*) '   Up speed                         = ',
     .                     up_speed
           write (6,*) '   Down speed                       = ',
     .                     down_speed
           write (6,*) '   Horizontal speed                 = ',
     .                     horizontal_speed
           write (6,*) '   Landing approach speed           = ',
     .                     landing_speed
           write (6,*) '   Landing speed at ground          = ',
     .                     landing_speed_at_ground
           write (6,*)
           write (6,*) '   Up acceleration                  = ',
     .                     vertical_acceleration
           write (6,*) '   Up acceleration distance         = '
     .                    ,up_acceleration_dist
           write (6,*) '   Up deceleration                  = ',
     .                     vertical_deceleration
           write (6,*) '   Up deceleration distance         = '
     .                    ,up_deceleration_dist
c          Now we put in some checks on these distances, to see if they are
c            sufficient.
           temp = up_acceleration_dist + up_deceleration_dist
           if (temp.gt.abs(height))
c             Doesn't get up to speed.  Our view is that the speed 
c             request dominates, so we change the acceleration to match.
     k        write (6,*) ' Warning: up acceleration and deceleration'
     .                 ,' are too small in magnitude: dist = ',
     .                  sngl(temp), ' > ',sngl(-height)
           write (6,*)
           write (6,*) '   Down acceleration distance       = '
     .                    ,down_acceleration_dist
           write (6,*) '   Down deceleration distance 1     = '
     .                    ,down_deceleration_dist_1
           write (6,*) '   Down deceleration distance 2     = '
     .                    ,down_deceleration_dist_2
           temp = down_acceleration_dist + down_deceleration_dist_1
     .                                   + down_deceleration_dist_2
           if (temp.gt.abs(height))
c             Doesn't get up to speed.  Our view is that the speed 
c             request dominates, so we change the acceleration to match.
     .        write (6,*) ' Warning: down acceleration and deceleration'
     .                 ,' are too small in magnitude: dist = ',
     .                  sngl(temp), ' > ',sngl(-height)

           write (6,*)
           write (6,*) '   Horizontal acceleration distance = '
     .                    ,horizontal_acceleration_dist
           temp = horizontal_acceleration_dist
           if (temp.gt.alength_start)
c             Doesn't get up to speed.  Our view is that the speed 
c             request dominates, so we change the acceleration to match.
     .        write (6,*) ' Warning: horizontal acceleration'
     .                 ,' is too small in magnitude: dist = ',
     .                  sngl(temp), ' > ',sngl(alength_start)
           write (6,*) '   Horizontal deceleration distance = '
     .                    ,horizontal_deceleration_dist
           temp = horizontal_deceleration_dist
           if (temp.gt.alength_start)
c             Doesn't get up to speed.  Our view is that the speed 
c             request dominates, so we change the acceleration to match.
     .        write (6,*) ' Warning: horizontal deceleration'
     .                 ,' is too small in magnitude: dist = ',
     .                  sngl(temp), ' > ',sngl(alength_start)

c       So we have a variety of parts to the path and we need to compute
c       some time increments for it (not sure how else to go about this).
c       We assume constant acceleration in each region.

c       First we have the piece for the acceleration going up
c       Reion 1
        delta_v = up_speed - 0.d0 ! from rest to vertical speed
        delta_t = 2.d0*(-up_acceleration_dist)/delta_v
        time_list(1)  = delta_t
        delta_t_list(1) = delta_t
        delta_v_list(1) = delta_v
        Xstart(1) = 0.d0
        Ystart(1) = 0.d0
        Zstart(1) = 0.d0
        VXstart(1) = 0.d0
        VYstart(1) = 0.d0
        VZstart(1) = 0.d0

c       Region 2
        delta_s = height - (-up_acceleration_dist) 
     .                   - (-up_deceleration_dist)
        delta_t = delta_s/up_speed
        time_list(2)  = time_list(1) + delta_t
        delta_t_list(2) = delta_t
        Xstart(2) = 0.d0
        Ystart(2) = 0.d0
        Zstart(2) = - up_acceleration_dist
        VXstart(2) = 0.d0
        VYstart(2) = 0.d0
        VZstart(2) = up_speed

c       Region 3
        delta_v = 0.d0 - up_speed ! from vertical speed to hover
        delta_t = 2.d0*(up_deceleration_dist)/delta_v
        time_list(3)  = time_list(2) + delta_t
        delta_t_list(3) = delta_t
        delta_v_list(3) = delta_v
        Xstart(3) = 0.d0
        Ystart(3) = 0.d0
        Zstart(3) = height + up_deceleration_dist
        VXstart(3) = 0.d0
        VYstart(3) = 0.d0
        VZstart(3) = up_speed

c       Now we are flying horizontally
c       Region 4
        delta_v = horizontal_speed - 0.d0
        delta_t = 2.d0*horizontal_acceleration_dist/delta_v
        time_list(4)  = time_list(3) + delta_t
        delta_t_list(4) = delta_t
        delta_v_list(4) = delta_v
        Xstart(4) = 0.d0
        Ystart(4) = 0.d0
        Zstart(4) = height
        VXstart(4) = 0.d0
        VYstart(4) = 0.d0
        VZstart(4) = 0.d0

c       Region 5
        delta_s = alength_start - horizontal_acceleration_dist
        delta_t = delta_s/horizontal_speed
        time_list(5)  = time_list(4) + delta_t
        delta_t_list(5) = delta_t
        Xstart(5) = horizontal_acceleration_dist
        Ystart(5) = 0.d0
        Zstart(5) = height
        VXstart(5) = horizontal_speed
        VYstart(5) = 0.d0
        VZstart(5) = 0.d0

c       Region 6
        delta_s = (pi/2.d0)*radius
        delta_t = delta_s/horizontal_speed
        time_list(6)  = time_list(5) + delta_t
        delta_t_list(6) = delta_t
        Xstart(6) = alength_start
        Ystart(6) = 0.d0
        Zstart(6) = height
        VXstart(6) = horizontal_speed
        VYstart(6) = 0.d0
        VZstart(6) = 0.d0

c       Region 7
        delta_s = alength_mid
        delta_t = delta_s/horizontal_speed
        time_list(7)  = time_list(6) + delta_t
        delta_t_list(7) = delta_t
        Xstart(7) = alength_start + radius
        Ystart(7) = radius
        Zstart(7) = height
        VXstart(7) = 0.d0
        VYstart(7) = horizontal_speed
        VZstart(7) = 0.d0

c       Region 8
        delta_s = (pi/2.d0)*radius
        delta_t = delta_s/horizontal_speed
        time_list(8)  = time_list(7) + delta_t
        delta_t_list(8) = delta_t
        Xstart(8) = alength_start + radius
        Ystart(8) = radius + alength_mid
        Zstart(8) = height
        VXstart(8) = 0.d0
        VYstart(8) = horizontal_speed
        VZstart(8) = 0.d0

c       Region 9
        delta_s = alength_start - horizontal_deceleration_dist
        delta_t = delta_s/horizontal_speed
        time_list(9)  = time_list(8) + delta_t
        delta_t_list(9) = delta_t
        Xstart(9) = alength_start + 2.d0*radius
        Ystart(9) = 2.d0*radius + alength_mid
        Zstart(9) = height
        VXstart(9) = horizontal_speed
        VYstart(9) = 0.d0
        VZstart(9) = 0.d0

c       Region 10       
        delta_v = 0.d0 - horizontal_speed
        delta_t = 2.d0*(-horizontal_deceleration_dist)/delta_v
        time_list(10) = time_list(9) + delta_t
        delta_t_list(10) = delta_t
        delta_v_list(10) = delta_v
        Xstart(10) = 2.d0*(alength_start + radius)
     .              - horizontal_deceleration_dist
        Ystart(10) = 2.d0*radius + alength_mid
        Zstart(10) = height
        VXstart(10) = horizontal_speed
        VYstart(10) = 0.d0
        VZstart(10) = 0.d0

c       Now we do the final drop
c       Region 11
        delta_v = down_speed - 0.d0
        delta_t = 2.d0*(down_acceleration_dist)/delta_v
        time_list(11) = time_list(10) + delta_t
        delta_t_list(11) = delta_t
        delta_v_list(11) = delta_v
        Xstart(11) = 2.d0*(alength_start + radius)
        Ystart(11) = 2.d0*radius + alength_mid
        Zstart(11) = height
        VXstart(11) = 0.d0
        VYstart(11) = 0.d0
        VZstart(11) = 0.d0

c       Region 12
        delta_s = height - (-down_acceleration_dist)
     .       - (-down_deceleration_dist_1) - (-down_deceleration_dist_2)
        delta_t = (-delta_s)/down_speed
        time_list(12) = time_list(11) + delta_t
        delta_t_list(12) = delta_t
        Xstart(12) = 2.d0*(alength_start + radius)
        Ystart(12) = 2.d0*radius + alength_mid
        Zstart(12) = height + down_acceleration_dist
        VXstart(12) = 0.d0
        VYstart(12) = 0.d0
        VZstart(12) = down_speed

c       Region 13       
        delta_v = landing_speed - down_speed
c       This is more complicated because neither end is zero.
        delta_t = 2.d0*(down_deceleration_dist_1)
     .         /(landing_speed + down_speed)
        time_list(13) = time_list(12) + delta_t
        delta_t_list(13) = delta_t
        delta_v_list(13) = delta_v
        Xstart(13) = 2.d0*(alength_start + radius)
        Ystart(13) = 2.d0*radius + alength_mid
        Zstart(13) =-down_deceleration_dist_1 - down_deceleration_dist_2
        VXstart(13) = 0.d0
        VYstart(13) = 0.d0
        VZstart(13) = down_speed

c       Region 14
        delta_v = landing_speed_at_ground - landing_speed
        delta_t = 2.d0*(down_deceleration_dist_2)
     .           /(landing_speed + landing_speed_at_ground)
        time_list(14) = time_list(13) + delta_t
        delta_t_list(14) = delta_t
        delta_v_list(14) = delta_v
        Xstart(14) = 2.d0*(alength_start + radius)
        Ystart(14) = 2.d0*radius + alength_mid
        Zstart(14) = - down_deceleration_dist_2
        VXstart(14) = 0.d0
        VYstart(14) = 0.d0
        VZstart(14) = landing_speed

c       Region 15
c       And we are there.  A constant drift at the landing speed, just in case we are not.
        delta_v = 0.d0
        delta_t = t_end_buffer
        time_list(15) = time_list(14) + delta_t
        delta_t_list(15) = delta_t
        delta_v_list(15) = delta_v
        Xstart(15) = 2.d0*(alength_start + radius)
        Ystart(15) = 2.d0*radius + alength_mid
        Zstart(15) = 0.d0
        VXstart(15) = 0.d0
        VYstart(15) = 0.d0
        VZstart(15) = landing_speed_at_ground

        Xstart(16) = 2.d0*(alength_start + radius)
        Ystart(16) = 2.d0*radius + alength_mid
        Zstart(16) = 0.d0 + landing_speed_at_ground * t_end_buffer
        VXstart(16) = 0.d0
        VYstart(16) = 0.d0
        VZstart(16) = landing_speed_at_ground


c       We print out the various results

           write (6,*)
           write (6,*)   '   Location                  i           X  '
     .                 ,'        Y          Z      '
     .             ,' Time    Delta time       Vx         Vy         Vz'
     .                ,'      Delta v  Acceleration '
           write (6,22) 'Start                       ',1
     .                  ,Xstart(1),Ystart(1),Zstart(1),0.d0
     .                  ,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0
           write (6,22) 'End of upward acceleration  ',2
     .                  ,Xstart(2),Ystart(2),Zstart(2),time_list(1)
     .                  ,delta_t_list(1)
     .                  ,VXstart(2),VYstart(2),VZstart(2)
     .                  ,delta_v_list(1)
     .                  ,delta_v_list(1)/delta_t_list(1)
           write (6,22) 'End of upward cruise        ',3
     .                  ,Xstart(3),Ystart(3),Zstart(3),time_list(2)
     .                  ,delta_t_list(2)
     .                  ,VXstart(3),VYstart(3),VZstart(3)
     .                  ,delta_v_list(2)
     .                  ,delta_v_list(2)/delta_t_list(2)
           write (6,22) 'End of upward deceleration  ',4
     .                  ,Xstart(4),Ystart(4),Zstart(4),time_list(3)
     .                  ,delta_t_list(3)
     .                  ,VXstart(4),VYstart(4),VZstart(4)
     .                  ,delta_v_list(3)
     .                  ,delta_v_list(3)/delta_t_list(3)
           write (6,22) 'End of lateral acceleration ',5
     .                  ,Xstart(5),Ystart(5),Zstart(5),time_list(4)
     .                  ,delta_t_list(4)
     .                  ,VXstart(5),VYstart(5),VZstart(5)
     .                  ,delta_v_list(4)
     .                  ,delta_v_list(4)/delta_t_list(4)
           write (6,22) 'Arrival at right turn       ',6
     .                  ,Xstart(6),Ystart(6),Zstart(6),time_list(5)
     .                  ,delta_t_list(5)
     .                  ,VXstart(6),VYstart(6),VZstart(6)
     .                  ,delta_v_list(5)
     .                  ,delta_v_list(5)/delta_t_list(5)
           write (6,22) 'Finish right turn           ',7
     .                  ,Xstart(7),Ystart(7),Zstart(7),time_list(6)
     .                  ,delta_t_list(6)
     .                  ,VXstart(7),VYstart(7),VZstart(7)
     .                  ,delta_v_list(6)
     .                  ,delta_v_list(6)/delta_t_list(6)
           write (6,22) 'Arrival at left turn        ',8
     .                  ,Xstart(8),Ystart(8),Zstart(8),time_list(7)
     .                  ,delta_t_list(7)
     .                  ,VXstart(8),VYstart(8),VZstart(8)
     .                  ,delta_v_list(7)
     .                  ,delta_v_list(7)/delta_t_list(7)
           write (6,22) 'Finish left turn            ',9
     .                  ,Xstart(9),Ystart(9),Zstart(9),time_list(8)
     .                  ,delta_t_list(8)
     .                  ,VXstart(9),VYstart(9),VZstart(9)
     .                  ,delta_v_list(8)
     .                  ,delta_v_list(8)/delta_t_list(8)
           write (6,22) 'End of cruise               ',10
     .                  ,Xstart(10),Ystart(10),Zstart(10),time_list(9)
     .                  ,delta_t_list(9)
     .                  ,VXstart(10),VYstart(10),VZstart(10)
     .                  ,delta_v_list(9)
     .                  ,delta_v_list(9)/delta_t_list(9)
           write (6,22) 'End of lateral deceleration ',11
     .                  ,Xstart(11),Ystart(11),Zstart(11),time_list(10)
     .                  ,delta_t_list(10)
     .                  ,VXstart(11),VYstart(11),VZstart(11)
     .                  ,delta_v_list(10)
     .                  ,delta_v_list(10)/delta_t_list(10)
           write (6,22) 'End of downward acceleration',12
     .                  ,Xstart(12),Ystart(12),Zstart(12),time_list(11)
     .                  ,delta_t_list(11)
     .                  ,VXstart(12),VYstart(12),VZstart(12)
     .                  ,delta_v_list(11)
     .                  ,delta_v_list(11)/delta_t_list(11)
           write (6,22) 'End of downward cruise      ',13
     .                  ,Xstart(13),Ystart(13),Zstart(13),time_list(12)
     .                  ,delta_t_list(12)
     .                  ,VXstart(13),VYstart(13),VZstart(13)
     .                  ,delta_v_list(12)
     .                  ,delta_v_list(12)/delta_t_list(12)
           write (6,22) 'Arrival at landing approach ',14
     .                  ,Xstart(14),Ystart(14),Zstart(14),time_list(13)
     .                  ,delta_t_list(13)
     .                  ,VXstart(14),VYstart(14),VZstart(14)
     .                  ,delta_v_list(13)
     .                  ,delta_v_list(13)/delta_t_list(13)
           write (6,22) 'Landing                     ',15
     .                  ,Xstart(15),Ystart(15),Zstart(15),time_list(14)
     .                  ,delta_t_list(14)
     .                  ,VXstart(15),VYstart(15),VZstart(15)
     .                  ,delta_v_list(14)
     .                  ,delta_v_list(14)/delta_t_list(14)
           write (6,22) 'Downward drift if needed    ',16
     .                  ,Xstart(16),Ystart(16),Zstart(16),time_list(15)
     .                  ,delta_t_list(15)
     .                  ,VXstart(16),VYstart(16),VZstart(16)
     .                  ,delta_v_list(15)
     .                  ,delta_v_list(15)/delta_t_list(15)
           write (6,*)

 22        format(2x,a30,i3,10f11.3)


c       Items from Alex's scoring setup.
c	The inf variable is used here in the thirdpoint array for straight line segments
	inf = huge(0.0)

c	The point array stores the current location of the aircraft
	point = (/  2400.,  1300.,     0./)
c	point = (/ 10, 10, -50/)

c	The pathsize variable tells the score subroutine how many path segments there are (because all of the arrays are statically allocated)
	pathsize = 9

c	The path array stores the endpoints of the segments that make up the path
c	Unlike the arrays below, its effective size is one more than the pathsize value (because we need a starting point)
	path(1,:)  = (/     0.,     0.,     0./)
	path(2,:)  = (/     0.,     0.,  -100./)
	path(3,:)  = (/    10.,     0.,  -100./)
	path(4,:)  = (/   700.,     0.,  -100./)
	path(5,:)  = (/  1200.,   500.,  -100./)
	path(6,:)  = (/  1200.,   800.,  -100./)
	path(7,:)  = (/  1700.,  1300.,  -100./)
	path(8,:)  = (/  2400.,  1300.,  -100./)
	path(9,:)  = (/  2400.,  1300.,   -90./)
	path(10,:) = (/  2400.,  1300.,     0./)

c	The thirdpoint array stores a third point on the segment to account for those segments that are circular arcs
c	For straight lines, these are just set to [inf, inf, inf] instead
	thirdpoint(1,:)  = (/      inf,      inf,      inf/)
	thirdpoint(2,:)  = (/      inf,      inf,      inf/)
	thirdpoint(3,:)  = (/      inf,      inf,      inf/)
	thirdpoint(4,:)  = (/  1053.55,   146.45,    -100./)
	thirdpoint(5,:)  = (/      inf,      inf,      inf/)
	thirdpoint(6,:)  = (/  1346.45,  1153.55,    -100./)
	thirdpoint(7,:)  = (/      inf,      inf,      inf/)
	thirdpoint(8,:)  = (/      inf,      inf,      inf/)
	thirdpoint(9,:)  = (/      inf,      inf,      inf/)

c	The allowerr array specifies how far away the aircraft can be from a path segment and still be "on the path"
	allowerr(1)  = 15
	allowerr(2)  = 30
	allowerr(3)  = 15
	allowerr(4)  = 15
	allowerr(5)  = 15
	allowerr(6)  = 15
	allowerr(7)  = 15
	allowerr(8)  = 30
	allowerr(9)  = 15

c	The pointrate array specifies the rate (per meter) at which the aircraft can accumulate points while traveling on that path segment
	pointrate(1)  = 0.1
	pointrate(2)  = 20.1
	pointrate(3)  = 0.1
	pointrate(4)  = 0.1
	pointrate(5)  = 0.1
	pointrate(6)  = 0.1
	pointrate(7)  = 0.1
	pointrate(8)  = 20.1
	pointrate(9)  = 0.1

        path_data_2(500) = float(9)  ! this is path size, the number of items in this list


c       Here at the end, we move all the data from the copy back to the original.
c       This was done just to allow the same original variable names to be used,
c       but to store everything in path_data.
        do i = 1, 1000
           path_data(i) = path_data_2(i)
        enddo

	return
	end
	




	subroutine flightpath_9H1 (time, timeslip, speed, Xpath, Xpathdot
     .             , lstraight, radius, icomplete, path_data)

c	This is an example of the simplest flight path possible - a stright line.
c       8/17/22  Now it has a variable speed, so it is more complicated.
c       9/19/22  An up from zero to hover to lateral motion path.
c       9/23/22  A complete path, starting from rest to landing, which is symmetric,
c                so that flying "back" is the same as flying forward,
c                which simplifies the with and without cargo testing.

        use iso_c_binding
	implicit none

	include 'pi.h'
	
	logical icomplete, lstraight
        
        double precision path_data  (1000)
        double precision path_data_2(1000)
	double precision time, timeslip, speed
	double precision Xpath(3), Xpathdot(3)
	double precision radius
        double precision Xstart      (20)
        double precision Ystart      (20)
        double precision Zstart      (20)
        double precision VXstart     (20)
        double precision VYstart     (20)
        double precision VZstart     (20)
        double precision delta_t_list(20)
        double precision delta_v_list(20)
        double precision time_list   (20)

        equivalence ( path_data_2(101), Xstart      (1) )
        equivalence ( path_data_2(121), Ystart      (1) )
        equivalence ( path_data_2(141), Zstart      (1) )
        equivalence ( path_data_2(161), VXstart     (1) )
        equivalence ( path_data_2(181), VYstart     (1) )
        equivalence ( path_data_2(201), VZstart     (1) )
        equivalence ( path_data_2(221), delta_t_list(1) )
        equivalence ( path_data_2(241), delta_v_list(1) )
        equivalence ( path_data_2(261), time_list   (1) )

        double precision up_speed
        double precision horizontal_speed
        double precision down_speed
        double precision landing_speed, landing_speed_at_ground
        double precision landing_approach_height
        double precision lateral_acceleration,  lateral_deceleration
        double precision vertical_acceleration, vertical_deceleration

c       Now we get all these terms from the inputs (from control common,
c       stored in path_data in subroutine fly)
        equivalence( up_speed                , path_data_2( 1) )
        equivalence( down_speed              , path_data_2( 2) )
        equivalence( horizontal_speed        , path_data_2( 3) )
        equivalence( landing_speed           , path_data_2( 4) )
        equivalence( landing_speed_at_ground , path_data_2( 5) )
        equivalence( vertical_acceleration   , path_data_2( 6) )
        equivalence( vertical_deceleration   , path_data_2( 7) )
        equivalence( landing_approach_height , path_data_2( 8) )
        equivalence( lateral_acceleration    , path_data_2( 9) )
        equivalence( lateral_deceleration    , path_data_2(10) )

        logical :: laccelerate = .false.
        logical :: ldecelerate = .false.

        double precision delta_t, delta_v, delta_s, accel, ct, st
        double precision adjusted_time, theta

        double precision :: t_end_buffer = 20.d0  ! twenty seconds at the end to recover.

        logical, save :: first_pass = .true.

        integer i

	icomplete = .false. 

        laccelerate = .false.
        ldecelerate = .false.

c       We move all the data to the copy.  Notice we do not move it back, hence
c       it is read only.
        do i = 1, 300 ! all we need
           path_data_2(i) = path_data(i)
        enddo

        radius = path_data_2(24)  ! don't equivalence this - it gets reset regularly

        adjusted_time = time - timeslip

c       Region 1: accelerate up
        if (adjusted_time.lt.time_list(1)) then

           delta_t = adjusted_time
           accel   = delta_v_list(1)/delta_t_list(1)

	   Xpath( 1) =  Xstart(1) ! X
	   Xpath( 2) =  Ystart(1) ! Y
	   Xpath( 3) =  Zstart(1) + 0.5d0*accel*delta_t**2  ! Z

	   Xpathdot( 1) =  0.d0     ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V   this is in the world frame
	   Xpathdot( 3) =  accel * delta_t   ! W in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .true.
           ldecelerate = .false.

c       Region 2: cruise up
        else if (adjusted_time.lt.time_list(2)) then

           delta_t = adjusted_time - time_list(1)

	   Xpath( 1) =  Xstart(2) ! X
	   Xpath( 2) =  Ystart(2) ! Y
	   Xpath( 3) =  Zstart(2) + up_speed * delta_t ! Z

	   Xpathdot( 1) =  0.d0     ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V   this is in the world frame
	   Xpathdot( 3) =  up_speed ! W           in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .false.
           ldecelerate = .false.

c       Region 3: deceleration up
        else if (adjusted_time.lt.time_list(3)) then

           delta_t = adjusted_time - time_list(2)
           accel   = delta_v_list(3)/delta_t_list(3)

	   Xpath( 1) =  Xstart(3) ! X
	   Xpath( 2) =  Ystart(3) ! Y
           Xpath( 3) =  Zstart(3) + up_speed * delta_t
     .                            + 0.5d0*accel*delta_t**2  ! Z

	   Xpathdot( 1) =  0.d0     ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V   this is in the world frame
	   Xpathdot( 3) =  up_speed + accel * delta_t   ! W in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .false.
           ldecelerate = .true.

c       Region 4: X acceleration
        else if (adjusted_time.lt.time_list(4)) then

           delta_t = adjusted_time - time_list(3)
           accel   = delta_v_list(4)/delta_t_list(4)

           Xpath( 1) =  Xstart(4) + 0.5d0*accel*delta_t**2  ! X
	   Xpath( 2) =  Ystart(4) ! Y
	   Xpath( 3) =  Zstart(4) ! Z

	   Xpathdot( 1) =  accel * delta_t     ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V   this is in the world frame
	   Xpathdot( 3) =  0.d0     ! W in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .true.
           ldecelerate = .false.

c       Region 5: X cruise
        else if (adjusted_time.lt.time_list(5)) then

           delta_t = adjusted_time - time_list(4)

           Xpath( 1) =  Xstart(5) + horizontal_speed * delta_t  ! X
	   Xpath( 2) =  Ystart(5) ! Y
           Xpath( 3) =  Zstart(5) ! Z

	   Xpathdot( 1) =  horizontal_speed     ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V   this is in the world frame
	   Xpathdot( 3) =  0.d0     ! W in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .false.
           ldecelerate = .false.

c       Region 6: X - Y turn
        else if (adjusted_time.lt.time_list(6)) then

           delta_t = adjusted_time - time_list(5)
           theta   = (horizontal_speed/radius) * delta_t
           st      = sin(theta)
           ct      = cos(theta)

	   Xpath( 1) =  Xstart(6) + radius * st
	   Xpath( 2) =  Ystart(6) + radius * (1.d0 - ct) 
           Xpath( 3) =  Zstart(6) ! Z

	   Xpathdot( 1) =  horizontal_speed * ct     ! U   this is in the world frame
	   Xpathdot( 2) =  horizontal_speed * st     ! V   this is in the world frame
	   Xpathdot( 3) =  0.d0     ! W in the world frame

	   lstraight = .false.
	   radius = - radius
           laccelerate = .false.
           ldecelerate = .false.

c       Region 7: Y cruise 
        else if (adjusted_time.lt.time_list(7)) then

           delta_t = adjusted_time - time_list(6)

	   Xpath( 1) =  Xstart(7) 
	   Xpath( 2) =  Ystart(7) + horizontal_speed * delta_t
           Xpath( 3) =  Zstart(7) ! Z

	   Xpathdot( 1) =  0.d0     ! U   this is in the world frame
	   Xpathdot( 2) =  horizontal_speed     ! V   this is in the world frame
	   Xpathdot( 3) =  0.d0     ! W in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .false.
           ldecelerate = .false.

c       Region 8: Y - X turn
        else if (adjusted_time.lt.time_list(8)) then

           delta_t = adjusted_time - time_list(7)
           theta   = (horizontal_speed/radius) * delta_t
           st      = sin(theta)
           ct      = cos(theta)

	   Xpath( 1) =  Xstart(8) + radius * (1.d0 - ct)
	   Xpath( 2) =  Ystart(8) + radius * st 
           Xpath( 3) =  Zstart(8) ! Z

	   Xpathdot( 1) =  horizontal_speed * st     ! U   this is in the world frame
	   Xpathdot( 2) =  horizontal_speed * ct     ! V   this is in the world frame
	   Xpathdot( 3) =  0.d0     ! W in the world frame

	   lstraight = .true.
	   radius = radius
           laccelerate = .false.
           ldecelerate = .false.

c       Region 9: X cruise 
        else if (adjusted_time.lt.time_list(9)) then

           delta_t = adjusted_time - time_list(8)

	   Xpath( 1) =  Xstart(9) + horizontal_speed * delta_t
	   Xpath( 2) =  Ystart(9) 
           Xpath( 3) =  Zstart(9) ! Z

	   Xpathdot( 1) =  horizontal_speed    ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V   this is in the world frame
	   Xpathdot( 3) =  0.d0     ! W in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .false.
           ldecelerate = .false.

c       Region 10: X deceleration
        else if (adjusted_time.lt.time_list(10)) then

           delta_t = adjusted_time - time_list(9)
           accel   = delta_v_list(10)/delta_t_list(10)

	   Xpath( 1) =  Xstart(10) + horizontal_speed * delta_t
     .                             + 0.5d0*accel*delta_t**2  ! X
	   Xpath( 2) =  Ystart(10) ! Y
           Xpath( 3) =  Zstart(10) ! Z

	   Xpathdot( 1) =  horizontal_speed + accel * delta_t  ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V   this is in the world frame
	   Xpathdot( 3) =  0.d0     ! W in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .false.
           ldecelerate = .true.

c       Region 11: accelerate down 
        else if (adjusted_time.lt.time_list(11)) then

           delta_t = adjusted_time - time_list(10)
           accel   = delta_v_list(11)/delta_t_list(11)
c        write (90,*) ' accel, delta_v, delta_t '
c     .            ,accel,delta_v_list(11),delta_t_list(11)

	   Xpath( 1) =  Xstart(11) ! X
	   Xpath( 2) =  Ystart(11) ! Y
	   Xpath( 3) =  Zstart(11) + 0.5d0*accel*delta_t**2  ! Z

	   Xpathdot( 1) =  0.d0     ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V   this is in the world frame
	   Xpathdot( 3) =  accel * delta_t   ! W in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .true.
           ldecelerate = .false.

c       Region 12: cruise down 
        else if (adjusted_time.lt.time_list(12)) then

           delta_t = adjusted_time - time_list(11)

	   Xpath( 1) =  Xstart(12) ! X
	   Xpath( 2) =  Ystart(12) ! Y
	   Xpath( 3) =  Zstart(12) + down_speed * delta_t  ! Z

	   Xpathdot( 1) =  0.d0     ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V   this is in the world frame
	   Xpathdot( 3) =  down_speed   ! W in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .false.
           ldecelerate = .false.

c       Region 13: decelerate down 
        else if (adjusted_time.lt.time_list(13)) then

           delta_t = adjusted_time - time_list(12)
           accel   = delta_v_list(13)/delta_t_list(13)

	   Xpath( 1) =  Xstart(13) ! X
	   Xpath( 2) =  Ystart(13) ! Y
	   Xpath( 3) =  Zstart(13) + down_speed * delta_t 
     .                             + 0.5d0*accel*delta_t**2  ! Z

	   Xpathdot( 1) =  0.d0     ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V   this is in the world frame
	   Xpathdot( 3) =  down_speed + accel * delta_t   ! W in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .false.
           ldecelerate = .true.

c       Region 14: landing 
        else if (adjusted_time.lt.time_list(14)) then

           delta_t = adjusted_time - time_list(13)
           accel   = delta_v_list(14)/delta_t_list(14)

	   Xpath( 1) =  Xstart(14) ! X
	   Xpath( 2) =  Ystart(14) ! Y
	   Xpath( 3) =  Zstart(14) + landing_speed * delta_t 
     .                             + 0.5d0*accel*delta_t**2  ! Z
c        write (90,*) ' time, Z ',adjusted_time,Xpath(3),Xpathdot(3),14

	   Xpathdot( 1) =  0.d0     ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V   this is in the world frame
	   Xpathdot( 3) =  landing_speed + accel * delta_t   ! W in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .false.
           ldecelerate = .true.

        else if (adjusted_time.lt.time_list(15)) then

c          We should have landed.  This is to try to push us down slowly

           delta_t = adjusted_time - time_list(14)
           accel   = delta_v_list(15)/delta_t_list(15)

	   Xpath( 1) =  Xstart(15) ! X
	   Xpath( 2) =  Ystart(15) ! Y
	   Xpath( 3) =  Zstart(15) + landing_speed_at_ground * delta_t  ! Z
c        write (90,*) ' time, time_list(14), Z ',adjusted_time
c     .                ,time_list(14),Xpath(3),Xpathdot(3),15

	   Xpathdot( 1) =  0.d0     ! U   this is in the world frame
	   Xpathdot( 2) =  0.d0     ! V   this is in the world frame
	   Xpathdot( 3) =  landing_speed_at_ground  ! W in the world frame

	   lstraight = .true.
	   radius = 0.d0
           laccelerate = .false.
           ldecelerate = .true.

        endif

        if (((adjusted_time.gt.time_list(13))
     .             .and.(Xpath(3) .gt. 0.d0))) icomplete = .true.  ! we stop when landed

        if (time.gt.time_list(15)) icomplete = .true.  ! something's wrong if we haven't landed yet,
c                                                        so it's time to stop the computation.

c       Notice that now subroutine fly in new_fdm controls icomplete,
c       and that the above setting is ignored.   jdw   10/19/22

c        if (first_pass) then
c        write (6,*) ' Xstart ', Xstart
c        write (6,*) ' Ystart ', Ystart
c        write (6,*) ' Zstart ', Zstart
c        write (6,*) ' Xpath ',Xpath
c        write (6,*) ' time_list ',time_list
c        write (6,*) ' delta_t_list ',delta_t_list
c        first_pass = .false.
c        endif
c        stop

c        control%laccelerate = laccelerate
c        control%ldecelerate = ldecelerate

	return
	end
	



