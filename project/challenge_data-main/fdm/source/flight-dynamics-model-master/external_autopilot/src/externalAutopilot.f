      module external
        contains
        subroutine externalAutopilot(
     .      time, xdim, x, xd, xo, udim, uc, aircraft, wing,
     .      propeller, battery, control, Xpath, Xpathdot,
     .      lstraight, radius, ipath )
     .  bind(C, name="fortranExternalAutopilot")

        USE, INTRINSIC :: ISO_C_BINDING
        IMPLICIT NONE
        include 'c_arguments.h'

        INTEGER(C_INT) xdim,udim

        REAL(C_DOUBLE) time, x(xdim), xd(xdim), xo(1500), uc(udim)

        REAL(C_DOUBLE) xtilde(13), xprime(13), xtrim(75)  ! changed from xtrim(13)  jdw  2/22/22
        REAL(C_DOUBLE) xtildeprime(13), xdiff(12)
        REAL(C_DOUBLE) Xpath(3), Xpathdot(3), radius
        REAL(C_DOUBLE) path_data(1000)
        REAL(C_DOUBLE) K(50,12), uctrim(50)
        REAL(C_DOUBLE) cospsi, sinpsi, coshalfpsi, sinhalfpsi
        REAL(C_DOUBLE) temp
        REAL(C_DOUBLE) xprime7orig, deltauc(50)
        REAL(C_DOUBLE) g_psi, l_psi, g_psi_old, l_psi_old

        INTEGER(C_INT) i, j, m, qskip, ipath

        LOGICAL(C_BOOL) ltrimset, lstraight

        REAL(C_DOUBLE) tblend, alpha, alpham1
        INTEGER(C_INT) i_speed_path_low, i_speed_path_high
        REAL(C_DOUBLE) speed_path, speed_path_low, speed_path_high
        INTEGER(C_INT), save :: i_trim_state, i_trim_state_last


        ! Types we can send through our C API function as pointers and recover
        ! here:
        type (aircraft_type_c), pointer  :: aircraft_c  ! only one aircraft
        type (wing_type_c), pointer      :: wing_c      (:)
        type (propeller_type_c), pointer :: propeller_c (:)
        type (battery_type_c), pointer   :: battery_c   (:)
        type (controls_type), pointer     :: control_c

        ! Test accessing a string type
        !character*80 :: cname

        call c_f_pointer(aircraft, aircraft_c)
        call c_f_pointer(wing, wing_c, shape=[maxparts])
        call c_f_pointer(propeller, propeller_c, shape=[maxparts])
        call c_f_pointer(battery, battery_c, shape=[maxparts])
        call c_f_pointer(control, control_c)

        !cname = transfer(aircraft_c%cname, cname)

        !write (6,*) 'CNAME: ', cname
        !write (6,*) 'grav: ', aircraft_f%grav
        !write (6,*) 'rho: ', aircraft_f%rho

        ! Coding to update uc, etc., follows.

        ! This initial coding using trim states.  We load the relevant trim
        ! state (or try to interpolate to find one).

        ! As long as things are working, we will not be updating the controller.

        control_c%compute_A = .false.

        ltrimset = .false.

        tblend = control_c%tblend

        ! Now we have trimstates related to speed and radii of curvature.  We look
        ! at the turning states first, since we will go with the lateral states if
        ! the there are no appropriate turning states.

        if ((.not.lstraight).and.(ipath.eq.3)) then

        ! For now, we look to see if the turning radius is within 100 meters of
        ! our trim state turning radii.
          if ((control_c%radius_1.ne.0.d0).and.(abs(radius -
     .         control_c%radius_1).lt.100.d0)) then

            do i = control_c%turn_1_num_trim_min,
     .        control_c%turn_1_num_trim_max
              if (control_c%turn_1_trim_if(i)) then
                if (abs(control_c%turn_1_trim_speed(i) -
     .          control_c%requested_lateral_speed).lt.0.05d0) then
                ! We are close enough to a trim state that we will use it
                  do j = 1, xdim
                    xtrim(j) = control_c%turn_1_trim_x (i,j)
                  enddo
                  do j = 1, udim
                    do m = 1, 12
                      K(j,m) = control_c%turn_1_trim_K(i,j,m)
                    enddo
                  enddo
                  qskip = control_c%turn_1_trim_qskip(i)
                  do j = 1, udim
                    uctrim(j) = control_c%turn_1_trim_uc(i,j)
                  enddo


                  ! Here we will try a blend, to see if this approach can do something
                  ! for us

                  if (time.lt.tblend) then

                    alpha = 1.d0 - time/tblend
                    alpha = max(0.d0, alpha)
                    alpha = min(1.d0, alpha)
                    alpham1 = 1.d0 - alpha

                    do j = 1, xdim
                      xtrim(j) = alpham1 *
     .                           control_c%turn_1_trim_x (i,j) +
     .                           alpha *
     .                           control_c%lateral_trim_x (i,j)
                    enddo
                    do j = 1, udim
                      do m = 1, 12
                        K(j,m) = alpham1 *
     .                           control_c%turn_1_trim_K (i,j,m) +
     .                           alpha *
     .                           control_c%lateral_trim_K(i,j,m)
                      enddo
                    enddo
                    qskip = control_c%turn_1_trim_qskip(i)
                    do j = 1, udim
                      uctrim(j) = alpham1 *
     .                            control_c%turn_1_trim_uc (i,j) +
     .                            alpha *
     .                            control_c%lateral_trim_uc(i,j)
                    enddo

                  endif

                  ltrimset = .true.
                endif
              endif
            enddo

          endif

          if (ltrimset) goto 200

          if ((control_c%radius_2.ne.0.d0).and.
     .    (abs(radius - control_c%radius_2).lt.100.d0)) then 

            do i = control_c%turn_2_num_trim_min,
     .        control_c%turn_2_num_trim_max
              if (control_c%turn_2_trim_if(i)) then
                if (abs(control_c%turn_2_trim_speed(i) -
     .          control_c%requested_lateral_speed).lt.0.05d0) then
                ! We are close enough to a trim state that we will use it
                  do j = 1, xdim
                    xtrim(j) = control_c%turn_2_trim_x (i,j)
                  enddo
                  do j = 1, udim
                    do m = 1, 12
                      K(j,m) = control_c%turn_2_trim_K(i,j,m)
                    enddo
                  enddo
                  qskip = control_c%turn_2_trim_qskip(i)
                  do j = 1, udim
                    uctrim(j) = control_c%turn_2_trim_uc(i,j)
                  enddo
                  ltrimset = .true.
                endif
              endif
            enddo
          endif

          if (ltrimset) goto 200

        endif

        speed_path = Xpathdot(1)**2 + Xpathdot(2)**2
        speed_path = sqrt(speed_path)
        i_speed_path_low  = -1
        i_speed_path_high = -1

        do i = control_c%lateral_num_trim_min,
     .         control_c%lateral_num_trim_max
          if (control_c%lateral_trim_if(i)) then

            ! Given lots of trim states, it worked to just look for one very nearby (within 0.5 m/s).
            ! Now we want to see if we can fly with sparser trim states.  The question is how to choose one.
            ! Our first choice is to pick the nearest one that is larger, but the problem with that
            ! is that there may not be a larger one.

            if (control_c%lateral_trim_speed(i).lt.speed_path) then
              if (i_speed_path_low.eq.-1) then
                i_speed_path_low = i
                speed_path_low = control_c%lateral_trim_speed(i)
              else
                if (control_c%lateral_trim_speed(i).gt.speed_path_low)
     .          then
                  i_speed_path_low = i
                  speed_path_low = control_c%lateral_trim_speed(i)
                endif
              endif
            else ! this means that lateral trim speed is ge speed path
              if (i_speed_path_high.eq.-1) then
                i_speed_path_high = i
                speed_path_high = control_c%lateral_trim_speed(i)
                ! If it is assumed the trim state speeds are  in order, then
                ! we can jump out of this loop at this point.
                goto 840
              else
                if(control_c%lateral_trim_speed(i).lt.speed_path_high)
     .          then
                  i_speed_path_high = i
                  speed_path_high = control_c%lateral_trim_speed(i)
                endif
              endif
            endif
          endif
        enddo

 840    continue

        ! Now we should have a bracket of trim states on the current speed,
        ! or hopefully at least one trim state.

        if ((i_speed_path_low.ne.-1).and.(i_speed_path_high.ne.-1)) then
          if ((speed_path - speed_path_low)
     .    .lt.(speed_path_high-speed_path)) then
            i = i_speed_path_low
          else
            i = i_speed_path_high
          endif
        else if ((i_speed_path_low.eq.-1).and.(i_speed_path_high.ne.-1))
     .  then
          i = i_speed_path_high
        else if ((i_speed_path_low.ne.-1).and.(i_speed_path_high.eq.-1))
     .      then
           i = i_speed_path_low
        else
          write (6,*) '  Did not find a trim state.'
          stop
        endif

        ! ***** This is a test
          if (i.eq.0) i = 1   ! though labelled as a test, for right now we always use the lateral 1 m/s, even for vertical ascent  jdw 9/27/22
          ! if (i.eq.45) i = 46  ! this solved a particular problem with the Axe - so commented out that problem is not fixed.  jdw  9/27/22

          ! i = 1  ! hard wire for vertical ascent tests  9/15/22  !!  is this truly what's been happening?  jdw  9/27/22
          ! if (i.gt.6) i = i_speed_path_high  didn't seem to work, so we will need to try to look at this again sometime.
        ! ***** end of test

        i_trim_state = i
        control_c%i_trim_state = i

        if (i_trim_state.ne.i_trim_state_last) then
          i_trim_state_last = i
        endif

        ! We are close enough to a trim state that we will use it

        do j = 1, xdim
          xtrim(j) = control_c%lateral_trim_x (i,j)
        enddo
        do j = 1, udim
          do m = 1, 12
            K(j,m) = control_c%lateral_trim_K(i,j,m)
          enddo
        enddo
        qskip = control_c%lateral_trim_qskip(i)
        do j = 1, udim
          uctrim(j) = control_c%lateral_trim_uc(i,j)
        enddo
        ltrimset = .true.

 200    continue

        ! In this region we look around and interpolate, or perhaps we
        ! try to solve for a specific case.

        if (.not.ltrimset) then

            ! This is not yet written

        endif

        ! We build a pseudo xtilde state vector - there is no PQR or  q0

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

        ! Our first pass through this to fly a planar path relying on the
        ! controls as determined during the trim state computations.
        ! Given that plan, we need to think through what we mean here.
        ! In particular, we expect to just rotate in the plane, so
        ! what does that mean in terms of the quaternions and the location.
        ! As I see it, it means that we look at the required rotation 
        ! based on the north direction (trim state) vs. the xtilde direction,
        ! again, just looking at the X-Y plane.  Thus, we compute the
        ! angle in the X-Y plane:

        ! The dot product of (1,0) and (xtilde,ytilde) for the yaw psi.
        temp   = sqrt(xtilde(1)**2+xtilde(2)**2)
        if (temp.gt.0.d0) then
          cospsi = xtilde(1)/temp  ! not consider Z direction
          sinpsi = xtilde(2)/temp
        else
          ! Assume no rotation
          cospsi = 1.d0
          sinpsi = 0.d0
        endif

        cospsi = max(cospsi,-1.d0)
        cospsi = min(cospsi, 1.d0)

        ! We are looking at how to address the double wrap problem with the half
        ! angle (i.e., with the quaternion).  The atan2(sin,cos) returns values
        ! from - pi to + pi, with the disontinuity at +- pi.  If I am going to
        ! track the double wrap (not sure that will solve the problem, though)
        ! I need to let psi range from - 2 pi to + 2 pi.  So I need to pay attention
        ! to what happens at +- pi.  I'm assuming some smoothness here.
        ! The thinking is that if g_psi > pi/2, say, then we expect the new
        ! g_psi to be in the range of 0 to  2 pi, and so if g_psi_temp < 0 
        ! g_psi_temp = atan2(sinpsi,cospsi)
        ! d_g_psi = 
        ! So - pi < g_psi_temp <= pi
        ! if (g_psi_temp.gt.0.d0) then
        !     if (
        !     if (g_psi.ge.twopi) g_psi = g_psi_temp + twopi
        ! if (g_psi.le.-twopi) g_psi = g
        ! Okay - so what is the problem.  We want to go to - 2 pi so that the
        ! wrap comes back to pi.  We want to go to 2 pi so that the wrap comes
        ! back to pi.
        !          X
        !          ^
        !      II  |   I      if psi is computed in I then it can be a large 
        !    -------------> Y negative psi or a small positive psi.
        !      III |  IV      We need to know the previous angle.
        !
        ! if ((cospsi.ge.0.d0).and.(sinpsi.ge.0.d0)) then
        !     We are in quadrant I
        !     if (g_psi.gt.-0.25d0*pi).and.(g_psi.lt.


        ! This rotation which needs to act in two ways - to
        ! rotate xtilde to x, rotate xtilde dot to xdot, and rotate the quaternion.
        ! Then the resulting uc should be fine as is, not requiring any adjustments.
        ! The matrix (c s , -s c) rotates xtildedot,ytildedot to (1,0) times
        ! sqrt(tildedot**2+ytildedot**2).

        ! So xprime is intended to be the desired trim state.  Given that, we
        ! need to set it up.  Since we are staying in the plane for this, the
        ! speed is given by xprime(1) and it should be close to the requested
        ! speed.  Hence, we only have a yaw.

        xprime( 1) =   x(1)
        xprime( 2) =   x(2)
        xprime( 3) =   x(3)
        xprime( 4) =   x(4)
        xprime( 5) =   x(5)
        xprime( 6) =   x(6)

        ! We need to move the quaternion for x into the correct orientation.
        ! To do the quaternion multiplication, we need the appropriate half angles.

        if (temp.gt.0.) then

          coshalfpsi = sqrt((1.d0 + cospsi)/2.d0)        ! this is always positive
          sinhalfpsi = sqrt((1.d0 - cospsi)/2.d0)
          if (sinpsi.lt.0.d0) sinhalfpsi = - sinhalfpsi  ! this preserves the sign of sinpsi

        else

          coshalfpsi = 1.d0
          sinhalfpsi  = 0.d0

        endif

        ! We reverse the sign so that we have -psi/2, as requested below (but we do not 
        ! reverse the sign of psi because that is the correct rotation for the location).
        sinhalfpsi = - sinhalfpsi  ! this reverses the sign of psi 

        ! Now, what is the order of multiplication.  We have the current q.  If we came
        ! from the initializtion, then we have q = q_psi * q_trim.  This argues that
        ! the correct multiplication is q_uc eval = q_(-psi)*q = q_(-psi)*q_psi*q_trim = q_trim.

        ! This is q_(-psi)*q_x
        xprime( 7) = coshalfpsi*x( 7) - sinhalfpsi*x(10)
        xprime( 8) = coshalfpsi*x( 8) - sinhalfpsi*x( 9)
        xprime( 9) = coshalfpsi*x( 9) + sinhalfpsi*x( 8)
        xprime(10) = coshalfpsi*x(10) + sinhalfpsi*x( 7)

        ! Okay, there is another way to deal with the double wrap problem, though it
        ! essentially assumes some things - probably in particular that the vehicle
        ! is going in roughly the intended direction.  For our trim state
        ! we have that q0 is around +1.  Thus, if our half angle rotation is not
        ! correct, it will likely flip the sign of the quaternion.  Thus, our
        ! attempt at a simple fix is as follows:
        xprime7orig = xprime(7)
        if (xprime(7).lt.0.d0) then
          do i = 7, 10
            xprime(i) = - xprime(i)
          enddo
        endif


        ! This is rotating the X location into the trim state
        ! However, for curves we need a translation followed by
        ! a rotation.  Our choice is to move Xpath to the origin for the 
        ! trim state.  Thus, we move x the same before performing the
        ! same rotation as with the straight line paths.
        xprime(11) =   cospsi * (x(11) - Xpath(1))
     .               + sinpsi * (x(12) - Xpath(2))
        xprime(12) = - sinpsi * (x(11) - Xpath(1))
     .               + cospsi * (x(12) - Xpath(2))
        xprime(13) =   x(13) - Xpath(3)


        ! **** this is a test trial
        ! What if we remove the X dependence here
        ! xprime(11) =   sinpsi * (x(12) - Xpath(2))
        ! xprime(12) =   cospsi * (x(12) - Xpath(2))
        ! xprime(13) =   x(13) - Xpath(3)
        ! This showed that you do not want to remove
        ! the x11 error from the flight.  In this case
        ! the vehicle flew much faster than requested.
        ! **** end of test trial


        ! We move the Xpath trim state.  tilde prime is the trim state except
        ! for the location information which comes from the flight path.

        do i = 1, 10
          xtildeprime(i) = xtrim(i)
        enddo
        ! This I think just rotates in the plane, nothing out of plane.
        ! The translation to xtilde takes us to the origin.  the trim state
        ! should be at the origin, but to be sure we pin it down.
        ! This is xtilde - Xpath
        xtildeprime(11) = 0.d0
        xtildeprime(12) = 0.d0
        xtildeprime(13) = 0.d0

        ! We now construct the difference that will be used in the control law.
        ! This also includes the qskip contraction.
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

        ! We are now ready to compute the updated control values
        ! uc = uctrim + K*(x - xtilde)
        do i= 1, udim
          uc(i) = uctrim(i)
          do j = 1, 12
            uc(i) = uc(i) + K(i,j)*xdiff(j)
          enddo
          deltauc(i) = uc(i) - uctrim(i)
          uc(i) = max(uc(i),0.d0)
          uc(i) = min(uc(i),1.d0)
        enddo

 57     format(15e12.4)
 100    format(a14,13e12.4)
 101    format(f12.5,13(1pe12.5))

        return
        end

      end module external