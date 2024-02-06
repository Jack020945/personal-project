      subroutine simplex (pdim,p,y,mp,np,ndim,tolerance,iterations,
     .	               m,n,UVWPQR,
     .                 time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)

c     4/3/21  Modified for the DARPA project, to do the call to the
c	      flight software.  We now pass the various files through
c	      the subroutine calls since there is really no other
c	      way to do it.  JDW
c
c     1/22/22 Placed a missing line updating contraction cost function values.
c             Also changed some inequalities from strict to ge, because
c             our barrier function was leading to cost evaluations with
c             exactly the same value, and the contraction was being
c             missed. JDW
c
c      2/3/22 To reduce programming and only have one fdcost, I update
c             the p state since in fdcost for minpack it can get 
c             changed (and that is very important) but for simplex
c             I don't think we want it to change.  JDW


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
c      parameter (alpha=1.0d0, beta=0.5d0, gamma=2.0d0, itmax=2000)
      parameter (alpha=1.0d0, beta=0.5d0, gamma=2.0d0, itmax=6000)   !  jdw 3/9/22
      double precision p(pdim,np), y(mp), tolerance, pstar(50), ystar
      double precision p2star(50), y2star, pbar(50), tol
      double precision ptemp(50)  ! jdw 2/3/22

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
	 ptemp(1:ndim) = pstar(1:ndim)
c	 call fdcost2(m, n, ptemp, za, iflag,
	 call fdcost(m, n, ptemp, za, iflag,
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
cold2         if (ystar .lt. y(low)) then
         if (ystar .le. y(low)) then
c           We form the double star terms.
            do i = 1, ndim
               p2star(i) = (one - gamma) * pbar(i) + gamma * pstar(i)
            enddo
cc            y2star = funk (p2star)
	    ptemp(1:ndim) = p2star(1:ndim)
c	    call fdcost2(m, n, ptemp, za, iflag,
	    call fdcost(m, n, ptemp, za, iflag,
     .                 UVWPQR, time, xdim, x, xd, xo, udim, uc,
     .                 aircraft, wing, propeller, battery, control)

cold	    call fdcost(Unorth, p2star, za,
cold     .                 time, xdim, x, xd, xo, udim, uc,
cold     .                 aircraft, wing, propeller, battery, control)
	    y2star = 0.d0
	    do i = 1, m
	       y2star = y2star + za(i)**2
	    enddo
cold2            if (y2star .lt. y(low)) then
            if (y2star .le. y(low)) then
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
cold2            if (ystar .gt. y(inexthigh)) then
            if (ystar .ge. y(inexthigh)) then
c              It is time for a contraction.
               do i = 1, ndim
                  p2star(i) = (one - beta) * pbar(i) + beta * p(ihigh,i)
               enddo
cc               y2star = funk (p2star)
	       ptemp(1:ndim) = p2star(1:ndim)
c	       call fdcost2(m, n, ptemp, za, iflag,
	       call fdcost(m, n, ptemp, za, iflag,
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
cold2               if (y2star .gt. y(ihigh)) then
               if (y2star .ge. y(ihigh)) then
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
	             ptemp(1:ndim) = p2star(1:ndim)
c	             call fdcost2(m, n, ptemp, za, iflag,
	             call fdcost(m, n, ptemp, za, iflag,
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
		     y(i) = ystar  ! missing line, jdw, 1/22/22
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
