	subroutine score9(x, y, z, flightdist, flightscore
     .                   , isin, minerr, path_data)
c	subroutine score(p, path, thirdpoint, allowerr, pathsize,
c     .			pointrate, flightdist, flightscore, isin)
c	This subroutine loops through each path segment and determines which point along the path the aircraft is closest to
c	The subroutine then provides the current score and whether or not the aircraft is within or outside of the allowable region
c	Alexander Carpenter, Southwest Research Institute, 2022

c	The score subroutine outputs three variables of interest:
c	The flightdist variable tells us how far the aircraft made it along the path
c	The flightscore variable tells us the current score
c	The isin variable is set to true if the aircraft is still within the allowable distance for at least one path segment
c	It is instead false if the aircraft is too far away from all path segments

c       path_data is how various arrays are passed, cutting down on the number of arrays to be passed.  jdw  10/4/22

	implicit none

        integer index_score
        parameter (index_score = 10)

        double precision x, y, z
	double precision path(index_score, 3),thirdpoint(index_score, 3)
	double precision allowerr(index_score), pointrate(index_score)
	double precision p(3), p1(3), p2(3), p3(3), ppath(3)
	double precision maxd, error, minerr, seg, curlen
	double precision runningdist, flightdist, altdist, alterr
	integer pathsize, runningscore, flightscore, altscore, i
	logical isin
	double precision inf
        double precision path_data  (1000)
        double precision path_data_2(1000)

c       These are variables for the scoring routine stored in path_data
        equivalence( path      (1,1) , path_data_2(501)               )
        equivalence( thirdpoint(1,1) , path_data_2(501+3*index_score) )
        equivalence( allowerr  (1)   , path_data_2(501+6*index_score) )
        equivalence( pointrate (1)   , path_data_2(501+7*index_score) )


c       We move the array over (read only, since we don't do the reverse)
        do i = 501, 501 + 8 * index_score  ! this is all we need
           path_data_2(i) = path_data(i)
        enddo
        pathsize = int(path_data(500)+0.5d0)

        p(1) = x
        p(2) = y
        p(3) = z


c	In this subroutine, inf is also used to specify a very large number (i.e. a starting error as we try to find the closest path segment)
	inf = huge(0.0)
	minerr = inf
	alterr = inf
	runningdist = 0.
	runningscore = 0
	isin = .false.

c	Loop through each path segment
	do i = 1, pathsize
		p1 = path(i,:)
		p2 = path(i+1,:)
		p3 = thirdpoint(i,:)
c	Determine whether or not segment is a line or a circular arc
		if (all(p3 .lt. inf)) then
			call pointtoarc(p, p1, p2, p3, ppath, error, seg, curlen)
		else
			call pointtoline(p, p1, p2, ppath, error, seg, curlen)
		endif
c	Now we see if this path segment is closer to the aircraft than any other path segment
c	Score is also calculated
c	Aircraft must be within the specified range for that path segment to count
		if ((error .lt. minerr) .and. (error .lt. allowerr(i))) then
			minerr = error
			flightdist = runningdist + curlen
			flightscore = runningscore + floor(curlen*pointrate(i))
			isin = .true.
c	We store an alternative measure of distance and score even if it is outside the allowable range
c	This is only needed if no path segment is close enough to the aircraft
		else if (error .lt. alterr) then
			alterr = error
			altdist = runningdist + curlen
			altscore = runningscore + floor(curlen*pointrate(i))
		endif
c	We keep a running measure of the distance and score for the path up to this path segment,
c	This is in case we needed it for calculations involving subsequent path segments
		runningdist = runningdist + seg
		runningscore = runningscore + floor(seg*pointrate(i))
	enddo
c	If the aircraft is not an acceptable distance from any path segment, the alternate measures become what is reported
c	However, now we also report that the aircraft is outside the acceptable range
	if (isin .eqv. .false.) then
		flightdist = altdist
		flightscore = altscore
	endif

c       For reporting back
        if (.not.isin) minerr = alterr

	return
	end

	subroutine pointtoline(point, p1, p2, pline, dist, seg, curlen)
c	This subroutine projects a point to a line segment and finds the distance between the original and projected points
c	Alexander Carpenter, Southwest Research Institute, 2022

	implicit none

	double precision point(3), p1(3), p2(3), pline(3), a(3), b(3)
	double precision dist, seg, curlen, t

c	The variable point is the current location of the aircraft, while p1 and p2 are the endpoints of the current path segment
	a = point - p1
	b = p2 - p1
c	Calculate the fraction of the way the projected point is from p1 to p2
	t = dot_product(a, b)/dot_product(b, b)
c	This is a line segment, so we make sure that fraction is between 0 and 1
	t = min(max(0., t), 1.)
c	Calculate the projected point
	pline = t*b + p1
c	Calculate the distance between the actual aircraft location and the projected point
	dist = norm2(point - pline)
c	Calculate the total length of the path segment
	seg = norm2(b)
c	Calculate the length along the path from p1 to pline
	curlen = t*seg
	
	return
	end

	subroutine pointtoarc(point, p1, p2, p3, parc, dist, seg, curlen)
c	This subroutine projects a point to an arc and finds the distance between the original and projected points
c	Alexander Carpenter, Southwest Research Institute, 2022


	implicit none

	double precision point(3), p1(3), p2(3), p3(3), parc(3)
	double precision ab(3), ac(3), bc(3), cross(3), norm(3), center(3)
	double precision va(3), vb(3), vp(3)
	double precision dist, seg, curlen
	double precision r, v11, v22, v12, b, k1, k2, angtot, ang

c	Curved paths are obviously more complicated
c	First we need to determine the radius of curvature for the path
	ab = p2 - p1
	ac = p3 - p1
	bc = p3 - p2
	call cross_product(ab, ac, cross)
	r = (norm2(ab)*norm2(ac)*norm2(bc))/(2.*norm2(cross))
c	We also calculate the normal to the plane containing the path segment
c	The negative sign is needed because of the order in which the points were defined here
	norm = -cross/norm2(cross)
c	Then we can project the aircraft location to the plane containing the path segment
	parc = point - dot_product(point - p1, norm)*norm
c	Now we determine the center for the circle that contains the path segment
	v11 = dot_product(ab, ab)
	v22 = dot_product(ac, ac)
	v12 = dot_product(ab, ac)
	b = (2.*(v11*v22 - v12*v12))
	k1 = v22*(v11 - v12)/b
	k2 = v11*(v22 - v12)/b
	center = p1 + k1*ab + k2*ac
c	Once we know the center, we can calculate the angle over which the path segment sweeps
	va = p1 - center
	vb = p2 - center
	vp = parc - center
	call cross_product(va, vb, cross)
	angtot = atan2(dot_product(cross, norm), dot_product(va, vb))
c	We can also calculate the angle from p1 to the projected aircraft location
	call cross_product(va, vp, cross)
	ang = atan2(dot_product(cross, norm), dot_product(va, vp))
c	To be on an arc segment, this angle must be between 0 and the total angle for the arc segment
	ang = min(max(0., ang), angtot)
c	Calculate the projected point that lies on the path
c	To do this, we simply take the vector between p1 and the center and rotate it by the angle of the projected point
	call cross_product(norm, va, cross)
	parc = va*cos(ang) + cross*sin(ang) + center
c	Calculate the distance between the actual aircraft location and the projected point
	dist = norm2(point - parc)
c	Calculate the total length of the path segment
	seg = angtot*r
c	Calculate the length along the path from p1 to parc
	curlen = ang*r

	return
	end

	subroutine cross_product(a, b, cross)
c	This subroutine computes the cross product of two 3D vectors
c	Alexander Carpenter, Southwest Research Institute, 2022
	
	implicit none

	double precision cross(3), a(3), b(3)

	cross(1) = a(2)*b(3) - a(3)*b(2)
	cross(2) = a(3)*b(1) - a(1)*b(3)
	cross(3) = a(1)*b(2) - a(2)*b(1)

	end
