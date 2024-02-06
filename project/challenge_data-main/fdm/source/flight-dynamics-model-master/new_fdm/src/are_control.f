	subroutine are_control(n,m,A,B,Qp,Qv,Qr,Qq,Rb,KK,isuccess)
c	This is to adapted from my twodtestrc.f that models
c	motion with two offset engines in free space.  
c	It uses RICPACK to solve the Algebraic Riccati Equation (ARE).
c	James Davidson Walker  24 May 2021

c	5/24/21  From my notebooks.
c	7/12/21  For the DARPA 3D autopilot.

c 	As input it has the two linear matrices A and B (called Alin and Blin in
c	fderiv where they are computed). 
c
c	We are minimizing int_0^inf (x^T Q x + u^T R u ) dt
c
c	Input:  A(n,n)   from xdot = A x + B u + Glin
c	        B(n,m)
c	        n, m
c	        Qp is weight on position in Q matrix  (10,11,12)
c	        Qv is weight on velocity in Q matrix  ( 1, 2, 3)
c		Qr is weight on angular velocit in Q  ( 4, 5, 6)
c		Qq is weight on quaternions in Q      ( 7, 8, 9)
c	        Rb is weight on R matrix (all u treated the same right now)
c
c	Output: KK(m,n)   where  u = u_trim + K*(x - xtilde) 
c	        isuccess = .true. if successful, .false. if not
c	

	implicit none

C     *****PARAMETERS:
c      INTEGER NR,NRD,NN,N, nb
c	parameter (nr = 6, nrd = 12, nn = 12, n = 6, nb = 2)  ! plane flight example

	integer n, m, nr, nn, nb, nrd
	logical isuccess

	integer IND(2*n),IORD,IBAL
      CHARACTER EFLAG,RDFLG,RFLAG,SFLAG
c      DOUBLE PRECISION G(2*n,2*n),F(2*n,2*n),E(n,n),Z(2*n,2*n),
      DOUBLE PRECISION G(24,24),F(24,24),E(12,12),Z(24,24),
     X          ALFR(2*n),ALFI(2*n),BETA(2*n),CPERM(2*n),CSCALE(2*n)
      LOGICAL TYPE
C
C     *****LOCAL VARIABLES:
      INTEGER I,IERR,IFAIL,IGH,J,LOW,NPJ
      DOUBLE PRECISION COND,EPS,EPS1
      LOGICAL MATZ

c	Variables as I define them
	integer k
	double precision A(n,n), B(n,m), Q(n,n)
	double precision Qv, Qr, Qq, Qp, Rb
c	double precision KK(m,n)
	double precision KK(50,n)
c        INTEGER NRX,NRW,IPVT(N)
c        DOUBLE PRECISION R(n,m),RI(n,m)
c     .                  ,S(n,m),FB(n,n),W(n,n),WK(n)


c	write (6,*) ' ARE n,m = ',n,m
c
c	write (6,*) ' ARE Alin = '
c	do nr = 1, n
c	   write(6,1000) (A(nr,nn),nn=1,12)
c	enddo
c	write (6,*) ' ARE Blin = '
c	do nr = 1, n
c	   write (6,1000) (B(nr,nn),nn=1,m)
c	enddo
 1000	format(12e12.5)

	nr  = n	

	nn  = 2*n
	nrd = 2*n

	nb  = m

c	We set up Q - it is diagonal
	Q = 0.d0  ! matrix
	Q( 1, 1) = Qv  ! velocity weight
	Q( 2, 2) = Qv
	Q( 3, 3) = Qv
	Q( 4, 4) = Qr  ! rotational velocity weight (on PQR)
	Q( 5, 5) = Qr
	Q( 6, 6) = Qr
	Q( 7, 7) = Qq  ! quaternions weight
	Q( 8, 8) = Qq
	Q( 9, 9) = Qq
	Q(10,10) = Qp  ! position weight
	Q(11,11) = Qp
	Q(12,12) = Qp

C
C     *****PURPOSE:
C     GIVEN THE 2N BY 2N MATRIX PENCIL
C
C              LAMBDA * F  -  G
C
C     THIS SUBROUTINE FINDS THE ORTHOGONAL MATRIX Z SUCH THAT
C
C              Q * (LAMBDA * F  -  G) * Z
C
C     IS IN GENERALIZED ORDERED REAL SCHUR FORM.  FURTHERMORE, THE
C     UPPER LEFT N BY N BLOCK OF THE TRANSFORMED PENCIL CONTAINS
C     THE EIGENVALUES SPECIFIED BY THE PARAMETER IORD.  THE
C     SUBROUTINE THEN SOLVES THE LINEAR SYSTEM
C
C              X * E * Z11  =  Z21
C
C     FOR X, WHERE Z11 AND Z21 ARE THE UPPER AND LOWER LEFT N BY N
C     BLOCKS OF Z.
C
C     REF.:  ARNOLD, W.F., "ON THE NUMERICAL SOLUTION OF
C     ALGEBRAIC MATRIX RICCATI EQUATIONS," PHD THESIS, USC,
C     DECEMBER 1983.
C
C     *****PARAMETER DESCRIPTION:
C
C     ON INPUT:
C
c       NR  = 2
C               ROW DIMENSION OF THE ARRAY CONTAINING THE MATRIX E
C               AS DECLARED IN THE MAIN CALLING PROGRAM DIMENSION
C               STATEMENT;
C
c       NRD = 4 
C               ROW DIMENSION OF THE ARRAYS CONTAINING THE MATRICES
C               G, F AND Z AS DECLARED IN THE MAIN CALLING PROGRAM
C               DIMENSION STATEMENT;
C
c       NN  = 4
C               ORDER OF THE SQUARE MATRICES G AND F;
C
c       N   = 2
C               ORDER OF THE SQUARE MATRIX E;
C
c	My understanding is that, for us
c
c	G = (   A              - B * R^(-1) * B^T )
c           ( -C^T * Q * C     - A^T              )
c
c	so we need to build this somewhat complicated matrix
c
c	We first start with the uppper left A
	do i = 1, nr
	   do j = 1, nr
	      G(i,j) = A(i,j)
	   enddo
	enddo

c	We now do the upper right, assuming R = I*bbb
	do i = 1, nr
	   do j = 1, nr
	      G(i,j+nr) = 0.d0
	      do k = 1, nb
	         G(i,j+nr) = G(i,j+nr) - b(i,k)*b(j,k)/Rb
	      enddo
	   enddo
	enddo
		 
c	For the lower right, we have C = I,
	do i = 1, nr
	   do j = 1, nr
	      G(i+nr,j) = - Q(i,j)
	   enddo
	enddo

c	For the lower right it's A transpose
	do i = 1, nr
	   do j = 1, nr
	      G(i+nr,j+nr) = - A(j,i)
	   enddo
	enddo


C       G       REAL(NRD,NN)
C               PENCIL MATRIX CORRESPONDING TO THE GENERALIZED RICCATI
C               PROBLEM, WHOSE CONTENTS ARE ALTERED BY THIS ROUTINE;
C
c	My understanding is that
c	
c	F = ( E   0  )
c           ( 0   E^T)
c
c	which for us will typically just be the identity
c
	F = 0.d0
	do i = 1, 2*nr
	   F(i,i) = 1.d0
	enddo
C       F       REAL(NRD,NN)
C               PENCIL MATRIX CORRESPONDING TO THE GENERALIZED RICCATI
C               PROBLEM, WHOSE CONTENTS ARE ALTERED BY THIS ROUTINE;
C
	E = 0.d0
	do i = 1, nr
	   E(i,i) = 1.d0
	enddo
C               DESCRIPTOR MATRIX
C
C       CPERM   REAL(NN)
C               WORKING VECTOR OF SIZE AT LEAST NN;
C
C       CSCALE  REAL(NN)
C               WORKING VECTOR OF SIZE AT LEAST NN;
C
C       IND     INTEGER(NN)
C               WORKING VECTOR OF SIZE AT LEAST NN;
C
       IORD = -1   
C               PARAMETER SPECIFYING THE SPECTRUM OF THE UPPER LEFT N
C               BY N BLOCK OF THE ORDERED REAL SCHUR FORM AS FOLLOWS:
C               =  1  GENERALIZED EIGENVALUES WHOSE MAGNITUDE IS LESS
C                     THAN UNITY
C               =  0  ANY ORDER
C               = -1  GENERALIZED EIGENVALUES WHOSE REAL PARTS ARE
C                     LESS THAN ZERO;
C
       IBAL = 1
C               PARAMETER SPECIFYING THE BALANCING BEING EMPLOYED AS
C               FOLLOWS:
C               = 1  WARD BALANCING
C               = 0  NO BALANCING
C
       TYPE  = .true.
C               = .TRUE.  FOR CONTINUOUS-TIME SYSTEM
C               = .FALSE. FOR DISCRETE-TIME SYSTEM;
C
       EFLAG = 'F'
C               FLAG SET TO 'Y' IF E IS A DESCRIPTOR MATRIX THAT IS
C               OTHER THAN THE IDENTITY MATRIX.
C
C     ON OUTPUT:
C
C       F       CONTAINS THE SOLUTION MATRIX X COMPUTED AS SHOWN
C               ABOVE;
C
C       Z       REAL(NRD,NN)
C               CONTAINS THE MATRIX PRODUCT
C                    ( E  0 )     ( Z11  Z12)
C                    (      )  *  (         )
C                    ( 0  I )     ( Z21  Z22)
C               WHERE Z IS THE ORTHOGONAL TRANSFORMATION MATRIX
C               DESCRIBED ABOVE;
C
C       ALFR    REAL(NN)
C               REAL PARTS OF THE DIAGONAL ELEMENTS THAT WOULD RESULT
C               IF THE Q AND Z TRANSFORMATIONS WERE APPLIED TO THE
C               G MATRIX SUCH THAT IT WOULD BE REDUCED COMPLETELY TO
C               TRIANGULAR FORM AND THE DIAGONAL ELEMENTS OF THE
C               TRANSFORMED F MATRIX (ALSO TRIANGULAR) WOULD BE REAL
C               AND POSITIVE;
C
C       ALFI    REAL(NN)
C               IMAGINARY PARTS OF THE DIAGONAL ELEMENTS THAT WOULD
C               RESULT IF THE Q AND THE Z TRANSFORMATIONS WERE
C               APPLIED TO THE G MATRIX SUCH THAT IT WOULD BE REDUCED
C               COMPLETELY TO TRIANGULAR FORM AND THE DIAGONAL
C               ELEMENTS OF THE TRANSFORMED F MATRIX (ALSO TRIANGULAR)
C               WOULD BE REAL AND POSITIVE.  NONZERO VALUES OCCUR IN
C               PAIRS; THE FIRST MEMBER IS POSITIVE AND THE SECOND
C               MEMBER IS NEGATIVE;
C
C       BETA    REAL(NN)
C               REAL NONNEGATIVE DIAGONAL ELEMENTS OF F THAT WOULD
C               RESULT IF G WERE REDUCED COMPLETELY TO TRIANGULAR
C               FORM; THE GENERALIZED EIGENVALUES ARE THEN GIVEN BY
C               THE RATIOS ((ALFR + I*ALFI)/BETA);
C
C       CPERM(1)
C               CONDITION ESTIMATE OF E*Z11 WITH RESPECT TO INVERSION;
C
C       IND(1)  ERROR FLAG AS FOLLOWS
C               = 0  INDICATES NORMAL RETURN
C               = NONZERO IF MORE THAT 50 ITERATIONS WERE REQUIRED TO
C               DETERMINE THE DIAGONAL BLOCKS FOR THE QUASITRIANGULAR
C               FORM;
C
C       IND(2)  ERROR FLAG AS FOLLOWS
C               = 0  INDICATES NORMAL RETURN
C               = 1  INDICATES ATTEMPTED REORDERING FAILED.
C

c	write (6,*) ' G = '
c	do i = 1,2*nr
c	   write (6,10) (G(i,j),j=1,2*nr)
c	enddo

      call RICSOL(NR,NRD,NN,N,G,F,E,Z,ALFR,ALFI,BETA,CPERM,
     X                  CSCALE,IND,IORD,IBAL,TYPE,EFLAG)

c	write (6,*) ' G = '
c	do i = 1,2*nr
c	   write (6,10) (G(i,j),j=1,2*nr)
c	enddo
c	write (6,*) ' F = '
c	do i = 1,2*nr
c	   write (6,10) (F(i,j),j=1,2*nr)
c	enddo
 10	format (12f12.4)

	write (6,*) ' RICPACK ARE solver returns Ind(1), Ind(2),'
     .              ,' CPERM(1): ', Ind(1), Ind(2), CPERM(1)
     .              , ' (Success = 0, 0, <1.e19)'
	if ((CPERM(1).gt.1.d19).or.(Ind(1).ne.0).or.(Ind(2).ne.0)) then
c	   write (6,*) ' CPERM = ',CPERM
	   write (6,*) ' ARE solve failed '
	   isuccess = .false.
c	   stop
	else
	   isuccess = .true.
	endif

c	Given our problems, let's check to see if the algebraic ricatti equation is even
c	being solved.  Part of th awkwardness here is that F is not even symmetric.

c	Now let write out the gains matrix, just so that we can see if it is looking reasonable.
c	KK = - R^-1 * B^T * P

	KK = 0.d0
	do i = 1, nb
	   do j = 1, nr
	      do k = 1, nr
		 KK(i,j) = KK(i,j) - (1.d0/Rb)*B(k,i)*F(k,j)
	      enddo
	   enddo
	enddo
c	write (6,*) ' K = '
c	do i = 1,nb
c	   write (6,10) (KK(i,j),j=1,nr)
c	enddo



c           EFLAG = 'F'  ! means that E is diagonal
c	   RDFLG = 'T'  ! means R is diagonal
c	   RFLAG = 'Y'  ! means R is not the identity matrix
c	   SFLAG = 'N'  ! means S is the zero matrix
c	   TYPE  = .true.  ! for continuous time system
c	   NRX   = 2*nr    ! size of above F matrix which now contains X in upper left corner
c	   NRW   = nb   ! size of KK(nb,n)
c	   R     = 0.d0
c	   do i=1, nb
c	      R(i,i) = bbb
c	   enddo
c	   RI    = 0.d0
c	   do i=1, nb
c	      RI(i,i) = 1.d0/bbb
c	   enddo
c
c	     Notice that this FBGAIN has an opposite sign to K from what I have, but
c	     otherwise it is the same, so I'll go with the simpler coding.
c
c            call FBGAIN (NR,NRX,NRW,nr,nb,A,B,E,R,RI,S,F,KK,W,WK,IPVT,
c     X                   EFLAG,RDFLG,RFLAG,SFLAG,TYPE)
C
c
c	write (6,*) ' K = '
c	do i = 1,nb
c	   write (6,10) (KK(i,j),j=1,nr)
c	enddo


c	   This is  how it is used, for example.
c	   u(1) = u1
c	   u(2) = u2
c	   do i = 1, nr
c	      do j = 1, nb
c	         u(j) = u(j) + KK(j,i)*(x(i)-xtilde(i))
c     .            , KK(j,i)*(x(i)-xtilde(i))
c	      enddo
c	   enddo
c	   do j = 1, nb
c	      u(j) = max(0.d0,min(1.d0,u(j)))
c	   enddo



	return
	end	   

