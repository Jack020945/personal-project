	subroutine propread(
     .       aircraft, wing, propeller, battery, control)

	use iso_c_binding
	implicit none
	include 'parts.h'

c	Written by Sidney Chocron, modified by James Walker for linking with the fdm.


!     4 April 2021 Software developed for DARPA Air Taxi by Sidney Chocron @ Southwest Research Institute
!                  Reads the propeller table and provides Ct and Cp for the given RPM and Advance Ratio      
!
!                  Reading propeller tables.
!                  Some assumptions
!                  01- James interested in variables J (advancement ratio), Ct, and Cp. They are always in the 2nd, 4th, and 5th column, I checked all the files
!                  02- Maximum number of decimals is four, checked all the tables.
!                  03- There are a few tables that have less header lines but "PROP RPM" is consistenly in the same place and can be used as a marker
!                  04- The data start four lines below "PROP RPM"
!                  05- Propeller RPMs are less than 100,000.
!
!                  Inputs needed: filename, Advance Ratio (Advin), and RPM (RPMin) that will be used to find Ct and Cp
!                  Outputs: the interpolated value called auxCt, the upper left corner of the square used in the table: iAdv,iOmega

!      program propread
!      implicit none
      integer, parameter :: lmax = 100
      character*80 trash, filename, filenames(50)
      character*6 crpm,Vch,Advch
      character*10 Pech,Ctch,Cpch
      integer i,il,irows(lmax),j,k,icolumns,icomlines,ios,n,m
      integer irpmaux,inumtables,iAdv,iOmega,irpm(lmax)
      real*8 V(lmax,lmax),Adv(lmax,lmax),Pe,Ct(lmax,lmax),Cp(lmax,lmax)
     .                 , RPMin, Advin
      real*8 aux,auxCt,auxCp
	double precision x, x_table(lmax), dxkm1, dxk
	integer          ierror, jindex, kk, kpast


!     Inputs coming from presumably a subroutine.
!      filename='/Users/ichocron/Documents/SwRI/DARPAair/PropData/PER3_10x10.dat'
!      filename='/Users/ichocron/Documents/SwRI/DARPAair/PropData/PER3_24x12E.dat'

	do n=1,aircraft%num_propellers

c	   We create a list of the file names
	   filenames(n) = propeller(n)%prop_fname

c	   Have we already read this file?
c	   do m = 1, n-1
c	      if (filenames(n).eq.filenames(m)) then
cc	         We already read this file, so we are not going to read it again
c		 propeller(n)%prop_num_pts_omega 
c     .                     =  propeller(m)%prop_num_pts_omega
c		 propeller(n)%prop_num_pts_J     
c     .                     => propeller(m)%prop_num_pts_J
c	         propeller(n)%J     => propeller(m)%J
c		 propeller(n)%Ct    => propeller(m)%Ct
c		 propeller(n)%Cp    => propeller(m)%Cp
c	         goto 125  ! we are done with this case
c	      endif
c	   enddo

cc	   So we have not read it yet.  Our objective is to read the table.  (Alloctable arrays didn't work here.)
	   filename = filenames(n)

ctest      RPMin=5555
ctest      Advin=0.33     ! Input advance ratio
      

      icolumns=1   ! We will consider RPMs as the columns and Adv. Ratio as the rows in the table. The values Ct and Cp are needed given RPM and Adv. Ratio.
      icomlines=17   ! Number of comment lines on the header, this changes so we will have to look for the marker

!     Read file with data 
      open (10,file=filename,status='old')
	

!     Main loop that reads the whole file for one propeller and saves the variables of interest. i sweeps the rows (Adv. Ratio) and j the columns (RPMs)
!     (odd terminology - I think we are really just saying J = (j,i), for example, so the row index i is RPM and the colum
!     il=line number j can also be interpreted as the table number. Unfortunately the number of rows is not consistent from table to table
      il=1
      j =1
      
      do 
        read (10,101,iostat=ios) trash; if (ios.ne.0) goto 200    ! Will read lines until it finds the keyword PROP RPM
101     format (a80)
        if (trash(10:17).eq.'PROP RPM') then
          crpm=trash(25:30)
          read(crpm,'(i6)') irpmaux   ! Trick to transform a string into an integer number. String length and format need to have the same length!
          irpm(j)=irpmaux             ! RPM value for the whole table j
          read (10,101,iostat=ios) trash; if (ios.ne.0) goto 200
          read (10,101,iostat=ios) trash; if (ios.ne.0) goto 200
          read (10,101,iostat=ios) trash; if (ios.ne.0) goto 200
          i=1
!         Now the loop to read data for a specific RPM            
          do 
            read (10,101,iostat=ios) trash; if (ios.ne.0) goto 200
            if (trim(trash).eq.'') then
!               write(*,*) 'end of list'
               exit
            endif
            Vch   = trim(trash( 7:13))
            Advch = trim(trash(19:25))
            Pech  = trim(trash(29:39))
            Ctch  = trim(trash(40:49))
            Cpch  = trim(trash(51:61))
c	    Sidney is clearly indexing the advance ratio J as i and the omega as j and calling it (i,j)
            read(Vch,  '(f6.1)',ERR=300) V(i,j)    ! Trick to transform a string into number. String length and format need to have the same length!
            read(Advch,'(f6.2)',ERR=300) Adv(i,j)  ! Note the error handling. James asked me to ignore a whole table when there is a NaN in it. 
            read(Ctch,'(f10.4)',ERR=300) Ct(i,j)   ! By going to line 300 when it finds a NaN or error it does not increase the table number j so it is like the table did not exist.
            read(Cpch,'(f10.4)',ERR=300) Cp(i,j)  
110         format (5(f8.4))
120         format (a,f10.4)

            irows(j) = i   ! Number of rows in table j  i.e., j is the index to omega, i is the index to J = advance ratio
            i  = i  + 1   
            il = il + 1
          enddo

        j=j+1
300     continue       ! When an error like reading a NaN happens it jumps here, and ignores the last table read. 
        endif
      enddo  ! think this means the file is read

200   continue    !  end of file, other error?

	   close(10) 

	   j = j - 1

c	   allocate( propeller(n)%omega   (j)    )
c	   allocate( propeller(n)%J (irows(1)  ) )
c	   allocate( propeller(n)%Cp(irows(1),j) )
c	   allocate( propeller(n)%Ct(irows(1),j) )

c	   Now we have the table, but the J may not match up.  I don't like this, so I am going to change the table
c	   to make them match.  This can lead to a series of one-dimensional interpolations.  Hopefully it is 
c	   reasonable.  We assume the first column is representative.

	   do jindex = 1, irows(1)
	     propeller(n)%Cp(jindex,1) = Cp(jindex,1)
	     propeller(n)%Ct(jindex,1) = Ct(jindex,1)
	   enddo

	   do m = 2, j  ! number of omega sheets in the table
c	    We now do linear interpolation on the rest of the tables so that we get a "square" table.
	    do jindex = 1, irows(m)
	     x_table (jindex) = Adv(jindex,m)  ! we move the mth omega sheet's J into the column
	    enddo
	    kpast = 0
	    do jindex = 1, irows(1)
	     x = Adv(jindex,1)  ! this is the value on the 1st momega sheet.
c	                          We are now interpolating it to the mth omega sheet
	     call table (irows(m),x, x_table, 
     .               dxkm1, dxk, kk, kpast, ierror)
	     propeller(n)%Cp(jindex,m) = dxkm1*Cp(kk-1,m) + dxk*Cp(kk,m)
	     propeller(n)%Ct(jindex,m) = dxkm1*Ct(kk-1,m) + dxk*Ct(kk,m)
	    enddo
	   enddo  ! this completes reformulation of the table

c	   We convert the rpm's to radians per second
	   do m = 1, j
	      propeller(n)%omega(m) = twopi * dfloat(irpm(m)) / 60.d0
	   enddo
	   do jindex = 1, irows(1)
	      x_table (jindex) = Adv(jindex,1)  ! our J column
	   enddo
	   propeller(n)%J(1:irows(1)) = x_table (1:irows(1))

c	   So presumably now everything is done.  We now assign the variables
c	   Hopefully someday I'll find out how to make an allocate work in this context.
	   propeller(n)%prop_num_pts_omega = j
	   propeller(n)%prop_num_pts_J     = irows(1)
c	   propeller(n)%Ct                 = aCt
c	   propeller(n)%Cp                 = aCp

c	   This is where we do the adjustment based on the Ct and Cp scale factors
	   do m = 1, propeller(n)%prop_num_pts_omega
	      do jindex = 1, propeller(n)%prop_num_pts_J
	         propeller(n)%Ct(jindex,m) = propeller(n)%Ct(jindex,m)
     .                                     * propeller(n)%Ct_scale
	         propeller(n)%Cp(jindex,m) = propeller(n)%Cp(jindex,m)
     .                                     * propeller(n)%Cp_scale
	      enddo
	   enddo

c 125	   continue ! jumping to here since we aready read this table.

	enddo ! on n, the propeller number.  So at this point all table should have been allocated and read.

	return

	end

