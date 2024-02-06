c   This is the include "header" for the common wing type fields
c   It is intended to be similar to a .h file.
c   The intent is to include this entire file when defining both the Fortran
c   C compatible versions of this type.
c   Joel Allardyce 13 July 2022    Southwest Research Institute
c
c   7/13/22 Initial coding
c
c       Information about wing or half wing
        real(C_DOUBLE)     :: surface_area ! (m^2) area of wing segement (i.e., half for the wing broken in half)
c                                            We assume that "forward" is in the x direction.  Given the x-direction being 
c                                            in the plane of the wing section, now the question is what is the normal 
c                                            direction to the wing in the other two directions, so (ny,nz) = (0,1) is for 
c                                            the horizontal wing or tail (pointing down) and (ny,nz) = (1,0) is the normal
c                                            pointing right for the vertical tail.  (This may require some clarification.)
        real(C_DOUBLE)     :: span         ! (m)   wing section span
        real(C_DOUBLE)     :: chord        ! (m)   wing chord (averge in an area sense?)
c                                            nx, ny, nz is the unit normal to the wing in the world frame (normalized internally
c                                            if not entered that way).  We expect nz < 0 for the top pointing "up".
c                                            Wings can only twist around dy (angle of incidence) or dx (stabilizer).
c                                            Either dx or dy must be zero.  This is enforced in the code.  Internally, things are changed.
c                                            No swept wings (i.e., nz cannot be zero).
        real(C_DOUBLE)     :: max_load = 0.d0     ! (N)   this is the specified maximum load of the wing section (to see if they are strong enough).
        real(C_DOUBLE)     :: nx =  0.d0          ! (-)   x unit normal   default is wing with no angle of incidence, flat in the XY plane
        real(C_DOUBLE)     :: ny =  0.d0          ! (-)   y unit normal 
        real(C_DOUBLE)     :: nz = -1.d0          ! (-)   z unit normal
        real(C_DOUBLE)     :: angle_of_incidence = 0.d0  ! this is computed internally if ny = 0 and nx is nonzero (= asin(nx)).
        real(C_DOUBLE)     :: i_wing_type  ! (-)   1 = right wing half
c                                                  2 = left wing half
c                                                  3 = horizontal stabilizer
c                                                  4 = vertical stabilizer (or V tail sections)
        real(C_DOUBLE)     :: a            ! (1/deg) slope of lift curve 
        real(C_DOUBLE)     :: C_L0         ! (-) zero angle of attack lift coefficient (= - AoAatL0 * a)
        real(C_DOUBLE)     :: C_Lmax       ! (-) maximum lift coefficient 
        real(C_DOUBLE)     :: C_Lmin       ! (-) minimum lift coefficient (= - C_Lmax)
        real(C_DOUBLE)     :: C_D0         ! (-) zero lift drag coefficient
        real(C_DOUBLE)     :: k            ! (-) induced drag coefficient
        real(C_DOUBLE)     :: C_Dfp = 1.d0 ! (-) drag coefficient at alpha = 90 deg
        real(C_DOUBLE)     :: AoAatC_Lmax  ! (deg) Angle of attack at maximum C_L
        real(C_DOUBLE)     :: AoAatL0      ! (deg) Angle of attack for zero lift from wing
        real(C_DOUBLE)     :: C_M0         ! (-) moment coefficient at zero lift
        real(C_DOUBLE)     :: delta1_max =   25.d0  ! (deg) max aileron deflection
        real(C_DOUBLE)     :: delta1_min = - 25.d0  ! (deg) min aileron deflection
        real(C_DOUBLE)     :: delta2_max =   25.d0  ! (deg) max flap deflection
        real(C_DOUBLE)     :: delta2_min = - 25.d0  ! (deg) min flap deflection
        integer(C_INT)     :: icontrol1    ! control channel for aileron
        integer(C_INT)     :: icontrol2    ! control channel for flap
        real(C_DOUBLE)     :: bias1        ! bias for aileron control
        real(C_DOUBLE)     :: bias2        ! bias for flap control
        logical(C_BOOL)    :: ic1 = .false. ! this flag can be set externally but is set internally
        logical(C_BOOL)    :: ic2 = .false. ! ibid.  This is .true. if the control channel is doing something,
c                                             For example, if icontrol1 is set to 0 then ic1 = .false.,
c                                             saying that the channel is not active.
c                          The idea here is that abs(bias1)+abs(bias2)=1., to 
c                          represent the whole wing.  The sign of the bias
c                          is intended to be negative for an aileron on the
c                          second half of the wing.  Thus, for wing(1)
c                          icontrol1=1 (say), icontrol2=1, bias1 = 1.
c                          bias2 = 0., and on wing(2) icontrol1=icontrol2=1
c                          bias1=-1. and bias2=0., as examples.
c                          The idea here is that a wing can have both flaps, which
c                          have the same magnitude and same sign bias, and ailerons,
c                          which have same magnitude and opposite sign bias.
c                          However, you can adjust the bias to reflect the flaps and aileron
c                          having different max deflections on a wing.
        real(C_DOUBLE)     :: ac1          ! slope of control curve 1, assumed linear (computed internally)
        real(C_DOUBLE)     :: bc1          ! intercept of control curve 1, assumed linear (computed internally)
        real(C_DOUBLE)     :: ac2          ! slope of control curve 2, assumed linear (computed internally)
        real(C_DOUBLE)     :: bc2          ! intercept of control curve 2, assumed linear (computed internally)
        real(C_DOUBLE)     :: tau_a = 0.4d0 ! flap effectiveness of ailerons
        real(C_DOUBLE)     :: eta          ! effect of prop wash on wing
        real(C_DOUBLE)     :: eta_w        ! effect of wing downwash on tail
        real(C_DOUBLE)     :: x            ! effective center x 
        real(C_DOUBLE)     :: y            ! effective center y
        real(C_DOUBLE)     :: z            ! effective center z