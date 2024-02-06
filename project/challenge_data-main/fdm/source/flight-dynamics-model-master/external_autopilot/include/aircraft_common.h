c   This is the include "header" for the common aircraft type fields
c   It is intended to be similar to a .h file.
c   The intent is to include this entire file when defining both the Fortran
c   C compatible versions of this type.
c   Joel Allardyce 13 July 2022    Southwest Research Institute
c
c   7/13/22 Initial coding
c
c       Conversion factors
        real(C_DOUBLE)     :: length_scale = 0.001d0   ! convert input values to meters.  Default is mm to meters, since CREO build is in millimeters and kilograms.
        real(C_DOUBLE)     :: mass_scale = 1.d0        ! convert input mass to kg.  Default is kg to kg.
        real(C_DOUBLE)     :: angle_scale = deg_to_rad ! convert input angles to radians.  Default is degree to radian (from pi.h).  Set to 1.d0 if you are inputting radians.
c
c       Internal physics constants
c       These constants are not adjusted internally - they default to MKS and should be changed if you are trying
c       to run internally in different units (though you'll need to guess units for the electrical stuff or power be wrong, which affects motor controls).
        real(C_DOUBLE)     :: grav = 9.80 ! (m/s^2)  gravitational acceleration units
        real(C_DOUBLE)     :: rho = 1.225 ! (kg/m^3) Air density in default units
c
c       Aircraft properties
        real(C_DOUBLE)     :: mass        ! (kg)     total mass
        real(C_DOUBLE)     :: x_cm        ! (m)      x of center of mass
        real(C_DOUBLE)     :: y_cm        ! (m)      xy of center of mass
        real(C_DOUBLE)     :: z_cm        ! (m)      xz of center of mass
        real(C_DOUBLE)     :: Ixx         ! (kg m^2) xx of moment of inertia tensor I
        real(C_DOUBLE)     :: Ixy         ! (kg m^2) xy of moment of inertia tensor I
        real(C_DOUBLE)     :: Ixz         ! (kg m^2) xz of moment of inertia tensor I
        real(C_DOUBLE)     :: Iyy         ! (kg m^2) yy of moment of inertia tensor I
        real(C_DOUBLE)     :: Iyz         ! (kg m^2) yz of moment of inertia tensor I
        real(C_DOUBLE)     :: Izz         ! (kg m^2) zz of moment of inertia tensor I
c       The inverse is computed by the software, it is not an input, so only dummy need appear in list read
        real(C_DOUBLE)     :: InvIxx      ! xx of inverse of I
        real(C_DOUBLE)     :: InvIxy      ! xy of inverse of I
        real(C_DOUBLE)     :: InvIxz      ! xz of inverse of I
        real(C_DOUBLE)     :: InvIyy      ! yy of inverse of I
        real(C_DOUBLE)     :: InvIyz      ! yz of inverse of I
        real(C_DOUBLE)     :: InvIzz      ! zz of inverse of I
        integer(C_INT)     :: num_wings      = 0 ! number of wing elements (wings, tails, etc.)  (input)
        integer(C_INT)     :: num_propellers = 0 ! number of propellers (each assumed to have a motor) (input)
        integer(C_INT)     :: num_batteries  = 0 ! number of batteries (input)
        integer(C_INT)     :: num_controls   = 0 ! number of controls (should equal the sum of the next three) (computed internally)
        integer(C_INT)     :: num_wing_controls = 0 ! number of wing controls (ailerons, flaps, etc.) (computed internally)
        integer(C_INT)     :: num_horizontal_motor_controls = 0 ! number of horizontal motor controls (computed internally)
        integer(C_INT)     :: num_vertical_motor_controls   = 0 ! number of vertical motor controls (computed internally)
c                          It should be the case that num_controls = num_wing_controls +
c                          num_horizontal_motor_controls + num_vertical_motor_controls,
c                          though we do not check because maybe something will change in the future.
        integer(C_INT)     :: num_ground_contacts    ! number of ground contact points
        real(C_DOUBLE)     :: X_fuseuu = -12345.d0   ! (m^2) x-axis force coefficient for the fuselage (presented area) (default -12345. means internally guess value
        real(C_DOUBLE)     :: Y_fusevv = -12345.d0   ! (m^2) y-axis force coefficient for the fuselage (presented area)  based on motor locations and assumed motor
        real(C_DOUBLE)     :: Z_fuseww = -12345.d0   ! (m^2) z-axis force coefficient for the fuselage (presented area)  height of 7 cm)
        real(C_DOUBLE)     :: x_fuse = -12345.d0     ! (m)   effective center of fuselage, body x (center of pressure?) (default -12345. means 
        real(C_DOUBLE)     :: y_fuse = -12345.d0     ! (m)   effective center of fuselage, body y                        internally set ._fuse to ._cm)
        real(C_DOUBLE)     :: z_fuse = -12345.d0     ! (m)   effective center of fuselage, body z (used for body drag calculation)
c
c       We aso inialize various items (or else there are defaults)
        real(C_DOUBLE)     :: x _initial(13) = (/ 0.d0, 0.d0, 0.d0,      ! (m/s)   initial velocity in body fixed frame
     .                                            0.d0, 0.d0, 0.d0,      ! (deg/s) initial rotation vector (P Q R) in body fixed frame
     .                                      1.d0, 0.d0, 0.d0, 0.d0,      ! (-)     initial quaternion for orientation
     .                                            0.d0, 0.d0, 0.d0 /)    ! (m)     initial world frame displacement (all initial x)
        real(C_DOUBLE)     :: xd_initial(13)   ! (various units) initial xd
        real(C_DOUBLE)     :: uc_initial(10)   ! (-)       initial uc (controls); dimensionless, 0 <= uc <= 1
        real(C_DOUBLE)     :: time = 0.d0      ! (seconds) initial time 
        real(C_DOUBLE)     :: dt = 0.001       ! (seconds) default fixed time step (seconds)
        real(C_DOUBLE)     :: dt_output = 1.d0 ! (seconds) time between output lines
        real(C_DOUBLE)     :: time_end  = 1.d0 ! (seconds) end time (seconds)
        real(C_DOUBLE)     :: Unwind    = 0.d0 ! (m/s)     North wind speed in world frame
        real(C_DOUBLE)     :: Vewind    = 0.d0 ! (m/s)     East wind speed in  world frame
        real(C_DOUBLE)     :: Wdwind    = 0.d0 ! (m/s)     Down wind speed in world frame
c
c       Within this type (since it is only one object) we store information about the computational request
        integer(C_INT)     :: i_analysis_type  ! what type analysis  1. Fly from initial conditions  2. Trim to U = x(1)  3. Fly a flight path
        integer(C_INT)     :: debug     = 0    ! = 1 means verbose debug printouts from the fdm subroutine, etc.
        logical(C_BOOL)    :: debug_mini = .false. ! prints fderiv debugs for the trim states only; overrides debug.
        integer(C_INT)     :: simplex   = 2    ! = 1 means to only use SIMPLEX for trim solver (rather than MINPACK), = 2 means common start value for simplex
        real(C_DOUBLE)     :: acceleration_tolerance = 0.01d0 ! the maximum acceptable amount of acceleration for a trim state (m/s^2, rad/s^2)
        real(C_DOUBLE)     :: tolerance = 1.d-10  ! tolerance for MINPACK
        real(C_DOUBLE)     :: weight(3) = (/1.d0, 1.d0, 1.d-4 /)   ! weightings for optimizations
        logical(C_BOOL)    :: motordetails = .false.  ! if true, provides a table of propeller performance vs. control variable
        integer(C_INT)     :: i_out_metrics = 80  ! default file number for metrics write out (shouldn't be changed)
        integer(C_INT)     :: i_out_score   = 79  ! default file number for score write out (shouldn't be changed)
        logical(C_BOOL)    :: ignore_electrical = .false.  ! this allows the vehicle to fly even with electrical problems, for debugging
        logical(C_BOOL)    :: ignore_wingload   = .false.  ! this allows the vehicle trim state even if wing loads are exceeded
        real(C_DOUBLE)     :: I_peak_ratio = 1.5d0  !  a default I_peak/I_max for the motors
        logical(C_BOOL)    :: current_limiter = .true. ! limits the current to I_peak
        logical(C_BOOL)    :: temp_debug = .false.  ! a flag for assisting in debugging