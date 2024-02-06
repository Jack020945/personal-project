c   This is the include "header" for the shared controls type.  The entire
c   type is C compatible, and so it does not need to be broken out like other
c   types have been.
c   It is intended to be similar to a .h file.
c   The intent is to include this entire file when defining both the Fortran
c   C compatible versions of this type.
c   Joel Allardyce 13 July 2022    Southwest Research Institute
c
c   7/13/22 Initial coding
c
c
      type, bind(C) :: controls_type
c       Controls are assumed to be 0 <= u <= 1 in all cases, and they are linear.
c       I think in this we are going to define our relationships, such as would be by
c       connecting cables.
c       The following are to use in the controller algorithm as primary or fallbacks.
        logical(C_BOOL)    :: outside_trim_states = .false. ! if true, fdm does not compute trim state, user provides below
c                                                             If you provide your own, you must set _trim_if_0 to .true.,
c                                                             you must set _trim_uc and _trim_theta,  and _trim_speed to
c                                                             float(j) (or it probably won't work). minpack_trim.f does the rest
        logical(C_BOOL)    :: strong_fdcost = .false.      ! if true, this forces all requirements into fdcost, 
c                                                            otherwise fdcost only has uc domain requirements.
        integer(C_INT)     :: lateral_num_trim_min =  0    ! minimum index to trim states in the array (>=0)
        integer(C_INT)     :: lateral_num_trim_max = 50    ! maximum index to trim states in the array (<=50)
        logical(C_BOOL)    :: lateral_trim_if_0 (0:50) = .false. ! true if we have a UVWPQRdot trim state - may not be a trim state due to other warnings
        logical(C_BOOL)    :: lateral_trim_if (0:50) = .false.   ! true if we have a trim state with a control law
        real(C_DOUBLE)     :: lateral_trim_speed (0:50)    ! lateral speed for the trim state
        real(C_DOUBLE)     :: lateral_trim_x     (0:50,75) ! x state of trim state (with qskip)
        real(C_DOUBLE)     :: lateral_trim_uc    (0:50,50) ! the lateral trim control settings
        real(C_DOUBLE)     :: lateral_trim_theta (0:50)    ! the lateral trim pitch angles
        integer(C_INT)     :: lateral_trim_qskip(0:50) = 0 ! the value of qskip for the trim state control law
        real(C_DOUBLE)     :: lateral_trim_K (0:50,50,12)  ! the control law for the trim states
        real(C_DOUBLE)     :: lateral_trim_fdcost (0:50)   ! cost as used by simplex
        real(C_DOUBLE)     :: lateral_trim_max_xd (0:50)   ! unweighted max xd(1:6)
	integer(C_INT)     :: i_trim_state                 ! current reference trim state, primarily for output (from autopilot)
c
c       The following are to use in the controller algorithm as primary or fallbacks.
        integer(C_INT)     :: vertical_num_trim_min         ! minimum index to trim states in the array (>=0)
        integer(C_INT)     :: vertical_num_trim_max         ! maximum index to trim states in the array (<=50)
        logical(C_BOOL)    :: vertical_trim_if_0 (0:50) = .false. ! true if we have a trim state (see lateral description)
        logical(C_BOOL)    :: vertical_trim_if (0:50) = .false.   ! true if we have a trim state control law
        real(C_DOUBLE)     :: vertical_trim_speed (0:50)    ! vertical speed for the trim state
        real(C_DOUBLE)     :: vertical_trim_x     (0:50,75) ! x state of trim state (with qskip)
        real(C_DOUBLE)     :: vertical_trim_uc    (0:50,50) ! the lateral trim control settings
        real(C_DOUBLE)     :: vertical_trim_theta (0:50)    ! the lateral trim pitch angles
        real(C_DOUBLE)     :: vertical_trim_fdcost (0:50)   ! cost as used by simplex
        real(C_DOUBLE)     :: vertical_trim_max_xd (0:50)   ! unweighted max xd(1:6)
c
c       These are to compute trim states for turns
        real(C_DOUBLE)     :: radius_1             = 500.d0 ! turn radius 1
        integer(C_INT)     :: turn_1_num_trim_min =  0      ! minimum index to trim states in the array (>=0)
        integer(C_INT)     :: turn_1_num_trim_max = 50      ! maximum index to trim states in the array (<=50)
        logical(C_BOOL)    :: turn_1_trim_if_0 (0:50) = .false. ! true if we have a trim state (see lateral description)
        logical(C_BOOL)    :: turn_1_trim_if (0:50) = .false.   ! true if we have a trim state control law
        real(C_DOUBLE)     :: turn_1_trim_speed (0:50)      ! tangential speed for the trim state
        real(C_DOUBLE)     :: turn_1_trim_x     (0:50,75)   ! x state of trim state (with qskip)
        real(C_DOUBLE)     :: turn_1_trim_uc    (0:50,50)   ! the turn trim control settings
        real(C_DOUBLE)     :: turn_1_trim_theta (0:50)      ! the turn trim pitch angles
        real(C_DOUBLE)     :: turn_1_trim_phi   (0:50)      ! the turn trim roll angles
        integer(C_INT)     :: turn_1_trim_qskip(0:50) = 0   ! the value of qskip for the trim state control law
        real(C_DOUBLE)     :: turn_1_trim_K (0:50,50,12)    ! the control law for the trim states
        real(C_DOUBLE)     :: turn_1_trim_fdcost (0:50)     ! cost as used by simplex
        real(C_DOUBLE)     :: turn_1_trim_max_xd (0:50)     ! unweighted max xd(1:6)
        real(C_DOUBLE)     :: radius_2            = -300.d0 ! turn radius 2
        integer(C_INT)     :: turn_2_num_trim_min =  0      ! minimum index to trim states in the array (>=0)
        integer(C_INT)     :: turn_2_num_trim_max = 50      ! maximum index to trim states in the array (<=50)
        logical(C_BOOL)    :: turn_2_trim_if_0 (0:50) = .false. ! true if we have a trim state (see lateral description)
        logical(C_BOOL)    :: turn_2_trim_if (0:50) = .false.   ! true if we have a trim state control law
        real(C_DOUBLE)     :: turn_2_trim_speed (0:50)      ! tangential speed for the trim state
        real(C_DOUBLE)     :: turn_2_trim_x     (0:50,75)   ! x state of trim state (with qskip)
        real(C_DOUBLE)     :: turn_2_trim_uc    (0:50,50)   ! the turn trim control settings
        real(C_DOUBLE)     :: turn_2_trim_theta (0:50)      ! the turn trim pitch angles
        real(C_DOUBLE)     :: turn_2_trim_phi   (0:50)      ! the turn trim roll angles
        integer(C_INT)     :: turn_2_trim_qskip(0:50) = 0   ! the value of qskip for the trim state control law
        real(C_DOUBLE)     :: turn_2_trim_K (0:50,50,12)    ! the control law for the trim states
        real(C_DOUBLE)     :: turn_2_trim_fdcost (0:50)     ! cost as used by simplex
        real(C_DOUBLE)     :: turn_2_trim_max_xd (0:50)     ! unweighted max xd(1:6)
c
c       These values are the requested behavior for following a path.
        integer(C_INT)     :: i_flight_path                ! which flight path are we flying (need aircraft%i_analysis_type = 3 to do this)
        integer(C_INT)     :: i_Hackathon      = 1         ! the hackathon number (=0 is the regular runs)
        real(C_DOUBLE)     :: requested_lateral_speed      ! requested lateral speed for following horizontal parts of the path
        real(C_DOUBLE)     :: requested_vertical_speed     ! requested vertical speed for following vertical parts of the path (if any)
        real(C_DOUBLE)     :: timeslip                     ! in case there is desired (or internally computed) slippage
        integer(C_INT)     :: start_trim_state = -1        ! if the desired beginning is a trim state (tries to compute if -1, otherwise is no)
c	These are the additional specifications related to variable flight speed.  9/27/22 JDW
	real(C_DOUBLE)     :: requested_vertical_down_speed    ! up and down are and may be different
        real(C_DOUBLE)     :: requested_lateral_acceleration   ! a constant acceleration assumption
        real(C_DOUBLE)     :: requested_lateral_deceleration
        real(C_DOUBLE)     :: requested_vertical_acceleration
        real(C_DOUBLE)     :: requested_vertical_deceleration
c       The following three numbers, with assumed constant deceleration, define the final approach
	real(C_DOUBLE)     :: vertical_landing_approach_speed  = 0.5d0   ! speed for final approach
        real(C_DOUBLE)     :: landing_approach_height          = 3.0d0 ! Z at which final approach speed is reached, 3 meters default
        real(C_DOUBLE)     :: vertical_landing_speed_at_ground = 0.1d0  ! speed at which you hit the ground, default 10 cm/s

c       These values provide the weights for the LQR solver in determining weightings for the 
c       controls.
        real(C_DOUBLE)     :: Q_position         = 1.d0   ! weight for position (XYZ) in Q matrix
        real(C_DOUBLE)     :: Q_velocity         = 1.d0   ! weight for velocity (UVW) in Q matrix
        real(C_DOUBLE)     :: Q_angular_velocity = 0.d0   ! weight for angular rates (PQR) in Q matrix
        real(C_DOUBLE)     :: Q_angles           = 1.d0   ! weight for quaternions in Q matrix
        real(C_DOUBLE)     :: R                  = 1.d0   ! weight for R (times B) matrix

c       External autopilot enabling
        integer(C_INT)     :: external_autopilot = 0
c
c       These are some internal variables for the controller algorithm.
        logical(C_BOOL)    :: outside_control_states = .false. ! if true, fdm does not compute controls, user provides below
        logical(C_BOOL)    :: compute_A          = .false. ! instructs fderiv to update Alin, Blin, Glin
        integer(C_INT)     :: qskip              = 0       ! which quaternion to leave out of control LQR
        real(C_DOUBLE)     :: Alin  (12,12)                ! the linearized FDM for A   ( xdot = A x + B u + G )
        real(C_DOUBLE)     :: Blin  (12,50)                ! the linearlzed FDM for B
        real(C_DOUBLE)     :: Glin  (12   )                ! the linearized FDM for G
        real(C_DOUBLE)     :: K     (50,12)                ! the control law   u = u_trim + K(x - xtilde)
        real(C_DOUBLE)     :: tblend             = 0.d0    ! blend time for controls transitions
c
c       Measures of flight path performance
        real(C_DOUBLE)     :: path_time              ! time to traverse the path
        real(C_DOUBLE)     :: path_distance          ! distance the vehicle traveled
        real(C_DOUBLE)     :: path_speed             ! time averaged speed for traversing the path
        real(C_DOUBLE)     :: longitudinal_error     ! signed error along the tangent line direction
        real(C_DOUBLE)     :: l1_error    = 0.d0     ! distance from tangent to flight path
        real(C_DOUBLE)     :: l_max_error = 0.d0     ! maximim distance from flight path (l1error) observed to date
        real(C_DOUBLE)     :: max_error_time         ! time at which maximum error occurred
        real(C_DOUBLE)     :: max_error_location (3) ! vehicle location at time of max error
        real(C_DOUBLE)     :: max_error_velocity (3) ! vehicle velocity at time of max error
        real(C_DOUBLE)     :: l1_error_int = 0.d0    ! distance integral along flight path of distance error
        real(C_DOUBLE)     :: ground_impact_speed = 0.d0 ! maximum speed at which the vehicle hit the ground, intentionally landing or otherwise
        real(C_DOUBLE)     :: path_score             ! numerical score given to performance, based on the stated requirements
c
c       These are historical and are not used by the current autopilot
        integer(C_INT)     :: iforward_thrust_right  ! a forward thrust on the right
        integer(C_INT)     :: iforward_thrust_left   ! a forward thrust on the left
        integer(C_INT)     :: iaileron               ! control channel for ailerons
        integer(C_INT)     :: iflap                  ! control channel for flaps
        integer(C_INT)     :: ielevator              ! control channel for horizontal elevator
        integer(C_INT)     :: irudder                ! control channel for vertical rudder
        integer(C_INT)     :: iquad_fr               ! control channel for vertical forward right thrust 
        integer(C_INT)     :: iquad_br               ! control channel for vertical back right thrust
        integer(C_INT)     :: iquad_bl               ! control channel for vertical back left thrust
        integer(C_INT)     :: iquad_fl               ! control channel for vertical forward left thrust
c
c                                                      fl   fr
c                                                                 up is out of the page (toward reader)   
c                                                      bl   br
c
c       These arrays are to facilitate information passage to a user-written autopilot subroutine
        integer(C_INT)     :: icontrol_array(100)    ! Array of integer(C_INT)s for control algorithms (user defined)
        real(C_DOUBLE)     :: xcontrol_array(100)    ! Array of reals for control algorithms (user defined)
c
c       New control inputs for the more complicated Hackathon 2 flight.  9/27/22  JDW
        logical(C_BOOL)    :: laccelerate = .false.  ! are we trying to accelerate
        logical(C_BOOL)    :: ldecelerate = .false.  ! are we trying to decelerate (both these are false for steady flight)
      end type controls_type
