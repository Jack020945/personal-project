c   This is the include "header" for the common propeller type fields
c   It is intended to be similar to a .h file.
c   The intent is to include this entire file when defining both the Fortran
c   C compatible versions of this type.
c   Joel Allardyce 13 July 2022    Southwest Research Institute
c
c   7/13/22 Initial coding
c
c       Propeller placement and properties
        real(C_DOUBLE)     :: x           ! (m)  x center of propeller
        real(C_DOUBLE)     :: y           ! (m)  y center of propeller
        real(C_DOUBLE)     :: z           ! (m)  z center of propeller
        real(C_DOUBLE)     :: nx          ! (-)  x normal to propeller, pointing in the direction of thrust.
        real(C_DOUBLE)     :: ny          ! (-)  y normal to propeller, pointing in the direction of thrust.
        real(C_DOUBLE)     :: nz          ! (-)  z normal to propeller, pointing in the direction of thrust.
        real(C_DOUBLE)     :: spin        ! (-)  direction of spin; +1. for counterclockwise from above,
c                                                                   -1. for clockwise from above.
c                                Notice that for puller props (the typical case), the motor is in the opposite 
c                                direction to vec(n) (i.e., it is downstream of the propeller), while for 
c                                pusher props vec(n) points in the direction of the motor (the motor is
c                                upstream of the propeller).
        real(C_DOUBLE)     :: radius      ! (m)  radius of the propeller
c       The propeller moment of inertia means the spinning part of the assembly.  All we want is the
c       moment of inertia around the shaft of rotation, and we assume off-axis terms are negligible.
c       It includes the propeller, the shaft, and the motor rotor.  It is included for gyroscopic effects.
        real(C_DOUBLE)     :: Ir          ! (kg m^2)  radial moment of inertia component.
c       This is table of data for the propeller.  Right now it is just three columns (it is stored here, but read internally)
        integer(C_INT)     :: prop_num_pts_omega  ! number of RPM (omega) points in the propeller performance table (computed internally)
        integer(C_INT)     :: prop_num_pts_J      ! number of J points in the propeller performance table (computed internally)
        real(C_DOUBLE)     :: omega (100) ! Array of omega (rotational speeds) for the tables
        real(C_DOUBLE)     :: J     (100) ! J = V_flight_normal/nD where n = revolutions per sec and D = propeller diameter
        real(C_DOUBLE)     :: Ct(100,100) ! Thrust coefficient Ct = thrust/(air density * n^2 * D^4)
        real(C_DOUBLE)     :: Cp(100,100) ! Power coefficient  Cp = power /(air density * n^3 * D^5)
c       real(C_DOUBLE), pointer :: omega (:) ! Array of omega (rotational speeds) for the tables
c       real(C_DOUBLE), pointer :: J     (:) ! J = V_flight_normal/nD where n = revolutions per sec and D = propeller diameter
c       real(C_DOUBLE), pointer :: Ct  (:,:) ! Thrust coefficient Ct = thrust/(air density * n^2 * D^4)
c       real(C_DOUBLE), pointer :: Cp  (:,:) ! Power coefficient  Cp = power /(air density * n^3 * D^5)
c       There are various constants due to concern about the data table goodness; it's here because it may be propeller specific
        real(C_DOUBLE)     :: Ct_scale = 0.9d0   ! Michael's multiplier for Ct, based on concern about data table goodness. 
        real(C_DOUBLE)     :: Cp_scale = 1.1d0   ! Michael's multiplier for Cp, based on concern about data table goodness. 
        real(C_DOUBLE)     :: P_factor           ! for pitch up due to prop torque/moment
c                                                  Note power = torque * omega = torque * 2 pi * n, (torque = Q) 
c       Motor definition - we assume there is only one direct-drive motor per propeller
        integer(C_INT)     :: icontrol           ! the controller for the motor
        real(C_DOUBLE)     :: ac                 ! slope of control curve 1, assumed linear (computed internally)
        real(C_DOUBLE)     :: bc                 ! intercept of control curve 1, assumed linear (computed internally)
c       The following (KV through Rw) are read from the motor file, or they can be entered directly
c       The units are those typically published - i.e., that may not be consistent with MKS, but are adjusted to MKS internally
        real(C_DOUBLE)     :: KV          ! (RPM/volts) motor velocity constant => RPM = KV * volts (RPM/Volts)
        real(C_DOUBLE)     :: KT          ! (Newton-meter/amps) motor torque constant
        real(C_DOUBLE)     :: I_max       ! (amps)      maximum current - you should only need one or the other (amps)
        real(C_DOUBLE)     :: I_peak = 0.d0 ! (amps)    transient peak current (amps)
        real(C_DOUBLE)     :: I_idle      ! (amps)      idle current (amps)
        real(C_DOUBLE)     :: maxpower    ! (watts)     maximum power (watts)
        real(C_DOUBLE)     :: Rw          ! (ohms)      winding resistance (ohms)
        real(C_DOUBLE)     :: torque_max  ! (Newton-meter)      the maximum torque the motor can supply (Newton-meter)
c
c       The following geometric information about the motor is needed by JSBSim
        real(C_DOUBLE)     :: motor_x     ! (m)  x center of the motor
        real(C_DOUBLE)     :: motor_y     ! (m)  y center of the motor
        real(C_DOUBLE)     :: motor_z     ! (m)  z center of the motor
        real(C_DOUBLE)     :: motor_nx    ! (-)  direction of the motor center shaft 
        real(C_DOUBLE)     :: motor_ny    ! (-)  direction of the motor center shaft 
        real(C_DOUBLE)     :: motor_nz    ! (-)  direction of the motor center shaft 
c
        real(C_DOUBLE)     :: tau_ESC = 0.05d0     ! (seconds)  ESC lag time (seconds)
        real(C_DOUBLE)     :: efficiency_ESC = 0.95d0  ! (-)        assumed energy efficiency of ESC
c
        integer(C_INT)     :: ibattery    ! the battery number that this motor is connected to