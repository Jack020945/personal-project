/**
 * @file fdm_types.h
 * @author Joel Allardyce (joel.allardyce@swri.org)
 * @brief Structured data types shared between the FORTRAN-based Flight Dynamics
 *        Model (FDM) code and the external autopilot.
 * @version 0.1
 * @date 2022-07-27
 *
 * @copyright Copyright (c) Southwest Research Institute 2022
 *
 * C/C++ header defining types and constants used to interact with FDM.
 */

#ifndef FDM_TYPES_H
#define FDM_TYPES_H

#include <stdbool.h>

/**
 * @brief Maximum number of each of the wing, propeller, and battery parts.
 *
 * This is the maximum number of wings, propellers and batteries a design is
 * allowed to have.  On startup FDM allocates enough space in memory for this
 * number of such components.
 *
 * For component details, see:
 * - wing_t
 * - propeller_t
 * - battery_t
 */
#define MAX_PARTS 50

/**
 * @brief Maximum length character array for strings.
 *
 * This is the maximum length of the strings in these structures.  FORTRAN uses
 * fixed length character arrays that are space filled to represent strings.  On
 * the FORTRAN side, the string lengths are kept track of by the compiler.
 */
#define MAX_STR 80

/**
 * @brief Structure representing the aircraft itself.
 */
typedef struct aircraft_t {
    char cname[MAX_STR];      ///< name of aircraft
    char ctype[MAX_STR];      ///< type of aircraft
    // Conversion factors
    double length_scale; // default: 0.001d0    ///< convert input values to meters.  Default is mm to meters, since CREO build is in millimeters and kilograms.
    double mass_scale;   // default: 1.d0       ///< convert input mass to kg.  Default is kg to kg.
    double angle_scale;  // default: deg_to_rad ///< convert input angles to radians.  Default is degree to radian (from pi.h).  Set to 1.d0 if you are inputting radians.
    //
    // Internal physics constants
    // These constants are not adjusted internally - they default to MKS and should be changed if you are trying
    // to run internally in different units (though you'll need to guess units for the electrical stuff or power be wrong, which affects motor controls).
    double grav;         // 9.80 (m/s^2)   ///< gravitational acceleration units
    double rho;          // 1.225 (kg/m^3) ///< Air density in default units
    //
    // Aircraft properties
    double mass;         ///< (kg)     total mass
    double x_cm;         ///< (m)      x of center of mass
    double y_cm;         ///< (m)      xy of center of mass
    double z_cm;         ///< (m)      xz of center of mass
    double Ixx;          ///< (kg m^2) xx of moment of inertia tensor I
    double Ixy;          ///< (kg m^2) xy of moment of inertia tensor I
    double Ixz;          ///< (kg m^2) xz of moment of inertia tensor I
    double Iyy;          ///< (kg m^2) yy of moment of inertia tensor I
    double Iyz;          ///< (kg m^2) yz of moment of inertia tensor I
    double Izz;          ///< (kg m^2) zz of moment of inertia tensor I
    // The inverse is computed by the software, it is not an input, so only dummy need appear in list read
    double InvIxx;       ///< xx of inverse of I
    double InvIxy;       ///< xy of inverse of I
    double InvIxz;       ///< xz of inverse of I
    double InvIyy;       ///< yy of inverse of I
    double InvIyz;       ///< yz of inverse of I
    double InvIzz;       ///< zz of inverse of I
    int num_wings;       ///< number of wing elements (wings, tails, etc.)  (input)
    int num_propellers;  ///< number of propellers (each assumed to have a motor) (input)
    int num_batteries;   ///< number of batteries (input)
    int num_controls;    ///< number of controls (should equal the sum of the next three) (computed internally)
    int num_wing_controls;   ///< number of wing controls (ailerons, flaps, etc.) (computed internally)
    int num_horizontal_motor_controls;  ///< number of horizontal motor controls (computed internally)
    int num_vertical_motor_controls;    ///< number of vertical motor controls (computed internally)
    // It should be the case that num_controls = num_wing_controls +
    // num_horizontal_motor_controls + num_vertical_motor_controls,
    // though we do not check because maybe something will change in the future.
    int num_ground_contacts; ///< number of ground contact points
    double X_fuseuu;     ///< (m^2) x-axis force coefficient for the fuselage (presented area) (default -12345. means internally guess value
    double Y_fusevv;     ///< (m^2) y-axis force coefficient for the fuselage (presented area)  based on motor locations and assumed motor
    double Z_fuseww;     ///< (m^2) z-axis force coefficient for the fuselage (presented area)  height of 7 cm)
    double x_fuse;       ///< (m)   effective center of fuselage, body x (center of pressure?) (default -12345. means 
    double y_fuse;       ///< (m)   effective center of fuselage, body y                        internally set ._fuse to ._cm)
    double z_fuse;       ///< (m)   effective center of fuselage, body z (used for body drag calculation)

    // We aso inialize various items (or else there are defaults)
    /**
     * Initial x values
     * default:
     * [      0.d0, 0.d0, 0.d0,    (m/s)   initial velocity in body fixed frame
     *        0.d0, 0.d0, 0.d0,    (deg/s) initial rotation vector (P Q R) in body fixed frame
     *  1.d0, 0.d0, 0.d0, 0.d0,    (-)     initial quaternion for orientation
     *        0.d0, 0.d0, 0.d0  ]  (m)     initial world frame displacement (all initial x)
     */
    double x_initial[13];
    double xd_initial[13];  ///< (various units) initial xd
    double uc_initial[10];  ///< (-)       initial uc (controls); dimensionless, 0 <= uc <= 1
    double time;            // default: 0.d0  (seconds) ///< initial time
    double dt;              // default: 0.001 (seconds) ///< default fixed time step (seconds)
    double dt_output;       // default: 1.d0  (seconds) ///< time between output lines
    double time_end;        // default: 1.d0  (seconds) ///< end time (seconds)
    double Unwind;          // default: 0.d0  (m/s)     ///< North wind speed in world frame
    double Vewind;          // default: 0.d0  (m/s)     ///< East wind speed in  world frame
    double Wdwind;          // default: 0.d0  (m/s)     ///< Down wind speed in world frame

    /** Within this type (since it is only one object) we store information about the computational request
     * Analysis Type:
     * 1. Fly from initial conditions
     * 2. Trim to U = x(1)
     * 3. Fly a flight path
     */
    int i_analysis_type;
    int debug;               // default: 0      ///< = 1 means verbose debug printouts from the fdm subroutine, etc.
    _Bool debug_mini;        // default: false  ///< prints fderiv debugs for the trim states only; overrides debug.
    /**
     * Trim solver:
     * 1 means to only use SIMPLEX for trim solver (rather than MINPACK)
     * 2 means common start value for simplex
     * default: 2
     */
    int simplex;
    double acceleration_tolerance; //(default: 0.01d0 ///< the maximum acceptable amount of acceleration for a trim state (m/s^2, rad/s^2)
    double tolerance;        // default: 1.d-10 ///< tolerance for MINPACK
    double weight[3];        // default: [1.d0, 1.d0, 1.d-4] ///< weightings for optimizations
    _Bool motordetails;      // default: false  ///< if true, provides a table of propeller performance vs. control variable
    int i_out_metrics;       // default: 80     ///< default file number for metrics write out (shouldn't be changed)
    int i_out_score;         // default: 79     ///< default file number for score write out (shouldn't be changed)
    _Bool ignore_electrical; // default: false  ///< this allows the vehicle to fly even with electrical problems, for debugging
    _Bool ignore_wingload;   // default: false  ///< this allows the vehicle trim state even if wing loads are exceeded
    double I_peak_ratio;     // default: 1.5d0  ///< a default I_peak/I_max for the motors
    _Bool current_limiter;   // default: true   ///< limits the current to I_peak
    _Bool temp_debug;        // default: false  ///< a flag for assisting in debugging
} aircraft_t;

/**
 * @brief Structure representing the aircraft wing-like elements.
 */
typedef struct wing_t {
    char cname[MAX_STR];      ///< name or description of wing, typically half main wing,
    // horizontal tail, or vertical tail.  May be needed/used in the context
    // of propwash, downwash calculations, etc. 
    char ctype[MAX_STR];      ///< Airfoil section name, for example, if a table lookup is needed.
    // Information about wing or half wing
    double surface_area; ///< (m^2) area of wing segement (i.e., half for the wing broken in half)
    // We assume that "forward" is in the x direction.  Given the x-direction being 
    // in the plane of the wing section, now the question is what is the normal 
    // direction to the wing in the other two directions, so (ny,nz) = (0,1) is for 
    // the horizontal wing or tail (pointing down) and (ny,nz) = (1,0) is the normal
    // pointing right for the vertical tail.  (This may require some clarification.)
    double span;         ///< (m)   wing section span
    double chord;        ///< (m)   wing chord (averge in an area sense?)
    // nx, ny, nz is the unit normal to the wing in the world frame (normalized internally
    // if not entered that way).  We expect nz < 0 for the top pointing "up".
    // Wings can only twist around dy (angle of incidence) or dx (stabilizer).
    // Either dx or dy must be zero.  This is enforced in the code.  Internally, things are changed.
    // No swept wings (i.e., nz cannot be zero).
    double max_load;     // default:  0.d0     ///< (N)   this is the specified maximum load of the wing section (to see if they are strong enough).
    double nx;           // default:  0.d0     ///< (-)   x unit normal   default is wing with no angle of incidence, flat in the XY plane
    double ny;           // default:  0.d0     ///< (-)   y unit normal 
    double nz;           // default: -1.d0     ///< (-)   z unit normal
    double angle_of_incidence; // default: 0.d0  ///< this is computed internally if ny = 0 and nx is nonzero (= asin(nx)).
    double i_wing_type;  ///< (-)   1 = right wing half
                         //       2 = left wing half
                         //       3 = horizontal stabilizer
                         //       4 = vertical stabilizer (or V tail sections)
    double a;            ///< (1/deg) slope of lift curve 
    double C_L0;         ///< (-) zero angle of attack lift coefficient (= - AoAatL0 * a)
    double C_Lmax;       ///< (-) maximum lift coefficient 
    double C_Lmin;       ///< (-) minimum lift coefficient (= - C_Lmax)
    double C_D0;         ///< (-) zero lift drag coefficient
    double k;            ///< (-) induced drag coefficient
    double C_Dfp;        // default: 1.d0 ///< (-) drag coefficient at alpha = 90 deg
    double AoAatC_Lmax;  ///< (deg) Angle of attack at maximum C_L
    double AoAatL0;      ///< (deg) Angle of attack for zero lift from wing
    double C_M0;         ///< (-) moment coefficient at zero lift
    double delta1_max;   // default:   25.d0 (deg) ///< max aileron deflection
    double delta1_min;   // default: - 25.d0 (deg) ///< min aileron deflection
    double delta2_max;   // default:   25.d0 (deg) ///< max flap deflection
    double delta2_min;   // default: - 25.d0 (deg) ///< min flap deflection
    int icontrol1;       ///< control channel for aileron
    int icontrol2;       ///< control channel for flap
    double bias1;        ///< bias for aileron control
    double bias2;        ///< bias for flap control
    _Bool ic1;           // default: false ///< this flag can be set externally but is set internally
    _Bool ic2;           // default: false ///< ibid.  This is true if the control channel is doing something,
    // For example, if icontrol1 is set to 0 then ic1 = false,
    // saying that the channel is not active.
    // The idea here is that abs(bias1)+abs(bias2)=1., to 
    // represent the whole wing.  The sign of the bias
    // is intended to be negative for an aileron on the
    // second half of the wing.  Thus, for wing(1)
    // icontrol1=1 (say), icontrol2=1, bias1 = 1.
    // bias2 = 0., and on wing(2) icontrol1=icontrol2=1
    // bias1=-1. and bias2=0., as examples.
    // The idea here is that a wing can have both flaps, which
    // have the same magnitude and same sign bias, and ailerons,
    // which have same magnitude and opposite sign bias.
    // However, you can adjust the bias to reflect the flaps and aileron
    // having different max deflections on a wing.
    double ac1;          ///< slope of control curve 1, assumed linear (computed internally)
    double bc1;          ///< intercept of control curve 1, assumed linear (computed internally)
    double ac2;          ///< slope of control curve 2, assumed linear (computed internally)
    double bc2;          ///< intercept of control curve 2, assumed linear (computed internally)
    double tau_a;        // default: 0.4d0 ///< flap effectiveness of ailerons
    double eta;          ///< effect of prop wash on wing
    double eta_w;        ///< effect of wing downwash on tail
    double x;            ///< effective center x 
    double y;            ///< effective center y
    double z;            ///< effective center z
} wing_t;

/**
 * @brief Structure representing the aircraft's propellers.
 */
typedef struct propeller_t {
    // It is assumed that each propeller comes with a motor, so this is where electric
    // motor information is also stored.
    char cname[MAX_STR];         ///< name of propller
    char ctype[MAX_STR];         ///< type of propller - used, for example, if a thrust table is looked up
    // Part of the propeller data table:
    char prop_fname[MAX_STR];    ///< data file name for propeller
    // Part of the motor data table:
    char mtype[MAX_STR];         ///< type of motor
    char motor_fname[MAX_STR];   ///< data file name for motor
    // Propeller placement and properties
    double x;           ///< (m)  x center of propeller
    double y;           ///< (m)  y center of propeller
    double z;           ///< (m)  z center of propeller
    double nx;          ///< (-)  x normal to propeller, pointing in the direction of thrust.
    double ny;          ///< (-)  y normal to propeller, pointing in the direction of thrust.
    double nz;          ///< (-)  z normal to propeller, pointing in the direction of thrust.
    double spin;        /**< (-)  direction of spin; +1. for counterclockwise from above,
                                                     -1. for clockwise from above. */
    // Notice that for puller props (the typical case), the motor is in the opposite 
    // direction to vec(n) (i.e., it is downstream of the propeller), while for 
    // pusher props vec(n) points in the direction of the motor (the motor is
    // upstream of the propeller).
    double radius;      ///< (m)  radius of the propeller
    // The propeller moment of inertia means the spinning part of the assembly.  All we want is the
    // moment of inertia around the shaft of rotation, and we assume off-axis terms are negligible.
    // It includes the propeller, the shaft, and the motor rotor.  It is included for gyroscopic effects.
    double Ir;          ///< (kg m^2)  radial moment of inertia component.
    // This is table of data for the propeller.  Right now it is just three columns (it is stored here, but read internally)
    int prop_num_pts_omega;  ///< number of RPM (omega) points in the propeller performance table (computed internally)
    int prop_num_pts_J;      ///< number of J points in the propeller performance table (computed internally)
    double omega[100];   ///< Array of omega (rotational speeds) for the tables
    double J[100];       ///< J = V_flight_normal/nD where n = revolutions per sec and D = propeller diameter
    double Ct[100][100]; ///< Thrust coefficient Ct = thrust/(air density * n^2 * D^4)
    double Cp[100][100]; ///< Power coefficient  Cp = power /(air density * n^3 * D^5)
    // double* omega; // dim: [100] // Array of omega (rotational speeds) for the tables
    // double* J;     // dim: [100] // J = V_flight_normal/nD where n = revolutions per sec and D = propeller diameter
    // double* Ct;    // dim: [100][100] // Thrust coefficient Ct = thrust/(air density * n^2 * D^4)
    // double* Cp;    // dim: [100][100] // Power coefficient  Cp = power /(air density * n^3 * D^5)
    // There are various constants due to concern about the data table goodness; it's here because it may be propeller specific
    double Ct_scale; // default: 0.9d0   ///< Michael's multiplier for Ct, based on concern about data table goodness. 
    double Cp_scale; // default: 1.1d0   ///< Michael's multiplier for Cp, based on concern about data table goodness. 
    double P_factor;           ///< for pitch up due to prop torque/moment
    // Note power = torque * omega = torque * 2 pi * n, (torque = Q) 
    // Motor definition - we assume there is only one direct-drive motor per propeller
    int icontrol;              ///< the controller for the motor
    double ac;                 ///< slope of control curve 1, assumed linear (computed internally)
    double bc;                 ///< intercept of control curve 1, assumed linear (computed internally)
    // The following (KV through Rw) are read from the motor file, or they can be entered directly
    // The units are those typically published - i.e., that may not be consistent with MKS, but are adjusted to MKS internally
    double KV;          ///< (RPM/volts) motor velocity constant => RPM = KV * volts (RPM/Volts)
    double KT;          ///< (Newton-meter/amps) motor torque constant
    double I_max;       ///< (amps)      maximum current - you should only need one or the other (amps)
    double I_peak;      // default: 0.d0 (amps)  ///< transient peak current (amps)
    double I_idle;      ///< (amps)      idle current (amps)
    double maxpower;    ///< (watts)     maximum power (watts)
    double Rw;          ///< (ohms)      winding resistance (ohms)
    double torque_max;  ///< (Newton-meter)      the maximum torque the motor can supply (Newton-meter)
    //
    // The following geometric information about the motor is needed by JSBSim
    double motor_x;     ///< (m)  x center of the motor
    double motor_y;     ///< (m)  y center of the motor
    double motor_z;     ///< (m)  z center of the motor
    double motor_nx;    ///< (-)  direction of the motor center shaft 
    double motor_ny;    ///< (-)  direction of the motor center shaft 
    double motor_nz;    ///< (-)  direction of the motor center shaft 
    // 
    double tau_ESC;        // default: 0.05d0 (seconds) ///< ESC lag time (seconds)
    double efficiency_ESC; // default: 0.95d0 (-)       ///< assumed energy efficiency of ESC
    // 
    int ibattery;    ///< the battery number that this motor is connected to
} propeller_t;

/**
 * @brief Structure representing the aircraft's batteries.
 */
typedef struct battery_t {
    char cname[MAX_STR];         ///< name of battery
    char ctype[MAX_STR];         ///< type of battery - used, for example, if a table must be looked up
    char fname[MAX_STR];         ///< file name of battery
    // The following values (num_cells through Rm) are read in from the file, or they can be entered directly
    int num_cells;      ///< (-) number of cells (not used at present)
    double voltage;     ///< (volts)  battery voltage
    double Capacity;    ///< (mAh = milli amp hours) Total battery charge (1 milli amp hour = 3.6 coulombs)
    double C_Peak;      ///< Max amp is (C / 1000) * Capacity in mAh (/1000 to get to Amps)
    // A battery of  1 C and 1000 mAh has a max discharge rate of  1 amp
    // A battery of 75 C and 1000 mAh has a max discharge rate of 75 amps
    // A battery of 30 C and 3000 mAh has a max discharge rate of 90 amps
    double C_Continuous;         ///< In regular use, max amp is (C / 1000) * capacity in mAh
    double Rm;                   ///< (ohms)    battery resistance (not used at present)
    double seconds_at_peak;      ///< (seconds) continuous use exceeding peak discharge
    double seconds_at_continous; ///< (seconds) seconds exceeding continuous discharge
    double remaining_charge;     ///< (mAh)     remaining charge on this battery, in milli amp hours
    double percent;              ///< (%)       percent charge remaining (equals ratio of previous and Capacity) (internal)
} battery_t;

/**
 * @brief Structure representing the controls.
 */
typedef struct controls_t {
    // Controls are assumed to be 0 <= u <= 1 in all cases, and they are linear.
    // I think in this we are going to define our relationships, such as would be by
    // connecting cables.
    // The following are to use in the controller algorithm as primary or fallbacks.
    _Bool outside_trim_states; // default: false ///< if true, fdm does not compute trim state, user provides below
    // If you provide your own, you must set _trim_if_0 to true,
    // you must set _trim_uc and _trim_theta,  and _trim_speed to
    // float(j) (or it probably won't work). minpack_trim.f does the rest
    _Bool strong_fdcost; // default: false      ///< if true, this forces all requirements into fdcost, 
    // otherwise fdcost only has uc domain requirements.
    int lateral_num_trim_min;          // default:  0    ///< minimum index to trim states in the array (>=0)
    int lateral_num_trim_max;          // default: 50    ///< maximum index to trim states in the array (<=50)
    _Bool lateral_trim_if_0[51];       // default: false ///< true if we have a UVWPQRdot trim state - may not be a trim state due to other warnings
    // Issue first here
    _Bool lateral_trim_if[51];         // default: false ///< true if we have a trim state with a control law
    double lateral_trim_speed[51];     ///< lateral speed for the trim state
    double lateral_trim_x[75][51];     ///< x state of trim state (with qskip)
    double lateral_trim_uc[50][51];    ///< the lateral trim control settings
    double lateral_trim_theta[51];     ///< the lateral trim pitch angles
    int lateral_trim_qskip[51];        // default: 0 ///< the value of qskip for the trim state control law
    double lateral_trim_K[12][50][51]; ///< the control law for the trim states
    double lateral_trim_fdcost[51];    ///< cost as used by simplex
    double lateral_trim_max_xd[51];    ///< unweighted max xd(1:6)
    // 
    // The following are to use in the controller algorithm as primary or fallbacks.
    int vertical_num_trim_min;         ///< minimum index to trim states in the array (>=0)
    int vertical_num_trim_max;         ///< maximum index to trim states in the array (<=50)
    _Bool vertical_trim_if_0[51];      // default: false ///< true if we have a trim state (see lateral description)
    _Bool vertical_trim_if[51];        // default: false ///< true if we have a trim state control law
    double vertical_trim_speed[51];    ///< vertical speed for the trim state
    double vertical_trim_x[75][51];    ///< x state of trim state (with qskip)
    double vertical_trim_uc[50][51];   ///< the lateral trim control settings
    double vertical_trim_theta[51];    ///< the lateral trim pitch angles
    double vertical_trim_fdcost[51];   ///< cost as used by simplex
    double vertical_trim_max_xd[51];   ///< unweighted max xd(1:6)
    // 
    // These are to compute trim states for turns
    double radius_1;                   // default: 500.d0 ///< turn radius 1
    int turn_1_num_trim_min;           // default:  0      ///< minimum index to trim states in the array (>=0)
    int turn_1_num_trim_max;           // default: 50      ///< maximum index to trim states in the array (<=50)
    _Bool turn_1_trim_if_0[51];        // default: false   ///< true if we have a trim state (see lateral description)
    _Bool turn_1_trim_if[51];          // default: false   ///< true if we have a trim state control law
    double turn_1_trim_speed[51];      ///< tangential speed for the trim state
    double turn_1_trim_x[75][51];      ///< x state of trim state (with qskip)
    double turn_1_trim_uc[50][51];     ///< the turn trim control settings
    double turn_1_trim_theta[51];      ///< the turn trim pitch angles
    double turn_1_trim_phi[51];        ///< the turn trim roll angles
    int turn_1_trim_qskip[51];         // default: 0   ///< the value of qskip for the trim state control law
    double turn_1_trim_K[12][50][51];  ///< the control law for the trim states
    double turn_1_trim_fdcost[51];     ///< cost as used by simplex
    double turn_1_trim_max_xd[51];     ///< unweighted max xd(1:6)
    double radius_2;                   // default: -300.d0 ///< turn radius 2
    int turn_2_num_trim_min;           // default:  0      ///< minimum index to trim states in the array (>=0)
    int turn_2_num_trim_max;           // default: 50      ///< maximum index to trim states in the array (<=50)
    _Bool turn_2_trim_if_0[51];        // default: false   ///< true if we have a trim state (see lateral description)
    _Bool turn_2_trim_if[51];          // default: false   ///< true if we have a trim state control law
    double turn_2_trim_speed[51];      ///< tangential speed for the trim state
    double turn_2_trim_x[75][51];      ///< x state of trim state (with qskip)
    double turn_2_trim_uc[50][51];     ///< the turn trim control settings
    double turn_2_trim_theta[51];      ///< the turn trim pitch angles
    double turn_2_trim_phi[51];        ///< the turn trim roll angles
    int turn_2_trim_qskip[51];         // default: 0   ///< the value of qskip for the trim state control law
    double turn_2_trim_K[12][50][51];  ///< the control law for the trim states
    double turn_2_trim_fdcost[51];     ///< cost as used by simplex
    double turn_2_trim_max_xd[51];     ///< unweighted max xd(1:6)
    // 
    // These values are the requested behavior for following a path.
    int i_flight_path;                ///< which flight path are we flying (need aircraft%i_analysis_type = 3 to do this)
    int i_Hackathon;                  // default: 1 ///< the hackathon number (=0 is the regular runs)
    double requested_lateral_speed;   ///< requested lateral speed for following horizontal parts of the path
    double requested_vertical_speed;  ///< requested vertical speed for following vertical parts of the path (if any)
    double timeslip;                  ///< in case there is desired (or internally computed) slippage
    int start_trim_state;             // default: -1 ///< if the desired beginning is a trim state (tries to compute if -1, otherwise is no)
    // 
    // These values provide the weights for the LQR solver in determining weightings for the 
    // controls.
    double Q_position;         // default: 1.d0   ///< weight for position (XYZ) in Q matrix
    double Q_velocity;         // default: 1.d0   ///< weight for velocity (UVW) in Q matrix
    double Q_angular_velocity; // default: 0.d0   ///< weight for angular rates (PQR) in Q matrix
    double Q_angles;           // default: 1.d0   ///< weight for quaternions in Q matrix
    double R;                  // default: 1.d0   ///< weight for R (times B) matrix

    ///< External autopilot enabling
    int external_autopilot; ///< default: 0
    //
    // These are some internal variables for the controller algorithm.
    _Bool outside_control_states; // default: false ///< if true, fdm does not compute controls, user provides below
    _Bool compute_A;        // default: false ///< instructs fderiv to update Alin, Blin, Glin
    int qskip;              // default: 0       ///< which quaternion to leave out of control LQR
    double Alin[12][12];    ///< the linearized FDM for A   ( xdot = A x + B u + G )
    double Blin[50][12];    ///< the linearlzed FDM for B
    double Glin[12];        ///< the linearized FDM for G
    double K[12][50];       ///< the control law   u = u_trim + K(x - xtilde)
    double tblend;          // default: 0.d0    ///< blend time for controls transitions
    //
    // Measures of flight path performance
    double path_time;              ///< time to traverse the path
    double path_distance;          ///< distance the vehicle traveled
    double path_speed;             ///< time averaged speed for traversing the path
    double longitudinal_error;     ///< signed error along the tangent line direction
    double l1_error;               // default: 0.d0 ///< distance from tangent to flight path
    double l_max_error;            // default: 0.d0 ///< maximim distance from flight path (l1error) observed to date
    double max_error_time;         ///< time at which maximum error occurred
    double max_error_location[3];  ///< vehicle location at time of max error
    double max_error_velocity[3];  ///< vehicle velocity at time of max error
    double l1_error_int;           // default: 0.d0 ///< distance integral along flight path of distance error
    double ground_impact_speed;    // default: 0.d0 ///< maximum speed at which the vehicle hit the ground, intentionally landing or otherwise
    double path_score;             ///< numerical score given to performance, based on the stated requirements
    // 
    // These are historical and are not used by the current autopilot
    int iforward_thrust_right;  ///< a forward thrust on the right
    int iforward_thrust_left;   ///< a forward thrust on the left
    int iaileron;               ///< control channel for ailerons
    int iflap;                  ///< control channel for flaps
    int ielevator;              ///< control channel for horizontal elevator
    int irudder;                ///< control channel for vertical rudder
    int iquad_fr;               ///< control channel for vertical forward right thrust 
    int iquad_br;               ///< control channel for vertical back right thrust
    int iquad_bl;               ///< control channel for vertical back left thrust
    int iquad_fl;               ///< control channel for vertical forward left thrust
    //
    // fl   fr
    // up is out of the page (toward reader)
    // bl   br
    //
    // These arrays are to facilitate information passage to a user-written autopilot subroutine
    int icontrol_array[100];    ///< Array of ints for control algorithms (user defined)
    double xcontrol_array[100]; ///< Array of reals for control algorithms (user defined)
} controls_t;

/**
 * @brief Structure defining the ground contact point.
 *
 * @note Defined in FORTRAN, but not currently passed into autopilot procedure.
 */
typedef struct ground_contact_t {
    double x;   ///< x of ground contact point
    double y;   ///< y of ground contact point
    double z;   ///< z of ground contact point
} ground_contact_t;

#endif // FDM_TYPES_H