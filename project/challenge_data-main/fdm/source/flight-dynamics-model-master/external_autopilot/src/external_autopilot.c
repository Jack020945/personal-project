/**
 * @file external_autopilot.c
 * @author Joel Allardyce (joel.allardyce@swri.org)
 * @brief Example external autopilot function for FDM written in C.
 * @version 0.1
 * @date 2022-07-27
 *
 * @copyright Copyright (c) Southwest Research Institute 2022
 *
 * This file contains an example of an external autopilot written in C.  The
 * primary intention is to show how parameters from FDM, which is written in
 * FORTRAN, are accessed from C.  The actual function is a stub that does
 * nothing except pass through the values into a copy of the FORTRAN autopilot
 * code, or at least a version of it that uses a C-compatible calling convention
 * and data structures.
 *
 * Optionally, this code can print out some values from the shared data
 * structures, including their sizes and locations in memory.  The intention
 * for this is simply to show how the data can be accessed, and also to prove
 * that this is working correctly.
 */

// Uncomment this line to print example data from shared structures out to
// stderr:
//#define PRINT_DATA
//
// After uncommenting the above, the values printed out to stderr can be
// redirected to a file using console stderr redirection.  In this way, they
// can be examined without affecting the expected output of the autopilot.

// Comment out this line to just pass back 0 values for the controls:
#define USE_FORTRAN_AUTOPILOT
// Obviously commenting out the above will not correctly fly the aircraft.

#ifdef PRINT_DATA
#include <stdio.h> // Needed for fprintf()
// Either of <inttypes.h> or <stdint.h> are used for the fixed-length C int type
// definitions. In this example, that's mostly realted to printing the addresses
// of the structures in memory.  Neither is necessary to work with the FDM
// types.
#include <inttypes.h> // Needed for fixed width integer types and fprintf()
                      // format specification macros.
//#include <stdint.h> // If format specification macros are not needed, this
                      // can be included instead of <inttypes.h>
#endif // PRINT_DATA

#include <fdm_types.h> // Shared FDM types
#include <fdm_utils.h> // Useful FDM type-related utilities
#include <external_autopilot.h> // Function prototype for the external autopilot
                                // that is being stubbed out below.

#ifdef USE_FORTRAN_AUTOPILOT
// For testing purposes, call the FORTRAN external autopilot that is basically
// a copy of the internal autopilot.
extern void fortranExternalAutopilot( double* time, int* xdim, double* x,
                                      double* xd, double* xo, int* udim,
                                      double* uc, aircraft_t* aircraft,
                                      wing_t* wing, propeller_t* propeller,
                                      battery_t* battery, controls_t* control,
                                      double* Xpath, double* Xpathdot,
                                      _Bool* lstraight, double* radius,
                                      int* ipath );

#endif // USE_FORTRAN_AUTOPILOT

/**
 * @brief An example stub implementation of an external autopilot.
 *
 * This stub autopilot just prints a lot of the values that are passed to it
 * from FDM, and then returns a control vector of all 0s.
 *
 * For details about the arguments passed in, see the function prototype
 * defined in <external_autopilot.h>.
 */
void externalAutopilot (
    double* time,
    int* xdim,
    double* x,                //   x[xdim]
    double* xd,               //  xd[xdim]
    double* xo,               //  xo[1500]
    int* udim,
    double* uc,               //  uc[udim] // output control values
    aircraft_t* aircraft,
    wing_t* wing,             //       wing[MAX_PARTS]
    propeller_t* propeller,   //  propeller[MAX_PARTS]
    battery_t* battery,       //    battery[MAX_PARS]
    controls_t* control,
    double* Xpath,            //    Xpath[3]
    double* Xpathdot,         // Xpathdot[3]
    _Bool* lstraight,
    double* radius,
    int* ipath
) {
    // This is just a stub example, to prove out that this works.
    int i;

#ifdef PRINT_DATA
    // String buffer used to hold converted FORTRAN strings.
    char str_buff[MAX_STR + 1];

    fprintf( stderr, "Reached 'externalAutopilot()'\n" );
    fprintf( stderr, "Values Received:\n" );
    fprintf( stderr, "  time: %f\n", *time );
    fprintf( stderr, "  xdim: %d\n", *xdim );
    fprintf( stderr, "  udim: %d\n", *udim );

    fprintf( stderr, "  X:\n" );
    for ( i = 0; i < *xdim; ++i ) {
        fprintf( stderr, "    x[%d]: %f\n", i, x[i] );
    }

    fprintf( stderr, "  Aircraft:\n" );
    fprintf( stderr, "    sizeof(aircraft_t):  %zu\n", sizeof(aircraft_t) );
    fprintf( stderr, "    aircraft addr: %" PRIu64 "\n", (uint64_t) aircraft );
    fprintf( stderr, "    aircraft->rho: %f\n", aircraft->rho );
    fprintf( stderr, "    aircraft->grav: %f\n", aircraft->grav );
    fprintf( stderr, "    aircraft->length_scale: %f\n", aircraft->length_scale );
    fprintf( stderr, "    aircraft->mass_scale: %f\n", aircraft->mass_scale );
    fprintf( stderr, "    aircraft->angle_scale: %f\n", aircraft->angle_scale );
    fprintf( stderr, "    cname (string printed 3 ways, with fields marked by {}):\n" );
    // Example of three ways to access FORTRAN strings.
    // Example 1:  If you only need to print the string, you can specify the
    //             MAX_STR as the field length.  This will print the complete
    //             FORTRAN string array, including a lot of trailing spaces.
    //             This requires almost no processing.
    fprintf( stderr, "      Using MAX_STR as field length:\n" );
    fprintf( stderr, "      {%.*s}\n", MAX_STR, aircraft->cname );
    // Example 2:  If you only need to print the string, but you don't want all
    //             of the trailing spaces, you can use the f_str_len() utility
    //             function to calculate the string length and then use that as
    //             the field length.  This will print the string without all of
    //             the trailing spaces, which of course presumes all of the
    //             trailing spaces are not desired.
    //             This requires a minimal amount processing.
    fprintf( stderr, "      Using f_str_len() to calculate the field length:\n" );
    fprintf( stderr, "      {%.*s}\n", f_str_len( aircraft->cname, MAX_STR ), aircraft->cname );
    // Example 3:  If you need the string to be a true C-style null-terminated
    //             string, you can use the f_to_c_str() utility function to make
    //             a copy of the FORTRAN string and append a null terminator.
    //             This requires making a copy of the string and a buffer of at
    //             least length MAX_STR+1 in size.
    fprintf( stderr, "      Using f_to_c_str() to create a C string:\n" );
    fprintf( stderr, "      {%s}\n", f_to_c_str( aircraft->cname, MAX_STR, str_buff ) );

    fprintf( stderr, "    ctype: %.*s\n", MAX_STR, aircraft->ctype );

    fprintf( stderr, "  Wings:\n" );
    fprintf( stderr, "    sizeof(wing_t):  %zu\n", sizeof(wing_t) );
    for ( i = 0; i < (( aircraft->num_wings == 0 ) ? 1 : aircraft->num_wings ) ; ++i )
    {
        fprintf( stderr, "    Wing %d of %d:\n", i+1, aircraft->num_wings );
        fprintf( stderr, "      wing[%d] addr: %" PRIu64 "\n", i, (uint64_t) &wing[i] );
        fprintf( stderr, "      wing[%d].delta1_max: %f\n", i, wing[i].delta1_max );
        fprintf( stderr, "      wing[%d].delta1_min: %f\n", i, wing[i].delta1_min );
        fprintf( stderr, "      wing[%d].delta2_max: %f\n", i, wing[i].delta2_max );
        fprintf( stderr, "      wing[%d].delta2_min: %f\n", i, wing[i].delta2_min );
        fprintf( stderr, "      wing[%d].cname: %.*s\n", i, MAX_STR, wing[i].cname );
        fprintf( stderr, "      wing[%d].ctype: %.*s\n", i, MAX_STR, wing[i].ctype );
    }

    fprintf( stderr, "  Propellers:\n" );
    fprintf( stderr, "    sizeof(propeller_t):  %zu\n", sizeof(propeller_t) );
    for ( i = 0; i < (( aircraft->num_propellers  == 0 ) ? 1 : aircraft->num_propellers ) ; ++i )
    {
        fprintf( stderr, "    Propeller %d of %d:\n", i+1, aircraft->num_propellers );
        fprintf( stderr, "      propeller[%d] addr: %" PRIu64 "\n", i, (uint64_t) &propeller[i] );
        fprintf( stderr, "      propeller[%d].Ct_scale: %f\n", i, propeller[i].Ct_scale );
        fprintf( stderr, "      propeller[%d].Cp_scale: %f\n", i, propeller[i].Cp_scale );
        fprintf( stderr, "      propeller[%d].tau_ESC: %f\n", i, propeller[i].tau_ESC );
        fprintf( stderr, "      propeller[%d].efficiency_ESC: %f\n", i, propeller[i].efficiency_ESC );
        fprintf( stderr, "      propeller[%d].cname: %.*s\n", i, MAX_STR, propeller[i].cname );
        fprintf( stderr, "      propeller[%d].ctype: %.*s\n", i, MAX_STR, propeller[i].ctype );
        fprintf( stderr, "      propeller[%d].mtype: %.*s\n", i, MAX_STR, propeller[i].mtype );
        fprintf( stderr, "      propeller[%d].motor_fname: %.*s\n", i, MAX_STR, propeller[i].motor_fname );
    }

    fprintf( stderr, "  Batteries:\n" );
    fprintf( stderr, "    sizeof(battery_t):  %zu\n", sizeof(battery_t) );
    for ( i = 0; i < (( aircraft->num_batteries  == 0 ) ? 1 : aircraft->num_batteries ) ; ++i )
    {
        fprintf( stderr, "    battery %d of %d:\n", i+1, aircraft->num_batteries );
        fprintf( stderr, "      battery[%d] addr: %" PRIu64 "\n", i, (uint64_t) &battery[i] );
        fprintf( stderr, "      battery[%d].voltage: %f\n", i, battery[i].voltage );
        fprintf( stderr, "      battery[%d].Capacity: %f\n", i, battery[i].Capacity );
        fprintf( stderr, "      battery[%d].C_Continuous: %f\n", i, battery[i].C_Continuous );
        fprintf( stderr, "      battery[%d].C_Peak: %f\n", i, battery[i].C_Peak );
        fprintf( stderr, "      battery[%d].cname: %.*s\n", i, MAX_STR, battery[i].cname );
        fprintf( stderr, "      battery[%d].ctype: %.*s\n", i, MAX_STR, battery[i].ctype );
        fprintf( stderr, "      battery[%d].fname: %.*s\n", i, MAX_STR, battery[i].fname );
    }

    fprintf( stderr, "  Control:\n" );
    fprintf( stderr, "    sizeof(controls_t):  %zu\n", sizeof(controls_t) );
    fprintf( stderr, "    control addr: %" PRIu64 "\n", (uint64_t) control );
    fprintf( stderr, "    Q_position: %f\n", control->Q_position );
    fprintf( stderr, "    Q_velocity: %f\n", control->Q_velocity );
    fprintf( stderr, "    Q_angular_velocity: %f\n", control->Q_angular_velocity );
    fprintf( stderr, "    Q_angles: %f\n", control->Q_angles );
    fprintf( stderr, "    R: %f\n", control->R );
    fprintf( stderr, "    external_autopilot: %d\n", control->external_autopilot );
#endif // PRINT_DATA

#ifdef USE_FORTRAN_AUTOPILOT

    // Rather than implement a controller in C, we're just calling a copy of
    // the FORTRAN external autopilot that has been modified, slightly, to work
    // with the C calling convention.

    fortranExternalAutopilot( time, xdim, x, xd, xo, udim, uc, aircraft, wing,
                              propeller, battery, control, Xpath, Xpathdot,
                              lstraight, radius, ipath );

#else // NOT USE_FORTRAN_AUTOPILOT

    // Example of returning control values:
    // Before returning, be sure to set te control variables.  In this example,
    // we're just setting them to all zeros:
    for ( i = 0; i < *udim; ++i ) {
        uc[i] = 0;
    }

#endif // USE_FORTRAN_AUTOPILOT

    return;
}