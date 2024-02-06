/**
 * @file external_autopilot.h
 * @author Joel Allardyce (joel.allardyce@swri.org)
 * @brief Function prototype for the external autopilot.
 * @version 0.1
 * @date 2022-07-27
 *
 * @copyright Copyright (c) Southwest Research Institute 2022
 *
 * C/C++ header file providing the function prototype for the external
 * autopilot function.
 */

#ifndef EXTERNAL_AUTOPILOT_H
#define EXTERNAL_AUTOPILOT_H

#include <fdm_types.h>

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

    /**
     * @brief Function prototype for the external autopilot.
     *
     * In for FDM to call a custom version of this function, this function
     * prototype much be implemented exactly as specified.
     *
     * @param[in] time The current simulation time, in seconds. Can be
     *                 fractional.
     * @param[in] xdim The state space dimension (though we only use 12).
     * @param[in] x The current state.
     *              Shape:  x[xdim]
     * @param[in] xd Currently unused.
     *               Shape:  xd[xdim]
     * @param[in] xo Currently unused.
     *               Shape:  xo[1500]
     * @param[in] udim The number of controls.
     * @param[out] uc The control settings, which is the main output.
     * @param[in] aircraft Pointer to the aircraft_t stucture representing the
     *                     aircraft.
     * @param[in] wing An array of wing_t structures describing the wing-like
     *                 parts of the aircraft.
     *                 Shape:  wing[MAX_PARTS]
     *                 Number of wings:  aircraft->num_wings
     * @param[in] propeller An array of propeller_t structures describing the
     *                      aircraft's propellers.
     *                      Shape:  propeller[MAX_PARTS]
     *                      Number of propellers:  aircraft->num_propellers
     * @param[in] battery An array of battery_t structures describing the
     *                    aircraft's batteries.
     *                    Shape:  battery[MAX_PARTS]
     *                    Number of propellers:  aircraft->num_batteries
     * @param[in] control Pointer to the control_t structure which describes the
     *                    aircraft's control state.  The control (K) is included
     *                    in this structure.
     * @param[in] Xpath The X, Y, Z of the in-world frame of the requested
     *                  flight path.
     *                  Shape:  Xpath[3]
     * @param[in] Xpathdot The U, V, W of the in-world frame of the requested
     *                     flight path.
     *                     Shape:  Xpathdot[3]
     * @param[in] lstraight True if flight path is straight.
     * @param[in] radius The turning radius
     * @param[in] ipath The path - used for some temporary ifs
     */
    void externalAutopilot (
        double*      time,
        int*         xdim,
        double*      x,           ///<   x[xdim]
        double*      xd,          ///<  xd[xdim]
        double*      xo,          ///<  xo[1500]
        int*         udim,
        double*      uc,          ///<  uc[udim] // output control values
        aircraft_t*  aircraft,
        wing_t*      wing,        ///<       wing[MAX_PARTS]
        propeller_t* propeller,   ///<  propeller[MAX_PARTS]
        battery_t*   battery,     ///<    battery[MAX_PARTS]
        controls_t*  control,
        double*      Xpath,       ///<    Xpath[3]
        double*      Xpathdot,    ///< Xpathdot[3]
        _Bool*       lstraight,
        double*      radius,
        int*         ipath
    );
#ifdef __cplusplus
}
#endif // __cplusplus

#endif // EXTERNAL_AUTOPILOT_H