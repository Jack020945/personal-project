/**
 * @file fdm_utils.h
 * @author Joel Allardyce (joel.allardyce@swri.org)
 * @brief Function declarations for useful FDM utilities.
 * @version 0.1
 * @date 2022-07-27
 *
 * @copyright Copyright (c) Southwest Research Institute 2022
 *
 * This C/C++ header provides declarations for useful, mostly string-related,
 * utility functions for working with the FDM types.
 */

#ifndef FDM_UTILS_H
#define FDM_UTILS_H

#include <fdm_types.h>

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

    /**
     * @brief Utility function that returns the length of a FORTRAN string.
     *
     * This utility function presumes, by necessity, that all trailing spaces
     * are not part of the FORTRAN string.
     *
     * @param[in] f_str The FORTRAN-style string, which is basically a fixed-
     *                  length char array.  Internally, FORTRAN keeps track of
     *                  the intended length of such strings, and padds the end
     *                  with spaces.  C has to assume the length based on the
     *                  last non-space character.
     * @param[in] max_len The maximum length of the FORTRAN string or, at least,
     *                    the maximum number of characters you want in the
     *                    resulting C-style string.
     * @return (int) Index of the last non-space character.
     */
    int f_str_len (char* f_str, int max_len);

    /**
     * @brief Utility function to convert FORTRAN strings of known maximum
     *        length into C-style null-terminated strings.
     *
     * Like the f_str_len() string length function this utility function
     * presumes, by necessity, that all trailing spaces are not part of the
     * FORTRAN string.
     *
     * @param[in] f_str The FORTRAN-style string, which is basically a fixed-
     *                  length char array.  Internally, FORTRAN keeps track of
     *                  the intended length of such strings, and padds the end
     *                  with spaces.  C has to assume the length based on the
     *                  last non-space character.
     * @param[in] max_len The maximum length of the FORTRAN string or, at least,
     *                    the maximum number of characters you want in the
     *                    resulting C-style string.
     * @param[in, out] c_str A character buffer used to store the converted
     *                       string.  It MUST be of at least size len + 1, since
     *                       a null terminating character will be appended.
     * @return (char*) Pointer to the c_str buffer.
     */
    char* f_to_c_str (char* f_str, int max_len, char* c_str);

#ifdef __cplusplus
}
#endif // __cplusplus

#endif // FDM_UTILS_H