/**
 * @file fdm_utils.c
 * @author Joel Allardyce (joel.allardyce@swri.org)
 * @brief Useful FDM utilities.
 * @version 0.1
 * @date 2022-07-27
 *
 * @copyright Copyright (c) Southwest Research Institute 2022
 *
 * Implementations of useful, mostly string-related, utility functions for
 * working with the FDM types.
 */

#include <stdio.h>
#include <fdm_utils.h>

int f_str_len (char* f_str, int max_len) {
    int i;
    // Find the length of the FORTRAN string
    // First, if the string is uninitialized in FORTRAN it's set to all NULLs,
    // so let's check for that and skip the loop:
    if ( f_str[0] == '\0' ) {
        return 0;
    }

    // NOTE:  For our purposes, FORTRAN strings are character arrays of a know
    //        max length that are space-padded.  Note that:  If they are
    //        entirely uninitialized, then they end up being filled with NULLs,
    //        but we've checked for that already.
    // Find the index of the last NON-space value
    for ( i = max_len; i > 0; --i ) {
        // Start from the end of the array, looking for non-space and non-null
        // values.
        if ( f_str[i - 1] != ' ' ) {
            // Found a non-space value at index i-1 so string is of length i.
            break;
        }
    }
    // Either we found a non-space character, or we got to 0.
    // Note:  Due to how for loops work in C, `i` will still be decremented to 0
    //        before the test `i > 0` is run the last time, and so this will
    //        correctly return a length of 0 for an all space string.
    return i;
}

char* f_to_c_str (char* f_str, int max_len, char* c_str) {
    int i;
    int str_len;
    // Find the length of the string
    str_len = f_str_len(f_str, max_len);

    // Copy string to buffer
    for (i = 0; i < str_len; ++i) {
        c_str[i] = f_str[i];
    }

    // Add null terminator.  As noted elsewhere, this implies that the `c_str`
    // buffer must be at least `max_len + 1` characters in size or the worst-
    // case scenario of a `max_len` length string will cause a silent error and
    // overwrite the next byte in memory.
    c_str[str_len] = '\0';

    // Also return the address of the buffer, so this can be used inline in, for
    // example, printf() along with a "%s" format specifier.
    return c_str;
}