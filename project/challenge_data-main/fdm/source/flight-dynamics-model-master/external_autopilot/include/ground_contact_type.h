c   This is the include "header" for the shared ground contact type.  The entire
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
      type, bind(C) :: ground_contact_type
        real(C_DOUBLE)     :: x                      ! x of ground contact point
        real(C_DOUBLE)     :: y
        real(C_DOUBLE)     :: z
      end type ground_contact_type