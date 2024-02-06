c   This is the include "header" for the aircraft parts.
c   It is intended to be similar to a .h file.
c   The specific instances are called out, at the bottom, as the intent is to include
c   this entire file each time and pass by pointer the various instances to each of
c   the subroutines.
c   James Walker  20 March 2021    Southwest Research Institute
c   3/20/21
c
c   3/20/21 Initial coding
c
c   4/7/21  The defaults here indicate a change in internal coding from cgs to MKS.
c           Before it was assumed radians were the angle measure; now the assumption is degrees.
c           The input values from CREo are assumed to be mmKS (i.e., length is in millimeters).
c
c   1/24/22 Added trim state if.
c
c   X/XX/XX C compatible conversion
c           https://gcc.gnu.org/onlinedocs/gfortran/Derived-Types-and-struct.html#Derived-Types-and-struct
c           also see http://www.chem.helsinki.fi/~manninen/fortran2014/6_Advanced_features.pdf for usage


      include 'types.h'

c     The plan is to include this include in its entirety all the time.  The below may be slightly
c     wasteful of memory, but it saves passing dimensions.
      include 'arguments.h'

