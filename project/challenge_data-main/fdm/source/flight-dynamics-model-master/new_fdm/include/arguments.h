c   This is the include "header" for the common FORTRAN procedure arguments
c   It is intended to be similar to a .h file, and add granularity to what is
c   included in different files.
c   It should only be included in a few common files, like parts.h
c
c   Joel Allardyce 13 July 2022    Southwest Research Institute
c
c   7/13/22 Initial coding
c
      type (aircraft_type)  :: aircraft  ! only one aircraft
      type (wing_type)      :: wing      (maxparts)
      type (propeller_type) :: propeller (maxparts)
      type (battery_type)   :: battery   (maxparts)
      type (controls_type)  :: control   ! only one control system, right now