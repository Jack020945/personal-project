c   This is the include "header" for the C compatible versions of aircraft parts.
c   It is intended to be similar to a .h file, and shares much of its common
c   field descriptions with the 'parts.h' file used to keep the types
c   and their fields common throughout the code.
c   Joel Allardyce  13 July 2022    Southwest Research Institute
c
c   7/13/22 Initial coding

      type, bind(C) :: aircraft_type_c
        character(KIND=C_CHAR) :: cname(80)                     ! name of aircraft
        character(KIND=C_CHAR) :: ctype(80)                     ! type of aircraft
c       Include the common parts:
        include 'aircraft_common.h'
      end type aircraft_type_c


      type, bind(C) :: wing_type_c
        character(KIND=C_CHAR) :: cname(80)         ! name or description of wing, typically half main wing,
c                          horizontal tail, or vertical tail.  May be needed/used in the context
c                          of propwash, downwash calculations, etc. 
        character(KIND=C_CHAR) :: ctype(80)         ! Airfoil section name, for example, if a table lookup is needed.
c       Include the common parts:
        include 'wing_common.h'
      end type wing_type_c


      type, bind(C) :: propeller_type_c
c       It is assumed that each propeller comes with a motor, so this is where electric
c       motor information is also stored.
        character(KIND=C_CHAR) :: cname(80)         ! name of propller
        character(KIND=C_CHAR) :: ctype(80)         ! type of propller - used, for example, if a thrust table is looked up
c       Part of the propeller data table:
        character(KIND=C_CHAR) :: prop_fname(80)   ! data file name for propeller
c       Part of the motor data table:
        character(KIND=C_CHAR) :: mtype(80)        ! type of motor
        character(KIND=C_CHAR) :: motor_fname(80)  ! data file name for motor
c       Include the common parts:
        include 'propeller_common.h'
      end type propeller_type_c


      type, bind(C) :: battery_type_c
        character(KIND=C_CHAR) :: cname(80)        ! name of battery
        character(KIND=C_CHAR) :: ctype(80)        ! type of battery - used, for example, if a table must be looked up
        character(KIND=C_CHAR) :: fname(80)        ! file name of battery
c       Include the common parts:
        include 'battery_common.h'
      end type battery_type_c