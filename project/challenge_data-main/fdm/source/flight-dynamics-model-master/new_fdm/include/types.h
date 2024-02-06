c   This is the include "header" for the common FORTRAN types
c   It is intended to be similar to a .h file, and add granularity to what is
c   included in different files.
c   It should only be included in a few common files, like parts.h
c
c   Joel Allardyce 13 July 2022    Southwest Research Institute
c
c   7/13/22 Initial coding
c
      include 'shared_parameters.h'

      include 'pi.h'  ! necessary due to some default change of values

      type :: aircraft_type
        sequence
        character(LEN=80) :: cname                     ! name of aircraft
        character(LEN=80) :: ctype                     ! type of aircraft
c       Include the common parts:
        include 'aircraft_common.h'
      end type aircraft_type


      type :: wing_type
        sequence
        character(LEN=80) :: cname         ! name or description of wing, typically half main wing,
c                          horizontal tail, or vertical tail.  May be needed/used in the context
c                          of propwash, downwash calculations, etc. 
        character(LEN=80) :: ctype         ! Airfoil section name, for example, if a table lookup is needed.
c       Include the common parts:
        include 'wing_common.h'
      end type wing_type


      type :: propeller_type
c       It is assumed that each propeller comes with a motor, so this is where electric
c       motor information is also stored.
        sequence
        character(LEN=80) :: cname         ! name of propller
        character(LEN=80) :: ctype         ! type of propller - used, for example, if a thrust table is looked up
c       Part of the propeller data table:
        character(LEN=80) :: prop_fname   ! data file name for propeller
c       Part of the motor data table:
        character(LEN=80) :: mtype        ! type of motor
        character(LEN=80) :: motor_fname  ! data file name for motor
c       Include the common parts:
        include 'propeller_common.h'
      end type propeller_type 


      type :: battery_type
        sequence
        character(LEN=80) :: cname        ! name of battery
        character(LEN=80) :: ctype        ! type of battery - used, for example, if a table must be looked up
        character(LEN=80) :: fname        ! file name of battery
c       Include the common parts:
        include 'battery_common.h'
      end type battery_type


c     Include the entire C-compatible controls type:
      include 'controls_type.h'


c     Include the entire C-compatible ground contact type:
      include 'ground_contact_type.h'