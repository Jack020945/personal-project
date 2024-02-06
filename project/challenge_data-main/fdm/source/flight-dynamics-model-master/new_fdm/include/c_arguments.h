c   This is the include "header" for the C compatible versions of function
c   arguments.
c   It is intended to be similar to a .h file, and shares much of its common
c   field descriptions with the 'parts.h' file used to keep the types
c   and their fields common throughout the code.
c   Joel Allardyce  13 July 2022    Southwest Research Institute
c
c   7/13/22 Initial coding

      include 'shared_parameters.h'
      include 'pi.h'  ! necessary due to some default change of values

      include 'c_types.h'


c     Include the entire C-compatible controls type:
      include 'controls_type.h'


c     Include the entire C-compatible ground contact type:
      include 'ground_contact_type.h'

      type(c_ptr),value       :: aircraft  ! only one aircraft
      type(c_ptr),value       :: wing
      type(c_ptr),value       :: propeller
      type(c_ptr),value       :: battery
      type(c_ptr),value       :: control   ! only one control system, right now

