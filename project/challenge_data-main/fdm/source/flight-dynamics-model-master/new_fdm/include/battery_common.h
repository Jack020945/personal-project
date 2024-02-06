c   This is the include "header" for the common battery type fields
c   It is intended to be similar to a .h file.
c   The intent is to include this entire file when defining both the Fortran
c   C compatible versions of this type.
c   Joel Allardyce 13 July 2022    Southwest Research Institute
c
c   7/13/22 Initial coding
c
c       The following values (num_cells through Rm) are read in from the file, or they can be entered directly
        integer(C_INT)     :: num_cells   ! (-) number of cells (not used at present)
        real(C_DOUBLE)     :: voltage     ! (volts)  battery voltage
        real(C_DOUBLE)     :: Capacity    ! (mAh = milli amp hours) Total battery charge (1 milli amp hour = 3.6 coulombs)
        real(C_DOUBLE)     :: C_Peak      ! Max amp is (C / 1000) * Capacity in mAh (/1000 to get to Amps)
c                                           A battery of  1 C and 1000 mAh has a max discharge rate of  1 amp
c                                           A battery of 75 C and 1000 mAh has a max discharge rate of 75 amps
c                                           A battery of 30 C and 3000 mAh has a max discharge rate of 90 amps
        real(C_DOUBLE)     :: C_Continuous         ! In regular use, max amp is (C / 1000) * capacity in mAh
        real(C_DOUBLE)     :: Rm                   ! (ohms)    battery resistance (not used at present)
        real(C_DOUBLE)     :: seconds_at_peak      ! (seconds) continuous use exceeding peak discharge
        real(C_DOUBLE)     :: seconds_at_continous ! (seconds) seconds exceeding continuous discharge
        real(C_DOUBLE)     :: remaining_charge     ! (mAh)     remaining charge on this battery, in milli amp hours
        real(C_DOUBLE)     :: percent              ! (%)       percent charge remaining (equals ratio of previous and Capacity) (internal)