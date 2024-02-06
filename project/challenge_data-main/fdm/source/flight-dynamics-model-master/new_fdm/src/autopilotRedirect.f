      subroutine autopilotRedirect(time, xdim, x, xd, xo, udim, uc,
     .                             aircraft, wing, propeller, battery,
     .                             control, Xpath, Xpathdot, lstraight,
     .                             radius, ipath)

        use iso_c_binding
        implicit none

        include 'types.h'
        type (aircraft_type), target  :: aircraft  ! only one aircraft
        type (wing_type), target      :: wing      (maxparts)
        type (propeller_type), target :: propeller (maxparts)
        type (battery_type), target   :: battery   (maxparts)
        type (controls_type), target  :: control   ! only one control system, right now
        include 'c_types.h'

        integer(C_INT) :: xdim, udim, ipath

        REAL(C_DOUBLE) :: time
        REAL(C_DOUBLE) :: x(xdim), xd(xdim)
        REAL(C_DOUBLE) :: xo(1500)
        REAL(C_DOUBLE) :: uc(udim)
        REAL(C_DOUBLE) :: Xpath(3), Xpathdot(3)
        REAL(C_DOUBLE) :: radius
        logical(C_BOOL) :: lstraight


        REAL(C_DOUBLE) :: my_time
        REAL(C_DOUBLE) :: my_x(xdim), my_xd(xdim)
        REAL(C_DOUBLE) :: my_xo(1500)
        REAL(C_DOUBLE) :: my_uc(udim)
        REAL(C_DOUBLE) :: my_Xpath(3), my_Xpathdot(3)
        REAL(C_DOUBLE) :: my_radius
        logical(C_BOOL) :: my_lstraight
        integer :: idx

c       Types we can send into our external C function
        type (c_ptr) :: p_aircraft_c, p_wing_c, p_propeller_c,
     .                  p_battery_c, p_control

        interface
          subroutine externalAutopilot(time, xdim, x, xd, xo, udim, uc,
     .                                 aircraft,
     .                                 wing, propeller, battery,
     .                                 control, Xpath,
     .                                 Xpathdot, lstraight, radius,
     .                                 ipath)
     .      bind(C, name="externalAutopilot")
            use, intrinsic :: iso_c_binding
            implicit none
            include 'c_arguments.h'
            integer(C_INT) :: xdim, udim, ipath

            REAL(C_DOUBLE) :: time
            REAL(C_DOUBLE) :: x(xdim), xd(xdim)
            REAL(C_DOUBLE) :: xo(1500)
            REAL(C_DOUBLE) :: uc(udim)
            REAL(C_DOUBLE) :: Xpath(3), Xpathdot(3)
            REAL(C_DOUBLE) :: radius
            LOGICAL(C_BOOL) :: lstraight
          end subroutine externalAutopilot
        end interface

c       Pass C compatible pointers to our C-ish-compatible Fortran types.
c       (ish because our string types are potentially problematic.  We're
c       counting on fact that the C versions of our common structures and the
c       FORTRAN versions take up the same space in memory.)
        p_aircraft_c = c_loc(aircraft)
        p_wing_c = c_loc(wing(1))
        p_propeller_c = c_loc(propeller(1))
        p_battery_c = c_loc(battery(1))
        p_control = c_loc(control)

        call externalAutopilot(
     .          time,
     .          xdim,
     .          x,
     .          xd,
     .          xo,
     .          udim,
     .          uc,
     .          p_aircraft_c,
     .          p_wing_c,
     .          p_propeller_c,
     .          p_battery_c,
     .          p_control,
     .          Xpath,
     .          Xpathdot,
     .          lstraight,
     .          radius,
     .          ipath)
      end