        subroutine body_drag(rho,U,V,W,P,Q,R
     .      ,x_bf ,y_bf ,z_bf
     .      ,Ax   ,Ay   ,Az
     .      ,x_cm ,y_cm ,z_cm
     .      ,mass ,Ixx, Iyy, Izz
     .      ,Fx,Fy,Fz,Mx,My,Mz)

        implicit none

        double precision :: rho               ! input air density
        double precision :: U, V, W, P, Q, R  ! input speed and rotation rate
        double precision :: x_bf, y_bf, z_bf  ! input body drag center (called fuselage)
        double precision :: Ax, Ay, Az        ! input front area face
        double precision :: x_cm, y_cm, z_cm  ! input center of mass
        double precision :: Fx, Fy, Fz, Mx, My, Mz  ! output body drag force and moment
        double precision :: mass, Ixx, Iyy, Izz ! mass and moment of inertia values (added by AJC, 6/2/2022)

        double precision Lx, Ly, Lz
        integer nx, ny, nz, i, j, k
        double precision dAxy, dAyz, dAxz, alpha
        double precision Ubar, Vbar, Wbar, Ub, Vb, Wb, x, y, z
        double precision Mx_tot, My_tot, Mz_tot
        double precision x_eff, y_eff, z_eff ! effective radius from rotation axis (added by AJC, 6/2/2022)
        ! Above values are required for box moment of inertia to match actual values per parallel axis theorem
        
c       Inline functions
        Ubar(y,z) = U + Q*(z - z_cm) - R*(y - y_cm)
        Vbar(x,z) = V - P*(z - z_cm) + R*(x - x_cm)
        Wbar(x,y) = W + P*(y - y_cm) - Q*(x - x_cm)

c No integration in this version, so these aren't needed (changed by AJC, 6/2/2022)
c        nx = 10
c        ny = 10
c        nz = 10

c       We now include an approximate body drag that includes rotation.
c       Here we have decoupled the terms to approximations along
c       the body coordinate axes.  JDW  5/30/22
c       We assume the X_fuseuu is the y-z projected area, etc.,
c       to come up with a characteristic length for the body drag.
        if (Ax.le.0.d0) then
           if (Ay.lt.Az) then
              Lx = sqrt(Az)
              Ly = Lx
              Lz = 0.d0
           else
              Lx = sqrt(Ay)
              Ly = 0.d0
              Lz = Lx
           endif
        else if (Ay.le.0.d0) then
           if (Ax.lt.Az) then
              Lx = sqrt(Az)
              Ly = Lx
              Lz = 0.d0
           else
              Lx = 0.d0
              Ly = sqrt(Ax)
              Lz = Ly
           endif
        else if (Az.le.0.d0) then
           if (Ax.lt.Ay) then
              Lx = sqrt(Ay)
              Ly = 0.d0
              Lz = Lx
           else
              Lx = 0.d0
              Ly = sqrt(Ax)
              Lz = Ly   
           endif
        else 
           Lx = sqrt(Ay*Az/Ax)
           Ly = sqrt(Ax*Az/Ay)
           Lz = sqrt(Ax*Ay/Az)
        endif
c Determine "effective radius" values required to match actual moments of inertia (added by AJC, 6/2/2022)
        x_eff = sqrt(Ixx/mass)
        y_eff = sqrt(Iyy/mass)
        z_eff = sqrt(Izz/mass)
c 
c       This is the old case as a reminder
c	Fx = - 0.5d0 * rho * aircraft%X_fuseuu * abs(Viw)*Viw
c	Fy = - 0.5d0 * rho * aircraft%Y_fusevv * abs(Vp )*Vp
c	Fz = - 0.5d0 * rho * aircraft%Z_fuseww * abs(Wp )*Wp
c       For right now we are doing the integrals by brute force
c       collocation.  Perhaps in the future we'll explicitly 
c       integrate them, but it is tedious due to the abs term.
        Fx = 0.d0
        Fy = 0.d0
        Fz = 0.d0
        Mx_tot = 0.d0
        My_tot = 0.d0
        Mz_tot = 0.d0
c 
        dAxy = Az/4. ! Not really an integration anymore, now we've just divided area into four parts (chaged by AJC, 6/3/2022)
        dAxz = Ay/4. ! Not really an integration anymore, now we've just divided area into four parts (chaged by AJC, 6/3/2022)
        dAyz = Ax/4. ! Not really an integration anymore, now we've just divided area into four parts (chaged by AJC, 6/3/2022)

c       We integrate on the x face first (no longer an integration, AJC, 6/3/2022)
        My = 0.d0
        Mz = 0.d0
        do i = 1, 2
           alpha = 2*i - 3
           y = y_bf + y_eff*alpha ! Now all area is lumped at effective radius from body drag center (changed by AJC, 6/3/2022)
           do j = 1, 2
              alpha = 2*i - 3
              z = z_bf + z_eff*alpha ! Now all area is lumped at effective radius from body drag center (changed by AJC, 6/3/2022)
              Ub = Ubar(y,z)
              Fx = Fx + abs(Ub)*Ub
              My = My + (z - z_cm)*abs(Ub)*Ub
              Mz = Mz - (y - y_cm)*abs(Ub)*Ub
           enddo
        enddo
        Fx = Fx * dAyz
        My = My * dAyz
        Mz = Mz * dAyz
        My_tot = My_tot + My
        Mz_tot = Mz_tot + Mz

c       We integrate on the y face (no longer an integration, AJC, 6/3/2022)
        Mx = 0.d0
        Mz = 0.d0
        do i = 1, 2
           alpha = 2*i - 3
           x = x_bf + x_eff*alpha ! Now all area is lumped at effective radius from body drag center (changed by AJC, 6/3/2022)
           do j = 1, 2
              alpha = 2*i - 3
              z = z_bf + z_eff*alpha ! Now all area is lumped at effective radius from body drag center (changed by AJC, 6/3/2022)
              Vb = Vbar(x,z)
              Fy = Fy + abs(Vb)*Vb
              Mx = Mx - (z - z_cm)*abs(Vb)*Vb
              Mz = Mz + (x - x_cm)*abs(Vb)*Vb
           enddo
        enddo
        Fy = Fy * dAxz
        Mx = Mx * dAxz
        Mz = Mz * dAxz
        Mx_tot = Mx_tot + Mx
        Mz_tot = Mz_tot + Mz
        
c       We integrate on the z face (no longer an integration, AJC, 6/3/2022)
        Mx = 0.d0
        My = 0.d0
        do i = 1, 2
           alpha = 2*i - 3
           x = x_bf + x_eff*alpha ! Now all area is lumped at effective radius from body drag center (changed by AJC, 6/3/2022)
           do j = 1, 2
              alpha = 2*i - 3
              y = y_bf + y_eff*alpha ! Now all area is lumped at effective radius from body drag center (changed by AJC, 6/3/2022)
              Wb = Wbar(x,y)
              Fz = Fz + abs(Wb)*Wb
              Mx = Mx + (y - y_cm)*abs(Wb)*Wb
              My = My - (x - x_cm)*abs(Wb)*Wb
           enddo
        enddo
        Fz = Fz * dAxy
        Mx = Mx * dAxy
        My = My * dAxy
        Mx_tot = Mx_tot + Mx
        My_tot = My_tot + My

        Mx = Mx_tot
        My = My_tot
        Mz = Mz_tot

c       Now we multiply by the force term
        Fx = - 0.5d0 * rho * Fx
        Fy = - 0.5d0 * rho * Fy
        Fz = - 0.5d0 * rho * Fz

        Mx = - 0.5d0 * rho * Mx
        My = - 0.5d0 * rho * My
        Mz = - 0.5d0 * rho * Mz

        return
        end
        
