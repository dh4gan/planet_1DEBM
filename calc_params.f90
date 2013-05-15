SUBROUTINE calc_params
  use ebmdata

  IMPLICIT NONE
  ! Subroutine calculates:
  ! Insolation (r)
  ! polar angle (lat)
  ! Infrared Cooling
  ! Ice Cover
  ! Heat Capacity

  real :: ang,sind,cosd,delta,phidot,tand


  ! Calculate orbital longitude
  phidot = 2.0*pi*(h_ang/(r*r))/yr
  phi  = phi + phidot*deltat

  IF(phi>2.0*pi) phi = MOD(phi,2.0*pi)

  ! Calculate current orbital radius

  r = semi_maj*(1.0-ecc*ecc)/(1.0+ecc*cos(phi-phi_peri))
  insol(:) = (q0*Lstar)/(pi*r*r) 

  ! Current solar declination (polar) angle
     sind = -DSIN(spin_obliq)*DCOS(phi-phi_peri - azim_obliq)     
     delta = ASIN(sind)
     cosd = sqrt(1.0-sind*sind)
     tand = tan(delta)

!$OMP PARALLEL &
!$OMP shared(nx,cos_H,insol,lat,sind,cosd,tand) &
!$OMP shared(T,C_tot,f_land,f_ocean) &
!$OMP shared(albedo,tau_ir,infrared,Q,hab) &
!$OMP private(i,H,C_ice)
!$OMP DO SCHEDULE(runtime)
  DO i=1,nx+1
     
     !		Diurnally averaged Hour Angle     
        cos_H(i) = -dtan(lat(i))*tand
     
     IF(ABS(cos_H(i))>1.0) cos_H(i) = cos_H(i)/ABS(cos_H(i))
        
     H = ACOS(cos_H(i))
     
     !		Insolation
     
    insol(i) = insol(i)*(H*sin(lat(i))*sind + cos(lat(i))*cosd*sin(H))

     !  Now calculate temperature dependent properties
     !	Ice Cover

     f_ice(i) = 1.0 - DEXP(-(freeze-T(i))/10.0)
     IF(f_ice(i)<0.0) f_ice(i) =0.0

     !	Heat Capacity

     IF(T(i)>=freeze) THEN
        C_ice = 0.0
     ELSE IF (T(i)<freeze.and. T(i)> freeze-10.0) THEN
        C_ice = 9.2*C_land
     ELSE IF(T(i)<=freeze-10.0) THEN
        C_ice = 2.0*C_land
     ENDIF

     C_tot(i) = f_land*C_land + f_ocean*(f_ice(i)*C_ice + (1.0-f_ice(i))*C_ocean)

     !	Albedo
     albedo(i) = 0.525 - 0.245*tanh((T(i)-freeze+5.0)/5.0)

     !	Optical Depth
     tau_ir(i) = 0.79*(T(i)/freeze)**3
     !	Infrared Cooling Function
     infrared(i) = sigma_SB*(T(i)*T(i)*T(i)*T(i))/(1.0 + 0.75*tau_ir(i))

     ! Net Heating Function
     Q(i) = insol(i)*(1.0-albedo(i)) - infrared(i)

     ! Habitability Function
     hab(i) = 0.0
     IF(T(i)>=freeze.and.T(i)<=boil) hab(i)=1.0

  ENDDO
!$OMP END DO
!$OMP END PARALLEL

END SUBROUTINE calc_params
