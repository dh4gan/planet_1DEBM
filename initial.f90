SUBROUTINE initial
  use ebmdata
  !		Sets up initial conditions based on inputs
  IMPLICIT NONE
  
  
  ! If using a restart file, then its parameters supersed the params.dat file

  IF(restart=='y') THEN
     OPEN(20,file=icfile,status='old')
     READ(20,*) nx, timeyr, step, dumpcount,dumpfreq
     time = timeyr*yr
  ENDIF

  !		Initialise all arrays
  allocate(T_old(nx+1))
  allocate(T(nx+1))
  allocate(t_try(nx+1))
  allocate(x(nx+1))
  allocate(lat(nx+1))
  allocate(latdeg(nx+1))
  allocate(cos_H(nx+1))
  allocate(f_ice(nx+1))
  allocate(C_tot(nx+1))
  allocate(albedo(nx+1))
  allocate(insol(nx+1))
  allocate(tau_ir(nx+1))
  allocate(infrared(nx+1))
  allocate(Q(nx+1))
  allocate(deltax(nx+1))
  allocate(hab(nx+1))
  
  dlat = pi/REAL(nx)		
  
  ! If restart, read in data from IC file
  IF(restart=='y') THEN

     DO i=1,nx+1
        READ(20,*) x(i), latdeg(i), T(i), C_tot(i), Q(i), infrared(i), albedo(i), insol(i), tau_ir(i), f_ice(i),hab(i)
        lat(i) = latdeg(i)*pi/180.0
     ENDDO
     CLOSE(20)

     ! If not a restart, then use the params.dat file to specify initial T, etc
  ELSE

     T(:) = To
     T_old(:) = T(:)
     time = 0.0
     DO i=1,nx+1
        lat(i) = -pi/2.0 + (i-1)*dlat
        x(i) = sin(lat(i))
     ENDDO
    
     latdeg(:) = lat(:)*180.0/pi
  ENDIF

  DO i=1,nx+1
    ! IF(i==1) deltax(i) = x(i+1) - x(i)
    ! IF(i/=1) deltax(i) = x(i)-x(i-1)
     deltax(i) = cos(lat(i))*dlat
    ! WRITE(88,*) i, x(i), lat(i), deltax(i)
  ENDDO
 ! STOP
  
  ! Check stellar luminosity
  ! If less than or equal to zero, assume we are using main sequence relations

  if(Lstar <= 0.0) then
     Lstar = Mstar**4.0
  endif

  ! 		Orbital period in years
  orb_period = sqrt(semi_maj*semi_maj*semi_maj/Mstar)

  orb_freq = 2.0*pi/(orb_period*yr) ! Orbital frequency in rad s-1
  
  ! Specific Orbital angular momentum (in code units)

  h_ang = sqrt(Mstar*semi_maj*(1.0-ecc*ecc))

  ! Convert angles to radians
  
  phi_peri = phi_peri*pi/180.0
  spin_obliq = spin_obliq*pi/180.0
  azim_obliq = azim_obliq*pi/180.0
  phi = phi*pi/180.0
  
  !		Current radial position
  
  r = semi_maj*(1.0-ecc*ecc)/(1.0+ecc*COS(phi-phi_peri))
  
  !		Diffusion coefficient
  ! (In these units, rot_period prop 1/angvel)

  diff = D*rot_period*rot_period
  
  !		Planet surface fractions
  
  IF(f_ocean>1.0) f_ocean = 1.0
  IF(f_ocean<0.0) f_ocean = 0.0
  
  f_land = 1.0-f_ocean
  

! Write parameters to parameters file
  
  OPEN(100,file=TRIM(prefix)//'.params', status='unknown')
  WRITE(100,*) prefix,           '    File prefix for this run'
  WRITE(100,*) nx,               '     Number of grid points'
  WRITE(100,*) Mstar,            '     Star Mass in Solar Masses'
  WRITE(100,*) semi_maj,         '     Semi Major Axis of Orbit (in AU)'
  WRITE(100,*) ecc,              '     Eccentricity' 
  WRITE(100,*) phi_peri,         '     Longitude of Periastron (in degrees)'
  WRITE(100,*) phi,              '     Orbital longitude (in degrees, periastron=0)'
  WRITE(100,*) rot_period,       '     Rotation period of Planet (in days)'
  WRITE(100,*) 2.0*pi*orb_period,'    Orbital Period of Planet (in years)'
  WRITE(100,*) spin_obliq,       '    Spin Obliquity (in degrees)'
  WRITE(100,*) azim_obliq,       '     Longitude of Winter Solstice (degrees, relative to periastron)'
  WRITE(100,*) f_ocean,          '     Fraction of planet surface that is ocean'
  WRITE(100,*) To,               '   Initial Surface Temperature (in K)'
  WRITE(100,*) maxtime,          '   Maximum Simulation Time (in years)'
  WRITE(100,*) dumpfreq,         '  Frequency at which simulation dumps data (in years)'
  CLOSE(100)

END SUBROUTINE initial
