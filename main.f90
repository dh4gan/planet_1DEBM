PROGRAM planet_1DEBM
  !		Program uses 1D latitudinal diffusion to simulate simple terrestrial climate model
  use ebmdata
  
  IMPLICIT NONE
  
  !		Display header
  print*, " "
  print*, "-----------------------------------------------"  
  print*, "          1D ENERGY BALANCE CLIMATE MODEL"
  print*, "     Created by D.Forgan, 28th July 2011       "
  print*, "-----------------------------------------------"
  print*, " Based on Spiegel et al (2008), ApJ 681, 1609"
  print*, " Makefile can be found in "
  print*, "    /disk1/dhf/programs/planet_1DEBM"
  print*, " "
  print*, "-----------------------------------------------"
  print*, " input parameters in ./planet_1DEBM.params"
  
  OPEN(10,file='planet_1DEBM.params', status='unknown')
  
  READ(10,*) restart            ! Is this a new run or simply a restart?
  READ(10,*) icfile             ! Name of the initial conditions file (if restart)
  READ(10,*) prefix		! Character prefix for filenames
  READ(10,*) nx			! Number of grid points
  READ(10,*) Mstar		! Star mass in solar masses
  READ(10,*) Lstar              ! Star luminosity in solar luminosities
  READ(10,*) semi_maj		! Semi major axis (AU)
  READ(10,*) ecc		! Eccentricity 
  READ(10,*) phi_peri           ! Longitude of Periastron in degrees
  READ(10,*) phi		! Current Orbital Longitude in degrees
  READ(10,*) rot_period		! Rotation Period of the Planet (days)
  READ(10,*) spin_obliq		! Obliquity of Spin Axis (degrees)
  READ(10,*) azim_obliq         ! Longitude of winter solstice relative to periastron(degrees)
  READ(10,*) f_ocean		! Fraction of surface that is ocean
  READ(10,*) To			! Initial planet temperature (K)
  READ(10,*) maxtime		! Maximum simulation time (years)
  READ(10,*) dumpfreq		! Time interval between outputs
  
  CLOSE(10)
  
  !		Set up initial conditions
  
  print*, 'Initial conditions successfully read in from ebm_params.dat'
  print*,"-----------------------------------------------"
  print*, 'Initial Conditions'
  print*, "-----------------------------------------------"
  print*, 'Number of Grid Points: ',nx
  print*,' Planet Semi-major Axis: ', semi_maj
  print*, 'Eccentricity: ', ecc
  print*, 'Longitude of Periastron: ',phi_peri, ' degrees'
  print*, 'Initial Orbital Longitude: ', phi, ' degrees'
  print*, 'Rotational Period: ', rot_period, ' days'
  print*, 'Axial Obliquity: ', spin_obliq, ' degrees'
  print*, 'Azimuthal Obliquity: ',azim_obliq, ' degrees'
  print*, 'Ocean Coverage: ',f_ocean
  print*, 'Land Coverage: ',(1.0-f_ocean)
  print*, 'Initial Global Temperature: ', To
  Print*, 'Maximum Runtime: ',maxtime
  print*, 'Output files to have prefix: ',prefix
  print*, "-----------------------------------------------"
  
  wait(3)

  print*, 'Initialising arrays'
  step = 1

  CALL initial
  IF(restart=='n') dumpcount=1

  print*, "Planet's orbital period: ",orb_period
  print*, "Orbital Angular Frequency: ",orb_freq, " radians per sec"
  
  print*, 'Calculating initial climate parameters'
  CALL calc_params
  print*, "-----------------------------------------------"		
  !		Output initial model

  OPEN(76,file=runfile,status='unknown')

  filename = TRIM(prefix)//'.initial'
  CALL output


  runfile = TRIM(prefix)//'.log'
  
  deltat = maxtime

  CALL timestep
  time =time+deltat

  CALL calc_params
  
 

  
  timeyr = time/yr
  
  !		Begin main loop over time (until some threshold time)		
  DO WHILE (timeyr<maxtime)

     !		Integrate diffusion equation
     CALL integrate
     
     CALL calc_params

     timeyr = time/yr
     
     !		Write out file if enough time has passed
     
     IF(timeyr>dumpfreq*dumpcount) THEN
        
        IF(dumpcount<=9) THEN
           WRITE(fileno,'(I1)') dumpcount
        ELSE IF(dumpcount>9.and.dumpcount<=99) THEN
           WRITE(fileno,'(I2)') dumpcount
        ELSE IF(dumpcount>99.and.dumpcount<=999) THEN
           WRITE(fileno,'(I3)') dumpcount
        ELSE IF(dumpcount>999.and.dumpcount<=9999) THEN
           WRITE(fileno,'(I4)') dumpcount
        ELSE IF(dumpcount>9999.and.dumpcount<=99999) THEN
           WRITE(fileno,'(I5)') dumpcount
        ENDIF
        fileno = TRIM(fileno)
        !print*, fileno
        filename = TRIM(prefix)//'.'//fileno
       ! print*, filename
        dumpcount = dumpcount +1
        
        CALL output
        
     ENDIF

     !Calculate new timestep 
     CALL timestep
     time =time+deltat
     step = step+1

  !  IF(step==2) STOP
  ENDDO
  !		End loop over time
  
  CLOSE(76)
  !		Deallocate all arrays
  
  deallocate(T,T_old,insol,cos_H,f_ice,C_tot)
  deallocate(lat,x,albedo,tau_ir,infrared,Q,hab,latdeg)
  
END PROGRAM planet_1DEBM
