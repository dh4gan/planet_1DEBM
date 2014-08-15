SUBROUTINE output
  use ebmdata
  IMPLICIT NONE

  real :: meanT,meanQ,meanS,meanIR, meanA, meanpCO2, subQ,habfrac,dbit

  !		Subroutine outputs all data to output file
  
  print*, "-----------------------------------------------"
  print*, 'Outputting data to file ', TRIM(filename)
  print*, 'Number of steps: ', step
  print*, 'Simulation Time: ', timeyr, ' years'
  print*, 'Timestep: ', deltat/yr, ' years'
 
  
  !		First write to dump file				
  OPEN(12,file=filename, status='unknown')
  WRITE(12,*) nx, timeyr, step, dumpcount,dumpfreq
  
  DO i=1,nx
     WRITE(12,*) x(i), latdeg(i), T(i), C_tot(i), Q(i), infrared(i), &
 albedo(i), insol(i), tau_ir(i), f_ice(i),&
 hab(i),ACOS(cos_H(i)), pCO2(i)
  ENDDO
  CLOSE(12)
  

  ! Calculate mean values of interesting properties

  meanT = 0.0
  meanQ = 0.0
  meanA = 0.0
  meanIR = 0.0
  meanS = 0.0
  meanPCO2 = 0.0
  habfrac = 0.0

  DO i=1,nx
     dbit = 0.5*cos(lat(i))*dlat
     meanT = meanT + T(i)*dbit
     meanQ = meanQ + Q(i)*dbit
     meanA = meanA + albedo(i)*dbit
     meanS = meanS + insol(i)*dbit
     meanIR = meanIR + infrared(i)*dbit
     meanPCO2 = meanPCO2 + pCO2(i)*dbit
     habfrac = habfrac + 0.5*hab(i)*COS(lat(i))*dlat
  ENDDO

  subQ = meanS*(1.0-meanA)-meanIR

  print*, 'Mean Insolation: ', meanS
  print*, 'Mean Albedo: ', meanA
  print*, 'Mean Albedo Screened insolation: ',meanS*(1.0-meanA)
  print*, 'Mean Infrared Cooling: ',meanIR
  print*, 'Mean Net Heating: ', meanQ
  print*, 'Mean Energy Diff: ',subQ
  print*, 'Mean Temperature: ',meanT, ' K'
  print*, 'Maximum T: ',maxval(T)
  print*, 'Minimum T: ',minval(T)

  print*, 'Habitable fraction: ', habfrac

  !		Now update log file
  
  WRITE(76,*) time/yr, step, deltat/yr, r,phi, meanT,&
meanIR,meanS,meanQ, &
maxval(T),minval(T),habfrac, meanPCO2
  
END SUBROUTINE output
