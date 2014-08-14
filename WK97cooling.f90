SUBROUTINE WK97cooling(temp, pCO2, infrared)
! Written 14/8/14 by D. Forgan
! This subroutine calculates infrared cooling using the 
! polynomial fit of Williams and Kasting (1997), Icarus 129, 254-267
! Relies on pCO2 as well as T

real :: temp, pCO2, infrared
real :: logp

call AdjustpCO2(temp,pCO2)

logp = log(pCO2/3.3e-4)
logp2 = logp*logp
logp3 = logp2*logp
logp4 = logp3*logp

temp2 = temp*temp
temp3 = temp2*temp

! Apply WK97 formula here (gives answer in W m^-2)

infrared = 9.468980 - 7.714727e-5*logp -2.794778*temp - 3.244753e-3*logp*temp &
-3.547406e-4*logp2 + 2.212108e-2*temp2 +2.229142e-3*logp2*temp + &
3.088497e-5*logp*temp2 -2.789815e-5*logp2*temp2 - &
3.442973e-4*logp3 -3.361939e-5*temp3 + 9.173169e-3*logp3*temp - &
7.775195e-5*logp3*temp2 -1.679112e-7*logp*temp3 +6.590999e-8*logp2*temp3 + &
1.528125e-7*logp3*temp3 -3.67567e-2*logp4 - &
1.631909e-4*logp4*temp +3.663871e-6*logp4*temp2 -9.255646e-9*logp4*temp3

! Convert answer to cgs

infrared = infrared*1.0e3;


!print*, pCO2, logp, temp, infrared

END SUBROUTINE WK97cooling
