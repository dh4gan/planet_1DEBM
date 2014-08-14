SUBROUTINE adjustpCO2(temp,pCO2)
! Written 14/8/14 by D.Forgan
! Given the temperature at any latitude, this modifies the 
! CO2 partial pressure according to Spiegel et al (2010), ApJ 721, 1308-1318

real, parameter :: T1 = 250.0
real,parameter :: T2 = 290.0

if(temp<=T1) then
    pCO2 = 0.01
else if (temp> T1 .and. temp< T2) then
    pCO2 = -2 -(temp-250.0)/27.0
    pCO2 = 10.0**pCO2
else if(temp >=T2) then
    pCO2 = 3.3e-4
endif


END SUBROUTINE adjustpCO2
