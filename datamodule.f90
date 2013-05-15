module ebmdata

!-----------------------------------------------------------------------
! Data module 
! DHF 01/09/2009
!-----------------------------------------------------------------------

  implicit none
  save

!-------------------Single values---------------------------------------
! Integers

  integer :: i,nx,step,dumpcount

! Reals
  real, parameter :: pi = 3.141592653
  real, parameter :: G = 6.673d-8
  real, parameter :: sigma_SB = 5.6704d-5
  real, parameter :: D = 5.394d2
  real, parameter :: C_land = 5.25d9
  real, parameter :: C_ocean = 40.0*C_land
  real, parameter :: freeze = 273.0	! Freezing Point of Water
  real, parameter :: boil = 373.0       ! Boiling Point of Water
  real, parameter :: Msol = 1.99d33
  real, parameter :: Lsol = 3.826e33
  real, parameter :: AU = 1.496e13
  real, parameter :: yr = 3.1556926d7
  real, parameter :: q0 = Lsol/(4.0*pi*AU*AU)	! Standard solar constant for 1Msol star at 1AU distance
  
  real :: semi_maj, ecc, phi, rot_period, orb_period,orb_freq, spin_obliq,azim_obliq, C_ice
  real :: f_ocean, f_land, To, time, maxtime, dlat,deltat,h_ang, phi_peri
  real :: r,H,Fj,Fj1,Mstar,Lstar,diff,dumpfreq,timeyr

! Characters

  character(len=100) :: prefix, filename, runfile
  character(len=100) :: fileno,restart,icfile
		      
!-------------------Array values----------------------------------------
! Integers


! Reals
  real, allocatable,dimension(:) :: T, T_old, insol,cos_H,t_try
  real, allocatable,dimension(:) :: f_ice, C_tot, lat, x,albedo,tau_ir
  real, allocatable,dimension(:) :: infrared, Q,deltax,hab,latdeg
end module ebmdata
