  PROGRAM equilibrium
    ! Program plots heating and cooling functions to find equilibrium temperatures

    integer,parameter:: nx = 1000
    real,parameter :: a = 1.0
    real,parameter :: sigma_SB = 5.67d-5

    real :: T, infrared, albedo, insol, Q, Q_old, dQdt, dT

    dT = (500.0-50.0)/REAL(nx)
    Q_old = 0.0
    dQdt = 0.0

    OPEN(10,file='equilibrium.dat',status = 'unknown')
    
    Do i=1,nx+1
       T = 50.0 +(i-1)*dT
       albedo = 0.525 - 0.245*tanh((T-268.0)/5.0)
       tau_ir = 0.79*(T/273.0)**3
       infrared = sigma_SB*T*T*T*T/(1.0+0.75*tau_ir)
       insol = 1.36e6*(1.0-albedo)/(4.0*a*a)
       
       Q = insol - infrared
       IF(i/=1) dQdt = (Q-Q_old)/dT
       Q_old = Q
       
       WRITE(10,*) T, albedo, tau_ir, infrared,insol,Q,dQdt
    ENDDO
    Close(10)
END PROGRAM equilibrium
