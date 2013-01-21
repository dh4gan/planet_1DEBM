SUBROUTINE timestep
  !		Calculates global minimum timestep based on diffusive condition
  use ebmdata
  implicit none
  integer :: imin
  real :: Dplus
  
  t_try(:) = 0.0

!$OMP PARALLEL &
!$OMP shared(nx,diff,x,deltax,t_try,C_tot,deltat) &
!$OMP private(i,Dplus)
!$OMP DO SCHEDULE(runtime)
  DO i=1,nx+1
     IF(i==nx+1) Dplus = diff*(1.0-x(i)*x(i))
     IF(i/=nx+1) Dplus = 0.5*diff*((1.0-x(i+1)*x(i+1)) + (1.0-x(i)*x(i)))

     IF(i/=1.and.i/=nx+1) THEN
        t_try(i) = (deltax(i)*deltax(i)*C_tot(i))/(2.0*Dplus)
     ELSE
        t_try(i) = 1.0e30
     ENDIF
  ENDDO
!$OMP END DO
!$OMP END PARALLEL

  deltat=0.9*minval(t_try)

  
END SUBROUTINE timestep
