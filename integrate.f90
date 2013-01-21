SUBROUTINE integrate
  !	Integrates the diffusion equation using a simple explicit scheme
  !     Spatial derivatives are zero at boundaries (Neumann BCs)
  use ebmdata
  IMPLICIT NONE

  real :: xhalf,xhalf1,Tplus1,T1,Tminus1,dx,Dplus,Dminus,dx1
  
  T_old(:) = T(:)

!$OMP PARALLEL &
!$OMP shared(nx,T,T_old,deltax,x,deltat) &
!$OMP shared(C_tot,Q,diff) &
!$OMP private(i,Tminus1,Tplus1,T1,Dplus,Dminus) &
!$OMP private(dx1,dx,Fj)
!$OMP DO SCHEDULE(runtime)
  DO i=1,nx+1

     IF(i==1) Tminus1 = T_old(i)
     IF(i/=1) Tminus1 = T_old(i-1)
     IF(i==nx+1) Tplus1 = T_old(i)
     IF(i/=nx+1) Tplus1 = T_old(i+1)

     T1 = T_old(i)

     IF(i==1) Dminus = diff*(1.0-x(i)*x(i))
     IF(i/=1) Dminus = 0.5*diff*((1.0-x(i-1)*x(i-1)) + (1.0-x(i)*x(i)))

     IF(i==nx+1) Dplus = diff*(1.0-x(i)*x(i))
     IF(i/=nx+1) Dplus = 0.5*diff*((1.0-x(i+1)*x(i+1)) + (1.0-x(i)*x(i)))

     !	Calculate spatial derivatives
     IF(i==1) THEN 
        dx1 = deltax(i)
        dx = 0.5*(deltax(i+1)+deltax(i))
     ELSE IF(i==nx+1) THEN        
        dx1 = 0.5*(deltax(i-1)+deltax(i))
        dx = deltax(i)
     ELSE        
        dx = 0.5*(deltax(i) + deltax(i+1))
        dx1 = 0.5*(deltax(i-1)+ deltax(i))
     ENDIF


!     Fj = (Dplus*(Tplus1 - T1) - Dminus*(T1 - Tminus1))/(deltax(i)*deltax(i))
!     Fj = (Dplus*(Tplus1 - T1) - Dminus*(T1 - Tminus1))/(dx*dx)
     Fj = (Dplus*(Tplus1-T1)/dx - Dminus*(T1-Tminus1)/dx1)/(0.5*(dx1+dx))

     T(i) = T1 + (deltat/C_tot(i))*(Q(i) +Fj)

  ENDDO
!$OMP END DO
!$OMP END PARALLEL

END SUBROUTINE integrate
