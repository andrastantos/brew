!*==AA0002.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!   SPAG options set to convert source form only
!     ============================================================
 
!   AIRFLOW IN A BOX
!   Version 2.0
!   (c) Hanley Innovaitons  1995
!
!  INPUT FILES
!  DATAX.inp    -  X-DIRECTION/PHYSICAL DATA (SEE README.TXT)
!  DATAY.inp    -  Y-DIRECTION DATA
!
!  OUTPUT FILES
!  PRES.out     -  COMPUTED PRESSURE
!  RHO.out      -  COMPUTED DENSITY
!  U.out        -  COMPUTED X_COMPONENT OF VELOCITY
!  V.out        -  COMPUTED Y_COMPONENT OF VELOCITY
!  MACH.out     -  MACH NUMBER
!  TOTP.out     -  TOTAL PRESSURE
!  SL.out       -  TECPLOT (tm) FORMATTED FILE
!  CHIST.out    -  CONVERGENCE HISTORY OF MASS,MOMENTUM & ENERGY RESIDUALS
!
!  VARIABLES
!  NX,NY        - MAXIMUM GRID IN X & Y DIRECTIONS RESPECTIVELY.  THESE
!                 PARAMETERS MUST BE VARIED IN MOST OF SUBROUTINES TO
!                 INCREASE THE SIZE OF THE SIMULATION
!
      IMPLICIT REAL*8(a-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION u1(NX,NY) , u2(NX,NY) , u3(NX,NY) , u4(NX,NY)
      DIMENSION u1b(NX,NY) , u2b(NX,NY) , u3b(NX,NY) , u4b(NX,NY)
      DIMENSION f1(NX,NY) , f2(NX,NY) , f3(NX,NY) , f4(NX,NY)
      DIMENSION g1(NX,NY) , g2(NX,NY) , g3(NX,NY) , g4(NX,NY)
      DIMENSION f1x(NX,NY) , f2x(NX,NY) , f3x(NX,NY) , f4x(NX,NY)
      DIMENSION g1y(NX,NY) , g2y(NX,NY) , g3y(NX,NY) , g4y(NX,NY)
!
! DIMENSION QUANTIES AT INTERMEDIATE STEP
!
      DIMENSION FP1(NX,NY) , FP2(NX,NY) , FP3(NX,NY) , FP4(NX,NY)
      DIMENSION FM1(NX,NY) , FM2(NX,NY) , FM3(NX,NY) , FM4(NX,NY)
      DIMENSION GP1(NX,NY) , GP2(NX,NY) , GP3(NX,NY) , GP4(NX,NY)
      DIMENSION GM1(NX,NY) , GM2(NX,NY) , GM3(NX,NY) , GM4(NX,NY)
      DIMENSION f1bx(NX,NY) , f2bx(NX,NY) , f3bx(NX,NY) , f4bx(NX,NY)
      DIMENSION g1by(NX,NY) , g2by(NX,NY) , g3by(NX,NY) , g4by(NX,NY)
!
! DIMENSION THE STATE VARIABLES AND METRICS
!
      DIMENSION U(NX,NY) , V(NX,NY) , P(NX,NY) , RHO(NX,NY) , E(NX,NY)
      DIMENSION xix(NX,NY) , xy(NX,NY) , ex(NX,NY) , ey(NX,NY)
      DIMENSION AS1(NX) , AS2(NX) , gxi(NX,NY) , ge(NX,NY)
      DIMENSION AS3(NX) , AS4(NX) , xp1(NX,NY) , yp1(NX,NY)
      DIMENSION ddx(NX,NY) , ddy(NX,NY) , dtt(NX,NY) , xmu(NX,NY)
      DIMENSION u1o(NX,NY) , u2o(NX,NY) , u3o(NX,NY) , u4o(NX,NY)
      DIMENSION iter1(500) , conv1(500)
!
! OTHER VARIABLES
!
      DIMENSION NPX(30) , x(NX) , NPY(30) , y(NY) , ALX(30) , bex(30)
      DIMENSION ALY(30) , bey(30) , spx(30) , epx(30) , spy(30) ,       &
     &          epy(30)
      DIMENSION wb1(NX) , wb2(NX) , SIG(NY) , sig2(NY)
      DIMENSION FP1x(30,NY) , FP2x(30,NY) , FP3x(30,NY) , FP4x(30,NY)
      DIMENSION FM1x(30,NY) , FM2x(30,NY) , FM3x(30,NY) , FM4x(30,NY)
      DIMENSION GP1y(30,NX) , GP2y(30,NX) , GP3y(30,NX) , GP4y(30,NX)
      DIMENSION GM1y(30,NX) , GM2y(30,NX) , GM3y(30,NX) , GM4y(30,NX)
      DIMENSION DXP2(30,NX) , DXP3(30,NX) , DXP4(30,NX)
      DIMENSION DXM2(30,NX) , DXM3(30,NX) , DXM4(30,NX)
      DIMENSION DYP2(30,NY) , DYP3(30,NY) , DYP4(30,NY)
      DIMENSION DYM2(30,NY) , DYM3(30,NY) , DYM4(30,NY)
 
!
! VISCOUS VARIABLES
!
      DIMENSION ux(NX,NY) , uy(NX,NY) , vx(NX,NY) , vy(NX,NY) ,         &
     &          tx(NX,NY)
      DIMENSION ty(NX,NY) , FV2(NX,NY) , GV2(NX,NY)
      DIMENSION FV3(NX,NY) , GV3(NX,NY) , FV4(NX,NY) , GV4(NX,NY)
      DIMENSION T(NX,NY)
!
! DERIVATIVE OPERATOR
!
      DIMENSION DX(NX,33) , DY(NY,33)
      REAL*8 mu0 , NX1(NX) , NX2(NX) , jac(NX,NY) , NY1(NX) , NY2(NX)
      REAL*8 kk0 , mu , kk , kx , ky , MINlet(NY) , GMA
      COMMON /XD1   / FP1 , FM1 , FP2 , FM2 , FP3 , FM3 , FP4 , FM4 ,   &
     &                FP1x , FM1x , FP2x , FM2x , FP3x , FM3x , FP4x ,  &
     &                FM4x , FV2 , FV3 , FV4 , DXP2 , DXM2 , DXP3 ,     &
     &                DXM3 , DXP4 , DXM4 , DX , NPX , ALX , NDX , MXPy
      COMMON /YD1   / GP1 , GM1 , GP2 , GM2 , GP3 , GM3 , GP4 , GM4 ,   &
     &                GP1y , GM1y , GP2y , GM2y , GP3y , GM3y , GP4y ,  &
     &                GM4y , GV2 , GV3 , GV4 , DYP2 , DYM2 , DYP3 ,     &
     &                DYM3 , DYP4 , DYM4 , DY , NPY , ALY , NDY , MXPx
      COMMON /BNDRY / U , V , P , RHO , T , E , AS1 , AS2 , AS3 , AS4 , &
     &                NX1 , NY1 , NX2 , NY2 , SIG , GMA , S0 , T0 , P0 ,&
     &                PE , HOO , RR , MINlet
      COMMON /W_CONST/ TWX_0 , TWX_n , TWY_0 , TWY_n , UW_0 , UW_n ,    &
     &                 VW_0 , VW_n
!
 
!  JRA June 98 - next statement to avoid undefined variable
      ttt = 0
      pi = 3.141592654D0
      xh = 1.0/2.0
      xq = 1.0/4.0
      nx11 = 1
      mx11 = 1
      ny11 = 2
      my11 = 1
! --------------------------------------------------------------------
 
      WRITE (6,*) '        AIRFLOW IN A BOX'
      WRITE (6,*) '           Version 2.0  '
      WRITE (6,*) '  (c) Hanley Innovations  1995 '
 
! SETUP THE NUMERICS
!
! READ IN CONSTANTS
!
      OPEN (UNIT=9,FILE='datax.inp',STATUS='OLD')
      OPEN (UNIT=10,FILE='datay.inp',STATUS='OLD')
      READ (9,*) ft , sig5 , UW_n , mu0 , irs
      READ (9,*) TWX_0 , TWX_n , TWY_0 , TWY_n , grav
      WRITE (6,*) ft , sig5 , UW_n , mu0 , irs
      WRITE (6,*) TWX_0 , TWX_n , TWY_0 , TWY_n , grav
      READ (9,*) NDX
      READ (10,*) NDY
      WRITE (6,*) NDX , NDY
      nu = 0.0
      PE = 1.0
      UW_0 = 0.0D0
      VW_0 = 0.0D0
      VW_n = 0.0D0
!
! COMPUTE THE DERIVATIVES OPERATOR
!
      WRITE (6,*) ' X-DATA'
      DO i = 1 , NDX
         READ (9,*) spx(i) , epx(i) , NPX(i)
         WRITE (6,*) i , spx(i) , epx(i) , NPX(i)
      ENDDO
      WRITE (6,*) ' Y-DATA'
      DO i = 1 , NDY
         READ (10,*) spy(i) , epy(i) , NPY(i)
         WRITE (6,*) spy(i) , epy(i) , NPY(i)
      ENDDO
!	READ(10,*) IDS
      CLOSE (UNIT=9)
      CLOSE (UNIT=10)
      WRITE (6,*) ' ITERATION#  TIME             FINAL' ,               &
     &            '          MASS RESIDUAL'
!
! FIND THE LEADING EDGE AND TRAILING EDGE
!
!	 NPS=0
!	 DO 413I=1,IDS
!413        NPS=NPS+NPY(I)+1
!	NPS=NPS+1
!	NPS2=NPS+1
      CALL BOUND(ALX,bex,spx,epx,NDX)
      CALL POINTS(DX,NPX,NDX)
      CALL BOUND(ALY,bey,spy,epy,NDY)
      CALL POINTS(DY,NPY,NDY)
!
! COMPUTE MAXIMUM NUMBER OF POINTS IN X AND Y DIRECTIONS
!
      MXPx = 0
      MXPy = 0
      DO i = 1 , NDX
         MXPx = MXPx + NPX(i) + 1
      ENDDO
      DO i = 1 , NDY
         MXPy = MXPy + NPY(i) + 1
      ENDDO
!
! GET X AND Y AT EACH POINT
!
      CALL XX(x,ALX,bex,NPX,NDX)
      CALL XX(y,ALY,bey,NPY,NDY)
!
! SETUP BOUNDARY CONDITION PARAMETERS
!
      CALL GRID6(x,y,xp1,yp1,xix,ex,xy,ey,jac,wb1,wb2,MXPx,MXPy)
!
! find delta x and delta y for local time stepping
!
      DO k = 1 , MXPy
         ict = 0
         DO i = 1 , NDX
            DO j = 1 , NPX(i) + 1
               ict = ict + 1
               IF ( j.LT.NPX(i)+1 ) THEN
                  ddx(ict,k) = xp1(ict+1,k) - xp1(ict,k)
               ELSE
                  ddx(ict,k) = xp1(ict,k) - xp1(ict-1,k)
               ENDIF
            ENDDO
         ENDDO
      ENDDO
 
      DO k = 1 , MXPx
         ict = 0
         DO i = 1 , NDY
            DO j = 1 , NPY(i) + 1
               ict = ict + 1
               IF ( j.LT.NPY(i)+1 ) THEN
                  ddy(k,ict) = yp1(k,ict+1) - yp1(k,ict)
               ELSE
                  ddy(k,ict) = yp1(k,ict) - yp1(k,ict-1)
               ENDIF
            ENDDO
         ENDDO
      ENDDO
 
      DO i = 1 , MXPx
         d1 = DSQRT(wb1(i)**2+1.)
         d2 = DSQRT(wb2(i)**2+1.)
         NX1(i) = -wb1(i)/d1
         NY1(i) = 1./d1
         NX2(i) = -wb2(i)/d2
         NY2(i) = 1./d2
!        WRITE(6,*)I,NX1(I),NY1(I),NX2(I),NY2(I)
      ENDDO
      DO i = 1 , MXPy
         a = (wb2(1)-wb1(1))/(y(MXPy)-y(1))
         b = wb2(1) - a*y(MXPy)
         SIG(i) = (a*y(i)+b)
!       WRITE(6,*) SIG(I)
      ENDDO
      DO i = 1 , MXPy
         a = (wb2(MXPx)-wb1(MXPx))/(y(MXPy)-y(1))
         b = wb2(MXPx) - a*y(MXPy)
         sig2(i) = (a*y(i)+b)
      ENDDO
      icn1 = 0
      icn2 = 0
!
! END NUMERICS SET UP
!---------------------------------------------------------------------
!
! SET UP THE PHYSICS
!
      GMA = DBLE(14)/DBLE(10)
      pi = 3.141592654D0
      RR = 287.00D0
      cp = GMA*RR/(GMA-1.0D0)
      pr = 0.690D0
      kk0 = mu0*cp/pr
      t02 = 272.777777777D0
!
! SET UP INITIAL CONDITIONS
!
      P0 = DBLE(101000)
      rho0 = 1.2
      HOO = GMA/(GMA-1)*P0/rho0
      a0 = DSQRT(GMA*P0/rho0)
      S0 = P0/rho0**GMA
      PE = PE*P0
      T0 = P0/(rho0*RR)
      v0 = 0.0
      u0 = 0.0
      d1tmp = ((GMA-1.0)/GMA)
      MINlet(1) = SQRT(((P0/PE)**d1tmp-1.0)*2./(GMA-1.0))
      DO j = 1 , MXPy
!	   MINLET(J)=SQRT(((P0/PE)**D1TMP-1.0)*2./(GMA-1.0))
         MINlet(j) = MINlet(1)
      ENDDO
      IF ( irs.NE.0 ) THEN
         OPEN (UNIT=8,FILE='pres.out',STATUS='OLD')
         OPEN (UNIT=9,FILE='rho.out',STATUS='OLD')
         OPEN (UNIT=10,FILE='u.out',STATUS='OLD')
         OPEN (UNIT=11,FILE='v.out',STATUS='OLD')
         READ (8,*) idx , idy
         READ (9,*) idx , idy
         READ (10,*) idx , idy
         READ (11,*) idx , idy
      ENDIF
!
      DO j = 1 , MXPy
         DO i = 1 , MXPx
            IF ( irs.NE.0 ) THEN
               READ (8,*) xt , yt , P(i,j)
               P(i,j) = P(i,j)*P0
               READ (9,*) xt , yt , RHO(i,j)
               RHO(i,j) = RHO(i,j)*rho0
               READ (10,*) xt , yt , U(i,j)
               U(i,j) = U(i,j)*a0
               READ (11,*) xt , yt , V(i,j)
               V(i,j) = V(i,j)*a0
            ELSE
               P(i,j) = PE
               xm1 = (((P0/PE)**((GMA-1.0)/GMA)-1.0)*2./(GMA-1.0))**0.5
               RHO(i,j) = rho0*(1.+(GMA-1.0)/2.0*xm1**2)**(1./(1.-GMA))
               c1 = a0
               U(i,j) = xm1*c1
               V(i,j) = v0
            ENDIF
            T(i,j) = P(i,j)/(RHO(i,j)*RR)
            E(i,j) = P(i,j)/((GMA-1.)*RHO(i,j)) + V(i,j)*V(i,j)         &
     &               /2. + U(i,j)*U(i,j)/2.0
            u1(i,j) = RHO(i,j)
            u2(i,j) = RHO(i,j)*U(i,j)
            u3(i,j) = RHO(i,j)*V(i,j)
            u4(i,j) = RHO(i,j)*E(i,j)
            gxi(i,j) = DSQRT(xix(i,j)*xix(i,j)+xy(i,j)*xy(i,j))
            ge(i,j) = DSQRT(ex(i,j)*ex(i,j)+ey(i,j)*ey(i,j))
         ENDDO
      ENDDO
      DO i = 1 , MXPx
         AS1(i) = DSQRT(GMA*P(i,1)/RHO(i,1))
         AS2(i) = DSQRT(GMA*P(i,MXPy)/RHO(i,MXPy))
      ENDDO
      DO j = 1 , MXPy
         AS3(j) = DSQRT(GMA*P(1,j)/RHO(1,j))
         AS4(j) = DSQRT(GMA*P(MXPx,j)/RHO(MXPx,j))
      ENDDO
!
      IF ( irs.NE.0 ) THEN
         CLOSE (UNIT=8)
         CLOSE (UNIT=9)
         CLOSE (UNIT=10)
         CLOSE (UNIT=11)
      ENDIF
!
      so1 = P0/rho0**GMA
      rp0 = u0 + 2.*a0/(GMA-1.)
      rm0 = u0 - 2.*a0/(GMA-1.)
!
! START TIME MARCHING - 2 STEP RUNGE-KUTTA
!
      it = 1
 2001 IF ( ttt.GE.ft ) GOTO 20
      IF ( it.EQ.1 ) GOTO 4000
! FIRST STEP
      DO i = 1 , MXPx
         DO j = 1 , MXPy
            u1b(i,j) = u1(i,j) - dtt(i,j)*jac(i,j)*(f1x(i,j)+g1y(i,j))
            u2b(i,j) = u2(i,j) - dtt(i,j)*jac(i,j)*(f2x(i,j)+g2y(i,j))
            u3b(i,j) = u3(i,j) - dtt(i,j)*jac(i,j)                      &
     &                 *(f3x(i,j)+g3y(i,j)-RHO(i,j)*grav)
            u4b(i,j) = u4(i,j) - dtt(i,j)*jac(i,j)                      &
     &                 *(f4x(i,j)+g4y(i,j)-RHO(i,j)*V(i,j)*grav)
         ENDDO
      ENDDO
!
! FIND THE STATE VARIABLES
!
      CALL STATE(u1b,u2b,u3b,u4b,U,V,RHO,P,E,T,MXPx,MXPy)
!
! Inlet Conditions
!
 
      CALL INLET(my11,MXPy,mx11,u1b,u2b,u3b,u4b,1)
!
! EXIT CONDITIONS
!
      CALL AEXIT(my11,MXPy,MXPx,u1b,u2b,u3b,u4b,1)
!
! BOTTOM WALL CONDITIONS FOR SUBSONIC FLOW
!
      CALL BOTWALL(nx11,MXPx,my11,MXPx,u1b,u2b,u3b,u4b,1)
!
! TOPWALL  CONDITIONS
!
      CALL TOPWALL(nx11+1,MXPx-1,MXPy,MXPx,u1b,u2b,u3b,u4b,1)
!
! COMPUTE THE NEW FBAR AND GBAR
!
!
! COMPUTE THE DERIVATIVES FOR THE VISCOUS TERMS
!
      CALL DERIVX(DX,U,ux,ALX,NPX,NDX,MXPy)
      CALL DERIVX(DX,V,vx,ALX,NPX,NDX,MXPy)
      CALL DERIVX(DX,T,tx,ALX,NPX,NDX,MXPy)
!
! TREAT THE INTERFACE POINTS
!
      ip = 0
      DO j = 1 , NDX - 1
         ip = ip + NPX(j) + 1
         DO k = 1 , MXPy
            ux(ip,k) = ux(ip+1,k)
            vx(ip,k) = vx(ip+1,k)
            tx(ip,k) = tx(ip+1,k)
         ENDDO
      ENDDO
!
      CALL DERIVY(DY,U,uy,ALY,NPY,NDY,MXPx)
      CALL DERIVY(DY,V,vy,ALY,NPY,NDY,MXPx)
      CALL DERIVY(DY,T,ty,ALY,NPY,NDY,MXPx)
! INTERFACE POINTS
      ip = 0
      DO j = 1 , NDY - 1
         ip = ip + NPY(j) + 1
         DO k = 1 , MXPx
            uy(k,ip) = uy(k,ip+1)
            vy(k,ip) = vy(k,ip+1)
            ty(k,ip) = ty(k,ip+1)
         ENDDO
      ENDDO
!
! COMPUTE THE FLUX TERMS
!
      DO i = 1 , MXPx
         DO j = 1 , MXPy
!
! compute vanleer fluxes
!
            ac = DSQRT(GMA*P(i,j)/RHO(i,j))
            ub = (xix(i,j)*U(i,j)+xy(i,j)*V(i,j))/gxi(i,j)
            vb = (ex(i,j)*U(i,j)+ey(i,j)*V(i,j))/ge(i,j)
            xm = ub/ac
            ym = vb/ac
            kx = xix(i,j)/gxi(i,j)
            ky = xy(i,j)/gxi(i,j)
            fmp = (RHO(i,j)*ac*(xm+1.0)**2)*xq
            fmm = -(RHO(i,j)*ac*(xm-1.0)**2)*xq
            fep = (-(GMA-1.0)*ub**2+2.0*(GMA-1.0)*ub*ac+2.*ac**2)       &
     &            /(GMA**2-1.0) + (U(i,j)**2+V(i,j)**2)*xh
            fem = (-(GMA-1.0)*ub**2-2.0*(GMA-1.0)*ub*ac+2.*ac**2)       &
     &            /(GMA**2-1.0) + (U(i,j)**2+V(i,j)**2)*xh
            FP1(i,j) = gxi(i,j)*fmp/jac(i,j)
            FM1(i,j) = gxi(i,j)*fmm/jac(i,j)
            FP2(i,j) = FP1(i,j)*(kx*(-ub+2.0*ac)/GMA+U(i,j))
            FM2(i,j) = FM1(i,j)*(kx*(-ub-2.0*ac)/GMA+U(i,j))
            FP3(i,j) = FP1(i,j)*(ky*(-ub+2.0*ac)/GMA+V(i,j))
            FM3(i,j) = FM1(i,j)*(ky*(-ub-2.0*ac)/GMA+V(i,j))
            FP4(i,j) = FP1(i,j)*fep
            FM4(i,j) = FM1(i,j)*fem
!
! MODIFY FOR SUPERSONIC FLOWS
!
            IF ( xm.GT.1.0 ) THEN
               FP1(i,j) = FP1(i,j) + FM1(i,j)
               FM1(i,j) = 0.0
               FP2(i,j) = FP2(i,j) + FM2(i,j)
               FM2(i,j) = 0.0
               FP3(i,j) = FP3(i,j) + FM3(i,j)
               FM3(i,j) = 0.0
               FP4(i,j) = FP4(i,j) + FM4(i,j)
               FM4(i,j) = 0.0
            ENDIF
!
            IF ( xm.LT.-1.0 ) THEN
               FM1(i,j) = FP1(i,j) + FM1(i,j)
               FP1(i,j) = 0.0
               FM2(i,j) = FP2(i,j) + FM2(i,j)
               FP2(i,j) = 0.0
               FM3(i,j) = FP3(i,j) + FM3(i,j)
               FP3(i,j) = 0.0
               FM4(i,j) = FP4(i,j) + FM4(i,j)
               FP4(i,j) = 0.0
            ENDIF
!
            kx = ex(i,j)/ge(i,j)
            ky = ey(i,j)/ge(i,j)
            gmp = (RHO(i,j)*ac*(ym+1.0)**2)*xq
            gmm = -(RHO(i,j)*ac*(ym-1.0)**2)*xq
            gep = (-(GMA-1.0)*vb**2+2.0*(GMA-1.0)*vb*ac+2.*ac**2)       &
     &            /(GMA**2-1.0) + (U(i,j)**2+V(i,j)**2)*xh
            gem = (-(GMA-1.0)*vb**2-2.0*(GMA-1.0)*vb*ac+2.*ac**2)       &
     &            /(GMA**2-1.0) + (U(i,j)**2+V(i,j)**2)*xh
            GP1(i,j) = ge(i,j)*gmp/jac(i,j)
            GM1(i,j) = ge(i,j)*gmm/jac(i,j)
            GP2(i,j) = GP1(i,j)*(kx*(-vb+2.0*ac)/GMA+U(i,j))
            GM2(i,j) = GM1(i,j)*(kx*(-vb-2.0*ac)/GMA+U(i,j))
            GP3(i,j) = GP1(i,j)*(ky*(-vb+2.0*ac)/GMA+V(i,j))
            GM3(i,j) = GM1(i,j)*(ky*(-vb-2.0*ac)/GMA+V(i,j))
            GP4(i,j) = GP1(i,j)*gep
            GM4(i,j) = GM1(i,j)*gem
!
! MODIFY FOR SUPERSONIC FLOWS
!
            IF ( ym.GT.1.0 ) THEN
               GP1(i,j) = GP1(i,j) + GM1(i,j)
               GM1(i,j) = 0.0
               GP2(i,j) = GP2(i,j) + GM2(i,j)
               GM2(i,j) = 0.0
               GP3(i,j) = GP3(i,j) + GM3(i,j)
               GM3(i,j) = 0.0
               GP4(i,j) = GP4(i,j) + GM4(i,j)
               GM4(i,j) = 0.0
            ENDIF
!
            IF ( ym.LT.-1.0 ) THEN
               GM1(i,j) = GP1(i,j) + GM1(i,j)
               GP1(i,j) = 0.0
               GM2(i,j) = GP2(i,j) + GM2(i,j)
               GP2(i,j) = 0.0
               GM3(i,j) = GP3(i,j) + GM3(i,j)
               GP3(i,j) = 0.0
               GM4(i,j) = GP4(i,j) + GM4(i,j)
               GP4(i,j) = 0.0
            ENDIF
!
! VISCOUS FLUXES
!
            mu = mu0*(T(i,j)/t02)**1.5*(t02+110.56)/(T(i,j)+110.56)
            kk = mu*cp/pr
            tuxx = 2.0/3.0*mu*(2.0*(xix(i,j)*ux(i,j)+ex(i,j)*uy(i,j))   &
     &             -(xy(i,j)*vx(i,j)+ey(i,j)*vy(i,j)))
            tuyy = 2.0/3.0*mu*(2.0*(xy(i,j)*vx(i,j)+ey(i,j)*vy(i,j))    &
     &             -(xix(i,j)*ux(i,j)+ex(i,j)*uy(i,j)))
            tuxy = mu*(xy(i,j)*ux(i,j)+ey(i,j)*uy(i,j)+xix(i,j)*vx(i,j) &
     &             +ex(i,j)*vy(i,j))
            qx = -kk*(xix(i,j)*tx(i,j)+ex(i,j)*ty(i,j))
            qy = -kk*(xy(i,j)*tx(i,j)+ey(i,j)*ty(i,j))
            fv44 = U(i,j)*tuxx + V(i,j)*tuxy - qx
            gv44 = U(i,j)*tuxy + V(i,j)*tuyy - qy
!
            FV2(i,j) = (xix(i,j)*tuxx+xy(i,j)*tuxy)/jac(i,j)
            GV2(i,j) = (ex(i,j)*tuxx+ey(i,j)*tuxy)/jac(i,j)
            FV3(i,j) = (xix(i,j)*tuxy+xy(i,j)*tuyy)/jac(i,j)
            GV3(i,j) = (ex(i,j)*tuxy+ey(i,j)*tuyy)/jac(i,j)
            FV4(i,j) = (xix(i,j)*fv44+xy(i,j)*gv44)/jac(i,j)
            GV4(i,j) = (ex(i,j)*fv44+ey(i,j)*gv44)/jac(i,j)
!
! NOW COMPUTE THE MODIFIED FBAR AND GBAR TO TAKE INTO ACCOUNT THE
! TRANSFORMATION
!
            f1(i,j) = FP1(i,j) + FM1(i,j)
            f2(i,j) = FP2(i,j) + FM2(i,j) - FV2(i,j)
            f3(i,j) = FP3(i,j) + FM3(i,j) - FV3(i,j)
            f4(i,j) = FP4(i,j) + FM4(i,j) - FV4(i,j)
 
            g1(i,j) = GP1(i,j) + GM1(i,j)
            g2(i,j) = GP2(i,j) + GM2(i,j) - GV2(i,j)
            g3(i,j) = GP3(i,j) + GM3(i,j) - GV3(i,j)
            g4(i,j) = GP4(i,j) + GM4(i,j) - GV4(i,j)
         ENDDO
      ENDDO
!
! FIND THE DERIVATIVE OF FBAR AND GBAR
!
      CALL DERIVX(DX,f1,f1bx,ALX,NPX,NDX,MXPy)
      CALL DERIVX(DX,f2,f2bx,ALX,NPX,NDX,MXPy)
      CALL DERIVX(DX,f3,f3bx,ALX,NPX,NDX,MXPy)
      CALL DERIVX(DX,f4,f4bx,ALX,NPX,NDX,MXPy)
! DERIVATIVES AT THE INTERFACE POINTS
!
      CALL FVSPLTX2
      ip = 0
      DO j = 1 , NDX - 1
         ip = ip + NPX(j) + 1
         DO k = 1 , MXPy
            f1bx(ip,k) = FP1x(j,k) + FM1x(j+1,k)
            f1bx(ip+1,k) = f1bx(ip,k)
            f2bx(ip,k) = FP2x(j,k) + FM2x(j+1,k) - DXP2(j,k)
            f2bx(ip+1,k) = f2bx(ip,k)
            f3bx(ip,k) = FP3x(j,k) + FM3x(j+1,k) - DXP3(j,k)
            f3bx(ip+1,k) = f3bx(ip,k)
            f4bx(ip,k) = FP4x(j,k) + FM4x(j+1,k) - DXP4(j,k)
            f4bx(ip+1,k) = f4bx(ip,k)
         ENDDO
      ENDDO
!
! REPLACE THE CORRECT FLUX VECTORS AND DERIVATIVES
!
      CALL DERIVY(DY,g1,g1by,ALY,NPY,NDY,MXPx)
      CALL DERIVY(DY,g2,g2by,ALY,NPY,NDY,MXPx)
      CALL DERIVY(DY,g3,g3by,ALY,NPY,NDY,MXPx)
      CALL DERIVY(DY,g4,g4by,ALY,NPY,NDY,MXPx)
!
! Y INTERFACE CONDITIONS
!
      CALL FVSPLTY2
      ip = 0
      DO j = 1 , NDY - 1
         ip = ip + NPY(j) + 1
         DO k = 1 , MXPx
            g1by(k,ip) = GP1y(j,k) + GM1y(j+1,k)
            g1by(k,ip+1) = g1by(k,ip)
            g2by(k,ip) = GP2y(j,k) + GM2y(j+1,k) - DYP2(j,k)
            g2by(k,ip+1) = g2by(k,ip)
            g3by(k,ip) = GP3y(j,k) + GM3y(j+1,k) - DYP3(j,k)
            g3by(k,ip+1) = g3by(k,ip)
            g4by(k,ip) = GP4y(j,k) + GM4y(j+1,k) - DYP4(j,k)
            g4by(k,ip+1) = g4by(k,ip)
         ENDDO
      ENDDO
 
!
! IMPLEMENT THE SECOND STEP OF THE INTEGRATION
!
      DO i = 1 , MXPx
         DO j = 1 , MXPy
            u1o(i,j) = u1(i,j)
            u2o(i,j) = u2(i,j)
            u3o(i,j) = u3(i,j)
            u4o(i,j) = u4(i,j)
            u1(i,j) = .5*(u1(i,j)-dtt(i,j)*f1bx(i,j)*jac(i,j)+u1b(i,j)  &
     &                -dtt(i,j)*g1by(i,j)*jac(i,j))
            u2(i,j) = .5*(u2(i,j)-dtt(i,j)*f2bx(i,j)*jac(i,j)+u2b(i,j)  &
     &                -dtt(i,j)*g2by(i,j)*jac(i,j))
            u3(i,j) = .5*(u3(i,j)-dtt(i,j)*f3bx(i,j)*jac(i,j)+u3b(i,j)  &
     &                -dtt(i,j)*g3by(i,j)*jac(i,j)+dtt(i,j)*RHO(i,j)    &
     &                *grav*jac(i,j))
            u4(i,j) = .5*(u4(i,j)-dtt(i,j)*f4bx(i,j)*jac(i,j)+u4b(i,j)  &
     &                -dtt(i,j)*g4by(i,j)*jac(i,j)-dtt(i,j)*RHO(i,j)    &
     &                *V(i,j)*grav*jac(i,j))
         ENDDO
      ENDDO
!
! FIND STATE VARIABLES AGAIN:
!
      CALL STATE(u1,u2,u3,u4,U,V,RHO,P,E,T,MXPx,MXPy)
 
!
! Inlet Conditions
!
 
      CALL INLET(my11,MXPy,mx11,u1,u2,u3,u4,1)
!
! EXIT CONDITIONS
!
      CALL AEXIT(my11,MXPy,MXPx,u1,u2,u3,u4,1)
!
! BOTTOM WALL CONDITIONS FOR SUBSONIC FLOW
!
      CALL BOTWALL(nx11,MXPx,my11,MXPx,u1,u2,u3,u4,1)
!
! TOPWALL  CONDITIONS
!
      CALL TOPWALL(nx11+1,MXPx-1,MXPy,MXPx,u1,u2,u3,u4,1)
!
! UPDATE THE FLUX VECTORS
!
! COMPUTE THE DERIVATIVES FOR THE VISCOUS TERMS
!
 4000 CALL DERIVX(DX,U,ux,ALX,NPX,NDX,MXPy)
      CALL DERIVX(DX,V,vx,ALX,NPX,NDX,MXPy)
      CALL DERIVX(DX,T,tx,ALX,NPX,NDX,MXPy)
!
! TREAT THE INTERFACE POINTS
!
      ip = 0
      DO j = 1 , NDX - 1
         ip = ip + NPX(j) + 1
         DO k = 1 , MXPy
            ux(ip,k) = ux(ip+1,k)
            vx(ip,k) = vx(ip+1,k)
            tx(ip,k) = tx(ip+1,k)
         ENDDO
      ENDDO
!
      CALL DERIVY(DY,U,uy,ALY,NPY,NDY,MXPx)
      CALL DERIVY(DY,V,vy,ALY,NPY,NDY,MXPx)
      CALL DERIVY(DY,T,ty,ALY,NPY,NDY,MXPx)
! INTERFACE POINTS
      ip = 0
      DO j = 1 , NDY - 1
         ip = ip + NPY(j) + 1
         DO k = 1 , MXPx
            uy(k,ip) = uy(k,ip+1)
            vy(k,ip) = vy(k,ip+1)
            ty(k,ip) = ty(k,ip+1)
         ENDDO
      ENDDO
!
! COMPUTE THE FLUX TERMS
      DO i = 1 , MXPx
         DO j = 1 , MXPy
!
! compute vanleer fluxes
!
            ac = DSQRT(GMA*P(i,j)/RHO(i,j))
            ub = (xix(i,j)*U(i,j)+xy(i,j)*V(i,j))/gxi(i,j)
            vb = (ex(i,j)*U(i,j)+ey(i,j)*V(i,j))/ge(i,j)
            xm = ub/ac
            ym = vb/ac
            kx = xix(i,j)/gxi(i,j)
            ky = xy(i,j)/gxi(i,j)
            fmp = (RHO(i,j)*ac*(xm+1.0)**2)*xq
            fmm = -(RHO(i,j)*ac*(xm-1.0)**2)*xq
            fep = (-(GMA-1.0)*ub**2+2.0*(GMA-1.0)*ub*ac+2.*ac**2)       &
     &            /(GMA**2-1.0) + (U(i,j)**2+V(i,j)**2)*xh
            fem = (-(GMA-1.0)*ub**2-2.0*(GMA-1.0)*ub*ac+2.*ac**2)       &
     &            /(GMA**2-1.0) + (U(i,j)**2+V(i,j)**2)*xh
            FP1(i,j) = gxi(i,j)*fmp/jac(i,j)
            FM1(i,j) = gxi(i,j)*fmm/jac(i,j)
            FP2(i,j) = FP1(i,j)*(kx*(-ub+2.0*ac)/GMA+U(i,j))
            FM2(i,j) = FM1(i,j)*(kx*(-ub-2.0*ac)/GMA+U(i,j))
            FP3(i,j) = FP1(i,j)*(ky*(-ub+2.0*ac)/GMA+V(i,j))
            FM3(i,j) = FM1(i,j)*(ky*(-ub-2.0*ac)/GMA+V(i,j))
            FP4(i,j) = FP1(i,j)*fep
            FM4(i,j) = FM1(i,j)*fem
!
! MODIFY OF SUPERSONIC FLOWS
!
            IF ( xm.GT.1.0 ) THEN
               FP1(i,j) = FP1(i,j) + FM1(i,j)
               FM1(i,j) = 0.0
               FP2(i,j) = FP2(i,j) + FM2(i,j)
               FM2(i,j) = 0.0
               FP3(i,j) = FP3(i,j) + FM3(i,j)
               FM3(i,j) = 0.0
               FP4(i,j) = FP4(i,j) + FM4(i,j)
               FM4(i,j) = 0.0
            ENDIF
!
            IF ( xm.LT.-1.0 ) THEN
               FM1(i,j) = FP1(i,j) + FM1(i,j)
               FP1(i,j) = 0.0
               FM2(i,j) = FP2(i,j) + FM2(i,j)
               FP2(i,j) = 0.0
               FM3(i,j) = FP3(i,j) + FM3(i,j)
               FP3(i,j) = 0.0
               FM4(i,j) = FP4(i,j) + FM4(i,j)
               FP4(i,j) = 0.0
            ENDIF
!
            kx = ex(i,j)/ge(i,j)
            ky = ey(i,j)/ge(i,j)
            gmp = (RHO(i,j)*ac*(ym+1.0)**2)*xq
            gmm = -(RHO(i,j)*ac*(ym-1.0)**2)*xq
            gep = (-(GMA-1.0)*vb**2+2.0*(GMA-1.0)*vb*ac+2.*ac**2)       &
     &            /(GMA**2-1.0) + (U(i,j)**2+V(i,j)**2)*xh
            gem = (-(GMA-1.0)*vb**2-2.0*(GMA-1.0)*vb*ac+2.*ac**2)       &
     &            /(GMA**2-1.0) + (U(i,j)**2+V(i,j)**2)*xh
            GP1(i,j) = ge(i,j)*gmp/jac(i,j)
            GM1(i,j) = ge(i,j)*gmm/jac(i,j)
            GP2(i,j) = GP1(i,j)*(kx*(-vb+2.0*ac)/GMA+U(i,j))
            GM2(i,j) = GM1(i,j)*(kx*(-vb-2.0*ac)/GMA+U(i,j))
            GP3(i,j) = GP1(i,j)*(ky*(-vb+2.0*ac)/GMA+V(i,j))
            GM3(i,j) = GM1(i,j)*(ky*(-vb-2.0*ac)/GMA+V(i,j))
            GP4(i,j) = GP1(i,j)*gep
            GM4(i,j) = GM1(i,j)*gem
!
! MODIFY for SUPERSONIC FLOWS
!
            IF ( ym.GT.1.0 ) THEN
               GP1(i,j) = GP1(i,j) + GM1(i,j)
               GM1(i,j) = 0.0
               GP2(i,j) = GP2(i,j) + GM2(i,j)
               GM2(i,j) = 0.0
               GP3(i,j) = GP3(i,j) + GM3(i,j)
               GM3(i,j) = 0.0
               GP4(i,j) = GP4(i,j) + GM4(i,j)
               GM4(i,j) = 0.0
            ENDIF
!
            IF ( ym.LT.-1.0 ) THEN
               GM1(i,j) = GP1(i,j) + GM1(i,j)
               GP1(i,j) = 0.0
               GM2(i,j) = GP2(i,j) + GM2(i,j)
               GP2(i,j) = 0.0
               GM3(i,j) = GP3(i,j) + GM3(i,j)
               GP3(i,j) = 0.0
               GM4(i,j) = GP4(i,j) + GM4(i,j)
               GP4(i,j) = 0.0
            ENDIF
!
! VISCOUS FLUXES
!
            mu = mu0*(T(i,j)/t02)**1.5*(t02+110.56)/(T(i,j)+110.56)
            xmu(i,j) = mu
            kk = mu*cp/pr
            tuxx = 2.0/3.0*mu*(2.0*(xix(i,j)*ux(i,j)+ex(i,j)*uy(i,j))   &
     &             -(xy(i,j)*vx(i,j)+ey(i,j)*vy(i,j)))
            tuyy = 2.0/3.0*mu*(2.0*(xy(i,j)*vx(i,j)+ey(i,j)*vy(i,j))    &
     &             -(xix(i,j)*ux(i,j)+ex(i,j)*uy(i,j)))
            tuxy = mu*(xy(i,j)*ux(i,j)+ey(i,j)*uy(i,j)+xix(i,j)*vx(i,j) &
     &             +ex(i,j)*vy(i,j))
            qx = -kk*(xix(i,j)*tx(i,j)+ex(i,j)*ty(i,j))
            qy = -kk*(xy(i,j)*tx(i,j)+ey(i,j)*ty(i,j))
            fv44 = U(i,j)*tuxx + V(i,j)*tuxy - qx
            gv44 = U(i,j)*tuxy + V(i,j)*tuyy - qy
!
            FV2(i,j) = (xix(i,j)*tuxx+xy(i,j)*tuxy)/jac(i,j)
            GV2(i,j) = (ex(i,j)*tuxx+ey(i,j)*tuxy)/jac(i,j)
            FV3(i,j) = (xix(i,j)*tuxy+xy(i,j)*tuyy)/jac(i,j)
            GV3(i,j) = (ex(i,j)*tuxy+ey(i,j)*tuyy)/jac(i,j)
            FV4(i,j) = (xix(i,j)*fv44+xy(i,j)*gv44)/jac(i,j)
            GV4(i,j) = (ex(i,j)*fv44+ey(i,j)*gv44)/jac(i,j)
!
! NOW COMPUTE THE MODIFIED FBAR AND GBAR TO TAKE INTO ACCOUNT THE
! TRANSFORMATION
!
            f1(i,j) = FP1(i,j) + FM1(i,j)
            f2(i,j) = FP2(i,j) + FM2(i,j) - FV2(i,j)
            f3(i,j) = FP3(i,j) + FM3(i,j) - FV3(i,j)
            f4(i,j) = FP4(i,j) + FM4(i,j) - FV4(i,j)
 
            g1(i,j) = GP1(i,j) + GM1(i,j)
            g2(i,j) = GP2(i,j) + GM2(i,j) - GV2(i,j)
            g3(i,j) = GP3(i,j) + GM3(i,j) - GV3(i,j)
            g4(i,j) = GP4(i,j) + GM4(i,j) - GV4(i,j)
         ENDDO
      ENDDO
 
      CALL DERIVX(DX,f1,f1x,ALX,NPX,NDX,MXPy)
      CALL DERIVX(DX,f2,f2x,ALX,NPX,NDX,MXPy)
      CALL DERIVX(DX,f3,f3x,ALX,NPX,NDX,MXPy)
      CALL DERIVX(DX,f4,f4x,ALX,NPX,NDX,MXPy)
!
! INTERFACE CONDITIONS; CORRECT CHARACTERISITIC VARIABLES AT INTERFACE
!
      CALL FVSPLTX2
!
      ip = 0
      DO j = 1 , NDX - 1
         ip = ip + NPX(j) + 1
         DO k = 1 , MXPy
            f1x(ip,k) = FP1x(j,k) + FM1x(j+1,k)
            f1x(ip+1,k) = f1x(ip,k)
            f2x(ip,k) = FP2x(j,k) + FM2x(j+1,k) - DXP2(j,k)
            f2x(ip+1,k) = f2x(ip,k)
            f3x(ip,k) = FP3x(j,k) + FM3x(j+1,k) - DXP3(j,k)
            f3x(ip+1,k) = f3x(ip,k)
            f4x(ip,k) = FP4x(j,k) + FM4x(j+1,k) - DXP4(j,k)
            f4x(ip+1,k) = f4x(ip,k)
         ENDDO
      ENDDO
 
      CALL DERIVY(DY,g1,g1y,ALY,NPY,NDY,MXPx)
      CALL DERIVY(DY,g2,g2y,ALY,NPY,NDY,MXPx)
      CALL DERIVY(DY,g3,g3y,ALY,NPY,NDY,MXPx)
      CALL DERIVY(DY,g4,g4y,ALY,NPY,NDY,MXPx)
!
! Y INTERFACE CONDITIONS
!
      CALL FVSPLTY2
!
      ip = 0
      DO j = 1 , NDY - 1
         ip = ip + NPY(j) + 1
         DO k = 1 , MXPx
            g1y(k,ip) = GP1y(j,k) + GM1y(j+1,k)
            g1y(k,ip+1) = g1y(k,ip)
            g2y(k,ip) = GP2y(j,k) + GM2y(j+1,k) - DYP2(j,k)
            g2y(k,ip+1) = g2y(k,ip)
            g3y(k,ip) = GP3y(j,k) + GM3y(j+1,k) - DYP3(j,k)
            g3y(k,ip+1) = g3y(k,ip)
            g4y(k,ip) = GP4y(j,k) + GM4y(j+1,k) - DYP4(j,k)
            g4y(k,ip+1) = g4y(k,ip)
         ENDDO
      ENDDO
!
! FIND THE LOCAL TIME STEPS
!
      dt = 100
      DO i = 1 , MXPx
         DO j = 1 , MXPy
            as = DSQRT(P(i,j)/RHO(i,j)*GMA)
            rdltx = RHO(i,j)*DABS(U(i,j))*ddx(i,j)/xmu(i,j)
            rdlty = RHO(i,j)*DABS(V(i,j))*ddy(i,j)/xmu(i,j)
            IF ( rdltx.LE.rdlty ) THEN
               rdlt = rdltx
            ELSE
               rdlt = rdlty
            ENDIF
            dxiv = as*DSQRT(1.0/ddx(i,j)**2+1./ddy(i,j)**2)
            dtcfl = 1./(DABS(U(i,j))/ddx(i,j)+DABS(V(i,j))/ddy(i,j)     &
     &              +dxiv)
!                 if (rdlt.gt.0) then
!                 dtt(i,j)=sig5*dtcfl/(1.0+2.0/rdlt)
!                 else
            dtt(i,j) = sig5*dtcfl
!                 endif
            IF ( dtt(i,j).LE.dt ) dt = dtt(i,j)
!        write(6,*) dtt(i,j),ddx(i,j),as,u(i,j),v(i,j)
         ENDDO
      ENDDO
      ttt = ttt + dt
      it = it + 1
      IF ( dt.EQ.0. ) THEN
         WRITE (6,*) ' a zero delta time was detected' , sig5 , dtcf1
         GOTO 20
      ENDIF
!
! find the local subdomain time
!
      minx = 1
      maxx = 0
      miny = 1
      maxy = 0
      DO ix = 1 , NDX
         maxx = maxx + NPX(ix) + 1
         miny = 1
         maxy = 0
         DO iy = 1 , NDY
            maxy = maxy + NPY(iy) + 1
            dtd = 100.0
            DO i = minx , maxx
               DO j = miny , maxy
                  IF ( dtt(i,j).LE.dtd ) dtd = dtt(i,j)
               ENDDO
            ENDDO
            DO i = minx , maxx
               DO j = miny , maxy
                  dtt(i,j) = dtd
               ENDDO
            ENDDO
            miny = miny + NPY(iy) + 1
         ENDDO
         minx = minx + NPX(ix) + 1
      ENDDO
!
! change the interface points
!
      ip = 0
      DO j = 1 , NDX - 1
         ip = ip + NPX(j) + 1
         DO k = 1 , MXPy
!                  if(dtt(ip+1,k).le.dtt(ip,k)) then
!                    dtt(ip,k)=dtt(ip+1,k)
            dtt(ip,k) = (dtt(ip+1,k)+dtt(ip,k))/2.0
!                  else
            dtt(ip+1,k) = dtt(ip,k)
!                  endif
         ENDDO
      ENDDO
      ip = 0
      DO j = 1 , NDY - 1
         ip = ip + NPY(j) + 1
         DO k = 1 , MXPx
!        write(6,*) j,k,dtt(k,ip+1),dtt(k,ip)
!                  if(dtt(k,ip+1).le.dtt(k,ip) ) then
!                    dtt(k,ip)=dtt(k,ip+1)
            dtt(k,ip) = (dtt(k,ip+1)+dtt(k,ip))/2.0
!                  else
            dtt(k,ip+1) = dtt(k,ip)
!                  endif
         ENDDO
      ENDDO
!
! convergence history
!
      icn2 = icn2 + 1
      kwrt = ft/dt/200.
      IF ( icn2.GE.kwrt ) THEN
         icn1 = icn1 + 1
         icn2 = 0
         iter1(icn1) = it
         con1 = 0.0
         DO i = 1 , MXPx
            DO j = 1 , MXPy
               con1 = con1 + DABS(u1(i,j)-u1o(i,j))/dtt(i,j)
            ENDDO
         ENDDO
         con1 = DLOG10(con1/DBLE(MXPy*MXPx))
         conv1(icn1) = con1
         WRITE (6,2121) it , ttt , ft , con1
 2121    FORMAT (1x,I8,2x,f16.12,2x,f11.8,2x,f10.6)
      ENDIF
!
      GOTO 2001
!
! END TIME MARCHING
!
 20   CONTINUE
      CONTINUE
      WRITE (6,*) 'deltat, final t, iterations'
      WRITE (6,*) dt , ttt , it
!
!  CHECK FOR CONVERGENCE
!
      con1 = 0.0
      con2 = 0.0
      con3 = 0.0
      con4 = 0.0
      DO i = 1 , MXPx
         DO j = 1 , MXPy
            con1 = con1 + DABS(u1(i,j)-u1o(i,j))/dtt(i,j)
            con2 = con2 + DABS(u2(i,j)-u2o(i,j))/dtt(i,j)
            con3 = con3 + DABS(u3(i,j)-u3o(i,j))/dtt(i,j)
            con4 = con4 + DABS(u4(i,j)-u4o(i,j))/dtt(i,j)
         ENDDO
      ENDDO
      con1 = DLOG10(con1/DBLE(MXPy*MXPx))
      con2 = DLOG10(con2/DBLE(MXPy*MXPx))
      con3 = DLOG10(con3/DBLE(MXPy*MXPx))
      con4 = DLOG10(con4/DBLE(MXPy*MXPx))
!
! DISPLAY THE RESULTS
!
      OPEN (UNIT=8,FILE='mach.out',STATUS='UNKNOWN')
      OPEN (UNIT=7,FILE='pres.out',STATUS='UNKNOWN')
      OPEN (UNIT=9,FILE='rho.out',STATUS='UNKNOWN')
      OPEN (UNIT=11,FILE='totp.out',STATUS='UNKNOWN')
      OPEN (UNIT=12,FILE='u.out',STATUS='UNKNOWN')
      OPEN (UNIT=13,FILE='v.out',STATUS='unknown')
      OPEN (UNIT=14,FILE='conv.out',STATUS='unknown')
      OPEN (UNIT=16,FILE='sl.out',STATUS='unknown')
!	open(unit=17,file='chist.out',status='unknown')
      DO i = 1 , icn1
         WRITE (*,*) iter1(i) , conv1(i)
      ENDDO
!	close(unit=17)
      WRITE (16,*) ' VARIABLES=X,Y,U,V,PRES,RHO,TEMP,TOTP,VORT'
      WRITE (16,*) ' ZONE   I=' , MXPx , ' , J=' , MXPy
 
      WRITE (14,*) con1
      WRITE (14,*) con2
      WRITE (14,*) con3
      WRITE (14,*) con4
      CLOSE (UNIT=14)
      WRITE (7,*) MXPx , MXPy
      WRITE (8,*) MXPx , MXPy
      WRITE (9,*) MXPx , MXPy
      WRITE (13,*) MXPx , MXPy
      WRITE (12,*) MXPx , MXPy
      WRITE (11,*) MXPx , MXPy
      DO jj = 1 , MXPy
         DO i = 1 , MXPx
            xma = DSQRT(RHO(i,jj)*(U(i,jj)**2+V(i,jj)**2)/(GMA*P(i,jj)))
            WRITE (9,*) xp1(i,jj) , yp1(i,jj) , RHO(i,jj)/rho0
            WRITE (7,*) xp1(i,jj) , yp1(i,jj) , P(i,jj)/P0
            WRITE (8,*) xp1(i,jj) , yp1(i,jj) ,                         &
     &                  DSQRT(RHO(i,jj)*(U(i,jj)**2+V(i,jj)**2)         &
     &                  /(GMA*P(i,jj)))
            ptot = P(i,jj)*(1.+(GMA-1.)*xma**2/2.)**(GMA/(GMA-1.))/P0
            WRITE (11,*) xp1(i,jj) , yp1(i,jj) , ptot
            WRITE (12,*) xp1(i,jj) , yp1(i,jj) , U(i,jj)/a0
            WRITE (13,*) xp1(i,jj) , yp1(i,jj) , V(i,jj)/a0
            vort = vx(i,jj) - uy(i,jj)
            WRITE (16,*) xp1(i,jj) , yp1(i,jj) , U(i,jj) , V(i,jj) ,    &
     &                   P(i,jj) , RHO(i,jj) , T(i,jj) , ptot , vort
         ENDDO
      ENDDO
      CLOSE (UNIT=13)
      CLOSE (UNIT=12)
      CLOSE (UNIT=11)
      CLOSE (UNIT=9)
      CLOSE (UNIT=8)
      CLOSE (UNIT=16)
      CLOSE (UNIT=7)
      CONTINUE
      END
!*==STATE.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
! SUBROUTINE TO DECODE THE STATE VARIABLES
!
 
      SUBROUTINE STATE(U1,U2,U3,U4,U,V,Rho,P,E,T,Mx,My)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION U(NX,NY) , Rho(NX,NY) , V(NX,NY) , E(NX,NY)
      DIMENSION P(NX,NY) , U1(NX,NY) , U2(NX,NY) , U3(NX,NY)
      DIMENSION U4(NX,NY) , T(NX,NY)
      gma = DBLE(14)/DBLE(10)
      one = DBLE(1)
      p5 = DBLE(5)/DBLE(10)
      gcn = (gma-one)
      rr = 287.0
      DO i = 1 , Mx
         DO j = 1 , My
            Rho(i,j) = U1(i,j)
            E(i,j) = U4(i,j)/U1(i,j)
            U(i,j) = U2(i,j)/U1(i,j)
            V(i,j) = U3(i,j)/U1(i,j)
            P(i,j) = gcn*Rho(i,j)                                       &
     &               *(E(i,j)-p5*(V(i,j)*V(i,j)+U(i,j)*U(i,j)))
            T(i,j) = P(i,j)/(Rho(i,j)*rr)
!          WRITE(6,*) U(I,J),V(I,J),I,J
         ENDDO
      ENDDO
!       WRITE(6,*)
      CONTINUE
      END
!*==SPECTOP.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
! other routines
!
      SUBROUTINE SPECTOP(Dr,N)
      IMPLICIT REAL*8(A-H,o-Z)
      DIMENSION d1(0:32,0:32) , Dr(0:32,0:32) , x(0:32)
      REAL*8 Dr
!
! PROGRAM TO COMPUTE THE CHEBYSHEV SPECTRAL OPERATOR
!
      ang = DBLE(1)
      s = DBLE(6)
      o = DBLE(1)
      t = DBLE(2)
      pi = t*DASIN(ang)
      DO i = 0 , N
         x(i) = DCOS(pi*DBLE(i)/DBLE(N))
      ENDDO
!
! IF J=K
!
      DO j = 1 , N - 1
         d1(j,j) = -x(j)/(t*(o-x(j)**2))
      ENDDO
      d1(0,0) = (t*DBLE(N)**2+o)/s
      d1(N,N) = -d1(0,0)
!
! IF J.NE.K
!
      fctr1 = 1.0D0
      DO k = 0 , N
         ck = 1.0D0
         IF ( k.EQ.0 ) ck = t
         IF ( k.EQ.N ) ck = t
         fctr2 = o
         DO j = 0 , N
            cj = o
            IF ( j.EQ.0 ) cj = t
            IF ( j.EQ.N ) cj = t
            fctr = fctr1*fctr2
            IF ( j.NE.k ) THEN
               d1(k,j) = ck*fctr/(cj*(x(k)-x(j)))
            ENDIF
            fctr2 = -o*fctr2
         ENDDO
         fctr1 = -o*fctr1
      ENDDO
      DO k = 0 , N
         DO j = 0 , N
            Dr(k,j) = d1(N-k,N-j)
         ENDDO
      ENDDO
      CONTINUE
      END
!*==BOUND.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
! PROGRAM TO FIND THE BOUNDARIES OF EACH DOMAIN
!
      SUBROUTINE BOUND(Al,Be,X1,X2,Nd)
      IMPLICIT REAL*8(A-H,o-Z)
      DIMENSION Al(30) , Be(30) , X1(30) , X2(30)
      t = DBLE(2)
      o = DBLE(1)
      DO j = 1 , Nd
         Al(j) = -t/(X1(j)-X2(j))
         Be(j) = o - X2(j)*Al(j)
      ENDDO
      CONTINUE
      END
!*==POINTS.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
! SUBROUTINE TO FIND THE NUMBER OF POINTS IN EACH DOMAIN, THEN FIND
! THE DERIVATIVE OPERATOR
!
      SUBROUTINE POINTS(D,Np,Nd)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION D(NX,33) , Np(30) , dd(0:32,0:32)
      jmin = 1
      jmax = 0
      DO i = 1 , Nd
!
! COMPUTE THE DERIVATIVE FOR EACH DOMAIN
!
         CALL SPECTOP(dd,Np(i))
         jmax = jmax + Np(i) + 1
         DO j = jmin , jmax
            DO k = 1 , Np(i) + 1
               D(j,k) = dd(j-jmin,k-1)
            ENDDO
         ENDDO
         jmin = jmin + Np(i) + 1
      ENDDO
      CONTINUE
      END
!*==FVSPLTX2.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
!
! SUBROUTINE TO COMPUTE THE DERIVATIVES
!
      SUBROUTINE FVSPLTX2
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION DX(NX,33) , ALX(30) , NPX(30)
      DIMENSION FP1(NX,NY) , FM1(NX,NY) , FP1x(30,NX) , FM1x(30,NX)
      DIMENSION FP2(NX,NY) , FM2(NX,NY) , FP2x(30,NX) , FM2x(30,NX)
      DIMENSION FP3(NX,NY) , FM3(NX,NY) , FP3x(30,NX) , FM3x(30,NX)
      DIMENSION FP4(NX,NY) , FM4(NX,NY) , FP4x(30,NX) , FM4x(30,NX)
      DIMENSION FV2(NX,NY) , DXP2(30,NX) , DXM2(30,NX)
      DIMENSION FV3(NX,NY) , DXP3(30,NX) , DXM3(30,NX)
      DIMENSION FV4(NX,NY) , DXP4(30,NX) , DXM4(30,NX)
      COMMON /XD1   / FP1 , FM1 , FP2 , FM2 , FP3 , FM3 , FP4 , FM4 ,   &
     &                FP1x , FM1x , FP2x , FM2x , FP3x , FM3x , FP4x ,  &
     &                FM4x , FV2 , FV3 , FV4 , DXP2 , DXM2 , DXP3 ,     &
     &                DXM3 , DXP4 , DXM4 , DX , NPX , ALX , NDX , MXPy
 
 
      DO ik = 1 , MXPy
         jmax = 0
         jmin = 1
         DO i = 1 , NDX
            jmax = jmax + NPX(i) + 1
!
! INITIALIZE
!
            FP1x(i,ik) = 0.
            FM1x(i,ik) = 0.
            FP2x(i,ik) = 0.
            FM2x(i,ik) = 0.
            FP3x(i,ik) = 0.
            FM3x(i,ik) = 0.
            FP4x(i,ik) = 0.
            FM4x(i,ik) = 0.
            DXP2(i,ik) = 0.
            DXM2(i,ik) = 0.
            DXP3(i,ik) = 0.
            DXM3(i,ik) = 0.
            DXP4(i,ik) = 0.
            DXM4(i,ik) = 0.
            DO k = 0 , NPX(i)
               jk = jmin + k
               FP1x(i,ik) = FP1x(i,ik) + DX(jmax,k+1)*FP1(jk,ik)
               FM1x(i,ik) = FM1x(i,ik) + DX(jmin,k+1)*FM1(jk,ik)
               FP2x(i,ik) = FP2x(i,ik) + DX(jmax,k+1)*FP2(jk,ik)
               FM2x(i,ik) = FM2x(i,ik) + DX(jmin,k+1)*FM2(jk,ik)
               FP3x(i,ik) = FP3x(i,ik) + DX(jmax,k+1)*FP3(jk,ik)
               FM3x(i,ik) = FM3x(i,ik) + DX(jmin,k+1)*FM3(jk,ik)
               FP4x(i,ik) = FP4x(i,ik) + DX(jmax,k+1)*FP4(jk,ik)
               FM4x(i,ik) = FM4x(i,ik) + DX(jmin,k+1)*FM4(jk,ik)
               DXP2(i,ik) = DXP2(i,ik) + DX(jmax,k+1)*FV2(jk,ik)
               DXM2(i,ik) = DXM2(i,ik) + DX(jmin,k+1)*FV2(jk,ik)
               DXP3(i,ik) = DXP3(i,ik) + DX(jmax,k+1)*FV3(jk,ik)
               DXM3(i,ik) = DXM3(i,ik) + DX(jmin,k+1)*FV3(jk,ik)
               DXP4(i,ik) = DXP4(i,ik) + DX(jmax,k+1)*FV4(jk,ik)
               DXM4(i,ik) = DXM4(i,ik) + DX(jmin,k+1)*FV4(jk,ik)
            ENDDO
            FP1x(i,ik) = FP1x(i,ik)*ALX(i)
            FM1x(i,ik) = FM1x(i,ik)*ALX(i)
            FP2x(i,ik) = FP2x(i,ik)*ALX(i)
            FM2x(i,ik) = FM2x(i,ik)*ALX(i)
            FP3x(i,ik) = FP3x(i,ik)*ALX(i)
            FM3x(i,ik) = FM3x(i,ik)*ALX(i)
            FP4x(i,ik) = FP4x(i,ik)*ALX(i)
            FM4x(i,ik) = FM4x(i,ik)*ALX(i)
            DXP2(i,ik) = DXP2(i,ik)*ALX(i)
            DXM2(i,ik) = DXM2(i,ik)*ALX(i)
            DXP3(i,ik) = DXP3(i,ik)*ALX(i)
            DXM3(i,ik) = DXM3(i,ik)*ALX(i)
            DXP4(i,ik) = DXP4(i,ik)*ALX(i)
            DXM4(i,ik) = DXM4(i,ik)*ALX(i)
            jmin = jmin + NPX(i) + 1
         ENDDO
      ENDDO
      CONTINUE
      END
!*==FVSPLTY2.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
! SUBROUTINE TO COMPUTE THE DERIVATIVES
!
      SUBROUTINE FVSPLTY2
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION DY(NX,33) , ALY(30) , NPY(30)
      DIMENSION GP1(NX,NY) , GM1(NX,NY) , GP1y(30,NY) , GM1y(30,NY)
      DIMENSION GP2(NX,NY) , GM2(NX,NY) , GP2y(30,NY) , GM2y(30,NY)
      DIMENSION GP3(NX,NY) , GM3(NX,NY) , GP3y(30,NY) , GM3y(30,NY)
      DIMENSION GP4(NX,NY) , GM4(NX,NY) , GP4y(30,NY) , GM4y(30,NY)
      DIMENSION GV2(NX,NY) , DYP2(30,NY) , DYM2(30,NY)
      DIMENSION GV3(NX,NY) , DYP3(30,NY) , DYM3(30,NY)
      DIMENSION GV4(NX,NY) , DYP4(30,NY) , DYM4(30,NY)
      COMMON /YD1   / GP1 , GM1 , GP2 , GM2 , GP3 , GM3 , GP4 , GM4 ,   &
     &                GP1y , GM1y , GP2y , GM2y , GP3y , GM3y , GP4y ,  &
     &                GM4y , GV2 , GV3 , GV4 , DYP2 , DYM2 , DYP3 ,     &
     &                DYM3 , DYP4 , DYM4 , DY , NPY , ALY , NDY , MXPx
      DO jk = 1 , MXPx
         jmax = 0
         jmin = 1
         DO i = 1 , NDY
            jmax = jmax + NPY(i) + 1
!
! INITIALIZE
!
            GP1y(i,jk) = 0.
            GM1y(i,jk) = 0.
            GP2y(i,jk) = 0.
            GM2y(i,jk) = 0.
            GP3y(i,jk) = 0.
            GM3y(i,jk) = 0.
            GP4y(i,jk) = 0.
            GM4y(i,jk) = 0.
            DYP2(i,jk) = 0.
            DYM2(i,jk) = 0.
            DYP3(i,jk) = 0.
            DYM3(i,jk) = 0.
            DYP4(i,jk) = 0.
            DYM4(i,jk) = 0.
            DO k = 0 , NPY(i)
               ik = jmin + k
               GP1y(i,jk) = GP1y(i,jk) + DY(jmax,k+1)*GP1(jk,ik)
               GM1y(i,jk) = GM1y(i,jk) + DY(jmin,k+1)*GM1(jk,ik)
               GP2y(i,jk) = GP2y(i,jk) + DY(jmax,k+1)*GP2(jk,ik)
               GM2y(i,jk) = GM2y(i,jk) + DY(jmin,k+1)*GM2(jk,ik)
               GP3y(i,jk) = GP3y(i,jk) + DY(jmax,k+1)*GP3(jk,ik)
               GM3y(i,jk) = GM3y(i,jk) + DY(jmin,k+1)*GM3(jk,ik)
               GP4y(i,jk) = GP4y(i,jk) + DY(jmax,k+1)*GP4(jk,ik)
               GM4y(i,jk) = GM4y(i,jk) + DY(jmin,k+1)*GM4(jk,ik)
               DYP2(i,jk) = DYP2(i,jk) + DY(jmax,k+1)*GV2(jk,ik)
               DYM2(i,jk) = DYM2(i,jk) + DY(jmin,k+1)*GV2(jk,ik)
               DYP3(i,jk) = DYP3(i,jk) + DY(jmax,k+1)*GV3(jk,ik)
               DYM3(i,jk) = DYM3(i,jk) + DY(jmin,k+1)*GV3(jk,ik)
               DYP4(i,jk) = DYP4(i,jk) + DY(jmax,k+1)*GV4(jk,ik)
               DYM4(i,jk) = DYM4(i,jk) + DY(jmin,k+1)*GV4(jk,ik)
            ENDDO
            GP1y(i,jk) = GP1y(i,jk)*ALY(i)
            GM1y(i,jk) = GM1y(i,jk)*ALY(i)
            GP2y(i,jk) = GP2y(i,jk)*ALY(i)
            GM2y(i,jk) = GM2y(i,jk)*ALY(i)
            GP3y(i,jk) = GP3y(i,jk)*ALY(i)
            GM3y(i,jk) = GM3y(i,jk)*ALY(i)
            GP4y(i,jk) = GP4y(i,jk)*ALY(i)
            GM4y(i,jk) = GM4y(i,jk)*ALY(i)
            DYP2(i,jk) = DYP2(i,jk)*ALY(i)
            DYM2(i,jk) = DYM2(i,jk)*ALY(i)
            DYP3(i,jk) = DYP3(i,jk)*ALY(i)
            DYM3(i,jk) = DYM3(i,jk)*ALY(i)
            DYP4(i,jk) = DYP4(i,jk)*ALY(i)
            DYM4(i,jk) = DYM4(i,jk)*ALY(i)
            jmin = jmin + NPY(i) + 1
         ENDDO
      ENDDO
      CONTINUE
      END
!*==DERIVX.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
! SUBROUTINE TO COMPUTE THE DERIVATIVES
!
      SUBROUTINE DERIVX(D,U,Ux,Al,Np,Nd,M)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION D(NX,33) , U(NX,NY) , Ux(NX,NY) , Al(30) , Np(30)
      DO jm = 1 , M
         jmax = 0
         jmin = 1
         DO i = 1 , Nd
            jmax = jmax + Np(i) + 1
            DO j = jmin , jmax
               uxt = 0.
               DO k = 0 , Np(i)
                  uxt = uxt + D(j,k+1)*U(jmin+k,jm)
               ENDDO
               Ux(j,jm) = uxt*Al(i)
            ENDDO
!
            jmin = jmin + Np(i) + 1
         ENDDO
      ENDDO
      CONTINUE
      END
!*==DERIVY.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
! SUBROUTINE TO COMPUTE THE DERIVATIVES
!
      SUBROUTINE DERIVY(D,U,Uy,Al,Np,Nd,M)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION D(NY,33) , U(NX,NY) , Uy(NX,NY) , Al(30) , Np(30)
      DO jm = 1 , M
         jmax = 0
         jmin = 1
         DO i = 1 , Nd
            jmax = jmax + Np(i) + 1
            DO j = jmin , jmax
               uyt = 0.
               DO k = 0 , Np(i)
                  uyt = uyt + D(j,k+1)*U(jm,jmin+k)
               ENDDO
               Uy(jm,j) = uyt*Al(i)
            ENDDO
            jmin = jmin + Np(i) + 1
         ENDDO
      ENDDO
      CONTINUE
      END
!*==XX.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
! PROGRAM TO COMPUTE X
!
      SUBROUTINE XX(X,Al,Be,Np,Nd)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION X(NX) , Np(30) , Al(30) , Be(30)
      ang = DBLE(1)
      pi = DBLE(2)*DASIN(ang)
      jmax = 0
      jmin = 1
      DO i = 1 , Nd
         jmax = jmax + Np(i) + 1
         DO j = jmin , jmax
            xi = -DCOS(pi*DBLE(j-jmin)/DBLE(Np(i)))
            X(j) = (xi-Be(i))/Al(i)
         ENDDO
         jmin = jmin + Np(i) + 1
      ENDDO
      CONTINUE
      END
!*==GRID6.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
!
! PROGRAM TO CALCULATE THE METRICS FOR THE TWO-DIMENSIONAL PROBLEM
!
      SUBROUTINE GRID6(X,Y,Xp1,Yp1,Xx,Ex,Xy,Ey,Jac,F1pp,F2pp,Mxpx,Mxpy)
      IMPLICIT REAL*8(a-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION X(NX) , Y(NY) , Xx(NX,NY) , Ex(NX,NY) , Xy(NX,NY)
      DIMENSION Ey(NX,NY) , Jac(NX,NY) , F1pp(NX) , F2pp(NX)
      DIMENSION Xp1(NX,NY) , Yp1(NX,NY)
      REAL*8 Jac , n0 , nm
      ang = DBLE(1)
      pi = 2.*DASIN(ang)
      n0 = Y(1)
      nm = Y(Mxpy)
      ii = 0.
      a = 0.1
      DO i = 1 , Mxpx
         IF ( X(i).GE.0 .AND. X(i).LE.1. ) THEN
!
! Flat plate
!
            f1 = 0.0D0
            F1pp(i) = 0.0D0
!
! cosine arc
!	   F1=0.05*(1.0+DCOS(2.0*PI*(X(I)-0.5)))
!	   F1PP(I)=-0.1*PI*DSIN(2.0*PI*(X(I)-0.5))
! Circular Arc
 
!         BN=(A/2.+1./(8.*A))**2-(X(I)-.5)**2
!         F1=BN**0.5-(1./(8.*A)-A/2.)
!         F1=4.*(0.25-(X(I)-.5)**2.)*0.05
!         IF(II.EQ.0) F1=0.
!         F1PP(I)=-8.*(X(I)-.5)*0.05
!         F1PP(I)=-(X(I)-0.5)*BN**(-0.5)
!
! exp function
!           F1=0.1*DEXP(-5.0*(X(I)-0.5)**2)
!           F1PP(I)=-10*(X(I)-.5)*F1
!
!         IF(II.EQ.0) F1PP(I)=0.
!         II=II+1
!         IF(X(I).EQ.1) II=0
!  uncover if not  exp function
         ELSE
            f1 = 0.2*EXP(-(X(i)-0.5)**2)*0.
            F1pp(i) = -2.*(X(i)-0.5)*f1
         ENDIF
         f2 = Y(Mxpy) - f1*0.
         f1p = F1pp(i)
         F2pp(i) = -f1p*0.
         f2p = F2pp(i)
         DO j = 1 , Mxpy
            yp = (Y(j)*(f2-f1)-(n0*f2-nm*f1))/(nm-n0)
            Yp1(i,j) = yp
            Xp1(i,j) = X(i)
            Xx(i,j) = 1.
            Xy(i,j) = 0.
            Ex(i,j) = (n0*f2p-nm*f1p)/(f2-f1)                           &
     &                - ((n0*f2-nm*f1+yp*(nm-n0))*(f2p-f1p))/(f2-f1)**2.
            Ey(i,j) = (nm-n0)/(f2-f1)
            Jac(i,j) = Xx(i,j)*Ey(i,j) - Xy(i,j)*Ex(i,j)
!            IF(J.EQ.1) THEN
!              F1PP(I)=F1PP(I)-EX(I,J)*(F2-F1)/(NM-N0)
!            ENDIF
!            IF(J.EQ.MXPY) THEN
!              F2PP(I)=F2PP(I)-EX(I,J)*(F2-F1)/(NM-N0)
!            ENDIF
         ENDDO
      ENDDO
      CONTINUE
      END
!*==INLET.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
! Implement boundary conditions at walls and inlets
!
      SUBROUTINE INLET(Spy,Epy,Apx,U1,U2,U3,U4,Iw)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION U(NX,NY) , V(NX,NY) , RHO(NX,NY) , T(NX,NY) , E(NX,NY)
      DIMENSION AS1(NX) , AS2(NX) , AS3(NY) , AS4(NY) , NX1(NX) ,       &
     &          NY1(NX)
      DIMENSION NX2(NX) , NY2(NX) , SIG(NX) , P(NX,NY)
      DIMENSION U1(NX,NY) , U2(NX,NY) , U3(NX,NY) , U4(NX,NY)
      REAL*8 NX1 , NX2 , NY1 , NY2 , MINlet(NY)
      INTEGER Spy , Epy , Apx
      COMMON /BNDRY / U , V , P , RHO , T , E , AS1 , AS2 , AS3 , AS4 , &
     &                NX1 , NY1 , NX2 , NY2 , SIG , GMA , S0 , T0 , P0 ,&
     &                PE , HOO , RR , MINlet
      COMMON /W_CONST/ TWX_0 , TWX_n , TWY_0 , TWY_n , UW_0 , UW_n ,    &
     &                 VW_0 , VW_n
!
! COMPUT RIEMANN INVERIANTS AT THE BOUNDARIES
!
      DO i = Spy , Epy
         IF ( Iw.EQ.1 ) THEN
            pp = DLOG(P(Apx,i)/P0) - GMA*U(Apx,i)/AS3(i)
            P(Apx,i) = P0*DEXP(pp)
            T(Apx,i) = TWY_0
            RHO(Apx,i) = P(Apx,i)/(T(Apx,i)*RR)
            AS3(i) = DSQRT(GMA*P(Apx,i)/RHO(Apx,i))
            U(Apx,i) = 0.0D0
            V(Apx,i) = VW_0
         ELSE
            IF ( MINlet(i).LT.1.0 ) THEN
               rmb = U(Apx,i) - 2.*DSQRT(GMA*P(Apx,i)/RHO(Apx,i))       &
     &               /(GMA-1.)
               aq = 1./(GMA-1.) + 2./(GMA-1.)**2*(1.+SIG(i)**2)
               bq = 2.*(1.+SIG(i)**2)*rmb/(GMA-1.)
               cq = rmb**2*(1.+SIG(i)**2)/2. - HOO
               c = (-bq+DSQRT(bq**2-4.*aq*cq))/(2.*aq)
               U(Apx,i) = rmb + 2.*c/(GMA-1.)
               V(Apx,i) = SIG(i)*U(Apx,i)
               RHO(Apx,i) = (GMA*S0/c**2)**(1./(1.-GMA))
               P(Apx,i) = RHO(Apx,i)*c**2/GMA
               T(Apx,i) = P(Apx,i)/(RHO(Apx,i)*RR)
            ELSE
               xft = 1.0 + (GMA-1.0)/2.0*MINlet(i)**2
               P(Apx,i) = P0/xft**(GMA/(GMA-1.0))
               rho0 = P0/(RR*T0)
               RHO(Apx,i) = rho0/xft**(1.0/(GMA-1.0))
               T(Apx,i) = P(Apx,i)/(RHO(Apx,i)*RR)
               ssp = DSQRT(GMA*P(Apx,i)/RHO(Apx,i))
               U(Apx,i) = ssp*MINlet(i)
               V(Apx,i) = SIG(i)*U(Apx,i)
            ENDIF
         ENDIF
         E(Apx,i) = P(Apx,i)/((GMA-1.)*RHO(Apx,i))                      &
     &              + (U(Apx,i)*U(Apx,i)+V(Apx,i)*V(Apx,i))/2.
         U1(Apx,i) = RHO(Apx,i)
         U2(Apx,i) = RHO(Apx,i)*U(Apx,i)
         U3(Apx,i) = RHO(Apx,i)*V(Apx,i)
         U4(Apx,i) = RHO(Apx,i)*E(Apx,i)
      ENDDO
      CONTINUE
      END
!*==AEXIT.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
!
      SUBROUTINE AEXIT(Spy,Epy,Apx,U1,U2,U3,U4,Iw)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION U(NX,NY) , V(NX,NY) , RHO(NX,NY) , T(NX,NY) , E(NX,NY)
      DIMENSION AS1(NX) , AS2(NX) , AS3(NY) , AS4(NY) , NX1(NX) ,       &
     &          NY1(NX)
      DIMENSION NX2(NX) , NY2(NX) , SIG(NX) , P(NX,NY)
      DIMENSION U1(NX,NY) , U2(NX,NY) , U3(NX,NY) , U4(NX,NY)
      INTEGER Spy , Epy , Apx
      REAL*8 NX1 , NX2 , NY1 , NY2 , MINlet(NY)
      COMMON /BNDRY / U , V , P , RHO , T , E , AS1 , AS2 , AS3 , AS4 , &
     &                NX1 , NY1 , NX2 , NY2 , SIG , GMA , S0 , T0 , P0 ,&
     &                PE , HOO , RR , MINlet
      COMMON /W_CONST/ TWX_0 , TWX_n , TWY_0 , TWY_n , UW_0 , UW_n ,    &
     &                 VW_0 , VW_n
!
      DO i = Spy , Epy
         IF ( Iw.EQ.1 ) THEN
            pp = DLOG(P(Apx,i)/P0) + GMA*U(Apx,i)/AS4(i)
            P(Apx,i) = P0*DEXP(pp)
            T(Apx,i) = TWY_n
            RHO(Apx,i) = P(Apx,i)/(T(Apx,i)*RR)
            AS4(i) = DSQRT(GMA*P(Apx,i)/RHO(Apx,i))
            U(Apx,i) = 0.0D0
            V(Apx,i) = VW_n
         ELSE
            ssp = DSQRT(GMA*P(Apx,i)/RHO(Apx,i))
            IF ( U(Apx,i).LT.ssp ) THEN
               sob = P(Apx,i)/RHO(Apx,i)**GMA
               rpb = U(Apx,i) + 2.*DSQRT(GMA*P(Apx,i)/RHO(Apx,i))       &
     &               /(GMA-1.)
               RHO(Apx,i) = (PE/sob)**(1./GMA)
               c = DSQRT(GMA*PE/RHO(Apx,i))
               U(Apx,i) = rpb - 2.*c/(GMA-1.)
!              IF(U(APX,I).LT.0.) V(APX,I)=0.0
               P(Apx,i) = PE
               T(Apx,i) = P(Apx,i)/(RHO(Apx,i)*RR)
            ENDIF
         ENDIF
         E(Apx,i) = P(Apx,i)/((GMA-1.)*RHO(Apx,i))                      &
     &              + (U(Apx,i)*U(Apx,i)+V(Apx,i)*V(Apx,i))/2.
         U1(Apx,i) = RHO(Apx,i)
         U2(Apx,i) = RHO(Apx,i)*U(Apx,i)
         U3(Apx,i) = RHO(Apx,i)*V(Apx,i)
         U4(Apx,i) = RHO(Apx,i)*E(Apx,i)
      ENDDO
      CONTINUE
      END
!*==BOTWALL.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
      SUBROUTINE BOTWALL(Spx,Epx,Apy,Mxpx,U1,U2,U3,U4,Iv)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION U(NX,NY) , V(NX,NY) , RHO(NX,NY) , T(NX,NY) , E(NX,NY)
      DIMENSION AS1(NX) , AS2(NX) , AS3(NY) , AS4(NY) , NX1(NX) ,       &
     &          NY1(NX)
      DIMENSION NX2(NX) , NY2(NX) , SIG(NX) , P(NX,NY)
      DIMENSION U1(NX,NY) , U2(NX,NY) , U3(NX,NY) , U4(NX,NY)
      INTEGER Spx , Epx , Apy
      REAL*8 NX1 , NX2 , NY1 , NY2 , MINlet(NY)
      COMMON /BNDRY / U , V , P , RHO , T , E , AS1 , AS2 , AS3 , AS4 , &
     &                NX1 , NY1 , NX2 , NY2 , SIG , GMA , S0 , T0 , P0 ,&
     &                PE , HOO , RR , MINlet
      COMMON /W_CONST/ TWX_0 , TWX_n , TWY_0 , TWY_n , UW_0 , UW_n ,    &
     &                 VW_0 , VW_n
!
      DO i = Spx , Epx
         vt1 = U(i,Apy)*NY1(i) - V(i,Apy)*NX1(i)
         vn1 = U(i,Apy)*NX1(i) + V(i,Apy)*NY1(i)
         sob1 = P(i,Apy)/RHO(i,Apy)**GMA
         rmb = vn1 - 2.*DSQRT(GMA*P(i,Apy)/RHO(i,Apy))/(GMA-1.)
         rpb = vn1 + 2.*DSQRT(GMA*P(i,Apy)/RHO(i,Apy))/(GMA-1.)
         ss = -((GMA-1.)/2.*rmb)
         ss2 = ss**2
!
! BOTTOM WALL CONDITIONS FOR SUBSONIC FLOW
!
 
!          v(i,1)=-nx1(i)*u(i,1)/ny1(i)
!          RHO(I,1)=(SS2/(GMA*SOB1))**(1./(GMA-1.))
         pp = DLOG(P(i,Apy)/P0) - GMA*vn1/AS1(i)
         P(i,Apy) = P0*DEXP(pp)
         IF ( Iv.EQ.0 ) THEN
            U(i,Apy) = (vt1*NY1(i))/(NY1(i)*NY1(i)+NX1(i)*NX1(i))
            V(i,Apy) = -(vt1*NX1(i))/(NY1(i)*NY1(i)+NX1(i)*NX1(i))
            RHO(i,Apy) = (P(i,Apy)/sob1)**(1.0/GMA)
            T(i,Apy) = P(i,Apy)/(RR*RHO(i,Apy))
         ELSE
            U(i,Apy) = UW_0
            V(i,Apy) = 0.0
            T(i,Apy) = TWX_0
!              WRITE(6,*) TWALL
            RHO(i,Apy) = P(i,Apy)/(T(i,Apy)*RR)
         ENDIF
         AS1(i) = DSQRT(GMA*P(i,Apy)/RHO(i,Apy))
!	   IF(I.EQ.MXPX) THEN
!	     P(I,APY)=PE
!	     RHO(I,APY)=(PE/(TWALL*RR))
!c            C=DSQRT(GMA*PE/RHO(I,APY))
!c            U(I,APY)=RMU1+2.*C/(GMA-1.)
!c            V(I,APY)=SIG2(1)*U(I,APY)
!	   ENDIF
!
         E(i,Apy) = P(i,Apy)/((GMA-1.)*RHO(i,Apy))                      &
     &              + (U(i,Apy)*U(i,Apy)+V(i,Apy)*V(i,Apy))/2.
         U1(i,Apy) = RHO(i,Apy)
         U2(i,Apy) = RHO(i,Apy)*U(i,Apy)
         U3(i,Apy) = RHO(i,Apy)*V(i,Apy)
         U4(i,Apy) = RHO(i,Apy)*E(i,Apy)
      ENDDO
      CONTINUE
      END
!*==TOPWALL.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!
      SUBROUTINE TOPWALL(Spx,Epx,Apy,Mxpx,U1,U2,U3,U4,Iv)
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (NX=150,NY=150)
      DIMENSION U(NX,NY) , V(NX,NY) , RHO(NX,NY) , T(NX,NY) , E(NX,NY)
      DIMENSION AS1(NX) , AS2(NX) , AS3(NY) , AS4(NY) , NX1(NX) ,       &
     &          NY1(NX)
      DIMENSION NX2(NX) , NY2(NX) , SIG(NX) , P(NX,NY)
      DIMENSION U1(NX,NY) , U2(NX,NY) , U3(NX,NY) , U4(NX,NY)
      INTEGER Spx , Epx , Apy
      REAL*8 NX1 , NX2 , NY1 , NY2 , MINlet(NY)
      COMMON /BNDRY / U , V , P , RHO , T , E , AS1 , AS2 , AS3 , AS4 , &
     &                NX1 , NY1 , NX2 , NY2 , SIG , GMA , S0 , T0 , P0 ,&
     &                PE , HOO , RR , MINlet
      COMMON /W_CONST/ TWX_0 , TWX_n , TWY_0 , TWY_n , UW_0 , UW_n ,    &
     &                 VW_0 , VW_n
!
! TOPWALL  CONDITIONS
!
      DO i = Spx , Epx
         vt2 = U(i,Apy)*NY2(i) - V(i,Apy)*NX2(i)
         vn2 = U(i,Apy)*NX2(i) + V(i,Apy)*NY2(i)
         sob2 = P(i,Apy)/RHO(i,Apy)**GMA
         rmb = vn2 - 2.*DSQRT(GMA*P(i,Apy)/RHO(i,Apy))/(GMA-1.)
         rpb = vn2 + 2.*DSQRT(GMA*P(i,Apy)/RHO(i,Apy))/(GMA-1.)
         ss = ((GMA-1.)/2.*rpb)
         ss2 = ss**2
         U(i,Apy) = (vt2*NY2(i))/(NY2(i)*NY2(i)+NX2(i)*NX2(i))
         V(i,Apy) = -(vt2*NX2(i))/(NY2(i)*NY2(i)+NX2(i)*NX2(i))
!          v(i,APY)=-nx2(i)*u(i,APY)/ny2(i)
         ss2 = ((GMA-1.)/2.*(rpb))**2
         pp = DLOG(P(i,Apy)/P0) + GMA*vn2/AS2(i)
         P(i,Apy) = P0*DEXP(pp)
         IF ( Iv.EQ.0 ) THEN
            U(i,Apy) = (vt2*NY2(i))/(NY2(i)*NY2(i)+NX2(i)*NX2(i))
            V(i,Apy) = -(vt2*NX2(i))/(NY2(i)*NY2(i)+NX2(i)*NX2(i))
            RHO(i,Apy) = (P(i,Apy)/sob2)**(1.0/GMA)
            T(i,Apy) = P(i,Apy)/(RR*RHO(i,Apy))
         ELSE
            U(i,Apy) = UW_n
            V(i,Apy) = 0.0
            T(i,Apy) = TWX_n
            RHO(i,Apy) = P(i,Apy)/(T(i,Apy)*RR)
         ENDIF
         AS2(i) = DSQRT(GMA*P(i,Apy)/RHO(i,Apy))
!	   IF(I.EQ.MXPX) THEN
!	     P(I,APY)=PE
!	     RHO(I,APY)=(PE/SOB2)**(1./GMA)
!            C=DSQRT(GMA*PE/RHO(I,APY))
!            U(I,APY)=RMU2+2.*C/(GMA-1.)
!            V(I,APY)=SIG2(APY)*U(I,APY)
!	  ENDIF
!         P(I,APY)=RHO(I,APY)*(RPB*(GMA-1)/2.)**2/GMA
         E(i,Apy) = P(i,Apy)/((GMA-1.)*RHO(i,Apy))                      &
     &              + (U(i,Apy)*U(i,Apy)+V(i,Apy)*V(i,Apy))/2.
         U1(i,Apy) = RHO(i,Apy)
         U2(i,Apy) = RHO(i,Apy)*U(i,Apy)
         U3(i,Apy) = RHO(i,Apy)*V(i,Apy)
         U4(i,Apy) = RHO(i,Apy)*E(i,Apy)
      ENDDO
      CONTINUE
      END
