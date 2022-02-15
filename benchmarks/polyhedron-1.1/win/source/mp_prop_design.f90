!************************************************************************
! WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
!************************************************************************
!     MP_PROP_DESIGN was a part of PROP_DESIGN, available at
!
!     http://propdesign.weebly.com/.
!
!     The current version of PROP_DESIGN has added new features and
!     corrected some latent bugs contained in MP_PROP_DESIGN.  The
!     current version of PROP_DESIGN contains specific benchmarking
!     versions.  PROP_DESIGN_MAPS_BENCHMARK was designed to replace
!     MP_PROP_DESIGN for benchmarking work.  Please do not use
!     MP_PROP_DESIGN for propeller design or analysis work, due to
!     the aforementioned bug fixes.  Instead, use the latest
!     version of PROP_DESIGN.
!
!*==AA0001.spg  processed by SPAG 6.70Dc at 12:33 on 16 Jun 2011
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     MP_PROP_DESIGN WAS WRITTEN BY ANTHONY FALZONE AND INITIALLY
!     RELEASED ON 03-12-10, AS PUBLIC DOMAIN SOFTWARE.  MP_PROP_DESIGN
!     IS INTENDED TO ADHERE TO THE FORTRAN 77 STANDARD.
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     START OF FORTRAN 77 INITIALIZATION BLOCK
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      IMPLICIT NONE
!*--AA000115
      INTEGER MAXSIZ
!
      PARAMETER (MAXSIZ=1000)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      REAL*8 PI , ZERO
!
      PARAMETER (PI=3.14159265358979320D0,ZERO=0.0D0)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     DATA FOR ATMOSPHERIC MODEL
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      REAL*8 xtab(45) , ytab1(45) , ytab2(45) , ytab3(45)
!
      DATA xtab/ - 6561.679790D0 , 0.0D0 , 6561.679790D0 ,              &
     &     13123.359580D0 , 19685.039370D0 , 26246.719160D0 ,           &
     &     32808.398950D0 , 39370.078740D0 , 45931.758530D0 ,           &
     &     52493.438320D0 , 59055.118110D0 , 65616.79790D0 ,            &
     &     72178.477690D0 , 78740.157480D0 , 85301.837270D0 ,           &
     &     91863.517060D0 , 98425.196850D0 , 104986.876640D0 ,          &
     &     111548.556430D0 , 118110.236220D0 , 124671.916010D0 ,        &
     &     131233.59580D0 , 137795.275590D0 , 144356.955380D0 ,         &
     &     150918.635170D0 , 157480.314960D0 , 164041.994750D0 ,        &
     &     170603.674540D0 , 177165.354330D0 , 183727.034120D0 ,        &
     &     190288.713910D0 , 196850.39370D0 , 203412.073490D0 ,         &
     &     209973.753280D0 , 216535.433070D0 , 223097.112860D0 ,        &
     &     229658.792650D0 , 236220.472440D0 , 242782.152230D0 ,        &
     &     249343.832020D0 , 255905.511810D0 , 262467.19160D0 ,         &
     &     269028.871390D0 , 275590.551180D0 , 282152.230970D0/
!
      DATA ytab1/1.4780D0 , 1.2250D0 , 1.0070D0 , 0.81930D0 ,           &
     &     0.66010D0 , 0.52580D0 , 0.41350D0 , 0.31190D0 , 0.22790D0 ,  &
     &     0.16650D0 , 0.12160D0 , 0.088910D0 , 0.064510D0 ,            &
     &     0.046940D0 , 0.034260D0 , 0.025080D0 , 0.018410D0 ,          &
     &     0.013550D0 , 0.0098870D0 , 0.0072570D0 , 0.0053660D0 ,       &
     &     0.0039950D0 , 0.0029950D0 , 0.0022590D0 , 0.0017140D0 ,      &
     &     0.0013170D0 , 0.0010270D0 , 0.00080550D0 , 0.00063890D0 ,    &
     &     0.00050440D0 , 0.00039620D0 , 0.00030960D0 , 0.00024070D0 ,  &
     &     0.0001860D0 , 0.00014290D0 , 0.00010910D0 , 0.000082810D0 ,  &
     &     0.000062360D0 , 0.000046370D0 , 0.00003430D0 ,               &
     &     0.000025230D0 , 0.000018450D0 , 0.000013410D0 ,              &
     &     0.000009690D0 , 0.0000069550D0/
!
      DATA ytab2/347.90D0 , 340.30D0 , 332.50D0 , 324.60D0 , 316.50D0 , &
     &     308.10D0 , 299.50D0 , 295.10D0 , 295.10D0 , 295.10D0 ,       &
     &     295.10D0 , 295.10D0 , 296.40D0 , 297.70D0 , 299.10D0 ,       &
     &     300.40D0 , 301.70D0 , 303.0D0 , 306.50D0 , 310.10D0 ,        &
     &     313.70D0 , 317.20D0 , 320.70D0 , 324.10D0 , 327.50D0 ,       &
     &     329.80D0 , 329.80D0 , 328.80D0 , 325.40D0 , 322.0D0 ,        &
     &     318.60D0 , 315.10D0 , 311.50D0 , 308.0D0 , 304.40D0 ,        &
     &     300.70D0 , 297.10D0 , 293.40D0 , 290.70D0 , 288.0D0 ,        &
     &     285.30D0 , 282.50D0 , 279.70D0 , 276.90D0 , 274.10D0/
!
      DATA ytab3/0.00001250D0 , 0.00001460D0 , 0.00001710D0 ,           &
     &     0.00002030D0 , 0.00002420D0 , 0.0000290D0 , 0.00003530D0 ,   &
     &     0.00004560D0 , 0.00006240D0 , 0.00008540D0 , 0.0001170D0 ,   &
     &     0.000160D0 , 0.0002220D0 , 0.0003070D0 , 0.0004240D0 ,       &
     &     0.0005840D0 , 0.0008010D0 , 0.00110D0 , 0.001530D0 ,         &
     &     0.002130D0 , 0.002930D0 , 0.004010D0 , 0.005440D0 ,          &
     &     0.007340D0 , 0.009830D0 , 0.01290D0 , 0.01660D0 , 0.0210D0 , &
     &     0.02610D0 , 0.03250D0 , 0.04070D0 , 0.05110D0 , 0.06460D0 ,  &
     &     0.0820D0 , 0.1050D0 , 0.1340D0 , 0.1740D0 , 0.2260D0 ,       &
     &     0.2990D0 , 0.3980D0 , 0.5320D0 , 0.7160D0 , 0.9680D0 ,       &
     &     1.320D0 , 1.80D0/
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      INTEGER alphico , b , bm , counta , countb , countc , cswitch ,   &
     &        gamco , hep , i , j , k , l1 , l2 , l3 , m1 , m2 , nvs ,  &
     &        p , rf , switch , tip
!
      REAL*8 advrat , ainc , alphirf , alphitol , alti , arg1 , arg2 ,  &
     &       cpow , cputimemins , cputimesecs , cthr , dispx , dispy ,  &
     &       distx , disty , distz , dlpphi , dlpx , dlpy , dltphi ,    &
     &       dltx , dlty , dltz , dphit , dwpz , dwtphi , dwtx , dwty , &
     &       dwtz , eff , eta , finish , flwrte , freq , gamrf ,        &
     &       gamtol , mom , nu , omega , pa , pb , pc , pd , phib ,     &
     &       phimax , phip , phit , pitch , pow , ptot , rho , rocr ,   &
     &       rpm , radius , sndspd , start , thr , trns , v , vend ,    &
     &       vstart , x0 , x1 , xp , xt , y1 , y2 , y3 , yp , yt , zt
!
      REAL*8 alphef(MAXSIZ) , alphi(MAXSIZ) , alphie(MAXSIZ) ,          &
     &       beta(MAXSIZ) , cd(MAXSIZ) , chord(MAXSIZ) , cl(MAXSIZ) ,   &
     &       delta(MAXSIZ) , dgame(MAXSIZ) , dl(MAXSIZ) , dphi(MAXSIZ) ,&
     &       dpr(MAXSIZ) , dr(MAXSIZ) , dx(MAXSIZ) , dy(MAXSIZ) ,       &
     &       gam(MAXSIZ) , lpr(MAXSIZ) , mach(MAXSIZ) , mompr(MAXSIZ) , &
     &       phi(MAXSIZ) , phie(MAXSIZ) , r(MAXSIZ) , re(MAXSIZ) ,      &
     &       tempa(MAXSIZ) , tempg(MAXSIZ) , theta(MAXSIZ) ,            &
     &       thetae(MAXSIZ) , thrpr(MAXSIZ) , vefn(MAXSIZ) ,            &
     &       vefnz(MAXSIZ) , vefphi(MAXSIZ) , vefz(MAXSIZ) ,            &
     &       wphi(MAXSIZ) , wz(MAXSIZ) , x(MAXSIZ) , xe(MAXSIZ) ,       &
     &       y(MAXSIZ) , ye(MAXSIZ)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      CALL CPU_TIME(start)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!

! POLYHEDRON - all output send to stdout for validation purposes.

!      OPEN (UNIT=3,FILE='MP_2D_FAN_EFFICIENCY_COMPARISON.DAT',          &
!     &      STATUS='UNKNOWN')
!!
!      OPEN (UNIT=4,FILE='MP_2D_MASS_FLOW_RATE_COMPARISON.DAT',          &
!     &      STATUS='UNKNOWN')
!!
!      OPEN (UNIT=7,FILE='MP_2D_PROPELLER_EFFICIENCY_COMPARISON.DAT',    &
!     &      STATUS='UNKNOWN')
!!
!      OPEN (UNIT=8,FILE='MP_2D_SHAFT_POWER_COMPARISON_KW.DAT',          &
!     &      STATUS='UNKNOWN')
!!
!      OPEN (UNIT=9,FILE='MP_2D_SHAFT_POWER_COMPARISON_SHP.DAT',         &
!     &      STATUS='UNKNOWN')
!!
!      OPEN (UNIT=10,FILE='MP_2D_SHAFT_POWER_COMPARISON_WATTS.DAT',      &
!     &      STATUS='UNKNOWN')
!!
!      OPEN (UNIT=11,FILE='MP_2D_THRUST_COMPARISON_KGF.DAT',             &
!     &      STATUS='UNKNOWN')
!!
!      OPEN (UNIT=12,FILE='MP_2D_THRUST_COMPARISON_LBF.DAT',             &
!     &      STATUS='UNKNOWN')
!!
!      OPEN (UNIT=13,FILE='MP_2D_THRUST_COMPARISON_N.DAT',               &
!     &      STATUS='UNKNOWN')
!!
!      OPEN (UNIT=14,FILE='MP_2D_THRUST_COMPARISON_OZF.DAT',             &
!     &      STATUS='UNKNOWN')
!!
!      OPEN (UNIT=15,FILE='MP_2D_VOLUMETRIC_FLOW_RATE_COMPARISON.DAT',   &
!     &      STATUS='UNKNOWN')
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      OPEN (UNIT=16,FILE='MP_INPUT_ONE.TXT',STATUS='OLD')
!
      OPEN (UNIT=17,FILE='MP_INPUT_TWO.TXT',STATUS='OLD')
!
      OPEN (UNIT=18,FILE='MP_OUTPUT.TXT',STATUS='UNKNOWN')
!
      OPEN (UNIT=19,FILE='MP_PROP_DESIGN_SETTINGS.TXT',STATUS='UNKNOWN')
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     END OF FORTRAN 77 INITIALIZATION BLOCK
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      WRITE (*,*)
!
      WRITE (*,*) 'PI SET TO =' , PI
!
      WRITE (*,*)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     READING SETTINGS FILE
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      READ (19,*) tip
!
      READ (19,*) gamco
!
      READ (19,*) gamtol
!
      READ (19,*) gamrf
!
      READ (19,*) alphico
!
      READ (19,*) alphitol
!
      READ (19,*) alphirf
!
      READ (19,*) dphit
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     DEFINING RESOLUTION OF PHI STEPPING
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      dphit = dphit*(PI/180.0D0)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      READ (19,*) ainc
!
      READ (19,*) trns
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     FINISHED READING SETTINGS FILE
!
!     READING INPUT FILE
!
!     START OF INPUT FILE LOOP
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      DO l2 = 0 , 1
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         switch = l2
!
         IF ( switch.EQ.0 ) rf = 16
!
         IF ( switch.EQ.1 ) rf = 17
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     READING INPUT FILE
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         READ (rf,*) vstart
!
         READ (rf,*) vend
!
         READ (rf,*) nvs
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     ALTITUDE
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         READ (rf,*) alti
!
         IF ( (alti.LT.xtab(1)) .OR. (alti.GT.xtab(45)) ) THEN
!
            WRITE (*,*) '*** FORCED EXIT; ALTITUDE OUT OF RANGE ***'
!
            GOTO 200
!
         ENDIF
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     START OF ATMOSPHERIC MODEL
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         x0 = alti
!
         WRITE (*,*) 'ALTITUDE (FEET) =' , alti
!
         DO m1 = 2 , 45
!
            IF ( x0.LE.xtab(m1) ) GOTO 50
!
         ENDDO
!
!
 50      m2 = m1 - 1
!
         x1 = (x0-xtab(m2))/(xtab(m2+1)-xtab(m2))
!
         y1 = (1.0D0-x1)*ytab1(m2) + x1*ytab1(m2+1)
!
         y2 = (1.0D0-x1)*ytab2(m2) + x1*ytab2(m2+1)
!
         y3 = (1.0D0-x1)*ytab3(m2) + x1*ytab3(m2+1)
!
         rho = y1
!
         sndspd = y2
!
         nu = y3
!
         WRITE (*,*) 'DENSITY (KG/M^3) =' , rho
!
         WRITE (*,*) 'SPEED OF SOUND (M/S) =' , sndspd
!
         WRITE (*,*) 'KINEMATIC VISCOSITY (M^2/S) =' , nu
!
         WRITE (*,*)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     END OF ATMOSPHERIC MODEL
!
!     FOUR USER SPECIFIED PITCH VALUES
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         READ (rf,*) pa , pb , pc , pd
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     RPM, ANGULAR VELOCITY IN UNITS OF REVOLUTIONS PER MINUTE
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         READ (rf,*) rpm
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     FREQ, ANGULAR VELOCITY IN UNITS OF REVOLUTIONS PER SECOND
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         freq = rpm/60.0D0
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     OMEGA, ANGULAR VELOCITY IN UNITS OF RADIANS PER SECOND
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         omega = 2.0D0*PI*freq
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     NUMBER OF BLADES
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         READ (rf,*) bm
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     BLADE RADIUS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         READ (rf,*) radius
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     MINIMUM RADIUS OVER CHORD RATIO, SETS THE CHORD OF THE BLADE
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         READ (rf,*) rocr
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     SWITCH THAT SELECTS THE CHORD DISTRIBUTION;
!     1 FOR STRAIGHT, 2 FOR ELLIPTICAL
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         READ (rf,*) cswitch
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     CALCULATING THE CHORD DISTRIBUTION
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         DO j = 1 , tip
!
            IF ( cswitch.EQ.1 ) THEN
!
               chord(j) = radius/rocr
!
            ELSEIF ( cswitch.EQ.2 ) THEN
!
               IF ( j.EQ.tip ) THEN
!
                  chord(j) = 0.0D0
!
               ELSE
!
                  chord(j) = SQRT(((radius/rocr)**2.0D0)*(1.0D0-((((REAL&
     &                       (j)-1.0D0)/(REAL(tip)-1.0D0))*radius)      &
     &                       **2.0D0/radius**2.0D0)))
!
               ENDIF
!
            ELSE
!
               WRITE (*,*)
!
               WRITE (*,*)                                              &
     &                  '*** FORCED EXIT; CHORD SWITCH MUST EQUAL 1 FOR'&
     &                  , ' CONSTANT, OR 2 FOR ELLIPTICAL ***'
!
               GOTO 200
!
            ENDIF
!
         ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     PHIMAX, DETERMINES THE AMOUNT OF BLADE SWEEP
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         READ (rf,*) phimax
!
         phimax = phimax*(PI/180.0D0)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     HUB END POINT
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         READ (rf,*) hep
!
         hep = hep + 1
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     FINISHED READING INPUT FILE
!
!     CALCULATING PROPELLER GEOMETRY AT BOUNDARIES
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         r(1) = ZERO
!
         phi(1) = ZERO
!
         x(1) = ZERO
!
         y(1) = ZERO
!
         dx(1) = ZERO
!
         dy(1) = ZERO
!
         dphi(1) = ZERO
!
         dr(1) = ZERO
!
         dl(1) = ZERO
!
         delta(1) = ZERO
!
         beta(1) = 90.0D0*(PI/180.0D0)
!
         r(tip) = radius
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     START OF PITCH LOOP
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
         DO l3 = 1 , 4
!
            IF ( l3.EQ.1 ) pitch = pa
!
            IF ( l3.EQ.2 ) pitch = pb
!
            IF ( l3.EQ.3 ) pitch = pc
!
            IF ( l3.EQ.4 ) pitch = pd
!
            WRITE (*,*) 'PITCH = ' , pitch
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     CACLULATING SWEEP ANGLE (DELTA) AND GEOMETRIC ANGLE-OF-ATTACK
!     (BETA)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
            DO j = 2 , tip
!
               r(j) = r(j-1) + (r(tip)/(REAL(tip)-1.0D0))
!
               phi(j) = phi(j-1) + (phimax/(REAL(tip)-1.0D0))
!
               x(j) = r(j)*COS(phi(j))
!
               y(j) = r(j)*SIN(phi(j))
!
               dx(j) = x(j) - x(j-1)
!
               dy(j) = y(j) - y(j-1)
!
               dphi(j) = phi(j) - phi(j-1)
!
               dr(j) = r(j)*COS(dphi(j)) - r(j-1)
!
               dl(j) = SQRT(ABS(dx(j))**2.0D0+ABS(dy(j))**2.0D0)
!
               delta(j) = ACOS(dr(j)/dl(j))
!
               arg1 = pitch
!
               arg2 = 2.0D0*PI*r(j)*COS(delta(j))
!
               IF ( (arg2.LT.ZERO) .OR. (arg2.GT.ZERO) ) THEN
!
                  beta(j) = ATAN(arg1/arg2)
!
               ELSE
!
                  beta(j) = ZERO
!
               ENDIF
!
            ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     START OF AIRCRAFT VELOCITY LOOP
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
            countc = 1
!
            v = vstart*sndspd
!
            DO l1 = 1 , nvs + 1
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     CALCULATING PROPELLER ADVANCE RATIO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               advrat = v/(freq*2.0D0*r(tip))
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     CALCULATING ANGLE THETA
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               theta(1) = ZERO
!
               DO j = 2 , tip
!
                  arg1 = v
!
                  arg2 = omega*r(j)
!
                  IF ( (arg2.LT.ZERO) .OR. (arg2.GT.ZERO) ) THEN
!
                     theta(j) = ATAN(arg1/arg2)
!
                  ELSE
!
                     theta(j) = ZERO
!
                  ENDIF
!
               ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     CALCULATING INPUTS AT INTERMEDIARY POINTS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               DO k = 1 , tip - 1
!
                  xe(k) = (x(k)+x(k+1))/2.0D0
!
                  ye(k) = (y(k)+y(k+1))/2.0D0
!
                  re(k) = SQRT(xe(k)**2.0D0+ye(k)**2.0D0)
!
                  arg1 = ye(k)
!
                  arg2 = xe(k)
!
                  IF ( (arg2.LT.ZERO) .OR. (arg2.GT.ZERO) ) THEN
!
                     phie(k) = ATAN(arg1/arg2)
!
                  ELSE
!
                     phie(k) = ZERO
!
                  ENDIF
!
                  arg1 = v
!
                  arg2 = omega*re(k)
!
                  IF ( (arg2.LT.ZERO) .OR. (arg2.GT.ZERO) ) THEN
!
                     thetae(k) = ATAN(arg1/arg2)
!
                  ELSE
!
                     thetae(k) = ZERO
!
                  ENDIF
!
               ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     BEGINNING TO ITERATE UPON CIRCULATION (GAM) AND INDUCED ANGLE OF
!     ATTACK (ALPHI)
!
!     DEFINING BOUNDARY CONDITIONS FOR INDUCED ANGLE OF ATTACK (ALPHI)
!     AND CIRCULATION (GAM)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               alphi(1) = ZERO
!
               alphi(tip) = ZERO
!
               gam(1) = ZERO
!
               gam(tip) = ZERO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     INITIAL GUESSES FOR INDUCED ANGLE OF ATTACK (ALPHI) AND
!     CIRCULATION (GAM)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               DO j = 2 , tip - 1
!
                  alphi(j) = ZERO
!
                  gam(j) = ZERO
!
               ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     INITIALZING COUNTERS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               counta = 1
!
               countb = 1
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
 60            DO k = 1 , tip - 1
!
                  alphie(k) = (alphi(k)+alphi(k+1))/2.0D0
!
                  dgame(k) = gam(k) - gam(k+1)
!
               ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     USING THE BIOT-SAVART LAW TO CALCULATE INDUCED VELOCITIES; WPHI
!     AND WZ
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               DO j = 1 , tip
!
                  wphi(j) = ZERO
!
                  wz(j) = ZERO
!
                  DO b = 1 , bm
!
                     phib = ((2.0D0*PI)/REAL(bm))*(REAL(b)-1.0D0)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     CALCULATING INDUCED VELOCITIES ON TRAILING VORTEX LINES; DWTPHI
!     AND DWTZ
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
                     DO k = 1 , tip - 1
!
                        DO i = 1 , 1 + NINT(2.0D0*PI*trns/dphit) ,      &
     &                     NINT(ainc/(dphit*(180.0D0/PI)))
!
                           phit = phib + phie(k) + (REAL(i)-0.50D0)     &
     &                            *dphit
!
                           xt = re(k)*COS(phit)
!
                           yt = re(k)*SIN(phit)
!
                           zt = -(REAL(i)-0.50D0)*dphit*re(k)           &
     &                          *TAN(thetae(k)+alphie(k))
!
                           distx = x(j) - xt
!
                           disty = y(j) - yt
!
                           distz = -zt
!
                           dltphi = -re(k)*dphit
!
                           dltz = re(k)*dphit*TAN(thetae(k)+alphie(k))
!
                           dltx = -dltphi*SIN(phit)
!
                           dlty = dltphi*COS(phit)
!
                           dwtx = (dgame(k)/(4.0D0*PI))                 &
     &                            *((dlty*distz-dltz*disty)             &
     &                            /(SQRT(ABS(distx)**2.0D0+ABS(disty)   &
     &                            **2.0D0+ABS(distz)**2.0D0))**3.0D0)
!
                           dwty = (dgame(k)/(4.0D0*PI))                 &
     &                            *((dltz*distx-dltx*distz)             &
     &                            /(SQRT(ABS(distx)**2.0D0+ABS(disty)   &
     &                            **2.0D0+ABS(distz)**2.0D0))**3.0D0)
!
                           dwtphi = dwty*COS(phi(j)) - dwtx*SIN(phi(j))
!
                           dwtz = (dgame(k)/(4.0D0*PI))                 &
     &                            *((dltx*disty-dlty*distx)             &
     &                            /(SQRT(ABS(distx)**2.0D0+ABS(disty)   &
     &                            **2.0D0+ABS(distz)**2.0D0))**3.0D0)
!
                           wphi(j) = -ABS(wphi(j)+dwtphi)
!
                           wz(j) = wz(j) + dwtz
!
                        ENDDO
!
                     ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     CALCULATING INDUCED VELOCITIES ON BOUND VORTEX LINES; DWPZ
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
                     DO p = 1 , tip
!
                        IF ( (b.NE.1) .AND. (p.NE.j) ) THEN
!
                           phip = phi(p) + phib
!
                           xp = r(p)*COS(phip)
!
                           yp = r(p)*SIN(phip)
!
                           dispx = x(j) - xp
!
                           dispy = y(j) - yp
!
                           IF ( p.EQ.1 ) THEN
!
                              dlpphi = -r(p)*(phie(1)-phi(1))
!
                           ELSEIF ( p.EQ.tip ) THEN
!
                              dlpphi = -r(p)*(phi(tip)-phie(tip-1))
!
                           ELSE
!
                              dlpphi = -r(p)*(phie(p)-phie(p-1))
!
                           ENDIF
!
                           dlpx = -dlpphi*SIN(phip)
!
                           dlpy = dlpphi*COS(phip)
!
                           dwpz = (gam(p)/(4.0D0*PI))                   &
     &                            *((dlpx*dispy-dlpy*dispx)             &
     &                            /(SQRT(ABS(dispx)**2.0D0+ABS(dispy)   &
     &                            **2.0D0))**3.0D0)
!
                           wz(j) = -ABS(wz(j)+dwpz)
!
                        ENDIF
!
                     ENDDO
!
                  ENDDO
!
               ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     DEFINING BOUNDARY CONDITIONS FOR INDUCED VELOCITY IN THE PHI
!     AND Z DIRECTIONS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               wphi(1) = ZERO
!
               wphi(tip) = ZERO
!
               wz(1) = ZERO
!
               wz(tip) = ZERO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               DO j = 1 , tip
!
                  vefphi(j) = (omega*r(j)) + wphi(j)
!
                  vefz(j) = -v + wz(j)
!
                  IF ( j.EQ.1 ) THEN
!
                     tempa(j) = ZERO
!
                  ELSE
!
                     arg1 = -vefz(j)
!
                     arg2 = vefphi(j)
!
                     IF ( (arg2.LT.ZERO) .OR. (arg2.GT.ZERO) ) THEN
!
                        tempa(j) = ATAN(arg1/arg2) - theta(j)
!
                     ELSE
!
                        tempa(j) = -theta(j)
!
                     ENDIF
!
                  ENDIF
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
                  vefn(j) = COS(delta(j))*vefphi(j)
!
                  arg1 = -vefz(j)
!
                  arg2 = vefn(j)
!
                  IF ( (arg2.LT.ZERO) .OR. (arg2.GT.ZERO) ) THEN
!
                     alphef(j) = beta(j) - ATAN(arg1/arg2)
!
                  ELSE
!
                     alphef(j) = beta(j)
!
                  ENDIF
!
                  vefnz(j) = SQRT((ABS(vefn(j)))**2.0D0+(ABS(vefz(j)))  &
     &                       **2.0D0)
!
                  mach(j) = vefnz(j)/sndspd
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     NOTES REGARDING THE AIRFOIL AND STALL MODELS BELOW:
!
!     BELOW IS THE BEST AIRFOIL AND STALL MODEL I CAN PROVIDE WITHOUT
!     USING EXPENSIVE SOFTWARE.  BASICALLY THE PROBLEM IS YOU NEED TO
!     DETERMINE AIROIL DRAG AND LIFT FOR A RANGE OF MACH NUMBERS,
!     REYNOLDS NUMBERS, AND ANGLES OF ATTACK. COMPRESSIBLITY EFFECTS
!     AND TURBULENT FLOW NEED TO BE ACCOUNTED FOR.
!
!     THE MODELS BELOW ARE BASED ON TEST DATA AND THUS LIMITED IN THEIR
!     RESOLUTION AND RANGE OF APPLICABILITY.  BOTH THE AIRFOIL AND STALL
!     MODEL DATA ARE LIMITED IN THEIR RANGE OF REYNOLDS NUMBER.  ITS NOT
!     CLEAR WHAT RANGE OF MACH NUMBERS THE STALL MODEL DATA IS BASED ON.
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     AIRFOIL MODEL
!
!     NACA 65A009
!
!     REYN = 2.7E6
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     MACH = .85
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
                  IF ( (alphef(j).GE.ZERO*(PI/180.0D0)) .AND.           &
     &                 (alphef(j).LE.8.0D0*(PI/180.0D0)) .AND.          &
     &                 (mach(j).LE.0.90D0) ) THEN
!
                     cd(j) = 0.0050D0*DEXP(22.6740D0*alphef(j))
!
                     cl(j) = 3.4570D0*alphef(j)**0.7470D0
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     MACH = .95
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
                  ELSEIF ( (alphef(j).GE.ZERO*(PI/180.0D0)) .AND.       &
     &                     (alphef(j).LE.8.0D0*(PI/180.0D0)) .AND.      &
     &                     (mach(j).GT.0.90D0) .AND. (mach(j).LE.1.0D0) &
     &                     ) THEN
!
                     cd(j) = 0.0270D0*DEXP(10.3420D0*alphef(j))
!
                     cl(j) = 10.0040D0*alphef(j)**1.3690D0
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     MACH = 1.05
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
                  ELSEIF ( (alphef(j).GE.ZERO*(PI/180.0D0)) .AND.       &
     &                     (alphef(j).LE.8.0D0*(PI/180.0D0)) .AND.      &
     &                     (mach(j).GT.1.0D0) .AND. (mach(j).LE.1.10D0) &
     &                     ) THEN
!
                     cd(j) = 0.050D0*DEXP(7.5560D0*alphef(j))
!
                     cl(j) = 4.4940D0*alphef(j)**0.9410D0
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     MACH = 1.15
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
                  ELSEIF ( (alphef(j).GE.ZERO*(PI/180.0D0)) .AND.       &
     &                     (alphef(j).LE.8.0D0*(PI/180.0D0)) .AND.      &
     &                     (mach(j).GT.1.10D0) .AND. (mach(j).LE.1.20D0)&
     &                     ) THEN
!
                     cd(j) = 0.0470D0*DEXP(8.0650D0*alphef(j))
!
                     cl(j) = 4.340D0*alphef(j)**0.9570D0
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     MACH = 1.25
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
                  ELSEIF ( (alphef(j).GE.ZERO*(PI/180.0D0)) .AND.       &
     &                     (alphef(j).LE.8.0D0*(PI/180.0D0)) .AND.      &
     &                     (mach(j).GT.1.20D0) .AND. (mach(j).LE.1.30D0)&
     &                     ) THEN
!
                     cd(j) = 0.0460D0*DEXP(7.3450D0*alphef(j))
!
                     cl(j) = 3.9390D0*alphef(j)**0.9320D0
!
                  ELSE
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     STALL MODEL
!
!     SUBSONIC 2D FLAT PLATE
!
!     .36E6 < REYN < .7E6
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
                     cd(j) = -0.50D0*1.960D0*(COS(2.0D0*alphef(j))-1.0D0&
     &                       ) + 0.020D0
!
                     cl(j) = 1.20D0*SIN(2.0D0*alphef(j))
!
                  ENDIF
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     USE THE KUTTA-JOUKOWSKI THEOREM TO CALCULATE CIRCULATION (GAM)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
                  tempg(j) = 0.50D0*vefnz(j)*cl(j)*chord(j)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     DEFINING BOUNDARY CONDITIONS FOR THE DRAG COEFFICIENT, THE
!     LIFT COEFFICIENT, AND THE CIRCULATION GUESS (TEMPG)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               cd(1) = ZERO
!
               cd(tip) = ZERO
!
               cl(1) = ZERO
!
               cl(tip) = ZERO
!
               tempg(1) = ZERO
!
               tempg(tip) = ZERO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     ITERATION SCHEME FOR CIRCULATION (GAM)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               DO j = 2 , tip - 1
!
!
                  IF ( ABS((tempg(j)-gam(j))/tempg(j))                  &
     &                 *100.0D0.GT.gamtol ) GOTO 70
                  GOTO 80
!
               ENDDO
!
!
 70            DO j = 2 , tip - 1
!
                  gam(j) = gam(j) + gamrf*(tempg(j)-gam(j))
!
               ENDDO
!
               counta = counta + 1
!
!
               IF ( counta.NE.gamco ) GOTO 60
!
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     ITERATION SCHEME FOR INDUCED ANGLE OF ATTACK (ALPHI)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
 80            DO j = 2 , tip - 1
!
!
                  IF ( ABS((tempa(j)-alphi(j))/tempa(j))                &
     &                 *100.0D0.GT.alphitol ) GOTO 90
                  GOTO 100
!
               ENDDO
!
!
 90            DO j = 2 , tip - 1
!
                  alphi(j) = alphi(j) + alphirf*(tempa(j)-alphi(j))
!
               ENDDO
!
               countb = countb + 1
!
               counta = 1
!
!
!
!
               IF ( countb.NE.alphico ) GOTO 60
!
               WRITE (*,*) 'AIRCRAFT VELOCITY LOOP NUMBER =' , countc , &
     &                    ', STATUS = FORCED CONTINUE; DID NOT CONVERGE'
!
               GOTO 120
!
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     CALCULATING DRAG, LIFT, MOMENT, AND THRUST PER UNIT RADIUS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
 100           DO j = hep , tip - 1
!
                  IF ( j.GT.1 ) THEN
!
                     dpr(j) = 0.50D0*rho*(vefnz(j)**2.)*cd(j)*chord(j)
!
                     lpr(j) = 0.50D0*rho*(vefnz(j)**2.)*cl(j)*chord(j)
!
                     mompr(j) = (lpr(j)*SIN(beta(j)-alphef(j))+dpr(j)*  &
     &                          COS(beta(j)-alphef(j)))*r(j)            &
     &                          *COS(delta(j))
!
                     thrpr(j) = (lpr(j)*COS(beta(j)-alphef(j))-dpr(j)*  &
     &                          SIN(beta(j)-alphef(j)))
!
                  ENDIF
!
               ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     CALCULATING MOMENT, POWER, THRUST, POWER COEFFICIENT, THRUST
!     COEFFICIENT, FLOW RATE, FAN EFFICIENCY, AND PROPELLER EFFICIENCY
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               mom = ZERO
!
               thr = ZERO
!
               DO j = hep , tip - 1
!
                  IF ( j.GT.1 ) THEN
!
                     mom = mom + (re(j)-re(j-1))*mompr(j)
!
                     thr = thr + (re(j)-re(j-1))*thrpr(j)
!
                  ENDIF
!
               ENDDO
!
               mom = REAL(bm)*mom
!
               pow = mom*omega
!
               thr = REAL(bm)*thr
!
               cpow = pow/(rho*(freq**3.0D0)*((2.0D0*r(tip))**5.0D0))
!
               cthr = thr/(rho*(freq**2.0D0)*((2.0D0*r(tip))**4.0D0))
!
               ptot = thr/(PI*r(tip)**2.0D0)
!
               flwrte = SQRT((2.0D0*ABS(thr)*(PI*r(tip)**2.0D0))/rho)
!
               eff = flwrte*ptot/pow
!
               eta = advrat*(cthr/cpow)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     LOGIC TO PREVENT OUTPUT OF ERRONEOUS INFORMATION
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               IF ( cpow.LT.ZERO ) THEN
!
                  WRITE (*,*) 'AIRCRAFT VELOCITY LOOP NUMBER =' ,       &
     &                        countc ,                                  &
     &          ', STATUS = FORCED CONTINUE; SHAFT POWER LESS THAN ZERO'
!
!
                  GOTO 120
!
               ELSEIF ( cthr.LT.ZERO ) THEN
!
                  WRITE (*,*) 'AIRCRAFT VELOCITY LOOP NUMBER =' ,       &
     &                        countc ,                                  &
     &               ', STATUS = FORCED CONTINUE; THRUST LESS THAN ZERO'
!
                  GOTO 120
!
               ELSEIF ( eff.LT.ZERO ) THEN
!
                  WRITE (*,*) 'AIRCRAFT VELOCITY LOOP NUMBER =' ,       &
     &                        countc ,                                  &
     &       ', STATUS = FORCED CONTINUE; FAN EFFICIENCY LESS THAN ZERO'
!
                  GOTO 120
!
               ELSEIF ( eff.GT.1.0D0 ) THEN
!
                  WRITE (*,*) 'AIRCRAFT VELOCITY LOOP NUMBER =' ,       &
     &                        countc ,                                  &
     &    ', STATUS = FORCED CONTINUE; FAN EFFICIENCY GREATER THAN 100%'
!
                  GOTO 120
!
               ELSEIF ( eta.LT.ZERO ) THEN
!
                  WRITE (*,*) 'AIRCRAFT VELOCITY LOOP NUMBER =' ,       &
     &                        countc ,                                  &
     &      ', STATUS = FORCED CONTINUE; PROPELLER EFFICIENCY LESS THAN'&
     &      , ' ZERO'
!
                  GOTO 120
!
               ELSEIF ( eta.GT.1.0D0 ) THEN
!
                  WRITE (*,*) 'AIRCRAFT VELOCITY LOOP NUMBER =' ,       &
     &                        countc ,                                  &
     &   ', STATUS = FORCED CONTINUE; PROPELLER EFFICIENCY GREATER THAN'&
     &   , ' 100%'
!
                  GOTO 120
!
               ENDIF
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               WRITE (*,*) 'AIRCRAFT VELOCITY LOOP NUMBER =' , countc , &
     &                     ', STATUS = CONVERGED'
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     CREATING OUTPUT FILES
!
!     FAN EFFICIENCY
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               WRITE (*,*) v/sndspd , eff*100.0D0
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     MASS FLOW RATE
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               WRITE (*,*) v/sndspd , rho*flwrte
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     PROPELLER EFFICIENCY
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               WRITE (*,*) v/sndspd , eta*100.0D0
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     SHAFT POWER
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               WRITE (*,*) v/sndspd , pow/1000.0D0
!
               WRITE (*,*) v/sndspd , pow*0.00134102209240D0
!
               WRITE (*,*) v/sndspd , pow
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     THRUST
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               WRITE (*,*) v/sndspd , thr*0.1019716210D0
!
               WRITE (*,*) v/sndspd , thr*0.224808943870D0
!
               WRITE (*,*) v/sndspd , thr
!
               WRITE (*,*) v/sndspd , thr*3.59694307910D0
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     VOLUMETRIC FLOW RATE
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               WRITE (*,*) v/sndspd , flwrte*2118.87997280D0
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     END OF AIRCRAFT VELOCITY LOOP
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
               countc = countc + 1
!
               v = v + sndspd*((vend-vstart)/REAL(nvs))
!
            ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     FORCED CONTINUE
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     END OF PITCH LOOP
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
 120        WRITE (*,'(/)')
!
            WRITE (*,'(/)')
!
            WRITE (*,'(/)')
!
            WRITE (*,'(/)')
!
            WRITE (*,'(/)')
!
            WRITE (*,'(/)')
!
            WRITE (*,'(/)')
!
            WRITE (*,'(/)')
!
            WRITE (*,'(/)')
!
            WRITE (*,'(/)')
!
            WRITE (*,'(/)')
!
            WRITE (*,*)
!
            WRITE (*,*)
!
         ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     END OF INPUT FILE LOOP
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      ENDDO
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     FORCED EXIT
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     DETERMINING AND PRINTING CPU TIME
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
 200  CALL CPU_TIME(finish)
!
      cputimesecs = finish - start
!
      cputimemins = cputimesecs/60.0D0
!
      WRITE (*,*) 'CPU TIME (MINUTES) =' , cputimemins
!
      WRITE (*,*)
!
      WRITE (*,*) 'MP_PROP_DESIGN HAS FINISHED RUNNING'
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     ENDING PROGRAM
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!      CLOSE (3)
!!
!      CLOSE (4)
!!
!      CLOSE (7)
!!
!      CLOSE (8)
!!
!      CLOSE (9)
!!
!      CLOSE (10)
!!
!      CLOSE (11)
!!
!      CLOSE (12)
!!
!      CLOSE (13)
!!
!      CLOSE (14)
!!
!      CLOSE (15)
!
      CLOSE (16)
!
      CLOSE (17)
!
      CLOSE (18)
!
      CLOSE (19)
!
      END
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
