!*==MDBNCH.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!   SPAG options set to convert source form only
!     INCLUDE 'timer.for'
!     MDBNCH                                F.ERCOLESSI  17-DEC-1988
!                                                    REV 17-MAR-1990
!                                                    REV 17-DEC-1992
!                                                    REV  9-NOV-1993
!                                                    REV  2-NOV-1994
!                                                    REV 30-NOV-1994
!
!     (ALL REVS ARE JUST COSMETIC CLEANUPS, MOSTLY TO IMPROVE STANDARD
!      CONFORMANCE.  THE PROGRAM DOES THE EXACT SAME THING SINCE THE
!      17-DEC-1988 RELEASE).
!
!     MDBNCH IS A MOLECULAR DYNAMICS BENCHMARK.
!     THE SYSTEM SIMULATED IS GOLD, USING A MANY-BODY 'GLUE'
!     INTERACTION POTENTIAL. THREE DIFFERENT NUMBER OF PARTICLES
!     ARE USED: 256, 2048 AND 16384.
!
!     THE BENCHMARK DOES NOT REQUIRE ANY INPUT, AND CAN BE RUN
!     SIMPLY BY COMPILE-LINK-GO.
!     IT WRITES ITS RESULTS ON THE 'STANDARD OUTPUT', I.E. USING
!     FORTRAN PRINT STATEMENTS.  NO OTHER OUTPUT IS PRODUCED.
!     THAT'S ALL I/O: NO SCRATCH FILES ARE USED.
!
!     THE BENCHMARK IS SELF-CONTAINED: NO EXTERNAL ROUTINES ARE
!     NEEDED, WITH THE FOLLOWING EXCEPTION.
!     A
!                DOUBLE PRECISION FUNCTION SECOND()
!JRA - all internal timing removed by JRA Polyhedron - Feb 2004
!
!     SHOULD BE MADE AVAILABLE AT LINK TIME FOR TIMING PURPOSES.
!
!     THE BENCHMARK CONTAINS A BLOCK DATA: MAKE SURE IT IS
!     APPROPRIATELY PROCESSED BY YOUR COMPILER AND/OR LINKER.
!
!     THE BENCHMARK IS INTENDED TO BE RUN USING 64-BIT PRECISION FOR
!     ALL REAL VARIABLES.  TO THIS END, ALL REAL VARIABLES AND
!     CONSTANTS ARE DECLARED 'DOUBLE PRECISION', MEANING THAT
!     WE HAVE 32-BIT MACHINES IN MIND AS A TARGET.  IF YOU ARE GOING
!     TO RUN IT ON 64-BIT MACHINES, MAKE SURE TO SPECIFY THE COMPILER
!     OPTION TO TREAT DOUBLE PRECISION AS SINGLE.
!
!     PLEASE SEND ALL RESULTS TO FURIO@SISSA.IT, INCLUDING OUTPUT,
!     EXACT MACHINE TYPE, OPERATING SYSTEM AND COMPILER RELEASE,
!     COMPILER OPTIONS, AND ANY OTHER USEFUL INFORMATION.   THANKS.
!
!     THERE IS AN OFFICIAL MDBNCH WWW PAGE AT THE URL
!                   HTTP://WWW.SISSA.IT/FURIO/MDBNCH.HTML
!     (EVERYTHING IN LOWER CASE, THAT CANNOT BE USED HERE :-)
!     ESTABLISHED ON NOVEMBER 3, 1994.
!     THE READER IS REFERRED THERE FOR FURTHER INFORMATIONS.
!
!     NO HUMANS ALLOWED BEYOND THIS POINT.
!
! Sorry!  JRA (Polyhedron) tweaked parameters to burn more CPU
 
      PROGRAM MDBNCH
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /CONST / TWOpi , BOLtz
      COMMON /CNTRL / NTAble , LIStem , LMEthd , NDIff , NSTat , NGOfr ,&
     &                NTRaj , IVDump , ILIn , LOCk(3,3) , LILock(2,14) ,&
     &                NFReed , LOCkcm , NSCale , ITPart , ITWall
      COMMON /COUNT / NFI , LCOunt , LISter , KNTsta , KNTgor , LEP ,   &
     &                MANyon
      COMMON /DEN   / RNRho , RCRho , RNRho2 , RCRho2 , DR2rho , RHO(KR)&
     &                , DRHo(KR)
      COMMON /GEAR  / F02 , F12 , F32 , F42 , F52
      COMMON /GLUE  / DMIn , DMAx , DD , UJ(KG) , DUJ(KG)
      COMMON /IDENT / ELEmen , REF , TODay , NOW
      CHARACTER ELEmen*10 , REF*72 , TODay*10 , NOW*10
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /LSTUPD/ RLIst(3,NM)
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      COMMON /PARAM / DELta , DELta2 , GAMma , VSCale , CTRlce ,        &
     &                CTRlmi , CTRlma , RSQupd , RANsq , VMAs , BOX(3,3)
      COMMON /POT   / RN , RC , RN2 , RC2 , DR2 , VJ(KP) , FJ(KP)
      COMMON /PRESS / PEXt , PAI(3,3)
      COMMON /PRINT / LNGprt , IPRind
      COMMON /SCRATC/ DUMmy1(NM) , DUMmy2(NM) , DUMmy3(NM) , DUMmy4(NM)
      COMMON /STATIS/ FGS(NG) , GRAng , FACng , SCAby2 , RESz , DONtr , &
     &                FONtr , SIG2 , NGS(NG) , NGMax , NZHigh , NZLow , &
     &                MULtip
      COMMON /SUMS  / TEMpsm , TEMwsm , EKInsm , POT2sm , PGLusm ,      &
     &                POStsm , TOTesm , DENssm , ALSm , VOLusm ,        &
     &                AREasm , HEIgsm
      COMMON /THRU  / ATMass , ECOh , R0 , SPAref
!JRA      COMMON/TIMERS/TIMSTR,TIMFIX,TIMBLD,TIMFRC,TIMINT
!JRA      EXTERNAL SECOND
      PRINT '(/,1X,2A)' ,                                               &
     &      '     MDBNCH: A MOLECULAR DYNAMICS BENCHMARK, VERSION ' ,   &
     &      'OF DECEMBER 17, 1988'
!JRA      TIMALL=SECOND()
      nbsize = 8
               !4
      CALL MTE(nbsize)
      nsteps = 1000
      nlist = 10
      method = 0
      skin = 1.0D0
      ncorr = 0
      nprint = 100
      CALL MASTER(nsteps,nlist,method,skin,ncorr,nprint)
      nbsize = 8
      CALL MTE(nbsize)
      nsteps = 100
      nlist = 10
      method = 1
      skin = 1.0D0
      ncorr = 0
      nprint = 10
      CALL MASTER(nsteps,nlist,method,skin,ncorr,nprint)
      nbsize = 8
      CALL MTE(nbsize)
      nsteps = 100
      nlist = 10
      method = 0
      skin = 1.0D0
      ncorr = 0
      nprint = 10
      CALL MASTER(nsteps,nlist,method,skin,ncorr,nprint)
      nbsize = 8
      CALL MTE(nbsize)
      nsteps = 100
      nlist = 10
      method = 0
      skin = 1.0D0
      ncorr = 10
      nprint = 10
      CALL MASTER(nsteps,nlist,method,skin,ncorr,nprint)
      nbsize = 8
      CALL MTE(nbsize)
      nsteps = 1000
      nlist = 10
      method = 1
      skin = 1.5D0
      ncorr = 0
      nprint = 100
      CALL MASTER(nsteps,nlist,method,skin,ncorr,nprint)
      nbsize = 16
      CALL MTE(nbsize)
      nsteps = 100
      nlist = 5
      method = 1
      skin = 0.3D0
      ncorr = 0
      nprint = 10
      CALL MASTER(nsteps,nlist,method,skin,ncorr,nprint)
      nbsize = 16
      CALL MTE(nbsize)
      nsteps = 100
                 !10
      nlist = 5
      method = 1
      skin = 0.5D0
      ncorr = 0
      nprint = 1
      CALL MASTER(nsteps,nlist,method,skin,ncorr,nprint)
!JRA      TIMALL=SECOND()-TIMALL
!JRA      PRINT '(/,1X,79(''*''),/,1X,A,F12.6,A,/)',
!JRA     $'COMPLETE BENCHMARK EXECUTION TIME :',
!JRA     $TIMALL,' CP SECONDS.'
      END
!*==MASTER.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE MASTER(Nsteps,Nlist,Method,Skin,Ncorr,Nprint)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /CONST / TWOpi , BOLtz
      COMMON /CNTRL / NTAble , LIStem , LMEthd , NDIff , NSTat , NGOfr ,&
     &                NTRaj , IVDump , ILIn , LOCk(3,3) , LILock(2,14) ,&
     &                NFReed , LOCkcm , NSCale , ITPart , ITWall
      COMMON /COUNT / NFI , LCOunt , LISter , KNTsta , KNTgor , LEP ,   &
     &                MANyon
      COMMON /DEN   / RNRho , RCRho , RNRho2 , RCRho2 , DR2rho , RHO(KR)&
     &                , DRHo(KR)
      COMMON /GEAR  / F02 , F12 , F32 , F42 , F52
      COMMON /GLUE  / DMIn , DMAx , DD , UJ(KG) , DUJ(KG)
      COMMON /IDENT / ELEmen , REF , TODay , NOW
      CHARACTER ELEmen*10 , REF*72 , TODay*10 , NOW*10
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /LSTUPD/ RLIst(3,NM)
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      COMMON /PARAM / DELta , DELta2 , GAMma , VSCale , CTRlce ,        &
     &                CTRlmi , CTRlma , RSQupd , RANsq , VMAs , BOX(3,3)
      COMMON /PBCS  / HALf , PBCx , PBCy , PBCz
      COMMON /POT   / RN , RC , RN2 , RC2 , DR2 , VJ(KP) , FJ(KP)
      COMMON /PRESS / PEXt , PAI(3,3)
      COMMON /PRINT / LNGprt , IPRind
      COMMON /SCRATC/ DUMmy1(NM) , DUMmy2(NM) , DUMmy3(NM) , DUMmy4(NM)
      COMMON /STATIS/ FGS(NG) , GRAng , FACng , SCAby2 , RESz , DONtr , &
     &                FONtr , SIG2 , NGS(NG) , NGMax , NZHigh , NZLow , &
     &                MULtip
      COMMON /SUMS  / TEMpsm , TEMwsm , EKInsm , POT2sm , PGLusm ,      &
     &                POStsm , TOTesm , DENssm , ALSm , VOLusm ,        &
     &                AREasm , HEIgsm
      COMMON /THRU  / ATMass , ECOh , R0 , SPAref
!JRA      COMMON/TIMERS/TIMSTR,TIMFIX,TIMBLD,TIMFRC,TIMINT
!JRA      EXTERNAL SECOND
      PRINT '(/,1X,79(''*''),/)'
      PRINT '(1X,A,I6,A,I5,A)' , 'MD BENCHMARK FOR' , MOLsa ,           &
     &      ' PARTICLES,' , Nsteps , ' STEPS.'
      IF ( Method.EQ.0 ) THEN
         PRINT '(1X,A,I3,A,F5.2)' ,                                     &
     &         'O(N**2) BRUTE FORCE LIST FORMATION EVERY' , Nlist ,     &
     &         ' WITH SKIN =' , Skin
      ELSE
         PRINT '(1X,A,I3,A,F5.2)' ,                                     &
     &         'O(N) CELL-METHOD LIST FORMATION EVERY' , Nlist ,        &
     &         ' WITH SKIN =' , Skin
      ENDIF
      IF ( Ncorr.EQ.0 ) THEN
         PRINT '(1X,A)' , 'PAIR CORRELATION FUNCTION NOT COMPUTED'
      ELSE
         PRINT '(1X,A,I3,A)' ,                                          &
     &         'PAIR CORRELATION FUNCTION COMPUTED EVERY' , Ncorr ,     &
     &         ' STEPS'
      ENDIF
      CALL MINIT(Nlist,Method,Skin,Ncorr)
      DO istep = 1 , Nsteps
         NFI = NFI + 1
         IF ( (MOD(istep,Nprint).EQ.0) .OR. (istep.EQ.1) .OR.           &
     &        (istep.EQ.Nsteps) ) THEN
            LNGprt = 1
         ELSE
            LNGprt = 0
         ENDIF
         CALL MSTEP
      ENDDO
!JRA      TIMSTP=SECOND()
!JRA      TIMCPU=TIMSTP-TIMSTR
!JRA      TIMSTE=TIMCPU-TIMFIX
      PRINT '(/,1X,I5,A,I5,A)' , Nsteps , ' TIME STEPS,' , LCOunt ,     &
     &      ' LIST UPDATES'
!JRA      IF(TIMFIX.NE.0.D0)
!JRA     $PRINT '(1X,F11.6,A)',TIMFIX,
!JRA     $' SEC. CP TIME FOR INITIALIZATION'
!JRA      IF(TIMBLD.NE.0.D0)
!JRA     $PRINT '(1X,F11.6,A,F10.6,A)',TIMBLD,
!JRA     $' SEC. CP TIME UPDATING THE LIST        (',
!JRA     $TIMBLD/LCOUNT,' SEC/UPD. )'
!JRA      IF(TIMFRC.NE.0.D0)
!JRA     $PRINT '(1X,F11.6,A,F10.6,A)',TIMFRC,
!JRA     $' SEC. CP TIME CALCULATING FORCES       (',
!JRA     $TIMFRC/NSTEPS,' SEC/STEP )'
!JRA      IF(TIMINT.NE.0.D0)
!JRA     $PRINT '(1X,F11.6,A,F10.6,A)',TIMINT,
!JRA     $' SEC. CP TIME FOR TIME INTEGRATION     (',
!JRA     $TIMINT/NSTEPS,' SEC/STEP )'
!JRA      TIMOTH=TIMCPU-(TIMFIX+TIMBLD+TIMFRC+TIMINT)
!JRA      IF(TIMOTH.NE.0.D0)
!JRA     $PRINT '(1X,F11.6,A,F10.6,A)',TIMOTH,
!JRA     $' SEC. CP TIME FOR OTHER TASKS          (',
!JRA     $TIMOTH/NSTEPS,' SEC/STEP )'
!JRA      IF(TIMSTE.NE.0.D0)
!JRA     $PRINT '(1X,F11.6,A,F10.6,A)',TIMSTE,
!JRA     $' SEC. CP TIME EXCLUDING INITIALIZATION (',
!JRA     $TIMSTE/NSTEPS,' SEC/STEP )'
!JRA      IF(TIMCPU.NE.0.D0)
!JRA     $PRINT '(1X,F11.6,A)',TIMCPU,
!JRA     $' SEC. TOTAL CP TIME'
      CONTINUE
      END
!*==MINIT.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE MINIT(Nlist,Method,Skin,Ncorr)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /CONST / TWOpi , BOLtz
      COMMON /CNTRL / NTAble , LIStem , LMEthd , NDIff , NSTat , NGOfr ,&
     &                NTRaj , IVDump , ILIn , LOCk(3,3) , LILock(2,14) ,&
     &                NFReed , LOCkcm , NSCale , ITPart , ITWall
      COMMON /COUNT / NFI , LCOunt , LISter , KNTsta , KNTgor , LEP ,   &
     &                MANyon
      COMMON /DEN   / RNRho , RCRho , RNRho2 , RCRho2 , DR2rho , RHO(KR)&
     &                , DRHo(KR)
      COMMON /GEAR  / F02 , F12 , F32 , F42 , F52
      COMMON /GLUE  / DMIn , DMAx , DD , UJ(KG) , DUJ(KG)
      COMMON /IDENT / ELEmen , REF , TODay , NOW
      CHARACTER ELEmen*10 , REF*72 , TODay*10 , NOW*10
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      COMMON /PARAM / DELta , DELta2 , GAMma , VSCale , CTRlce ,        &
     &                CTRlmi , CTRlma , RSQupd , RANsq , VMAs , BOX(3,3)
      COMMON /PBCS  / HALf , PBCx , PBCy , PBCz
      COMMON /POT   / RN , RC , RN2 , RC2 , DR2 , VJ(KP) , FJ(KP)
      COMMON /PRESS / PEXt , PAI(3,3)
      COMMON /PRINT / LNGprt , IPRind
      COMMON /SCRATC/ DUMmy1(NM) , DUMmy2(NM) , DUMmy3(NM) , DUMmy4(NM)
      COMMON /STATIS/ FGS(NG) , GRAng , FACng , SCAby2 , RESz , DONtr , &
     &                FONtr , SIG2 , NGS(NG) , NGMax , NZHigh , NZLow , &
     &                MULtip
      COMMON /SUMS  / TEMpsm , TEMwsm , EKInsm , POT2sm , PGLusm ,      &
     &                POStsm , TOTesm , DENssm , ALSm , VOLusm ,        &
     &                AREasm , HEIgsm
      COMMON /THRU  / ATMass , ECOh , R0 , SPAref
!JRA      COMMON/TIMERS/TIMSTR,TIMFIX,TIMBLD,TIMFRC,TIMINT
      DIMENSION H(3,3) , H1(3,3) , H2(3,3) , H3(3,3) , H4(3,3) , H5(3,3)&
     &          , HIN(3,3)
      EQUIVALENCE (X0(1,-2),H(1,1))
      EQUIVALENCE (X(1,-2,1),H1(1,1))
      EQUIVALENCE (X(1,-2,2),H2(1,1))
      EQUIVALENCE (X(1,-2,3),H3(1,1))
      EQUIVALENCE (X(1,-2,4),H4(1,1))
      EQUIVALENCE (X(1,-2,5),H5(1,1))
      EQUIVALENCE (XIN(1,-2),HIN(1,1))
      DIMENSION hinput(3,3)
      CHARACTER oldref*72 , ystrdy*10 , before*10
!JRA      EXTERNAL SECOND
      CHARACTER*10 VERS
      PARAMETER (VERS='62.2      ')
      PARAMETER (PI=3.141592653589793D0)
      DATA cf/1.03641D-28/
 3    FORMAT (/,1X,A)
 1    FORMAT ('1')
!JRA      TIMSTR=SECOND()
!JRA      TIMFIX=0.D0
!JRA      TIMBLD=0.D0
!JRA      TIMFRC=0.D0
!JRA      TIMINT=0.D0
      TWOpi = PI + PI
      BOLtz = 11606.D0
      oldref = REF
      ystrdy = TODay
      before = NOW
      dltold = DELta
      CALL RESET(X,3*(NM+3)*5)
      CALL IRESET(NGS,NGMax)
      CALL RESET(FGS,NGMax)
      MANyon = 1
      ibeg = 0
      NTRaj = 0
      NSTat = 0
      NGOfr = Ncorr
      NDIff = 0
      IPRind = 0
      LNGprt = 0
      NTAble = 0
      LIStem = Nlist
      DELta = 0.05D0
      NSCale = 0
      VSCale = 1.D0
      ITPart = 0
      ITWall = 0
      centre = -3.0000D0
      toller = 0.0002D0
      ihchge = 1
      spacng = 4.15D0
      LOCkcm = 0
      LOCk(1,1) = -2
      LOCk(2,1) = -1
      LOCk(3,1) = -1
      LOCk(1,2) = -1
      LOCk(2,2) = -2
      LOCk(3,2) = -1
      LOCk(1,3) = -1
      LOCk(2,3) = -1
      LOCk(3,3) = -2
      VMAs = 0.5D0
      iwdamp = 0
      GAMma = 0.D0
      PEXt = 0.D0
      DO i = 1 , 3
         PAI(1,i) = 0.D0
         PAI(2,i) = 0.D0
         PAI(3,i) = 0.D0
      ENDDO
      ILIn = 0
      IF ( NTRaj.EQ.0 ) THEN
         IVDump = 0
      ELSEIF ( NTRaj.GT.0 ) THEN
         IVDump = 1
      ELSE
         IVDump = 0
         NTRaj = -NTRaj
      ENDIF
      Skin = ABS(Skin)
      LMEthd = Method
      IF ( LIStem.EQ.1 ) Skin = 0.D0
      IF ( NSCale.LE.0 ) VSCale = 1.D0
      IF ( iwdamp.EQ.0 ) GAMma = 0.D0
      IF ( toller.LT.0.D0 ) toller = -toller
      LISter = LIStem
      LCOunt = 0
      RSQupd = (0.5D0*Skin)**2
      F02 = 3.D0/16.D0
      F12 = 251.D0/360.D0
      F32 = 11.D0/18.D0
      F42 = 1.D0/6.D0
      F52 = 1.D0/60.D0
      DELta2 = DELta**2
      CTRlce = centre
      CTRlmi = centre - toller
      CTRlma = centre + toller
      LPBcsm = LPBc(1) + LPBc(2) + LPBc(3)
      HALf = 0.5D0
      PBCx = LPBc(1)
      PBCy = LPBc(2)
      PBCz = LPBc(3)
      IF ( LPBcsm.GT.1 ) THEN
         spcing = R0*BOX(3,3)/NPLa
      ELSE
         spcing = R0
      ENDIF
      SIG2 = 1.D0/(2.D0*(spcing/2.D0)**2)
      l = LPBc(1) + LPBc(2) + LPBc(3)
      CALL POTENT
      IF ( MANyon.NE.0 ) THEN
         CALL DENSIT
         CALL ELGLUE
         RANsq = (MAX(RC,RCRho)+Skin)**2
      ELSE
         RANsq = (RC+Skin)**2
      ENDIF
      IF ( (DELta.NE.dltold) .AND. (dltold.GT.0.D0) ) THEN
         ratio = DELta/dltold
         ratio2 = ratio**2
         ratio4 = ratio2**2
         DO k = 1 , 3
            DO i = -2 , MOLsp
               X(k,i,1) = X(k,i,1)*ratio
               X(k,i,2) = X(k,i,2)*ratio2
               X(k,i,3) = X(k,i,3)*ratio2*ratio
               X(k,i,4) = X(k,i,4)*ratio4
               X(k,i,5) = X(k,i,5)*ratio4*ratio
            ENDDO
         ENDDO
      ENDIF
      IF ( ihchge.NE.0 ) THEN
         r0new = R0*spacng/SPAref
         DO k = 1 , 3
            DO j = 1 , 3
               hinput(j,k) = r0new*BOX(j,k)
            ENDDO
         ENDDO
         DO k = 1 , 3
            DO j = 1 , 3
               IF ( hinput(j,k).NE.H(j,k) ) GOTO 1094
            ENDDO
         ENDDO
         ihchge = 0
 1094    CONTINUE
      ENDIF
      IF ( ihchge.NE.0 ) THEN
         DO k = 1 , 3
            DO j = 1 , 3
               H(j,k) = hinput(j,k)
            ENDDO
         ENDDO
      ENDIF
      malock = 0
      DO k = 1 , 3
         DO j = 1 , 3
            IF ( LOCk(j,k).GT.malock ) THEN
               malock = LOCk(j,k)
            ELSEIF ( LOCk(j,k).LT.0 ) THEN
               H1(j,k) = 0.D0
               H2(j,k) = 0.D0
               H3(j,k) = 0.D0
               H4(j,k) = 0.D0
               H5(j,k) = 0.D0
               IF ( LOCk(j,k).EQ.-1 ) THEN
                  H(j,k) = 0.D0
                  HIN(j,k) = 0.D0
                  IF ( j.EQ.k ) PRINT 2 ,                               &
     &                         'MINIT: A DIAGONAL ELEMENT OF H IS ZERO.'
 2                FORMAT (1X,A)
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      l = 1
      DO i = 1 , malock
         m = 0
         DO k = 1 , 3
            DO j = 1 , 3
               IF ( LOCk(j,k).EQ.i ) THEN
                  m = m + 1
                  LILock(1,l+m) = j
                  LILock(2,l+m) = k
               ENDIF
            ENDDO
         ENDDO
         IF ( m.GT.1 ) THEN
            LILock(1,l) = m
            LILock(2,l) = 0
            l = l + m + 1
         ELSEIF ( m.EQ.1 ) THEN
            LOCk(LILock(1,l+m),LILock(2,l+m)) = 0
            PRINT '(1X,A,I3)' ,                                         &
     &            'MINIT: SINGLE ELEMENT IN LOCK CLASS NR.' , i
         ELSE
         ENDIF
      ENDDO
      LILock(1,l) = 0
      LILock(2,l) = 0
      IF ( l.GT.14 ) PRINT '(/,1X,A,I3)' ,                              &
     &                     'MINIT: LILOCK OVERFLOW, INDEX IS' , l
      CALL SYMM(H1)
      CALL SYMM(H2)
      CALL SYMM(H3)
      CALL SYMM(H4)
      CALL SYMM(H5)
      NFReed = 0
      DO j = 1 , 3
         DO i = 1 , 3
            IF ( LOCk(i,j).EQ.0 ) NFReed = NFReed + 1
         ENDDO
      ENDDO
      l = 1
 200  CONTINUE
      IF ( LILock(1,l).NE.0 ) THEN
         NFReed = NFReed + 1
         l = l + LILock(1,l) + 1
         GOTO 200
      ENDIF
      IF ( ibeg.EQ.0 ) THEN
         DO i = -2 , MOLsa
            XIN(1,i) = X0(1,i)
            XIN(2,i) = X0(2,i)
            XIN(3,i) = X0(3,i)
         ENDDO
         CALL RESET(FGS,NGMax)
         CALL IRESET(NGS,NGMax)
         NFI = 0
         KNTgor = 0
         KNTsta = 0
         TEMpsm = 0.D0
         TEMwsm = 0.D0
         EKInsm = 0.D0
         POT2sm = 0.D0
         PGLusm = 0.D0
         POStsm = 0.D0
         TOTesm = 0.D0
         DENssm = 0.D0
         ALSm = 0.D0
         VOLusm = 0.D0
         AREasm = 0.D0
         HEIgsm = 0.D0
      ENDIF
      PRINT '(/,1X,2A)' , ' STEP LP  KIN.E   POT.E   TOT.E ' ,          &
     &      '  DIFFUS     PX       PY       PZ   '
      PRINT '(1X,2A)' , ' ---- -- ------- ------- -------' ,            &
     &      ' -------- -------- -------- --------'
!JRA      TIMFIX=TIMFIX+(SECOND()-TIMSTR)
      CONTINUE
      END
!*==SYMM.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE SYMM(Hn)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Hn(3,3)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /CNTRL / NTAble , LIStem , LMEthd , NDIff , NSTat , NGOfr ,&
     &                NTRaj , IVDump , ILIn , LOCk(3,3) , LILock(2,14) ,&
     &                NFReed , LOCkcm , NSCale , ITPart , ITWall
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      DIMENSION H(3,3)
      EQUIVALENCE (X0(1,-2),H(1,1))
      l = 1
 1    CONTINUE
      IF ( LILock(1,l).NE.0 ) THEN
         sum = 0.D0
         DO m = 1 , LILock(1,l)
            i = LILock(1,l+m)
            j = LILock(2,l+m)
            IF ( H(i,j).NE.0.D0 ) THEN
               sum = sum + Hn(i,j)/H(i,j)
            ELSE
               sum = sum + Hn(i,j)
            ENDIF
         ENDDO
         sum = sum/LILock(1,l)
         DO m = 1 , LILock(1,l)
            i = LILock(1,l+m)
            j = LILock(2,l+m)
            IF ( H(i,j).NE.0.D0 ) THEN
               Hn(i,j) = sum*H(i,j)
            ELSE
               Hn(i,j) = sum
            ENDIF
         ENDDO
         l = l + LILock(1,l) + 1
         GOTO 1
      ENDIF
      CONTINUE
      END
!*==POTENT.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE POTENT
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /POT   / RN , RC , RN2 , RC2 , DR2 , VJ(KP) , FJ(KP)
      COMMON /THRU  / ATMass , ECOh , R0 , SPAref
      CHARACTER*72 title
      title = 'TWO-BODY POTENTIAL'
      nint = KP
      RN = 1.69D0
      RC = 3.7D0
      R0 = 0.2878207442D+01
      rhard = RN
      RN2 = RN**2
      RC2 = RC**2
      DR2 = (RC2-RN2)/(nint-1)
      DO i = 1 , KP
         rsq = RN2 + (i-1)*DR2
         r = SQRT(rsq)
         CALL POTFUN(r,phi,dphi,d2phi)
         VJ(i) = phi
         FJ(i) = -dphi/r
      ENDDO
      CONTINUE
      END
!*==DENSIT.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE DENSIT
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /DEN   / RNRho , RCRho , RNRho2 , RCRho2 , DR2rho , RHO(KR)&
     &                , DRHo(KR)
      CHARACTER*72 title
      title = 'DENSITY FUNCTION'
      nint = KR
      RNRho = 1.69D0
      RCRho = 3.9D0
      RNRho2 = RNRho**2
      RCRho2 = RCRho**2
      DR2rho = (RCRho2-RNRho2)/(nint-1)
      DO i = 1 , KR
         rsq = RNRho2 + (i-1)*DR2rho
         r = SQRT(rsq)
         CALL DENFUN(r,rh,drh,d2rh)
         RHO(i) = rh
         DRHo(i) = -drh/r
      ENDDO
      CONTINUE
      END
!*==ELGLUE.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE ELGLUE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /GLUE  / DMIn , DMAx , DD , UJ(KG) , DUJ(KG)
      CHARACTER*72 title
      title = 'GLUE'
      nint = KG
      DMIn = 0.D0
      DMAx = 20.D0
      DD = (DMAx-DMIn)/(nint-1)
      DO i = 1 , KG
         dens = DMIn + (i-1)*DD
         CALL GLUFUN(dens,u0,u1,u2)
         UJ(i) = u0
         DUJ(i) = u1
      ENDDO
      CONTINUE
      END
!*==AU053.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      BLOCKDATA AU053
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /DENDAT/ RRD , RRB , RRC , RHOd , RHOa , R1I , R2I , R3I , &
     &                R1Ii , R2Ii , R3Ii , R2Iii , R3Iii
      COMMON /GLUDAT/ DB , UB , DSW , B0I , B1I , B2I , B3I , B4I ,     &
     &                B2Ii , B3Ii , B4Ii , B2Iii , B3Iii
      COMMON /POTDAT/ D , A , RC , PHI1 , PHI2 , A0I , A1I , A2I , A3I ,&
     &                A4I , A0Ii , A1Ii , A2Ii , A3Ii , A4Ii , A5Ii ,   &
     &                A6Ii , A3Iii , A4Iii , A5Iii
      DATA RRD , RRB , RRC , RHOd , RHOa , R1I , R2I , R3I , R1Ii ,     &
     &     R2Ii , R3Ii , R2Iii , R3Iii/0.2878207442141723D+01 ,         &
     &     0.3500000000000000D+01 , 0.3900000000000000D+01 ,            &
     &     0.1000000000000000D+01 , 0.0000000000000000D+00 ,            &
     &     -0.6800000000000000D+00 , 0.7500000000000000D+00 ,           &
     &     -0.1333333333333333D+01 , -0.6800000000000000D+00 ,          &
     &     0.7500000000000000D+00 , -0.1527241171296038D+01 ,           &
     &     0.5578188675490974D+01 , 0.6132971688727435D+01/
      DATA DB , UB , DSW/0.1200000000000000D+02 ,                       &
     &     -0.3300000000000000D+01 , 0.9358157767784574D+01/
      DATA B0I , B1I , B2I , B3I , B4I , B2Ii , B3Ii , B4Ii , B2Iii ,   &
     &     B3Iii/ - 0.2793388616771698D+01 , -0.3419999999999999D+00 ,  &
     &     0.3902327808424106D-01 , 0.7558829951858879D-02 ,            &
     &     0.3090472511796849D-03 , 0.8618226772941980D-01 ,            &
     &     0.4341701445034724D-02 , -0.3044398779375916D-03 ,           &
     &     0.8618226772941980D-01 , 0.4325981467602070D-02/
      DATA D , A , RC , PHI1 , PHI2/0.2878207442141723D+01 ,            &
     &     0.4070400000000000D+01 , 0.3700000000000000D+01 ,            &
     &     -0.8000000000000000D-01 , 0.0000000000000000D+00/
      DATA A0I , A1I , A2I , A3I , A4I , A0Ii , A1Ii , A2Ii , A3Ii ,    &
     &     A4Ii , A5Ii , A6Ii , A3Iii , A4Iii ,                         &
     &     A5Iii/ - 0.8000000000000000D-01 , 0.0000000000000000D+00 ,   &
     &     0.7619231375231362D+00 , -0.8333333333333333D+00 ,           &
     &     -0.1211483464993159D+00 , -0.8000000000000000D-01 ,          &
     &     0.0000000000000000D+00 , 0.7619231375231362D+00 ,            &
     &     -0.8333333333333333D+00 , -0.1096009851140349D+01 ,          &
     &     0.2158417178555998D+01 , -0.9128915709636862D+00 ,           &
     &     0.0000000000000000D+00 , 0.0000000000000000D+00 ,            &
     &     0.0000000000000000D+00/
      END
!*==DENFUN.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE DENFUN(R,Rho,Drho,D2rho)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /DENDAT/ RRD , RRB , RRC , RHOd , RHOa , R1I , R2I , R3I , &
     &                R1Ii , R2Ii , R3Ii , R2Iii , R3Iii
      IF ( R.GE.RRC ) THEN
         Rho = 0.D0
         Drho = 0.D0
         D2rho = 0.D0
      ELSEIF ( R.GE.RRB ) THEN
         x = R - RRC
         Rho = (x**2)*(R2Iii+x*R3Iii)
         Drho = x*(2.D0*R2Iii+x*3.D0*R3Iii)
         D2rho = 2.D0*R2Iii + x*6.D0*R3Iii
      ELSEIF ( R.GE.RRD ) THEN
         x = R - RRD
         Rho = RHOd + x*(R1Ii+x*(R2Ii+x*R3Ii))
         Drho = R1Ii + x*(2.D0*R2Ii+x*3.D0*R3Ii)
         D2rho = 2.D0*R2Ii + x*6.D0*R3Ii
      ELSE
         x = R - RRD
         Rho = RHOd + x*(R1I+x*(R2I+x*R3I))
         Drho = R1I + x*(2.D0*R2I+x*3.D0*R3I)
         D2rho = 2.D0*R2I + x*6.D0*R3I
      ENDIF
      CONTINUE
      END
!*==GLUFUN.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE GLUFUN(Dens,U,U1,U2)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /GLUDAT/ DB , UB , DSW , B0I , B1I , B2I , B3I , B4I ,     &
     &                B2Ii , B3Ii , B4Ii , B2Iii , B3Iii
      IF ( Dens.GT.DB ) THEN
         x = Dens - DB
         U = UB + (x**2)*(B2Iii+x*B3Iii)
         U1 = x*(2.D0*B2Iii+x*3.D0*B3Iii)
         U2 = 2.D0*B2Iii + x*6.D0*B3Iii
      ELSEIF ( Dens.GT.DSW ) THEN
         x = Dens - DB
         U = UB + (x**2)*(B2Ii+x*(B3Ii+x*B4Ii))
         U1 = x*(2.D0*B2Ii+x*(3.D0*B3Ii+x*4.D0*B4Ii))
         U2 = 2.D0*B2Ii + x*(6.D0*B3Ii+x*12.D0*B4Ii)
      ELSE
         x = Dens - DSW
         U = B0I + x*(B1I+x*(B2I+x*(B3I+x*B4I)))
         U1 = B1I + x*(2.D0*B2I+x*(3.D0*B3I+x*4.D0*B4I))
         U2 = 2.D0*B2I + x*(6.D0*B3I+x*12.D0*B4I)
      ENDIF
      CONTINUE
      END
!*==POTFUN.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE POTFUN(R,Phi,Dphi,D2phi)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /POTDAT/ D , A , RC , PHI1 , PHI2 , A0I , A1I , A2I , A3I ,&
     &                A4I , A0Ii , A1Ii , A2Ii , A3Ii , A4Ii , A5Ii ,   &
     &                A6Ii , A3Iii , A4Iii , A5Iii
      IF ( R.GE.RC ) THEN
         Phi = 0.D0
         Dphi = 0.D0
         D2phi = 0.D0
      ELSEIF ( R.GE.A ) THEN
         x = R - RC
         Phi = (x**3)*(A5Iii*x**2+A4Iii*x+A3Iii)
         Dphi = (x**2)*(5.D0*A5Iii*x**2+4.D0*A4Iii*x+3.D0*A3Iii)
         D2phi = x*(20.D0*A5Iii*x**2+12.D0*A4Iii*x+6.D0*A3Iii)
      ELSEIF ( R.GE.D ) THEN
         x = R - D
         Phi = A0Ii + x*(A1Ii+x*(A2Ii+x*(A3Ii+x*(A4Ii+x*(A5Ii+x*A6Ii))))&
     &         )
         Dphi = A1Ii +                                                  &
     &          x*(2.D0*A2Ii+x*(3.D0*A3Ii+x*(4.D0*A4Ii+x*(5.D0*A5Ii+    &
     &          x*6.D0*A6Ii))))
         D2phi = 2.D0*A2Ii +                                            &
     &           x*(6.D0*A3Ii+x*(12.D0*A4Ii+x*(20.D0*A5Ii+x*30.D0*A6Ii))&
     &           )
      ELSE
         x = R - D
         Phi = A0I + x*(A1I+x*(A2I+x*(A3I+x*A4I)))
         Dphi = A1I + x*(2.D0*A2I+x*(3.D0*A3I+x*4.D0*A4I))
         D2phi = 2.D0*(A2I+x*(3.D0*A3I+x*6.D0*A4I))
      ENDIF
      CONTINUE
      END
!*==MSTEP.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE MSTEP
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /CNTRL / NTAble , LIStem , LMEthd , NDIff , NSTat , NGOfr ,&
     &                NTRaj , IVDump , ILIn , LOCk(3,3) , LILock(2,14) ,&
     &                NFReed , LOCkcm , NSCale , ITPart , ITWall
      COMMON /CONST / TWOpi , BOLtz
      COMMON /COUNT / NFI , LCOunt , LISter , KNTsta , KNTgor , LEP ,   &
     &                MANyon
      COMMON /DEN   / RNRho , RCRho , RNRho2 , RCRho2 , DR2rho , RHO(KR)&
     &                , DRHo(KR)
      COMMON /GEAR  / F02 , F12 , F32 , F42 , F52
      COMMON /GLUE  / DMIn , DMAx , DD , UJ(KG) , DUJ(KG)
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /LISCOM/ LISt(LL) , MRKr1(NM) , MRKr2(NM) , LISlen
      COMMON /LSTUPD/ RLIst(3,NM)
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      COMMON /MOTION/ VIRkin(3,3) , VIRpot(3,3) , XNP(3,-2:NM)
      COMMON /PARAM / DELta , DELta2 , GAMma , VSCale , CTRlce ,        &
     &                CTRlmi , CTRlma , RSQupd , RANsq , VMAs , BOX(3,3)
      COMMON /POT   / RN , RC , RN2 , RC2 , DR2 , VJ(KP) , FJ(KP)
      COMMON /PRESS / PEXt , PAI(3,3)
      COMMON /PRINT / LNGprt , IPRind
      COMMON /SCRATC/ DUMmy1(NM) , DUMmy2(NM) , DUMmy3(NM) , DUMmy4(NM)
      COMMON /STATIS/ FGS(NG) , GRAng , FACng , SCAby2 , RESz , DONtr , &
     &                FONtr , SIG2 , NGS(NG) , NGMax , NZHigh , NZLow , &
     &                MULtip
      COMMON /SUMS  / TEMpsm , TEMwsm , EKInsm , POT2sm , PGLusm ,      &
     &                POStsm , TOTesm , DENssm , ALSm , VOLusm ,        &
     &                AREasm , HEIgsm
!JRA      COMMON/TIMERS/TIMSTR,TIMFIX,TIMBLD,TIMFRC,TIMINT
      COMMON /WALLS / HI(3,3) , G(3,3) , DH , AREa , VOLume , SCM(3)
      DIMENSION H(3,3) , HD(3,3) , HIN(3,3)
      DIMENSION hihd(3,3) , hihdt(3,3) , hhh(3,3) , hcom(3,3)
      DIMENSION hdt(3,3) , hsav(3,3) , ht(3,3)
      DIMENSION gi(3,3)
      DIMENSION VM(3,3)
      DIMENSION smin(3) , smax(3)
      EQUIVALENCE (X0(1,-2),H(1,1))
      EQUIVALENCE (X(1,-2,1),HD(1,1))
      EQUIVALENCE (XNP(1,-2),VM(1,1))
      EQUIVALENCE (XIN(1,-2),HIN(1,1))
!JRA      EXTERNAL SECOND
      SAVE temp , temw , temn , ekinp , ekinw , tote
      LOGICAL firstp , update , upgofr
      SAVE firstp , update
      CHARACTER*1 chard , charl , chars , chart , charp , charw , charv
      SAVE chard , charl , chars , chart , charp , charw , charv
      DATA chard , charl , chars , chart , charp , charw , charv/' ' ,  &
     &     ' ' , ' ' , ' ' , ' ' , ' ' , ' '/
      DATA firstp , update/.TRUE. , .TRUE./
!JRA      CPTIME=SECOND()
      IF ( firstp ) THEN
         temp = CTRlce
         temw = CTRlce
         temn = CTRlce
         tote = CTRlce
         firstp = .FALSE.
      ELSE
         IF ( (ITPart.EQ.2) .OR. (ITWall.EQ.2) ) THEN
            ekinc = 0.D0
            IF ( ITPart.EQ.2 ) ekinc = ekinc + ekinp
            IF ( ITWall.EQ.2 ) ekinc = ekinc + ekinw
         ENDIF
      ENDIF
      IF ( NSCale.GT.0 ) THEN
         charv = ' '
         IF ( MOD(NFI,NSCale).EQ.0 ) THEN
            charv = 'V'
            pscale = VSCale
            wscale = VSCale
         ELSE
            pscale = 1.D0
            wscale = 1.D0
         ENDIF
      ELSE
         pscale = 1.D0
         wscale = 1.D0
      ENDIF
      IF ( ITPart.NE.0 ) THEN
         charp = ' '
         IF ( ITPart.EQ.1 ) THEN
            IF ( (temn.LT.CTRlmi) .OR. (temn.GT.CTRlma) ) THEN
               IF ( temn.GT.0.D0 ) THEN
                  pscale = SQRT(CTRlce/temn)
                  charp = 'P'
               ENDIF
            ENDIF
         ELSE
            IF ( (tote.LT.CTRlmi) .OR. (tote.GT.CTRlma) ) THEN
               IF ( ekinc.GT.0.D0 ) THEN
                  pscal2 = 1.D0 + (CTRlce-tote)/ekinc
                  IF ( pscal2.GT.0.D0 ) THEN
                     pscale = SQRT(pscal2)
                  ELSE
                     pscale = 0.D0
                  ENDIF
                  charp = 'P'
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF ( ITWall.NE.0 ) THEN
         charw = ' '
         IF ( ITWall.EQ.1 ) THEN
            IF ( (temw.LT.CTRlmi) .OR. (temw.GT.CTRlma) ) THEN
               IF ( temw.GT.0.D0 ) THEN
                  wscale = SQRT(temp/temw)
                  charw = 'W'
               ENDIF
            ENDIF
         ELSE
            IF ( (tote.LT.CTRlmi) .OR. (tote.GT.CTRlma) ) THEN
               IF ( ekinc.GT.0.D0 ) THEN
                  wscal2 = 1.D0 + (CTRlce-tote)/ekinc
                  IF ( wscal2.GT.0.D0 ) THEN
                     wscale = SQRT(wscal2)
                  ELSE
                     wscale = 0.D0
                  ENDIF
                  charw = 'W'
               ENDIF
            ENDIF
         ENDIF
      ENDIF
!JRA      TIMREF=SECOND()
      DO k = 1 , 3
         DO i = -2 , MOLsp
            scafac = pscale
            IF ( i.LE.0 ) scafac = wscale
            X(k,i,1) = X(k,i,1)*scafac
            X0(k,i) = X0(k,i) + X(k,i,1) + X(k,i,2) + X(k,i,3)          &
     &                + X(k,i,4) + X(k,i,5)
            X(k,i,1) = X(k,i,1) + 2.D0*X(k,i,2) + 3.D0*X(k,i,3)         &
     &                 + 4.D0*X(k,i,4) + 5.D0*X(k,i,5)
            X(k,i,2) = X(k,i,2) + 3.D0*X(k,i,3) + 6.D0*X(k,i,4)         &
     &                 + 10.D0*X(k,i,5)
            X(k,i,3) = X(k,i,3) + 4.D0*X(k,i,4) + 10.D0*X(k,i,5)
            X(k,i,4) = X(k,i,4) + 5.D0*X(k,i,5)
            XNP(k,i) = 0.D0
         ENDDO
      ENDDO
!JRA      TIMINT=TIMINT+(SECOND()-TIMREF)
      DO i = 1 , 3
         DO j = 1 , 3
            ht(i,j) = H(j,i)
            hdt(i,j) = HD(j,i)
         ENDDO
      ENDDO
      CALL MTXINV(H,HI,DH)
      CALL MTXMTP(ht,H,G)
      CALL MTXMTP(hdt,HD,hhh)
      urans = 0.5D0*(hhh(1,1)+hhh(2,2)+hhh(3,3))/(DELta2*VMAs)
      DO k = 1 , 3
         IF ( LPBc(k).EQ.0 ) THEN
            SCM(k) = 0.D0
            smin(k) = 0.D0
            smax(k) = 0.D0
            DO i = 1 , MOLsa
               sski = X0(k,i)
               SCM(k) = SCM(k) + sski
               IF ( sski.GT.smax(k) ) smax(k) = sski
               IF ( sski.LT.smin(k) ) smin(k) = sski
            ENDDO
            SCM(k) = SCM(k)/MOLsa
            IF ( LOCkcm.NE.0 ) THEN
               DO i = 1 , MOLsa
                  X0(k,i) = X0(k,i) - SCM(k)
               ENDDO
               SCM(k) = 0.D0
            ENDIF
         ELSE
            SCM(k) = 0.D0
            smin(k) = -0.5D0
            smax(k) = +0.5D0
         ENDIF
      ENDDO
      VOLume = DH*(smax(1)-smin(1))*(smax(2)-smin(2))*(smax(3)-smin(3))
      AREa = HI(3,3)*DH*(smax(1)-smin(1))*(smax(2)-smin(2))
      heig = VOLume/AREa
      CALL RESET(VIRkin,9)
      DO i = 1 , MOLsp
         velocx = H(1,1)*X(1,i,1) + H(1,2)*X(2,i,1) + H(1,3)*X(3,i,1)
         velocy = H(2,1)*X(1,i,1) + H(2,2)*X(2,i,1) + H(2,3)*X(3,i,1)
         velocz = H(3,1)*X(1,i,1) + H(3,2)*X(2,i,1) + H(3,3)*X(3,i,1)
         VIRkin(1,1) = VIRkin(1,1) + velocx*X(1,i,1)
         VIRkin(2,1) = VIRkin(2,1) + velocy*X(1,i,1)
         VIRkin(3,1) = VIRkin(3,1) + velocz*X(1,i,1)
         VIRkin(1,2) = VIRkin(1,2) + velocx*X(2,i,1)
         VIRkin(2,2) = VIRkin(2,2) + velocy*X(2,i,1)
         VIRkin(3,2) = VIRkin(3,2) + velocz*X(2,i,1)
         VIRkin(1,3) = VIRkin(1,3) + velocx*X(3,i,1)
         VIRkin(2,3) = VIRkin(2,3) + velocy*X(3,i,1)
         VIRkin(3,3) = VIRkin(3,3) + velocz*X(3,i,1)
      ENDDO
      upgofr = .FALSE.
      IF ( NGOfr.GT.0 ) THEN
         IF ( MOD(NFI-1,NGOfr).EQ.0 ) THEN
            upgofr = .TRUE.
            update = .TRUE.
         ENDIF
      ENDIF
      IF ( .NOT.update ) THEN
         IF ( LIStem.GT.0 ) THEN
            update = (LISter.EQ.LIStem)
         ELSE
            DO i = 1 , MOLsa
               rxi = H(1,1)*X0(1,i) + H(1,2)*X0(2,i) + H(1,3)*X0(3,i)
               ryi = H(2,1)*X0(1,i) + H(2,2)*X0(2,i) + H(2,3)*X0(3,i)
               rzi = H(3,1)*X0(1,i) + H(3,2)*X0(2,i) + H(3,3)*X0(3,i)
               rsq = (rxi-RLIst(1,i))**2 + (ryi-RLIst(2,i))             &
     &               **2 + (rzi-RLIst(3,i))**2
               IF ( rsq.GT.RSQupd ) THEN
                  update = .TRUE.
                  GOTO 306
               ENDIF
            ENDDO
 306        CONTINUE
         ENDIF
      ENDIF
      IF ( update ) THEN
!JRA      TIMREF=SECOND()
         LISter = 1
         IF ( upgofr ) THEN
            charl = 'G'
            CALL MLIST(-1)
            KNTgor = KNTgor + 1
         ELSE
            charl = 'L'
            CALL MLIST(LMEthd)
         ENDIF
!JRA      TIMBLD=TIMBLD+(SECOND()-TIMREF)
         LCOunt = LCOunt + 1
         IF ( LIStem.LE.0 ) THEN
            DO i = 1 , MOLsa
               RLIst(1,i) = H(1,1)*X0(1,i) + H(1,2)*X0(2,i) + H(1,3)    &
     &                      *X0(3,i)
               RLIst(2,i) = H(2,1)*X0(1,i) + H(2,2)*X0(2,i) + H(2,3)    &
     &                      *X0(3,i)
               RLIst(3,i) = H(3,1)*X0(1,i) + H(3,2)*X0(2,i) + H(3,3)    &
     &                      *X0(3,i)
            ENDDO
         ENDIF
         update = .FALSE.
      ELSE
         charl = ' '
         LISter = LISter + 1
      ENDIF
!JRA      TIMREF=SECOND()
      CALL MFORCE(pot2,pglu)
      post = 0.D0
      DO i = 1 , 3
         DO j = 1 , 3
            VM(i,j) = VMAs*(VIRkin(i,j)/DELta2+VIRpot(i,j)-(PAI(j,1)*H(i&
     &                ,1)+PAI(j,2)*H(i,2)+PAI(j,3)*H(i,3))*(1-ILIn)     &
     &                -PAI(i,j)*ILIn-PEXt*DH*HI(j,i)) - HD(i,j)         &
     &                *GAMma/DELta
            IF ( LOCk(i,j).LT.0 ) VM(i,j) = 0.D0
            post = post + (1-ILIn)*0.5D0*PAI(i,j)*G(i,j) + ILIn*PAI(i,j)&
     &             *H(i,j)
         ENDDO
      ENDDO
      CALL SYMM(VM)
!JRA      TIMFRC=TIMFRC+(SECOND()-TIMREF)
      diffus = 0.D0
      DO i = 1 , MOLsp
         posxi = H(1,1)*X0(1,i) + H(1,2)*X0(2,i) + H(1,3)*X0(3,i)
         posyi = H(2,1)*X0(1,i) + H(2,2)*X0(2,i) + H(2,3)*X0(3,i)
         poszi = H(3,1)*X0(1,i) + H(3,2)*X0(2,i) + H(3,3)*X0(3,i)
         disxi = posxi - (HIN(1,1)*XIN(1,i)+HIN(1,2)*XIN(2,i)+HIN(1,3)  &
     &           *XIN(3,i))
         disyi = posyi - (HIN(2,1)*XIN(1,i)+HIN(2,2)*XIN(2,i)+HIN(2,3)  &
     &           *XIN(3,i))
         diszi = poszi - (HIN(3,1)*XIN(1,i)+HIN(3,2)*XIN(2,i)+HIN(3,3)  &
     &           *XIN(3,i))
         diffus = diffus + disxi**2 + disyi**2 + diszi**2
      ENDDO
      IF ( MOLsp.GT.0 ) diffus = diffus/MOLsp
      trans = 0.D0
      DO i = 1 , MOLsp
         velocx = H(1,1)*X(1,i,1) + H(1,2)*X(2,i,1) + H(1,3)*X(3,i,1)
         velocy = H(2,1)*X(1,i,1) + H(2,2)*X(2,i,1) + H(2,3)*X(3,i,1)
         velocz = H(3,1)*X(1,i,1) + H(3,2)*X(2,i,1) + H(3,3)*X(3,i,1)
         trans = trans + velocx**2 + velocy**2 + velocz**2
      ENDDO
      trans = 0.5D0*trans/DELta2
!JRA      TIMREF=SECOND()
      CALL MTXMTP(HI,HD,hihd)
      CALL MTXMTP(hdt,H,hihdt)
      CALL MTXINV(G,gi,dg)
      CALL MTXMTP(gi,hihdt,hcom)
      DO i = 1 , 3
         hsav(i,1) = hcom(i,1) + hihd(i,1)
         hsav(i,2) = hcom(i,2) + hihd(i,2)
         hsav(i,3) = hcom(i,3) + hihd(i,3)
      ENDDO
      DO i = 1 , MOLsp
         velocx = hsav(1,1)*X(1,i,1) + hsav(1,2)*X(2,i,1) + hsav(1,3)   &
     &            *X(3,i,1)
         velocy = hsav(2,1)*X(1,i,1) + hsav(2,2)*X(2,i,1) + hsav(2,3)   &
     &            *X(3,i,1)
         velocz = hsav(3,1)*X(1,i,1) + hsav(3,2)*X(2,i,1) + hsav(3,3)   &
     &            *X(3,i,1)
         XNP(1,i) = XNP(1,i) - velocx/DELta2
         XNP(2,i) = XNP(2,i) - velocy/DELta2
         XNP(3,i) = XNP(3,i) - velocz/DELta2
      ENDDO
      DO k = 1 , 3
         DO i = -2 , MOLsp
            XNP(k,i) = X(k,i,2) - 0.5D0*DELta2*XNP(k,i)
            X0(k,i) = X0(k,i) - XNP(k,i)*F02
            X(k,i,1) = X(k,i,1) - XNP(k,i)*F12
            X(k,i,2) = X(k,i,2) - XNP(k,i)
            X(k,i,3) = X(k,i,3) - XNP(k,i)*F32
            X(k,i,4) = X(k,i,4) - XNP(k,i)*F42
            X(k,i,5) = X(k,i,5) - XNP(k,i)*F52
         ENDDO
      ENDDO
!JRA      TIMINT=TIMINT+(SECOND()-TIMREF)
      sumpx = 0.D0
      sumpy = 0.D0
      sumpz = 0.D0
      DO i = 1 , MOLsa
         velocx = H(1,1)*X(1,i,1) + H(1,2)*X(2,i,1) + H(1,3)*X(3,i,1)
         velocy = H(2,1)*X(1,i,1) + H(2,2)*X(2,i,1) + H(2,3)*X(3,i,1)
         velocz = H(3,1)*X(1,i,1) + H(3,2)*X(2,i,1) + H(3,3)*X(3,i,1)
         sumpx = sumpx + velocx
         sumpy = sumpy + velocy
         sumpz = sumpz + velocz
      ENDDO
      sumpx = sumpx/DELta
      sumpy = sumpy/DELta
      sumpz = sumpz/DELta
      al = (VOLume/(NBX*NBY*NBZ))**(1.D0/3.D0)
      dens = MOLsa/VOLume
      ekinp = trans/MOLsa
      ekinw = urans/MOLsa
      ekin = ekinp + ekinw
      pot2 = pot2/MOLsa
      pglu = pglu/MOLsa
      pote = pot2 + pglu
      post = (post+PEXt*DH)/MOLsa
      tote = ekin + pote + post
      IF ( NFReed.GT.0 ) THEN
         temw = BOLtz*2.D0*urans/NFReed
      ELSE
         temw = 0.D0
      ENDIF
      temp = BOLtz*2.D0*trans/(3*MOLsp)
      temn = BOLtz*2.D0*(urans+trans)/(3*MOLsp+NFReed)
      TEMpsm = TEMpsm + temp
      TEMwsm = TEMwsm + temw
      EKInsm = EKInsm + ekin
      POT2sm = POT2sm + pot2
      PGLusm = PGLusm + pglu
      POStsm = POStsm + post
      TOTesm = TOTesm + tote
      DENssm = DENssm + dens
      ALSm = ALSm + al
      VOLusm = VOLusm + VOLume
      AREasm = AREasm + AREa
      HEIgsm = HEIgsm + heig
!JRA      TIMMST=SECOND()-CPTIME
      IF ( LNGprt.GT.0 ) THEN
         PRINT '(1X,I5,1X,2A1,3F8.4,F9.4,3E9.1)' , NFI , charl , charp ,&
     &         ekin , pote , tote , diffus , sumpx , sumpy , sumpz
      ENDIF
      CONTINUE
      END
!*==MLIST.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE MLIST(Lmethd)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /COUNT / NFI , LCOunt , LISter , KNTsta , KNTgor , LEP ,   &
     &                MANyon
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /LISCOM/ LISt(LL) , MRKr1(NM) , MRKr2(NM) , LISlen
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      COMMON /PARAM / DELta , DELta2 , GAMma , VSCale , CTRlce ,        &
     &                CTRlmi , CTRlma , RSQupd , RANsq , VMAs , BOX(3,3)
      COMMON /PBCS  / HALf , PBCx , PBCy , PBCz
      COMMON /PRINT / LNGprt , IPRind
      COMMON /SCRATC/ DUMmy1(NM) , DUMmy2(NM) , DUMmy3(NM) , DUMmy4(NM)
      COMMON /STATIS/ FGS(NG) , GRAng , FACng , SCAby2 , RESz , DONtr , &
     &                FONtr , SIG2 , NGS(NG) , NGMax , NZHigh , NZLow , &
     &                MULtip
      COMMON /WALLS / HI(3,3) , G(3,3) , DH , AREa , VOLume , SCM(3)
      DIMENSION H(3,3) , HIN(3,3)
      EQUIVALENCE (X0(1,-2),H(1,1))
      EQUIVALENCE (XIN(1,-2),HIN(1,1))
      IF ( LPBcsm.GT.0 ) THEN
         DO i = 1 , MOLsp
            boxjmp = PBCx*INT(X0(1,i)+SIGN(HALf,X0(1,i)))
            X0(1,i) = X0(1,i) - boxjmp
            XIN(1,i) = XIN(1,i) - boxjmp
            boxjmp = PBCy*INT(X0(2,i)+SIGN(HALf,X0(2,i)))
            X0(2,i) = X0(2,i) - boxjmp
            XIN(2,i) = XIN(2,i) - boxjmp
            boxjmp = PBCz*INT(X0(3,i)+SIGN(HALf,X0(3,i)))
            X0(3,i) = X0(3,i) - boxjmp
            XIN(3,i) = XIN(3,i) - boxjmp
         ENDDO
      ENDIF
      IF ( Lmethd.EQ.1 ) THEN
         CALL CBUILD(RANsq,icode)
         IF ( icode.EQ.2 ) THEN
            CALL FBUILD(RANsq,icode)
         ENDIF
      ELSEIF ( Lmethd.EQ.-1 ) THEN
         CALL GBUILD(RANsq,icode)
      ELSE
         CALL FBUILD(RANsq,icode)
      ENDIF
      IF ( icode.NE.0 ) THEN
         IF ( icode.EQ.1 ) THEN
            PRINT 3 , '''LIST'' ARRAY OVERFLOW IN CBUILD/FBUILD' ,      &
     &            '(''LL'' TOO SMALL)'
 3          FORMAT (/,1X,2A)
         ELSEIF ( icode.EQ.3 ) THEN
            PRINT 2 , '''NPC'' ARRAY OVERFLOW IN CBUILD,' ,             &
     &            '(''NCEMAX'' TOO SMALL).'
         ELSEIF ( icode.EQ.4 ) THEN
            PRINT 2 , '''KNTNTS'' ARRAY OVERFLOW IN CBUILD' ,           &
     &            '(''KNTSIZ'' TOO SMALL).'
         ELSEIF ( icode.EQ.5 ) THEN
            PRINT 2 , '''NEIGH'' ARRAY OVERFLOW IN CBUILD' ,            &
     &            '(''NNEMAX'' TOO SMALL).'
         ENDIF
         PRINT 2 , 'INCREASE DIMENSIONS, RECOMPILE AND RERUN.'
         STOP
      ENDIF
      CONTINUE
 2    FORMAT (1X,2A)
      END
!*==GBUILD.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE GBUILD(Ransq,Icode)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /COUNT / NFI , LCOunt , LISter , KNTsta , KNTgor , LEP ,   &
     &                MANyon
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /LISCOM/ LISt(LL) , MRKr1(NM) , MRKr2(NM) , LISlen
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      COMMON /PBCS  / HALf , PBCx , PBCy , PBCz
      COMMON /PRINT / LNGprt , IPRind
      COMMON /SCRATC/ DUMmy1(NM) , DUMmy2(NM) , DUMmy3(NM) , DUMmy4(NM)
      DIMENSION SLIce(NM)
      EQUIVALENCE (SLIce,DUMmy1)
      DIMENSION RRPos(3,NM)
      EQUIVALENCE (RRPos,DUMmy2)
      COMMON /STATIS/ FGS(NG) , GRAng , FACng , SCAby2 , RESz , DONtr , &
     &                FONtr , SIG2 , NGS(NG) , NGMax , NZHigh , NZLow , &
     &                MULtip
      COMMON /WALLS / HI(3,3) , G(3,3) , DH , AREa , VOLume , SCM(3)
      DIMENSION H(3,3)
      EQUIVALENCE (X0(1,-2),H(1,1))
      g11 = G(1,1)
      g22 = G(2,2)
      g33 = G(3,3)
      g12d = G(1,2) + G(2,1)
      g13d = G(1,3) + G(3,1)
      g23d = G(2,3) + G(3,2)
      contr = VOLume/DONtr/MOLsa**2
      IF ( LPBcsm.GT.1 ) THEN
         gontr = AREa/FONtr
         gslsc = FACng
      ELSE
         gontr = 1.D0/FONtr
         gslsc = NGMax/2.D0
      ENDIF
      DO i = 1 , MOLsp
         RRPos(1,i) = H(1,1)*X0(1,i) + H(1,2)*X0(2,i) + H(1,3)*X0(3,i)
         RRPos(2,i) = H(2,1)*X0(1,i) + H(2,2)*X0(2,i) + H(2,3)*X0(3,i)
         RRPos(3,i) = H(3,1)*X0(1,i) + H(3,2)*X0(2,i) + H(3,3)*X0(3,i)
         IF ( LPBcsm.GT.1 ) THEN
            SLIce(i) = RRPos(3,i)
         ELSE
            SLIce(i) = RRPos(1,i)**2 + RRPos(2,i)**2 + RRPos(3,i)**2
         ENDIF
      ENDDO
      nll = 1
      DO i = 1 , MOLsa
         MRKr1(i) = nll
         DO j = i + 1 , MOLsa
            xij = X0(1,i) - X0(1,j)
            IF ( xij.GT.+HALf ) xij = xij - PBCx
            IF ( xij.LT.-HALf ) xij = xij + PBCx
            yij = X0(2,i) - X0(2,j)
            IF ( yij.GT.+HALf ) yij = yij - PBCy
            IF ( yij.LT.-HALf ) yij = yij + PBCy
            zij = X0(3,i) - X0(3,j)
            IF ( zij.GT.+HALf ) zij = zij - PBCz
            IF ( zij.LT.-HALf ) zij = zij + PBCz
            rsq = xij*(g11*xij+g12d*yij+g13d*zij)                       &
     &            + yij*(g22*yij+g23d*zij) + g33*zij*zij
            nbet = INT(FACng*rsq) + 1
            IF ( nbet.LE.NGMax ) THEN
               FGS(nbet) = FGS(nbet) + contr
               NGS(nbet) = NGS(nbet) + 1
            ENDIF
            IF ( rsq.LT.Ransq ) THEN
               LISt(nll) = j
               nll = nll + 1
            ENDIF
         ENDDO
         MRKr2(i) = nll - 1
      ENDDO
      LISlen = nll - 1
      IF ( LISlen.GT.LL ) THEN
         Icode = 1
      ELSE
         Icode = 0
      ENDIF
      IF ( LNGprt.GT.0 ) THEN
         PRINT '(1X,A8,I8,A1,I8)' , 'LENGTH =' , LISlen , '/' , LL
      ENDIF
      CONTINUE
      END
!*==FBUILD.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE FBUILD(Ransq,Icode)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /LISCOM/ LISt(LL) , MRKr1(NM) , MRKr2(NM) , LISlen
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      COMMON /PBCS  / HALf , PBCx , PBCy , PBCz
      COMMON /PRINT / LNGprt , IPRind
      COMMON /SCRATC/ DUMmy1(NM) , DUMmy2(NM) , DUMmy3(NM) , DUMmy4(NM)
      INTEGER MARk(NM)
      EQUIVALENCE (MARk,DUMmy1)
      COMMON /WALLS / HI(3,3) , G(3,3) , DH , AREa , VOLume , SCM(3)
      DIMENSION H(3,3)
      EQUIVALENCE (X0(1,-2),H(1,1))
      g11 = G(1,1)
      g22 = G(2,2)
      g33 = G(3,3)
      g12d = G(1,2) + G(2,1)
      g13d = G(1,3) + G(3,1)
      g23d = G(2,3) + G(3,2)
      nll = 1
      DO i = 1 , MOLsa
         DO j = i + 1 , MOLsa
            xij = X0(1,i) - X0(1,j)
            IF ( xij.GT.+HALf ) xij = xij - PBCx
            IF ( xij.LT.-HALf ) xij = xij + PBCx
            yij = X0(2,i) - X0(2,j)
            IF ( yij.GT.+HALf ) yij = yij - PBCy
            IF ( yij.LT.-HALf ) yij = yij + PBCy
            zij = X0(3,i) - X0(3,j)
            IF ( zij.GT.+HALf ) zij = zij - PBCz
            IF ( zij.LT.-HALf ) zij = zij + PBCz
            rsq = xij*(g11*xij+g12d*yij+g13d*zij)                       &
     &            + yij*(g22*yij+g23d*zij) + g33*zij*zij
            MARk(j) = 0
            IF ( rsq.LT.Ransq ) MARk(j) = 1
         ENDDO
         MRKr1(i) = nll
         DO j = i + 1 , MOLsa
            LISt(nll) = j
            nll = nll + MARk(j)
         ENDDO
         MRKr2(i) = nll - 1
      ENDDO
      LISlen = nll - 1
      IF ( LISlen.GT.LL ) THEN
         Icode = 1
      ELSE
         Icode = 0
      ENDIF
      IF ( LNGprt.GT.0 ) THEN
         PRINT '(1X,A8,I8,A1,I8)' , 'LENGTH =' , LISlen , '/' , LL
      ENDIF
      CONTINUE
      END
!*==CBUILD.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE CBUILD(Ransq,Icode)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /LISCOM/ LISt(LL) , MRKr1(NM) , MRKr2(NM) , LISlen
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      COMMON /PBCS  / HALf , PBCx , PBCy , PBCz
      COMMON /PRINT / LNGprt , IPRind
      COMMON /SCRATC/ DUMmy1(NM) , DUMmy2(NM) , DUMmy3(NM) , DUMmy4(NM)
      INTEGER LCEll(NM)
      EQUIVALENCE (LCEll,DUMmy1)
      INTEGER MARk(NM)
      EQUIVALENCE (MARk,DUMmy1)
      EQUIVALENCE (KNTnts,DUMmy2)
      COMMON /WALLS / HI(3,3) , G(3,3) , DH , AREa , VOLume , SCM(3)
      DIMENSION H(3,3)
      EQUIVALENCE (X0(1,-2),H(1,1))
      PARAMETER (NNEMAX=512)
      INTEGER neigh(NNEMAX)
      PARAMETER (NMHALF=NM/2,NMHAL1=NMHALF+1)
      PARAMETER (NCEMAX=NMHALF)
      PARAMETER (NCEMA1=NCEMAX-1)
      INTEGER npc(0:NCEMA1)
      PARAMETER (KNTSIZ=3*NM)
      INTEGER KNTnts(KNTSIZ)
      LOGICAL nofldx , nofldy , nofldz
      INTEGER nix(13) , niy(13) , niz(13)
      DATA nix/ - 1 , -1 , -1 , 0 , 0 , -1 , 1 , -1 , 0 , 1 , -1 , 0 ,  &
     &     1/
      DATA niy/0 , -1 , 1 , 1 , 0 , 0 , 0 , -1 , -1 , -1 , 1 , 1 , 1/
      DATA niz/0 , 0 , 0 , 0 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1/
      range = SQRT(Ransq)
      g11 = G(1,1)
      g22 = G(2,2)
      g33 = G(3,3)
      g12d = G(1,2) + G(2,1)
      g13d = G(1,3) + G(3,1)
      g23d = G(2,3) + G(3,2)
      nlx = INT(1.D0/(range*HI(1,1)))
      nly = INT(1.D0/(range*HI(2,2)))
      nlz = INT(1.D0/(range*HI(3,3)))
      IF ( (nlx.LT.3) .AND. (LPBc(1).NE.0) ) nlx = 1
      IF ( (nly.LT.3) .AND. (LPBc(2).NE.0) ) nly = 1
      IF ( (nlz.LT.3) .AND. (LPBc(3).NE.0) ) nlz = 1
      nofldx = (LPBc(1).EQ.0) .OR. (nlx.EQ.1)
      nofldy = (LPBc(2).EQ.0) .OR. (nly.EQ.1)
      nofldz = (LPBc(3).EQ.0) .OR. (nlz.EQ.1)
      fnlx = nlx
      fnly = nly
      fnlz = nlz
      ncells = nlx*nly*nlz
      IF ( ncells.GT.NCEMAX ) THEN
         Icode = 3
         RETURN
      ENDIF
      npcmax = KNTSIZ/ncells
      DO i = 1 , MOLsa
         ix = INT((X0(1,i)+HALf)*fnlx)
         IF ( ix.GE.nlx ) ix = nlx - 1
         IF ( ix.LT.0 ) ix = 0
         iy = INT((X0(2,i)+HALf)*fnly)
         IF ( iy.GE.nly ) iy = nly - 1
         IF ( iy.LT.0 ) iy = 0
         iz = INT((X0(3,i)+HALf)*fnlz)
         IF ( iz.GE.nlz ) iz = nlz - 1
         IF ( iz.LT.0 ) iz = 0
         LCEll(i) = ix + nlx*(iy+nly*iz)
      ENDDO
      DO icell = 0 , ncells - 1
         npc(icell) = 0
      ENDDO
      DO i = 1 , MOLsa
         icell = LCEll(i)
         npc(icell) = npc(icell) + 1
         KNTnts(npc(icell)+npcmax*icell) = i
      ENDDO
      npcusd = 0
      DO icell = 0 , ncells - 1
         IF ( npc(icell).GT.npcusd ) npcusd = npc(icell)
      ENDDO
      IF ( npcusd.GT.npcmax ) THEN
         Icode = 4
         RETURN
      ENDIF
      nll = 1
      nneusd = 0
      DO icell = 0 , ncells - 1
         IF ( npc(icell).NE.0 ) THEN
            icoffs = npcmax*icell
            iz = icell/(nlx*nly)
            icelxy = icell - (nlx*nly)*iz
            iy = icelxy/nlx
            ix = icelxy - nlx*iy
            nneigc = 0
            DO kc = 1 , 13
               jz = iz + niz(kc)
               IF ( jz.GE.nlz ) THEN
                  IF ( nofldz ) GOTO 2200
                  jz = 0
               ELSEIF ( jz.LT.0 ) THEN
                  IF ( nofldz ) GOTO 2200
                  jz = nlz - 1
               ENDIF
               jy = iy + niy(kc)
               IF ( jy.GE.nly ) THEN
                  IF ( nofldy ) GOTO 2200
                  jy = 0
               ELSEIF ( jy.LT.0 ) THEN
                  IF ( nofldy ) GOTO 2200
                  jy = nly - 1
               ENDIF
               jx = ix + nix(kc)
               IF ( jx.GE.nlx ) THEN
                  IF ( nofldx ) GOTO 2200
                  jx = 0
               ELSEIF ( jx.LT.0 ) THEN
                  IF ( nofldx ) GOTO 2200
                  jx = nlx - 1
               ENDIF
               jcell = jx + nlx*(jy+nly*jz)
               jcoffs = npcmax*jcell
               DO jpc = 1 , npc(jcell)
                  neigh(nneigc+jpc) = KNTnts(jpc+jcoffs)
               ENDDO
               nneigc = nneigc + npc(jcell)
 2200       ENDDO
            DO ipc = 1 , npc(icell)
               i = KNTnts(ipc+icoffs)
               DO jpc = ipc + 1 , npc(icell)
                  neigh(nneigc+jpc-ipc) = KNTnts(jpc+icoffs)
               ENDDO
               nneigi = nneigc + npc(icell) - ipc
               IF ( nneigi.GT.nneusd ) nneusd = nneigi
               DO icand = 1 , nneigi
                  j = neigh(icand)
                  xij = X0(1,i) - X0(1,j)
                  IF ( xij.GT.+HALf ) xij = xij - PBCx
                  IF ( xij.LT.-HALf ) xij = xij + PBCx
                  yij = X0(2,i) - X0(2,j)
                  IF ( yij.GT.+HALf ) yij = yij - PBCy
                  IF ( yij.LT.-HALf ) yij = yij + PBCy
                  zij = X0(3,i) - X0(3,j)
                  IF ( zij.GT.+HALf ) zij = zij - PBCz
                  IF ( zij.LT.-HALf ) zij = zij + PBCz
                  rsq = xij*(g11*xij+g12d*yij+g13d*zij)                 &
     &                  + yij*(g22*yij+g23d*zij) + g33*zij*zij
                  MARk(icand) = 0
                  IF ( rsq.LT.Ransq ) MARk(icand) = 1
               ENDDO
               MRKr1(i) = nll
               DO icand = 1 , nneigi
                  LISt(nll) = neigh(icand)
                  nll = nll + MARk(icand)
               ENDDO
               MRKr2(i) = nll - 1
            ENDDO
         ENDIF
      ENDDO
      LISlen = nll - 1
      IF ( LISlen.GT.LL ) THEN
         Icode = 1
      ELSEIF ( nneusd.GT.NNEMAX ) THEN
         Icode = 5
      ELSE
         Icode = 0
      ENDIF
      IF ( LNGprt.GT.0 ) THEN
         PRINT                                                          &
     &'(1X,A7,I7,A1,I7,                                           A8,I5,&
     &A1,I5,                                                      A8,I3,&
     &A1,I3,                                                      A8,I5,&
     &A1,I5)' , 'LENGTH=' , LISlen , '/' , LL , ', CELLS=' , ncells ,   &
     &'/' , NCEMAX , ', PA/CL=' , npcusd , '/' , npcmax , ', CA/PA=' ,  &
     &nneusd , '/' , NNEMAX
      ENDIF
      CONTINUE
      END
!*==MFORCE.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE MFORCE(Pot2,Pglu)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /COUNT / NFI , LCOunt , LISter , KNTsta , KNTgor , LEP ,   &
     &                MANyon
      COMMON /DEN   / RNRho , RCRho , RNRho2 , RCRho2 , DR2rho , RHO(KR)&
     &                , DRHo(KR)
      COMMON /GLUE  / DMIn , DMAx , DD , UJ(KG) , DUJ(KG)
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /LISCOM/ LISt(LL) , MRKr1(NM) , MRKr2(NM) , LISlen
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      COMMON /MOTION/ VIRkin(3,3) , VIRpot(3,3) , XNP(3,-2:NM)
      COMMON /PBCS  / HALf , PBCx , PBCy , PBCz
      COMMON /POT   / RN , RC , RN2 , RC2 , DR2 , VJ(KP) , FJ(KP)
      COMMON /SCRATC/ DUMmy1(NM) , DUMmy2(NM) , DUMmy3(NM) , DUMmy4(NM)
      DIMENSION DERu(NM)
      EQUIVALENCE (DERu,DUMmy1)
      DIMENSION DNSty(NM)
      EQUIVALENCE (DNSty,DUMmy2)
      DIMENSION PP(NM)
      EQUIVALENCE (PP,DUMmy3)
      COMMON /WALLS / HI(3,3) , G(3,3) , DH , AREa , VOLume , SCM(3)
      DIMENSION H(3,3)
      EQUIVALENCE (X0(1,-2),H(1,1))
      CALL RESET(VIRpot,9)
      LEP = 0
      Pglu = 0.D0
      g11 = G(1,1)
      g22 = G(2,2)
      g33 = G(3,3)
      g12d = G(1,2) + G(2,1)
      g13d = G(1,3) + G(3,1)
      g23d = G(2,3) + G(3,2)
      IF ( MANyon.NE.0 ) THEN
         DO i = 1 , MOLsa
            DNSty(i) = 0.D0
         ENDDO
         DO i = 1 , MOLsa
            DO nll = MRKr1(i) , MRKr2(i)
               j = LISt(nll)
               xij = X0(1,i) - X0(1,j)
               IF ( xij.GT.+HALf ) xij = xij - PBCx
               IF ( xij.LT.-HALf ) xij = xij + PBCx
               yij = X0(2,i) - X0(2,j)
               IF ( yij.GT.+HALf ) yij = yij - PBCy
               IF ( yij.LT.-HALf ) yij = yij + PBCy
               zij = X0(3,i) - X0(3,j)
               IF ( zij.GT.+HALf ) zij = zij - PBCz
               IF ( zij.LT.-HALf ) zij = zij + PBCz
               rsq = xij*(g11*xij+g12d*yij+g13d*zij)                    &
     &               + yij*(g22*yij+g23d*zij) + g33*zij*zij
               IF ( rsq.LT.RCRho2 ) THEN
                  sqd = (rsq-RNRho2)/DR2rho + 1.D0
                  m = INT(sqd)
                  m = MAX(m,1)
                  delrsq = sqd - m
                  rterm = RHO(m+1)*delrsq + RHO(m)*(1.D0-delrsq)
                  DNSty(i) = DNSty(i) + rterm
                  DNSty(j) = DNSty(j) + rterm
               ENDIF
            ENDDO
         ENDDO
         DO i = 1 , MOLsa
            sqd = (DNSty(i)-DMIn)/DD + 1.D0
            m = INT(sqd)
            m = MAX(m,1)
            m = MIN(m,KG-1)
            deld = sqd - m
            eeld = 1.D0 - deld
            DERu(i) = DUJ(m+1)*deld + DUJ(m)*eeld
            pterm = UJ(m+1)*deld + UJ(m)*eeld
            PP(i) = pterm
            Pglu = Pglu + pterm
         ENDDO
         DO i = 1 , MOLsa
            DO nll = MRKr1(i) , MRKr2(i)
               j = LISt(nll)
               xij = X0(1,i) - X0(1,j)
               IF ( xij.GT.+HALf ) xij = xij - PBCx
               IF ( xij.LT.-HALf ) xij = xij + PBCx
               yij = X0(2,i) - X0(2,j)
               IF ( yij.GT.+HALf ) yij = yij - PBCy
               IF ( yij.LT.-HALf ) yij = yij + PBCy
               zij = X0(3,i) - X0(3,j)
               IF ( zij.GT.+HALf ) zij = zij - PBCz
               IF ( zij.LT.-HALf ) zij = zij + PBCz
               rsq = xij*(g11*xij+g12d*yij+g13d*zij)                    &
     &               + yij*(g22*yij+g23d*zij) + g33*zij*zij
               ccelt = 0.D0
               IF ( rsq.LT.RC2 ) THEN
                  sqd = (rsq-RN2)/DR2 + 1.D0
                  m = INT(sqd)
                  IF ( m.LE.0 ) LEP = LEP + 1
                  m = MAX(m,1)
                  delrsq = sqd - m
                  eelrsq = 1.D0 - delrsq
                  pterm = 0.5D0*(VJ(m+1)*delrsq+VJ(m)*eelrsq)
                  ccelt = FJ(m+1)*delrsq + FJ(m)*eelrsq
                  PP(i) = PP(i) + pterm
                  PP(j) = PP(j) + pterm
               ENDIF
               ccelg = 0.D0
               IF ( rsq.LT.RCRho2 ) THEN
                  sqd = (rsq-RNRho2)/DR2rho + 1.D0
                  m = INT(sqd)
                  m = MAX(m,1)
                  delrsq = sqd - m
                  rterm = DRHo(m+1)*delrsq + DRHo(m)*(1.D0-delrsq)
                  ccelg = (DERu(i)+DERu(j))*rterm
               ENDIF
               ccel = ccelt + ccelg
               IF ( ccel.NE.0.D0 ) THEN
                  rxij = H(1,1)*xij + H(1,2)*yij + H(1,3)*zij
                  ryij = H(2,1)*xij + H(2,2)*yij + H(2,3)*zij
                  rzij = H(3,1)*xij + H(3,2)*yij + H(3,3)*zij
                  xij = xij*ccel
                  yij = yij*ccel
                  zij = zij*ccel
                  VIRpot(1,1) = VIRpot(1,1) + rxij*xij
                  VIRpot(2,1) = VIRpot(2,1) + ryij*xij
                  VIRpot(3,1) = VIRpot(3,1) + rzij*xij
                  VIRpot(1,2) = VIRpot(1,2) + rxij*yij
                  VIRpot(2,2) = VIRpot(2,2) + ryij*yij
                  VIRpot(3,2) = VIRpot(3,2) + rzij*yij
                  VIRpot(1,3) = VIRpot(1,3) + rxij*zij
                  VIRpot(2,3) = VIRpot(2,3) + ryij*zij
                  VIRpot(3,3) = VIRpot(3,3) + rzij*zij
                  XNP(1,i) = XNP(1,i) + xij
                  XNP(2,i) = XNP(2,i) + yij
                  XNP(3,i) = XNP(3,i) + zij
                  XNP(1,j) = XNP(1,j) - xij
                  XNP(2,j) = XNP(2,j) - yij
                  XNP(3,j) = XNP(3,j) - zij
               ENDIF
            ENDDO
         ENDDO
      ELSE
         DO i = 1 , MOLsa
            PP(i) = 0.D0
         ENDDO
         DO i = 1 , MOLsa
            DO nll = MRKr1(i) , MRKr2(i)
               j = LISt(nll)
               xij = X0(1,i) - X0(1,j)
               IF ( xij.GT.+HALf ) xij = xij - PBCx
               IF ( xij.LT.-HALf ) xij = xij + PBCx
               yij = X0(2,i) - X0(2,j)
               IF ( yij.GT.+HALf ) yij = yij - PBCy
               IF ( yij.LT.-HALf ) yij = yij + PBCy
               zij = X0(3,i) - X0(3,j)
               IF ( zij.GT.+HALf ) zij = zij - PBCz
               IF ( zij.LT.-HALf ) zij = zij + PBCz
               rsq = xij*(g11*xij+g12d*yij+g13d*zij)                    &
     &               + yij*(g22*yij+g23d*zij) + g33*zij*zij
               IF ( rsq.LT.RC2 ) THEN
                  sqd = (rsq-RN2)/DR2 + 1.D0
                  m = INT(sqd)
                  IF ( m.LE.0 ) LEP = LEP + 1
                  m = MAX(m,1)
                  delrsq = sqd - m
                  eelrsq = 1.D0 - delrsq
                  pterm = 0.5D0*(VJ(m+1)*delrsq+VJ(m)*eelrsq)
                  ccel = FJ(m+1)*delrsq + FJ(m)*eelrsq
                  PP(i) = PP(i) + pterm
                  PP(j) = PP(j) + pterm
                  rxij = H(1,1)*xij + H(1,2)*yij + H(1,3)*zij
                  ryij = H(2,1)*xij + H(2,2)*yij + H(2,3)*zij
                  rzij = H(3,1)*xij + H(3,2)*yij + H(3,3)*zij
                  xij = xij*ccel
                  yij = yij*ccel
                  zij = zij*ccel
                  VIRpot(1,1) = VIRpot(1,1) + rxij*xij
                  VIRpot(2,1) = VIRpot(2,1) + ryij*xij
                  VIRpot(3,1) = VIRpot(3,1) + rzij*xij
                  VIRpot(1,2) = VIRpot(1,2) + rxij*yij
                  VIRpot(2,2) = VIRpot(2,2) + ryij*yij
                  VIRpot(3,2) = VIRpot(3,2) + rzij*yij
                  VIRpot(1,3) = VIRpot(1,3) + rxij*zij
                  VIRpot(2,3) = VIRpot(2,3) + ryij*zij
                  VIRpot(3,3) = VIRpot(3,3) + rzij*zij
                  XNP(1,i) = XNP(1,i) + xij
                  XNP(2,i) = XNP(2,i) + yij
                  XNP(3,i) = XNP(3,i) + zij
                  XNP(1,j) = XNP(1,j) - xij
                  XNP(2,j) = XNP(2,j) - yij
                  XNP(3,j) = XNP(3,j) - zij
               ENDIF
            ENDDO
         ENDDO
      ENDIF
      ptot = 0.D0
      DO i = 1 , MOLsa
         ptot = ptot + PP(i)
      ENDDO
      Pot2 = ptot - Pglu
      CONTINUE
      END
!*==IRESET.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE IRESET(Iarray,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Iarray(N)
      DO i = 1 , N
         Iarray(i) = 0
      ENDDO
      CONTINUE
      END
!*==RESET.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE RESET(Array,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Array(N)
      DO i = 1 , N
         Array(i) = 0.D0
      ENDDO
      CONTINUE
      END
!*==MTXMTP.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE MTXMTP(A,B,C)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(3,3) , B(3,3) , C(3,3)
      DO j = 1 , 3
         C(1,j) = A(1,1)*B(1,j) + A(1,2)*B(2,j) + A(1,3)*B(3,j)
         C(2,j) = A(2,1)*B(1,j) + A(2,2)*B(2,j) + A(2,3)*B(3,j)
         C(3,j) = A(3,1)*B(1,j) + A(3,2)*B(2,j) + A(3,3)*B(3,j)
      ENDDO
      CONTINUE
      END
!*==MTXINV.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE MTXINV(Hm,Hi,Dh)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Hm(3,3) , Hi(3,3)
      d11 = Hm(2,2)*Hm(3,3) - Hm(2,3)*Hm(3,2)
      d12 = Hm(2,3)*Hm(3,1) - Hm(2,1)*Hm(3,3)
      d21 = Hm(3,2)*Hm(1,3) - Hm(1,2)*Hm(3,3)
      d31 = Hm(1,2)*Hm(2,3) - Hm(2,2)*Hm(1,3)
      d32 = Hm(1,3)*Hm(2,1) - Hm(1,1)*Hm(2,3)
      d13 = Hm(2,1)*Hm(3,2) - Hm(3,1)*Hm(2,2)
      d22 = Hm(1,1)*Hm(3,3) - Hm(1,3)*Hm(3,1)
      d23 = Hm(3,1)*Hm(1,2) - Hm(1,1)*Hm(3,2)
      d33 = Hm(1,1)*Hm(2,2) - Hm(1,2)*Hm(2,1)
      Dh = Hm(1,1)*d11 + Hm(1,2)*d12 + Hm(1,3)*d13
      IF ( Dh.LE.0.D0 ) THEN
         IF ( Dh.EQ.0.D0 ) THEN
            PRINT '(''0MTXINV ERROR: DH=0'')'
            STOP
         ELSE
            PRINT '(''0MTXINV WARNING: DH<0'')'
         ENDIF
      ENDIF
      Hi(1,1) = d11/Dh
      Hi(2,2) = d22/Dh
      Hi(3,3) = d33/Dh
      Hi(1,2) = d21/Dh
      Hi(1,3) = d31/Dh
      Hi(2,3) = d32/Dh
      Hi(2,1) = d12/Dh
      Hi(3,1) = d13/Dh
      Hi(3,2) = d23/Dh
      CONTINUE
      END
!*==MTE.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE MTE(Nbsize)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /COUNT / NFI , LCOunt , LISter , KNTsta , KNTgor , LEP ,   &
     &                MANyon
      COMMON /IDENT / ELEmen , REF , TODay , NOW
      CHARACTER ELEmen*10 , REF*72 , TODay*10 , NOW*10
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      COMMON /PARAM / DELta , DELta2 , GAMma , VSCale , CTRlce ,        &
     &                CTRlmi , CTRlma , RSQupd , RANsq , VMAs , BOX(3,3)
      COMMON /STATIS/ FGS(NG) , GRAng , FACng , SCAby2 , RESz , DONtr , &
     &                FONtr , SIG2 , NGS(NG) , NGMax , NZHigh , NZLow , &
     &                MULtip
      COMMON /SUMS  / TEMpsm , TEMwsm , EKInsm , POT2sm , PGLusm ,      &
     &                POStsm , TOTesm , DENssm , ALSm , VOLusm ,        &
     &                AREasm , HEIgsm
      COMMON /THRU  / ATMass , ECOh , R0 , SPAref
      DIMENSION H(3,3)
      EQUIVALENCE (X0(1,-2),H(1,1))
      PARAMETER (HALF=0.5D0,TWO=2.D0)
      PARAMETER (PI=3.141592653589793D0)
      NFI = 0
      KNTsta = 0
      KNTgor = 0
      DELta = 0.D0
      TEMpsm = 0.D0
      TEMwsm = 0.D0
      EKInsm = 0.D0
      POT2sm = 0.D0
      PGLusm = 0.D0
      POStsm = 0.D0
      TOTesm = 0.D0
      DENssm = 0.D0
      ALSm = 0.D0
      VOLusm = 0.D0
      AREasm = 0.D0
      HEIgsm = 0.D0
      CALL RESET(BOX,9)
      CALL RESET(X0,3*(NM+3))
      CALL RESET(X,3*(NM+3)*5)
      CALL RESET(XIN,3*(NM+3))
      ELEmen = 'GOLD'
      alat = 4.0704D0
      ATMass = 196.967D0
      ECOh = 3.78D0
      SPAref = alat
      R0 = SPAref/SQRT(TWO)
      LPBc(1) = 1
      LPBc(2) = 1
      LPBc(3) = 1
      LPBcsm = LPBc(1) + LPBc(2) + LPBc(3)
      CALL CRYSTL(R0,Nbsize)
      NGMax = NG
      NZHigh = NH
      NZLow = NL
      CALL DEFLTS(LPBc,H,R0,MOLsa,scadef,gramax)
      scale = scadef
      GRAng = gramax
      SCAby2 = scale/TWO
      RESz = NZHigh/scale
      MULtip = NZHigh/NZLow
      FACng = NGMax/(GRAng**2)
      DONtr = PI/(FACng*SQRT(FACng))
      FONtr = HALF*PI/FACng
      REF = 'IN-MEMORY GENERATED SAMPLE FOR BENCHMARKING'
      TODay = '*****NEW '
      NOW = 'SAMPLE***'
      amp = 0.5D0
      CALL RANPOS(amp)
      CALL COPYIN
      CALL CENTCM
      CONTINUE
      END
!*==CRYSTL.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE CRYSTL(R0,Nbsize)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      COMMON /PARAM / DELta , DELta2 , GAMma , VSCale , CTRlce ,        &
     &                CTRlmi , CTRlma , RSQupd , RANsq , VMAs , BOX(3,3)
      DIMENSION H(3,3)
      EQUIVALENCE (X0(1,-2),H(1,1))
      PARAMETER (NREC=7)
      PARAMETER (NBASE=4)
      DIMENSION mm(3,NBASE,NREC) , cmul(3,NREC) , nplane(NREC)
      CHARACTER*72 name(0:NREC)
      PARAMETER (HALF=0.5D0,ONE=1.D0,TWO=2.D0,FOUR=4.D0,TWELVE=12.D0)
      PARAMETER (SQRT2=1.41421356237310D0,SQRT3=1.73205080756888D0,     &
     &           SQRT8=TWO*SQRT2,SQ8BY3=SQRT8/SQRT3,SQ4BY3=TWO/SQRT3,   &
     &           SQ16B3=FOUR/SQRT3)
      DATA mm/0 , 0 , 0 , 6 , 6 , 0 , 0 , 6 , 6 , 6 , 0 , 6 , 0 , 0 ,   &
     &     0 , 6 , 6 , 3 , 0 , 0 , 6 , 6 , 6 , 9 , 0 , 0 , 0 , 6 , 6 ,  &
     &     3 , 0 , 0 , 6 , 6 , 6 , 9 , 0 , 0 , 0 , 6 , 6 , 0 , 0 , 4 ,  &
     &     6 , 6 , 10 , 6 , 0 , 0 , 0 , 6 , 6 , 0 , 0 , 4 , 6 , 6 , 10 ,&
     &     6 , 0 , 0 , 0 , 6 , 6 , 3 , 0 , 0 , 6 , 6 , 6 , 9 , 0 , 0 ,  &
     &     0 , 6 , 6 , 3 , 0 , 0 , 6 , 6 , 6 , 9/
      DATA cmul/SQRT2 , SQRT2 , SQRT2 , ONE , ONE , SQRT8 , ONE ,       &
     &     SQRT2 , TWO , ONE , SQRT3 , SQ8BY3 , ONE , SQRT3 , SQ8BY3 ,  &
     &     ONE , ONE , SQRT8 , SQ4BY3 , SQ4BY3 , SQ16B3/
      DATA nplane/2 , 4 , 4 , 2 , 2 , 4 , 4/
      DATA name/'READ COORDINATES FROM EXTERNAL FILE' ,                 &
     &     'FCC 100 , LATERAL FACES 100' ,                              &
     &     'FCC 100 , LATERAL FACES 110' ,                              &
     &     'FCC 110 , CLEAN OR RECONSTRUCTED' ,                         &
     &     'FCC 111 , CLEAN OR RECONSTRUCTED' ,                         &
     &     'HCP HEXAGONAL TOP FACE' ,                                   &
     &     'FCC 100 , LATERAL FACES 110 , TOP LAYER RECONSTRUCTED' ,    &
     &     'BCC 100 , LATERAL FACES 100'/
 2    FORMAT (1X,8A)
 3    FORMAT (/,1X,8A)
 4    FORMAT (1X,8A)
 5    FORMAT (/,1X,8A)
 6    FORMAT (4X,8A)
      NBX = Nbsize
      NBY = Nbsize
      NBZ = Nbsize
      nstr = 1
      BOX(1,1) = NBX*cmul(1,nstr)
      BOX(2,2) = NBY*cmul(2,nstr)
      BOX(3,3) = NBZ*cmul(3,nstr)
      NPLa = NBZ*nplane(nstr)
      m = 0
      DO k = 1 , NBZ
         DO l = 1 , NBASE
            DO j = 1 , NBY
               DO i = 1 , NBX
                  m = m + 1
                  IF ( m.GT.NM ) GOTO 9950
                  X0(1,m) = ((i-1)+mm(1,l,nstr)/TWELVE)/NBX
                  X0(2,m) = ((j-1)+mm(2,l,nstr)/TWELVE)/NBY
                  X0(3,m) = ((k-1)+mm(3,l,nstr)/TWELVE)/NBZ
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      GOTO 9951
 9950 CONTINUE
      PRINT '(/,1X,A,I7,A,/)' , '***ROOM FOR' , NM ,                    &
     &      ' PARTICLES ONLY: CRYSTAL TRUNCATED.***'
      m = m - 1
 9951 CONTINUE
      MOLsa = m
      MOLsp = MOLsa
      H(1,1) = R0*BOX(1,1)
      H(2,2) = R0*BOX(2,2)
      H(3,3) = R0*BOX(3,3)
      CALL COPYIN
      CALL CENTCM
      CONTINUE
      END
!*==CENTCM.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE CENTCM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      cm1 = 0.D0
      cm2 = 0.D0
      cm3 = 0.D0
      DO i = 1 , MOLsa
         cm1 = cm1 + X0(1,i)
         cm2 = cm2 + X0(2,i)
         cm3 = cm3 + X0(3,i)
      ENDDO
      cm1 = cm1/MOLsa
      cm2 = cm2/MOLsa
      cm3 = cm3/MOLsa
      IF ( (cm1.EQ.0.D0) .AND. (cm2.EQ.0.D0) .AND. (cm3.EQ.0.D0) )      &
     &     RETURN
      DO i = 1 , MOLsa
         X0(1,i) = X0(1,i) - cm1
         X0(2,i) = X0(2,i) - cm2
         X0(3,i) = X0(3,i) - cm3
         XIN(1,i) = XIN(1,i) - cm1
         XIN(2,i) = XIN(2,i) - cm2
         XIN(3,i) = XIN(3,i) - cm3
      ENDDO
      CONTINUE
      END
!*==RANPOS.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE RANPOS(Amp)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      DIMENSION H(3,3) , hi(3,3) , delta(3)
      EQUIVALENCE (X0(1,-2),H(1,1))
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      EXTERNAL RANFM
      DOUBLE PRECISION RANFM
      CALL MTXINV(H,hi,dh)
      idum = -1
      DO i = 1 , MOLsp
         DO k = 1 , 3
            delta(k) = 2.D0*Amp*(RANFM(idum)-0.5D0)
         ENDDO
         DO k = 1 , 3
            X0(k,i) = X0(k,i) + hi(k,1)*delta(1) + hi(k,2)*delta(2)     &
     &                + hi(k,3)*delta(3)
         ENDDO
      ENDDO
      CONTINUE
      END
!*==COPYIN.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE COPYIN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      DO i = -2 , MOLsa
         XIN(1,i) = X0(1,i)
         XIN(2,i) = X0(2,i)
         XIN(3,i) = X0(3,i)
      ENDDO
      CONTINUE
      END
!*==DEFLTS.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE DEFLTS(Lpbc,H,R0,Molsa,Scadef,Gramax)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Lpbc(3) , H(3,3)
      PARAMETER (HALF=0.5D0,TWO=2.D0)
      PARAMETER (THIRD=1.D0/3.D0)
      PARAMETER (PI=3.141592653589793D0)
      PARAMETER (SQRT2=1.41421356237310D0)
      Scadef = 1.25D0*H(3,3)
      Gramax = MAX(HALF*H(1,1),HALF*H(2,2),HALF*H(3,3))
      DO j = 1 , 3
         IF ( Lpbc(j).NE.0 ) THEN
            gramaj = HALF*H(j,j)
            IF ( Gramax.GT.gramaj ) Gramax = gramaj
         ENDIF
      ENDDO
      CONTINUE
      END
!*==RANFM.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      DOUBLE PRECISION FUNCTION RANFM(Idum)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (M=714025,IA=1366,IC=150889,RM=1.4005112D-6)
      DIMENSION ir(97)
      SAVE iff , ir , iy
      DATA iff/0/
      IF ( Idum.LT.0 .OR. iff.EQ.0 ) THEN
         iff = 1
         Idum = MOD(IC-Idum,M)
         DO j = 1 , 97
            Idum = MOD(IA*Idum+IC,M)
            ir(j) = Idum
         ENDDO
         Idum = MOD(IA*Idum+IC,M)
         iy = Idum
      ENDIF
      j = 1 + (97*iy)/M
      iy = ir(j)
      RANFM = iy*RM
      Idum = MOD(IA*Idum+IC,M)
      ir(j) = Idum
      CONTINUE
      END
