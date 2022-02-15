!*==DODUC.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!   SPAG options set to convert source form only
! 						 30/avril/84
!
!
!                 F O R T R A N    B E N C H M A R K
!                 ----------------------------------
      PROGRAM DODUC
!
!         This is a " vanilla " fortran program that is adapted
!     to be run on any machine .
!         The program has a pure CP profile with quasi-nil I/O .
!     * it reads an input file of 100 cards : READ (5,fmt) and
!       writes only 15 lines of output with : PRINT fmt , list ,
!     * it has no dependance feature on any particular compiler
!       hardware or concept , and doesn't use any scratch file ,
!     * it occupies , at execution , about 350Kbytes (absolute
!       memory size) and finally
!     * it takes about 2000 s of a VAX 11/780
!
!
! Notes :
! ------
!
! 1- 'double precision' means "IMPLICIT DOUBLEPRECISION (A-H,O-Z)"
!    and is meant for 32-bits machines ;
!    'single precision' version , meanning "IMPLICIT REAL (A-H,O-Z)" ,
!    is also available on request .
!
! 2- the data file is planned initially for "small" computer
!    (i.e. VAX,Prime..) If the machine is a microcomputer we
!    should decrease the amount of time needed while if it's a
!    large mainframe , this amount should be increased ;
!    The following applies :
!    the 12th line of the data is :   050.       -5.       20.
!    050 means simulation of 50 event-time seconds , and will
!    take , about 2000 seconds of a Vax 11/780 (with FPA) ;
!    for a microcomputer , change 050 to 036 to use one fourth
!    of CP time and while for a large mainframe , change 050
!    to 100 to increase fourfold.
!    The data is given separately from the source file ;
!    please check that there are exactly 100 hundred 'lines' .
!
! 3- For an UNIX machine :
!    * interactively :
!      f77 -w -Nn4000 sourced.f       to compile source file ,
!          -w flag to avoid warning message and -Nn4000 to in
!           crease size of some tables during compilation .
!      (time a.out < data)&          to execute in background
!                                    mode and get CPU time
!    * in "batch mode"
!      with sh  : time a.out < data > result 2 >& 1 &
!      with csh : time a.out >& result &
!
! 4- examples of output :
!  MAIN : FIN S00002
!  MAIN : FIN S00001
!  MAIN : FIN S00011
!  MAIN : FIN S00022
!  TEMPS = 33.00000000 , NITERA :    1  !                     !   !
!  TEMPS = 34.00010300 , NITERA :  196  !                     !   !
!  TEMPS = 35.00010300 , NITERA : 1217  !                     !   !
!  TEMPS = 36.00184250 , NITERA : 1757  !<< 1/4 of CP Time    !   !
!  TEMPS = 37.00208664 , NITERA : 2021                        !   !
!  TEMPS = 38.00031281 , NITERA : 2317                        !   !
!  TEMPS = 39.00342941 , NITERA : 2615                        !   !
!  TEMPS = 40.00069809 , NITERA : 2915       1/2 of CP Time >>!   !
!  TEMPS = 45.00157166 , NITERA : 4424                            !
!  TEMPS = 50.00123596 , NITERA : 5997             full CP Time >>!
! 2644.8u 0.0s 47:53 92% 0+245k 0+13io 233pf+0w
!
! 5- Please send any result(s) and remark(s)/comment(s) to :
!  Nhuan DODUC
!  FRAMENTEC
!  La Boursidiere
!  F 92350 Le Plessis Rpbinson
! uucp     : ...seismo!mcvax!inria!ftc!ndoduc
! 'ARPA'   : 'mcvax!inria!ftc!ndoduc'@SEISMO.CSS.GOV
! Bit/Earn : Ndoduc at FRSAC11
!
! 6- Please feel free to make comment(s)/remark(s) about this
! program or this benchmark .
! There is no condition about using this freeware : however
! I would appreciate receiving any new result from you to enrich
! the benchmark report , which is also available freely on request .
!
!-CDC-PROGRAM MAIN (TAPE5,OUTPUT,TAPE6=OUTPUT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      COMMON /AAA33 / VCO , XL0055 , D876 , DINt , DEXt , VOL002 ,      &
     &                VOL005 , LCO , NCRay
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA88 / FLUp(22,2) , FLUv(22,2) , FLUl(22,2) , FLUiv(22,2)&
     &                , DEBil(22,2) , FLUil(22,2) , DEBi(22) , DEBhi(22)&
     &                , DEBiv(22,2)
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA16 / DBEnt(20) , TENt(20) , HENt(20) , DEBe , ENTe ,   &
     &                DBSor(20) , TSOr(20) , DEBs , DPDtt(20) ,         &
     &                TDPdt(20) , HSOr(20) , THSor(20) , ENTs , NDBent ,&
     &                NDPdt , NDBsor , NHSor
      COMMON /AAA17 / DDPdt , DDH(22,2) , DDV(22,2) , DDM(22,2) ,       &
     &                DDU(22,2)
      COMMON /AAA18 / DDEb(21) , DDDeb(21) , DXAt(21) , DXAg(21) ,      &
     &                DXBt(21) , DXBg(21) , DNUg(21) , DNUf(21) ,       &
     &                DHE(22,2) , DDEbo(21)
      COMMON /AAA20 / DQP(22,2) , DFLul(22) , DFLuv(22) , DFLuil(22) ,  &
     &                DFLuiv(22) , DDEbil(22) , DDEbiv(22) , DQCei(2) , &
     &                DQCed(22,12) , DDTuo(22,12) , DDTga(22,12) ,      &
     &                DDTmi(2) , DDTmu(2)
      DOUBLE PRECISION M
 

      OPEN (5,FILE='doduc.in')
      CALL S00000(0)
      READ (5,99001) ilect , ISOrt , IPLot , nlect
99001 FORMAT (3(I1,9X),I2)
      CALL S00002
      PRINT 99002
99002 FORMAT (' MAIN : FIN S00002')
      CALL S00001
      PRINT 99003
99003 FORMAT (' MAIN : FIN S00001')
      CALL S00011
      PRINT 99004
99004 FORMAT (' MAIN : FIN S00011')
      CALL S00022
      PRINT 99005
99005 FORMAT (' MAIN : FIN S00022')
      NITera = 1
      NECrit = 0
      NSTar = 0
      V00001 = .01
 100  CALL S00001
      CALL S00012
      CALL S00015
      CALL S00013
      CALL S00014
      CALL S00009
      CALL S00020
      CALL S00017
      nivo = NIV
      CALL S00018(i12,i21,iorg)
      CALL S00005(iorga)
      CALL S00019
      CALL S00004(ICAt(2),nivo,iorg)
      NITera = NITera + 1
      CALL S00006(istop)
      IF ( istop.EQ.0 ) GOTO 100
      TEMimp = TEM - 100000.D+00
      CALL S00005(iorga)
      CLOSE (5)
      STOP
      END
!*==S00006.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00006(Istop)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      Istop = 0
      znn = ZNIv
      IF ( znn.GT.ZLImax ) Istop = 1
      IF ( znn.LT.ZLImin ) Istop = 1
      IF ( TEM.GE.TLIm ) Istop = 1
      CONTINUE
      END
!*==S00009.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00009
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA88 / FLUp(22,2) , FLUv(22,2) , FLUl(22,2) , FLUiv(22,2)&
     &                , DEBil(22,2) , FLUil(22,2) , DEBi(22) , DEBhi(22)&
     &                , DEBiv(22,2)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      COMMON /AAA16 / DBEnt(20) , TENt(20) , HENt(20) , DEBe , ENTe ,   &
     &                DBSor(20) , TSOr(20) , DEBs , DPDtt(20) ,         &
     &                TDPdt(20) , HSOr(20) , THSor(20) , ENTs , NDBent ,&
     &                NDPdt , NDBsor , NHSor
      DOUBLE PRECISION M , cc(22,2)
      coef = 144.D+00/778.D+00
      DO i = 2 , NC1
         j = i - 1
         ity = ITYp(i)
         j1 = 2
         IF ( ity.EQ.1 ) j1 = 1
         IF ( ity.EQ.2 ) THEN
            DM(i,1) = DEBl(j) - DEBl(i) - DEBi(i)
            DM(i,2) = DEBv(j) - DEBv(i) + DEBi(i)
            DUU(i,1) = DEBl(j)*HE(j,1) - DEBl(i)*HE(i,1) + FLUl(i,1)    &
     &                 + FLUil(i,1) - DEBil(i,1)*HF(i,1) + DEBiv(i,2)   &
     &                 *HFS - DEBiv(i,1)*HGS + DEBil(i,2)*HFS
            DUU(i,2) = DEBv(j)*HE(j,2) - DEBv(i)*HE(i,2) + FLUv(i,2)    &
     &                 + FLUiv(i,2) + DEBil(i,1)*HGS - DEBiv(i,2)       &
     &                 *HG(i,2) + DEBiv(i,1)*HGS - DEBil(i,2)*HFS
         ELSEIF ( ity.EQ.3 ) THEN
            DM(i,1) = DEB(j) - DEBi(i)
            DM(i,2) = -DEB(i) + DEBi(i)
            DUU(i,1) = DEBl(j)*HE(j,1) + DEBv(j)*HE(j,2) - DEBhi(i)     &
     &                 + FLUp(i,1)
            DUU(i,2) = -DEBl(i)*HE(i,1) - DEBv(i)*HE(i,2) + DEBhi(i)    &
     &                 + FLUp(i,2)
         ELSE
            DM(i,1) = DEB(j) - DEB(i)
            DUU(i,1) = DEBl(j)*HE(j,1) + DEBv(j)*HE(j,2) - DEBl(i)      &
     &                 *HE(i,1) - DEBv(i)*HE(i,2) + FLUp(i,1)
         ENDIF
         DO k = 1 , j1
            cc(i,k) = DUU(i,k) - H(i,k)*DM(i,k)
            AA(i,k) = VV(i,k)*DM(i,k) + DVVh(i,k)*cc(i,k)
            DV(i,k) = AA(i,k) + BB(i,k)*DPDt
            IF ( DABS(DV(i,k)).LT.1.D-10 ) DV(i,k) = 0.D+00
            epsm = DELm*VC(i)/VVFs
            ym = M(i,k)
            IF ( ym.LT.epsm ) ym = epsm
            DH(i,k) = (cc(i,k)+coef*V(i,k)*DPDt)/ym
            DNU(i,k) = DVVh(i,k)*DH(i,k) + DVVp(i,k)*DPDt
         ENDDO
      ENDDO
      taft = TEM + 1.D-4
      CALL S00934(THSor,HSOr,HSOr,taft,haft,x,2)
      DH(NC2,1) = (haft-H(NC2,1))/1.D-4
      DNU(NC2,1) = DVVh(NC2,1)*DH(NC2,1) + DVVp(NC2,1)*DPDt
      CALL S00934(TENt,HENt,HENt,taft,haft,x,20)
      DH(1,1) = (haft-H(1,1))/1.D-4
      DNU(1,1) = DVVh(1,1)*DH(1,1) + DVVp(1,1)*DPDt
      IF ( ICAt(2).LE.2 ) RETURN
      ISS = 1
      VITess = DV(NIV,1)
      IF ( VITess.LT.0. ) ISS = 2
      CONTINUE
      END
!*==S00055.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00055(Ii)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA33 / VCO , XL0055 , D876 , DINt , DEXt , VOL002 ,      &
     &                VOL005 , LCO , NCRay
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      pi = 3.14159
      IF ( NMUltr.LE.1 ) RETURN
      n1 = NASl(Ii)
      n2 = NSL(Ii)
      i2 = 0
      DO i = 1 , n2
         i1 = i2 + 1
         i2 = i*NMUltr
         xmcg = 0.D+00
         xmcu = 0.D+00
         DO k = i1 , i2
            xmcg = xmcg + TGAi(Ii,k)*XMCga(Ii,k)
            xmcu = xmcu + T876(Ii,k)*XMCuo(Ii,k)
         ENDDO
         XMCga(Ii,i) = XMCga(Ii,i1)*NMUltr
         XMCuo(Ii,i) = XMCuo(Ii,i1)*NMUltr
         TGAi(Ii,i) = xmcg/XMCga(Ii,i)
         T876(Ii,i) = xmcu/XMCuo(Ii,i)
         PL(Ii,i) = PL(Ii,i1)
         XHTc(Ii,i) = XHTc(Ii,i1)*NMUltr
         QREpa(Ii,i) = QREpa(Ii,i1)*NMUltr
         TUMax(Ii,i) = T876(Ii,i) + PL(Ii,i)*PLMoy/8./pi/XL876
         TUMin(Ii,i) = T876(Ii,i) - PL(Ii,i)*PLMoy/8./pi/XL876
         TGAii(Ii,i) = TGAi(Ii,i) + PL(Ii,i)                            &
     &                 *PLMoy/2./pi/XLGai*DLOG(DEXt/DINt)
      ENDDO
      NASl(Ii) = NSL(Ii)
      CONTINUE
      END
!*==S00015.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00015
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      DOUBLE PRECISION M
      DO i = 1 , NC1
         sec = SC(i)
         DEBav(i) = DEB(i)
         idoduc = ITYp(i)
         IF ( idoduc.EQ.2 ) THEN
            vfb = VV(i,1)
            vgb = VV(i,2)
            hfb = HF(i,1)
            hgb = HG(i,2)
         ELSEIF ( idoduc.EQ.3 ) THEN
            ica = IST(i,2)
            vfb = VVFs
            vgb = VVGs
            IF ( ica.EQ.1 ) vfb = VV(i,2)
            IF ( ica.EQ.3 ) vgb = VV(i,2)
            hfb = HF(i,2)
            hgb = HG(i,2)
         ELSE
            ica = IST(i,1)
            vfb = VVFs
            vgb = VVGs
            IF ( ica.EQ.1 ) vfb = VV(i,1)
            IF ( ica.EQ.3 ) vgb = VV(i,1)
            hfb = HF(i,1)
            hgb = HG(i,1)
         ENDIF
         j = i + 1
         idoduc = ITYp(j)
         IF ( idoduc.EQ.2 ) THEN
            vft = VV(j,1)
            vgt = VV(j,2)
            hft = HF(j,1)
            hgt = HG(j,2)
         ELSEIF ( idoduc.EQ.3 ) THEN
            ica = IST(j,1)
            vft = VVFs
            vgt = VVGs
            hft = HF(j,1)
            hgt = HG(j,1)
            IF ( ica.EQ.1 ) vft = VV(j,1)
            IF ( ica.EQ.3 ) vgt = VV(j,1)
         ELSE
            ica = IST(j,1)
            vft = VVFs
            vgt = VVGs
            IF ( ica.EQ.1 ) vft = VV(j,1)
            IF ( ica.EQ.3 ) vgt = VV(j,1)
            hft = HF(j,1)
            hgt = HG(j,1)
         ENDIF
         TJOnc(i) = TS
         HJOnb(i,1) = hfb
         HJOnb(i,2) = hgb
         HJOnt(i,1) = hft
         HJOnt(i,2) = hgt
         VE(i,1) = (vfb+vft)/2.D+00
         VE(i,2) = (vgb+vgt)/2.D+00
         CALL S33019(i,sec)
      ENDDO
      CONTINUE
      END
!*==F00113.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      DOUBLE PRECISION FUNCTION F00113(Iopt,P,Diahy,Twa,Tfl,Al)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION r(15) , f(15)
      DATA r/.15 , .25 , .35 , .45 , .65 , .85 , 1.05 , 1.25 , 1.45 ,   &
     &     1.95 , 2.45 , 2.95 , 3.45 , 3.95 , 4.45/
      DATA f/.4258 , .5359 , .6235 , .6981 , .8238 , .9295 , 1.0222 ,   &
     &     1.1056 , 1.1820 , 1.3506 , 1.4967 , 1.6271 , 1.7459 ,        &
     &     1.8555 , 1.9578/
      tw = Twa/1000. + .45967
      tf = Tfl/1000. + .45967
      IF ( Iopt.GE.3 ) THEN
         ccc = (1.-.6*Al)*1.73/(1.25-.15*Al)
      ELSE
         pwl = (P/14.696)*Diahy*tw/tf
         tt = tw*1000.D+00
         eh2o = S00093(pwl,tt)
         rap = tf/tw
         IF ( rap.LT..45 ) THEN
            i1 = INT(rap/.1-1.5) + 1
         ELSEIF ( rap.GE.1.45 ) THEN
            i1 = INT(rap/.5-2.9) + 9
         ELSE
            i1 = INT(rap/.2-2.25) + 4
         ENDIF
         i2 = i1 + 1
         r1 = r(i1)
         r2 = r(i2)
         f1 = f(i1)
         f2 = f(i2)
         ff = f1 + (rap-r1)*(f2-f1)/(r2-r1)
         ah2o = 2.*eh2o*ff
         ewall = .6
         ccc = 1.73/(1./ewall+1./ah2o-1.)
      ENDIF
      F00113 = ccc*(tw+tf)*(tw*tw+tf*tf)/3600.D+00
      CONTINUE
      END
!*==S00061.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00061(I,Hv,Hvt,Hvme,Hvms,Ynu,Re,Tfl,Dtfl,Iopt)
      IMPLICIT DOUBLE PRECISION(a-H,O-Z)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      COMMON /AAA18 / DDEb(21) , DDDeb(21) , DXAt(21) , DXAg(21) ,      &
     &                DXBt(21) , DXBg(21) , DNUg(21) , DNUf(21) ,       &
     &                DHE(22,2) , DDEbo(21)
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      DOUBLE PRECISION x(21) , y(21)
      DATA a , b , c , d , e/.76082D-15 , -.74064D-11 , .23241D-7 ,     &
     &     .14794D-4 , .10467D-1/
      DATA r , s/1.7399D-2 , 5.4997D-5/
      CALL X21Y21(x,y)
      Hvt = 0.D+00
      Hvme = 0.D+00
      Hvms = 0.D+00
      tfla = Tfl
      IF ( Tfl.GE.1600. ) tfla = 1600.D+00
      tf = tfla
      ie = I - 1
      debe = DEBav(ie)
      debs = DEBav(I)
      deve = XA(ie)*debe + XB(ie)
      devs = XA(I)*debs + XB(I)
      sec = SCApa(I)
      debm = (deve+devs)/2.D+00
      eps = 1.D+00
      IF ( debm.LT.0. ) eps = -1.D+00
      vis = (r+s*tf)/3600.D+00
      Re = eps*debm*DIAhy/(vis*sec)
      reo = Re
      IF ( Re.LT.1000. ) THEN
         i1 = INT(Re/200.) + 1
      ELSEIF ( Re.LT.4000. ) THEN
         i1 = INT(Re/500.) + 4
      ELSEIF ( Re.LT.10000. ) THEN
         i1 = INT(Re/2000.) + 10
      ELSEIF ( Re.GE.30000. ) THEN
         IF ( Re.GE.49999. ) Re = 49999.D+00
         i1 = INT(Re/10000.) + 16
      ELSE
         i1 = INT(Re/5000.) + 13
      ENDIF
      i2 = i1 + 1
      y1 = y(i1)
      y2 = y(i2)
      x1 = x(i1)
      x2 = x(i2)
      rap = (y2-y1)/(x2-x1)
      tmre = y1 + rap*(Re-x1)
      Ynu = .023*tmre
      ynuo = Ynu
      IF ( Ynu.LT.3.5 ) Ynu = 3.5
      tf = tfla
      con = e + tf*(d+tf*(c+tf*(b+a*tf)))
      Hv = Ynu*con/(3600.*DIAhy)
      IF ( Iopt.LE.0 ) RETURN
      dtfla = Dtfl
      IF ( Tfl.GE.1600. ) dtfla = 0.D+00
      dnut = 0.D+00
      IF ( ynuo.GE.3.5 ) THEN
         IF ( reo.LE.49999. ) THEN
            dqe = (DXAg(ie)*debe+XA(ie)+DXBg(ie))*DDEb(ie) + DXAt(ie)   &
     &            *debe + DXBt(ie)
            dqs = (DXAg(I)*debs+XA(I)+DXBg(I))*DDEb(I) + DXAt(I)        &
     &            *debs + DXBt(I)
            dqt = .5*(dqe+dqs)
            dmut = s*dtfla/3600.D+00
            dret = eps*DIAhy*(dqt-debm*dmut/vis)/(vis*sec)
            dnut = .023*rap*dret
            ynumax = 5.*ynuo
            abnut = DABS(dnut)
            IF ( abnut.GE.ynumax ) dnut = dnut*ynumax/abnut
         ENDIF
      ENDIF
      dlat = (d+tf*(2.*c+tf*(3.*b+4.*a*tf)))*dtfla
      Hvt = (Ynu*dlat+dnut*con)/(3600.*DIAhy)
      CONTINUE
      END
!*==S22202.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      DOUBLE PRECISION FUNCTION S22202(Al,Dal,Tw,Dtw,Tf,Dtf,P,Dpdt,     &
     &                                 Diahy,Iopt)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION r(15) , f(15)
      DATA r/.15 , .25 , .35 , .45 , .65 , .85 , 1.05 , 1.25 , 1.45 ,   &
     &     1.95 , 2.45 , 2.95 , 3.45 , 3.95 , 4.45/
      DATA f/.4258 , .5359 , .6235 , .6981 , .8238 , .9295 , 1.0222 ,   &
     &     1.1056 , 1.1820 , 1.3506 , 1.4967 , 1.6271 , 1.7459 ,        &
     &     1.8555 , 1.9578/
      twra = Tw/1000. + .45967
      tfra = Tf/1000. + .45967
      tfc = tfra*tfra
      twc = twra*twra
      ts = twra + tfra
      tsc = ts*ts
      hrt = ts*(twc+tfc)
      hrft = tsc + 2.*tfc
      hrwt = tsc + 2.*twc
      IF ( Iopt.EQ.3 ) THEN
         cc = 1.73/3600.D+00
         den = 1.25 - .15*Al
         dhdtf = cc*(1.-.6*Al)*hrft/den
         dhdtw = cc*(1.-.6*Al)*hrwt/den
         dhdal = cc*hrt*(-.6)/(den*den)
         S22202 = dhdtf*Dtf + dhdtw*Dtw + dhdal*Dal
         RETURN
      ELSE
         rr = tfra/twra
         IF ( rr.LT..45 ) THEN
            i1 = INT(rr/.1-1.5) + 1
         ELSEIF ( rr.GE.1.45 ) THEN
            i1 = INT(rr/.5-2.9) + 9
         ELSE
            i1 = INT(rr/.2-2.25) + 4
         ENDIF
         i2 = i1 + 1
         r1 = r(i1)
         r2 = r(i2)
         f1 = f(i1)
         f2 = f(i2)
         pent = (f2-f1)/(r2-r1)
         ff = f1 + (rr-r1)*pent
         dtwa = Dtw/1000.D+00
         dtfa = Dtf/1000.D+00
         xt = 1.D-4
         px = P + xt*Dpdt
         twx = twra + dtwa*xt
         tfx = tfra + dtfa*xt
         pwlx = (px/14.696)*Diahy*twx/tfx
         pwl = (P/14.696)*Diahy*twra/tfra
         ewall = .6
         tt = twra*1000.D+00
         ttx = twx*1000.D+00
         eh2o = S00093(pwl,tt)
         eh2ox = S00093(pwlx,ttx)
         deh2o = (eh2ox-eh2o)/xt
         ah2o = 2.*eh2o*ff
         drr = (dtfa-rr*dtwa)/twra
         df = pent*drr
         dah2o = 2.*(eh2o*df+deh2o*ff)
         den = 1./ewall + 1./ah2o - 1.D+00
         dcc = 1.73*dah2o/(den*den*ah2o*ah2o)
         cc = 1.73/den
         S22202 = (dcc*hrt+cc*(dtwa*hrwt+dtfa*hrft))/3600.D+00
         RETURN
      ENDIF
      END
!*==S00089.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00089(I,Al,Dal,Dtf,Xx)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA88 / FLUp(22,2) , FLUv(22,2) , FLUl(22,2) , FLUiv(22,2)&
     &                , DEBil(22,2) , FLUil(22,2) , DEBi(22) , DEBhi(22)&
     &                , DEBiv(22,2)
      COMMON /AAA17 / DDPdt , DDH(22,2) , DDV(22,2) , DDM(22,2) ,       &
     &                DDU(22,2)
      COMMON /AAA20 / DQP(22,2) , DFLul(22) , DFLuv(22) , DFLuil(22) ,  &
     &                DFLuiv(22) , DDEbil(22) , DDEbiv(22) , DQCei(2) , &
     &                DQCed(22,12) , DDTuo(22,12) , DDTga(22,12) ,      &
     &                DDTmi(2) , DDTmu(2)
      DOUBLE PRECISION Dal(12) , Dtf(22,2) , Xx(8) , Al(12) , dq(2) ,   &
     &                 xtss(2)
      DOUBLE PRECISION M , dxtss(2) , tff(2) , dtff(2)
      IF ( ITYp(I).EQ.3 ) THEN
         dq(1) = 0.D+00
         dq(2) = 0.D+00
         tff(1) = T(I,1)
         tff(2) = T(I,2)
         dtff(1) = Dtf(I,1)
         dtff(2) = Dtf(I,2)
         n1 = NASl(I)
         xtss(1) = 1.D+00
         xtss(2) = 1.D+00
         dxtss(1) = 0.D+00
         dxtss(2) = 0.D+00
         IF ( TS.GT.tff(1) ) THEN
            ect = TS - tff(1)
            ectc = ect*ect + 2.4
            xtss(1) = .025 + 2.34/ectc
            dxtss(1) = (4.68*ect/(ectc*ectc))*(dtff(1)-DTSat)
         ENDIF
         DO j = 1 , n1
            IF ( j.NE.ISLni ) THEN
               tga = TGAi(I,j)
               dtga = DTGai(I,j)
               ind = (n1+j-ISLni)/n1 + 1
               xts = xtss(ind)
               tfl = tff(ind)
               dtfl = dtff(ind)
               xsi = 1.D+00
               dxsi = 0.D+00
               IF ( tfl.GT.tga ) THEN
                  ect = tfl - tga + 2.D+00
                  xsi = .1 + 1.8/ect
                  dxsi = -1.8*(dtfl-dtga)/(ect*ect)
               ENDIF
               hcoef = HLCf*(2-ind) + hvcf*(ind-1)
               dhvcf = 0.D+00
               dhcoe = (ind-1)*hvcf
               hr = HTC(I,j) - hcoef*xsi*xts
               dhr = S22202(Al(ind),Dal(ind),tga,dtga,tfl,dtfl,x1,x2,x3,&
     &               3)
               dhr = S22202(Al(j),Dal(j),tga,dtga,tfl,dtfl,x1,x2,x3,3)
               dh1 = hcoef*(xsi*dxtss(ind)+dxsi*xts) + dhcoe*xsi*xts +  &
     &               dhr
               DQCed(I,j) = (HTC(I,j)*(dtga-dtfl)+dh1*(tga-tfl))        &
     &                      *XHTc(I,j)
               dq(ind) = dq(ind) + DQCed(I,j)
            ENDIF
         ENDDO
         DO j = 1 , 2
            tfl = tff(j)
            dtfl = dtff(j)
            tga = TMI(j)
            dtga = DTMi(j)
            xsi = 1.D+00
            dxsi = 0.D+00
            IF ( tfl.GT.tga ) THEN
               ect = tfl - tga + 2.D+00
               xsi = .1 + 1.8/ect
               dxsi = -1.8*(dtfl-dtga)/(ect*ect)
            ENDIF
            xy = RI(j)
            dxy = DV(I,j)*n1/VC(I)
            xts = xtss(j)
            dxts = dxtss(j)
            x = xsi*dxts + dxsi*xts
            hcoef = HLCf*(2-j) + hvcf*(j-1)
            dhvcf = 0.D+00
            dhcoe = (j-1)*dhvcf
            dhr = S22202(Al(j),Dal(j),tga,dtga,tfl,dtfl,x1,x2,x3,3)
            dh1 = dhcoe*xsi*xts + hcoef*x + dhr
            DQCei(j) = (HTC(I,j)*(xy*(dtga-dtfl)+dxy*(tga-tfl))         &
     &                 +(tga-tfl)*xy*dh1)*XHTc(I,ISLni)
            dq(j) = dq(j) + DQCei(j)
         ENDDO
         Xx(1) = dq(1)
         Xx(2) = dq(2)
         RETURN
      ELSE
         hvcf = HVCff(I)
         n1 = NASl(I)
         tfl = T(I,1)
         dtfl = Dtf(I,1)
         dqpa = 0.D+00
         IF ( ICAt(2).LE.2 ) THEN
            tvap = T(I,1)
            dtvap = Dtf(I,1)
            IF ( ITYp(I).NE.1 ) THEN
               tvap = T(I,2)
               dtvap = Dtf(I,2)
               tfl = TS
               dtfl = DTSat
               dfluv1 = 0.D+00
               dfluv2 = 0.D+00
               dflul1 = 0.D+00
               dflul2 = 0.D+00
               dflvme = 0.D+00
               dflvms = 0.D+00
            ENDIF
            CALL S00061(I,hv,hvt,hvme,hvms,ynu,re,tvap,dtvap,1)
            DO j = 1 , n1
               IF ( ITYp(I).NE.1 ) THEN
                  hvcf = HVCfo
                  dhvcf = 0.D+00
               ENDIF
               tga = TGAi(I,j)
               dtga = DTGai(I,j)
               alff = Al(j)
               dalf = Dal(j)
               xsi = 1.D+00
               dxsi = 0.D+00
               IF ( tfl.GT.tga ) THEN
                  ect = tfl - tga + 2.D+00
                  xsi = .1 + 1.8/ect
                  dxsi = -1.8*(dtfl-dtga)/(ect*ect)
               ENDIF
               xts = 1.D+00
               dxts = 0.D+00
               IF ( tfl.LT.TS ) THEN
                  ect = TS - tfl
                  ectc = ect*ect + 2.4
                  xts = .025 + 2.34/ectc
                  dxts = (4.68*ect/(ectc*ectc))*(dtfl-DTSat)
               ENDIF
               hmil = 100./3600.D+00
               zz1 = ZCOt(IBCh-1)
               zz2 = ZCOt(IHCh)
               zz = ZCOt(I-1) + (XL(I)*(j-.5))/n1
               yy = ((zz-zz1)/(zz2-zz1)) - .5
               tbord = 700.D+00
               tmil = 900.D+00
               aco = (tmil-tbord)*4.D+00
               tmoy = tmil - aco*yy*yy
               tmin = tmoy - 100.D+00
               tmax = tmoy + 100.D+00
               ect = tmax - tmin
               z1 = (tmax-tga)/ect
               dz1 = -dtga/ect
               z2 = (tga-tmin)/ect
               dz2 = dtga/ect
               IF ( tga.GT.tmax ) THEN
                  z1 = 0.D+00
                  z2 = 1.D+00
                  dz1 = 0.D+00
                  dz2 = 0.D+00
               ELSEIF ( tga.LT.tmin ) THEN
                  z1 = 1.D+00
                  z2 = 0.D+00
                  dz1 = 0.D+00
                  dz2 = 0.D+00
               ENDIF
               IF ( alff.GT.1.D-4 ) THEN
                  pt1 = -.1
                  pt2 = 2.*(hvcf-hmil)/hvcf
                  qt1 = 1.D+00
                  qt2 = 1. - pt2
                  al1 = .625
                  al2 = .95
                  IF ( alff.LE.al1 ) THEN
                     y1 = (pt1*alff+qt1)*HLCf
                     dy1 = pt1*HLCf*Dal(j)
                  ELSEIF ( alff.GT.al2 ) THEN
                     y1 = (pt2*alff+qt2)*hvcf
                     dy1 = 2.*(hvcf-hmil)*dalf + (2.*alff-1.)*dhvcf
                  ELSE
                     xla = al2 - al1
                     x1 = pt1*al1 + qt1
                     x2 = pt2*al2 + qt2
                     h1 = x1*HLCf
                     h2 = x2*hvcf
                     u1 = -pt1/x1
                     u2 = -pt2/x2
                     tc1 = 2. - u1*xla
                     tc2 = 2. + u2*xla
                     xla3 = xla*xla*xla
                     te1 = alff - al1
                     te2 = al2 - alff
                     p1 = te2*te2*(tc1*te1+xla)/xla3
                     p2 = te1*te1*(tc2*te2+xla)/xla3
                     y1 = (h1*p1+h2*p2)
                     dp1dt = -te2*(tc1*(3.*alff-2.*al1-al2)+2.*xla)     &
     &                       *Dal(j)/xla3
                     dp2dt = -te1*(tc2*(3.*alff-2.*al2-al1)-2.*xla)     &
     &                       *Dal(j)/xla3
                     dh2 = (pt2*al2+qt2)*dhvcf
                     dy1 = dp1dt*h1 + dp2dt*h2 + dh2*p2
                  ENDIF
               ELSE
                  y1 = HLCf
                  dy1 = 0.D+00
               ENDIF
               IF ( alff.GT..5 ) THEN
                  y2 = 2.*(hvcf-hmil)*(alff-1.) + hvcf
                  dy2 = 2.*(hvcf-hmil)*dalf + (2.*alff-1.)*dhvcf
               ELSE
                  beta = (hmil-hvcf)/(HLCf-hvcf)
                  zb = (.5-beta)/(.5*(1.-beta))
                  za = zb*zb/(1.-zb)
                  zc = -1./(za+zb)
                  y2 = 1./(za*alff+zb) + zc
                  y2 = y2*(HLCf-hvcf) + hvcf
                  dnom = za*alff + zb
                  dy2 = -(HLCf-hvcf)*za*dalf/(dnom*dnom)                &
     &                  + (1.-1./dnom-zc)*dhvcf
               ENDIF
               dhc = dz1*y1 + dz2*y2 + z1*dy1 + z2*dy2
               hcoef = z1*y1 + z2*y2
               IF ( ITYp(I).EQ.2 ) THEN
                  y = (hcoef-hvcf)/(HLCf-hvcf)
                  dydt = dhc/(HLCf-hvcf)
                  hvcf = HVCff(I)
                  dhvcf = hvt
                  sur = XHTc(I,j)
                  hr1 = F00113(1,P,DIAhy,tga,T(I,1),0.0D0)
                  hr2 = F00113(1,P,DIAhy,tga,T(I,2),1.0D0)
                  dhr1 = S22202(x1,x2,tga,dtga,T(I,1),Dtf(I,1),P,DPDt,  &
     &                   DIAhy,1)
                  dhr2 = S22202(x1,x2,tga,dtga,T(I,2),Dtf(I,2),P,DPDt,  &
     &                   DIAhy,1)
                  hl1 = HLCf + hr1
                  hv1 = HVAp + hr1
                  hl2 = HCOn + hr2
                  hv2 = hvcf + hr2
                  dhl1 = dhr1
                  dhv1 = dhr1
                  dhl2 = dhr2
                  dhv2 = dhr2 + dhvcf
                  s1 = sur*y
                  s2 = sur*(1.-y)
                  ds1 = sur*dydt
                  ds2 = -ds1
                  dtf1 = Dtf(I,1)
                  dtf2 = Dtf(I,2)
                  tf1 = T(I,1)
                  tf2 = T(I,2)
                  IF ( TS.LT.tga ) THEN
                     dul1 = hl1*((DTSat-dtf1)*s1+(TS-tf1)*ds1)          &
     &                      + dhl1*(TS-tf1)*s1
                     dul2 = 0.D+00
                     duv1 = hv1*((dtga-DTSat)*s1+(tga-TS)*ds1)          &
     &                      + dhv1*(tga-TS)*s1
                     duv2 = hv2*((dtga-dtf2)*s2+(tga-tf2)*ds2)          &
     &                      + dhv2*(tga-tf2)*s2
                     duvme = hvme*(tga-tf2)*s2
                     duvms = hvms*(tga-tf2)*s2
                     fluvv2 = hv2*(tga-tf2)*s2
                     flull1 = hl1*(TS-tf1)*s1
                  ELSE
                     duv1 = 0.D+00
                     duv2 = hv2*((DTSat-dtf2)*s2+(TS-tf2)*ds2)          &
     &                      + dhv2*(TS-tf2)*s2
                     duvme = hvme*(TS-tf2)*s2
                     duvms = hvms*(TS-tf2)*s2
                     dul1 = hl1*((dtga-dtf1)*s1+(tga-tf1)*ds1)          &
     &                      + dhl1*(tga-tf1)*s1
                     dul2 = hl2*((dtga-DTSat)*s2+(tga-TS)*ds2)          &
     &                      + dhl2*(tga-TS)*s2
                     fluvv2 = hv2*(TS-tf2)*s2
                     flull1 = hl1*(tga-tf1)*s1
                  ENDIF
                  dtcor = 0.D+00
                  duv1 = duv1 + dtcor
                  duv2 = duv2 - dtcor
                  dfluv1 = dfluv1 + duv1
                  dfluv2 = dfluv2 + duv2
                  dflul1 = dflul1 + dul1
                  dflul2 = dflul2 + dul2
                  dflvme = dflvme + duvme
                  dflvms = dflvms + duvms
                  DQCed(I,j) = dul1 + dul2 + duv1 + duv2
               ELSE
                  x1 = TEM
                  dhr = S22202(x1,x2,tga,dtga,tfl,dtfl,P,DPDt,DIAhy,1)
                  dhtc = dhr + dhc*xsi*xts + hcoef*(dxsi*xts+xsi*dxts)
                  DQCed(I,j) = XHTc(I,j)                                &
     &                         *(dhtc*(tga-tfl)+HTC(I,j)*(dtga-dtfl))
                  dqpa = dqpa + DQCed(I,j)
               ENDIF
            ENDDO
            IF ( ITYp(I).EQ.2 ) THEN
               Xx(1) = dflul1
               Xx(2) = dfluv2
               Xx(7) = dflvme
               Xx(8) = dflvms
               DQP(I,1) = dflul1 + dfluv1
               DQP(I,2) = dflul2 + dfluv2
               x = V(I,2)/VC(I)
               dx = DV(I,2)/VC(I)
               smax = 1.8*SHTc(I)
               IF ( x.GE..8 ) THEN
                  ro = .0295/12.D+00
                  IF ( x.GE..95 ) THEN
                     IF ( x.LT..99 ) THEN
                        r = ro
                        rp = 0.D+00
                     ELSEIF ( x.GE..999 ) THEN
                        r = ro/5.D+00
                        rp = 0.D+00
                     ELSE
                        r = ro*(89.-.8*x/.009)
                        rp = -.8*ro/.009
                     ENDIF
                     si = 3.*(1.-x)*VC(I)/r
                     dsi = -3.*VC(I)*(1.+(1.-x)*rp/r)*dx/r
                  ELSE
                     xla = .15
                     y2 = .15*VC(I)/ro
                     rap = -20.D+00
                     tc1 = .95 - x
                     tc2 = x - .8
                     si1 = smax*tc1*tc1*(2.*tc2+xla)
                     si2 = y2*tc2*tc2*((2.-rap*xla)*tc1+xla)
                     si = (si1+si2)/3.375D-3
                     dsi = dx*tc2*                                      &
     &                     (-6.*smax*tc1+y2*((2.-rap*xla)*(2.7-3.*x)    &
     &                     +2.*xla))/3.375D-3
                  ENDIF
               ELSE
                  si = smax*(-1.5625*x+2.5)*x
                  dsi = smax*(-3.125*x+2.5)*dx
               ENDIF
               dfisup = HVIn*(si*(Dtf(I,2)-DTSat)+dsi*(T(I,2)-TS))
               dfiinf = HLIn*(si*(DTSat-Dtf(I,1))+dsi*(TS-T(I,1)))
               duiv1 = dfisup
               Xx(3) = dfiinf
               Xx(4) = -dfisup
               duil2 = -dfiinf
               Xx(5) = (dfluv1+duiv1+DEBil(I,1)*(DH(I,1)-DHGps*DPDt))   &
     &                 /(HGS-H(I,1))
               Xx(6) = (dflul2+duil2+DEBiv(I,2)*(DH(I,2)-DHFps*DPDt))   &
     &                 /(HFS-H(I,2))
               RETURN
            ELSE
               Xx(1) = dqpa
               RETURN
            ENDIF
         ELSE
            ind = 1 + (NC1+I-NIV)/NC1
            DO j = 1 , n1
               tga = TGAi(I,j)
               dtga = DTGai(I,j)
               hcoe = HTC(I,j)
               xts = 1.D+00
               dxts = 0.D+00
               IF ( tfl.LT.TS ) THEN
                  ect = TS - tfl
                  x = 1./(2.4+ect*ect)
                  xts = .025 + 2.34*x
                  dxts = 4.68*ect*x*x*(dtfl-DTSat)
               ENDIF
               xsi = 1.D+00
               dxsi = 0.D+00
               IF ( tfl.GT.tga ) THEN
                  ect = tfl - tga + 2.D+00
                  xsi = .1 + 1.8/ect
                  dxsi = -1.8*(dtfl-dtga)/(ect*ect)
               ENDIF
               h1 = HLCf*(2-ind) + hvcf*(ind-1)
               dhvcf = 0.D+00
               dh1 = dhvcf*(ind-1)
               hr = hcoe - h1*xts*xsi
               dhr = S22202(Al(j),Dal(j),tga,dtga,tfl,dtfl,x1,x2,x3,3)
               dhcoe = h1*(xsi*dxts+dxsi*xts) + dh1*xsi*xts + dhr
               dqpa = dqpa + XHTc(I,j)                                  &
     &                *(hcoe*(dtga-dtfl)+dhcoe*(tga-tfl))
            ENDDO
            Xx(1) = dqpa
            RETURN
         ENDIF
      ENDIF
      END
!*==S00013.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00013
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA88 / FLUp(22,2) , FLUv(22,2) , FLUl(22,2) , FLUiv(22,2)&
     &                , DEBil(22,2) , FLUil(22,2) , DEBi(22) , DEBhi(22)&
     &                , DEBiv(22,2)
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      DOUBLE PRECISION M , xy(3) , alff(12)
      DO i = 2 , NC1
         sec = SCApa(i)
         IF ( ICAt(2).LT.3 ) THEN
            tfl = T(i,1)
            IF ( ITYp(i).EQ.2 ) tfl = T(i,2)
            CALL S00061(i,hvcf,x1,x2,x3,ynu,re,tfl,x4,0)
            HVCff(i) = hvcf
         ENDIF
         n1 = NASl(i)
         DO j = 1 , n1
            alff(j) = TVId(i,j)
         ENDDO
         idoduc = ITYp(i)
         IF ( idoduc.EQ.2 ) THEN
            FLUv(i,1) = 0.D+00
            FLUv(i,2) = 0.D+00
            FLUl(i,1) = 0.D+00
            FLUl(i,2) = 0.D+00
            n1 = NASl(i)
            x = V(i,2)/VC(i)
            DO j = 1 , n1
               tga = TGAi(i,j)
               CALL S44553(i,1,j,alff(j),hcoef,hray)
               ica = ICAt(2)
               hray1 = F00113(ica,P,DIAhy,tga,T(i,1),0.0D0)
               hray2 = F00113(ica,P,DIAhy,tga,T(i,2),1.0D0)
               hlc = HLCf + hray1
               hva = HVAp + hray1
               hvc = hvcf + hray2
               hco = HCOn + hray2
               y = (hcoef-HVCfo)/(HLCf-HVCfo)
               HTC(i,j) = hcoef
               sht1 = XHTc(i,j)*y
               sht2 = XHTc(i,j)*(1.-y)
               IF ( TS.LT.tga ) THEN
                  flull1 = hlc*(TS-T(i,1))*sht1
                  fluvv1 = hva*(tga-TS)*sht1
                  flull2 = 0.D+00
                  fluvv2 = hvc*(tga-T(i,2))*sht2
               ELSE
                  fluvv1 = 0.D+00
                  flull1 = hlc*(tga-T(i,1))*sht1
                  flull2 = hco*(tga-TS)*sht2
                  fluvv2 = hvc*(TS-T(i,2))*sht2
               ENDIF
               tcor = 0.D+00
               FLUv(i,1) = FLUv(i,1) + fluvv1 + tcor
               FLUv(i,2) = FLUv(i,2) + fluvv2 - tcor
               FLUl(i,1) = FLUl(i,1) + flull1
               FLUl(i,2) = FLUl(i,2) + flull2
               QCEd(i,j) = flull1 + flull2 + fluvv1 + fluvv2
            ENDDO
            x = V(i,2)/VC(i)
            smax = 1.8*SHTc(i)
            IF ( x.GE..8 ) THEN
               ro = .0295/12.D+00
               IF ( x.GE..95 ) THEN
                  r = ro
                  IF ( x.GT..99 ) THEN
                     r = ro/5.D+00
                     IF ( x.LE..999 ) r = ro*(89.-.8*x/.009)
                  ENDIF
                  sint = 3.*(1.-x)*VC(i)/r
               ELSE
                  xla = .15
                  y2 = .15*VC(i)/ro
                  rap = -20.D+00
                  tc1 = .95 - x
                  tc2 = x - .8
                  sint1 = smax*tc1*tc1*(2.*tc2+xla)
                  sint2 = y2*tc2*tc2*((2.-rap*xla)*tc1+xla)
                  sint = (sint1+sint2)/3.375D-3
               ENDIF
            ELSE
               sint = smax*(-1.5625*x+2.5)*x
            ENDIF
            fiinf = HLIn*sint*(TS-T(i,1))
            fisup = HVIn*sint*(T(i,2)-TS)
            FLUiv(i,1) = fisup
            FLUil(i,1) = fiinf
            FLUiv(i,2) = -fisup
            FLUil(i,2) = -fiinf
            FLUp(i,1) = FLUl(i,1) + FLUv(i,1)
            FLUp(i,2) = FLUl(i,2) + FLUv(i,2)
            DEBil(i,1) = (FLUv(i,1)+FLUiv(i,1))/(HGS-HF(i,1))
            DEBiv(i,2) = (FLUl(i,2)+FLUil(i,2))/(HFS-HG(i,2))
            DEBiv(i,1) = 0.D+00
            DEBil(i,2) = 0.D+00
            DEBi(i) = DEBil(i,1) + DEBiv(i,1) - DEBil(i,2) - DEBiv(i,2)
         ELSEIF ( idoduc.EQ.3 ) THEN
            n1 = NASl(i)
            FLUp(i,1) = 0.D+00
            FLUp(i,2) = 0.D+00
            isl1 = ISLni - 1
            isl2 = ISLni + 1
            iopt1 = 2
            IF ( isl1.NE.0 ) THEN
               DO j = 1 , isl1
                  CALL S44553(i,1,j,ALFa(i,1),hco,hray)
                  hcoef = hco + hray
                  HTC(i,j) = hcoef
                  QCEd(i,j) = hcoef*(TGAi(i,j)-T(i,1))*XHTc(i,j)
                  FLUp(i,1) = FLUp(i,1) + QCEd(i,j)
               ENDDO
            ENDIF
            j = isl1 + 1
            zz = HNIv(i)
            hslab = XL(i)/n1
            zs = zz - isl1*hslab
            r1 = zs/hslab
            r2 = 1. - r1
            sec1 = XHTc(i,j)*r1
            sec2 = XHTc(i,j)*r2
            tm1 = TMI(1)
            tm2 = TMI(2)
            RI(1) = r1
            RI(2) = r2
            CALL S44553(i,1,j,ALFa(i,1),hco,hray)
            hco1 = hco + hray
            CALL S44553(i,2,j,ALFa(i,2),hco,hray)
            hco2 = hco + hray
            q1 = hco1*(tm1-T(i,1))*sec1
            q2 = hco2*(tm2-T(i,2))*sec2
            QCEd(i,j) = q1 + q2
            FLUp(i,1) = FLUp(i,1) + q1
            FLUp(i,2) = FLUp(i,2) + q2
            QCEi(1) = q1
            QCEi(2) = q2
            IF ( isl2.LE.n1 ) THEN
               DO j = isl2 , n1
                  CALL S44553(i,2,j,ALFa(i,2),hco,hray)
                  hcoef = hco + hray
                  HTC(i,j) = hcoef
                  QCEd(i,j) = hcoef*(TGAi(i,j)-T(i,2))*XHTc(i,j)
                  FLUp(i,2) = FLUp(i,2) + QCEd(i,j)
               ENDDO
            ENDIF
         ELSE
            n1 = NASl(i)
            fi = 0.D+00
            DO j = 1 , n1
               CALL S44553(i,1,j,alff(j),hco,hray)
               hcoef = hco + hray
               HTC(i,j) = hcoef
               QCEd(i,j) = hcoef*(TGAi(i,j)-T(i,1))*XHTc(i,j)
               fi = fi + QCEd(i,j)
            ENDDO
            FLUp(i,1) = fi
         ENDIF
      ENDDO
      IF ( ICAt(2).LE.2 ) RETURN
      i = NIV
      vg = VVGs
      vf = VVFs
      IF ( IST(i,1).EQ.1 ) vf = VV(i,1)
      IF ( IST(i,1).EQ.3 ) vg = VV(i,1)
      tempe = T(i,1)
      sec = SCApa(i)
      i = NIV
      z1 = XL(i-1)/2.D+00
      z2 = HNIv(i)/2.D+00
      hj = F00111(i-1)
      x = (hj-HFS)/(HGS-HFS)
      alb = x/(x+(1.-x)*VVFs/VVGs)
      alt = ALFa(i,1)
      IF ( alb.LE..05 ) z1 = z1*alb/.05
      IF ( alt.LE..05 ) z2 = z2*alt/.05
      ALFad(i-1) = (z2*alb+z1*alt)/(z1+z2)
      PP1 = (alt-alb)/(z1+z2)
      IF ( HNIv(i).GT.(XL(i)/2.) ) THEN
         ALNiv = ALFad(i-1) + PP1*XL(i)/2. + PP2*(HNIv(i)-XL(i)/2.)
      ELSE
         ALNiv = ALFad(i-1) + PP1*HNIv(i)
      ENDIF
      IF ( ALNiv.GT..9999 ) ALNiv = .9999
      al = ALNiv
      IF ( ALNiv.LT.0. ) ALNiv = 0.D+00
      gg = 1.D+6
      IF ( KGLiss.EQ.4 ) CALL F88345(gg,al,al,vg,vf,tempe,x1,x2,x3,x4,  &
     &                               vgj,x5,x6,ix,x7,x8,iy)
      fluxg = 0.D+00
      IF ( al.LE..99999 ) fluxg = sec*vgj*al/((1.-al)*vg)
      vg = VVGs
      vf = VVFs
      IF ( IST(i,2).EQ.1 ) vf = VV(i,2)
      IF ( IST(i,2).EQ.3 ) vg = VV(i,2)
      tempe = T(i,2)
      al = ALFa(i,2)
      IF ( KGLiss.EQ.4 ) CALL F88345(gg,al,al,vg,vf,tempe,x1,x2,x3,x4,  &
     &                               vgj,x5,x6,ix,x7,x8,iy)
      fluxf = ((1.-al)*fluxg*vg/al-vgj*sec)/vf
      DEBi(i) = fluxg + fluxf
      DEBhi(i) = fluxg*HG(i,1) + fluxf*HF(i,2)
      CONTINUE
      END
!*==S00014.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00014
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA88 / FLUp(22,2) , FLUv(22,2) , FLUl(22,2) , FLUiv(22,2)&
     &                , DEBil(22,2) , FLUil(22,2) , DEBi(22) , DEBhi(22)&
     &                , DEBiv(22,2)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      COMMON /AAA16 / DBEnt(20) , TENt(20) , HENt(20) , DEBe , ENTe ,   &
     &                DBSor(20) , TSOr(20) , DEBs , DPDtt(20) ,         &
     &                TDPdt(20) , HSOr(20) , THSor(20) , ENTs , NDBent ,&
     &                NDPdt , NDBsor , NHSor
      DOUBLE PRECISION M , num , d(22) , e(22) , f(22)
      eps = 1.D-8
      DO i = 2 , NC1
         A(i) = 0.D+00
         B(i) = 0.D+00
         C(i) = 0.D+00
         d(i) = 0.D+00
         e(i) = 0.D+00
         f(i) = 0.D+00
      ENDDO
      IF ( KDEb.EQ.0 ) THEN
         DEB(1) = DEBe
      ELSE
         ret = 15.D+00
         d1 = DEB(1)
         dmcdt = DEB(1) - DEB(NC1)
         d2 = DEBe - SDSc*dmcdt
         DEB(1) = d1 + (DT/ret)*(d2-d1)
      ENDIF
      IF ( KPRess.NE.1 ) DEB(NC1) = DEBs
      DO i = 2 , NC1
         j = i - 1
         idoduc = ITYp(i)
         IF ( idoduc.EQ.2 ) THEN
            xsli = DELh(i,1) + DVVh(i,1)*HE(i,1)
            xslj = DELh(i,1) + DVVh(i,1)*HE(j,1)
            xsvi = DELh(i,2) + DVVh(i,2)*HE(i,2)
            xsvj = DELh(i,2) + DVVh(i,2)*HE(j,2)
            xsifs1 = DELh(i,1) + DVVh(i,1)*HFS
            xsifs2 = DELh(i,2) + DVVh(i,2)*HFS
            xsigs1 = DELh(i,1) + DVVh(i,1)*HGS
            xsigs2 = DELh(i,2) + DVVh(i,2)*HGS
            xsifi = DELh(i,1) + DVVh(i,1)*HF(i,1)
            xsigi = DELh(i,2) + DVVh(i,2)*HG(i,2)
            A(i) = XB(i)*xsli - XB(j)*xslj + DVVh(i,1)                  &
     &             *(FLUl(i,1)+FLUil(i,1)) + XB(j)*xsvj - XB(i)         &
     &             *xsvi + DVVh(i,2)*(FLUv(i,2)+FLUiv(i,2)) + DEBil(i,1)&
     &             *(xsigs2-xsifi) + DEBiv(i,2)*(xsifs1-xsigi)          &
     &             + DEBiv(i,1)*(xsigs2-xsigs1) + DEBil(i,2)            &
     &             *(xsifs1-xsifs2)
            B(i) = (1.-XA(j))*xslj + XA(j)*xsvj
            C(i) = -(1.-XA(i))*xsli - XA(i)*xsvi
            d(i) = BB(i,1) + BB(i,2)
            e(i) = -A(i)
         ELSEIF ( idoduc.EQ.3 ) THEN
            A(i) = (DELh(i,2)-DELh(i,1))*DEBi(i) + DVVh(i,1)            &
     &             *(HB(j)-DEBhi(i)+FLUp(i,1)) + DVVh(i,2)              &
     &             *(-HB(i)+DEBhi(i)+FLUp(i,2))
            B(i) = DELh(i,1) + DVVh(i,1)*HA(j)
            C(i) = -(DELh(i,2)+DVVh(i,2)*HA(i))
            d(i) = BB(i,1) + BB(i,2)
            e(i) = -A(i)
         ELSE
            A(i) = DVVh(i,1)*(HB(j)-HB(i)+FLUp(i,1))
            B(i) = DELh(i,1) + DVVh(i,1)*HA(j)
            C(i) = -(DELh(i,1)+DVVh(i,1)*HA(i))
            d(i) = BB(i,1)
            e(i) = -A(i)
         ENDIF
      ENDDO
      IF ( KPRess.EQ.1 ) THEN
         taft = TEM + 1.D-4
         CALL S00934(TDPdt,DPDtt,DPDtt,taft,paft,x,13)
         DPDt = (paft-P)/1.D-4
         DO i = 2 , NC1
            DEB(i) = (-d(i)*DPDt-A(i)-B(i)*DEB(i-1))/C(i)
         ENDDO
      ELSE
         e(2) = e(2) - B(2)*DEB(1)
         e(NC1) = e(NC1) - C(NC1)*DEBs
         DEB(NC1) = DEBs
         f(2) = 1.D+00
         num = 0.D+00
         denom = 0.D+00
         DO i = 2 , NC
            num = num + f(i)*e(i)
            denom = denom + f(i)*d(i)
            f(i+1) = -f(i)*C(i)/B(i+1)
         ENDDO
         num = num + f(NC1)*e(NC1)
         denom = denom + f(NC1)*d(NC1)
         DPDt = num/denom
         DEB(2) = (e(2)-d(2)*DPDt)/C(2)
         DO i = 3 , NC
            DEB(i) = (e(i)-B(i)*DEB(i-1)-d(i)*DPDt)/C(i)
         ENDDO
      ENDIF
      DO i = 1 , NC1
         DEBv(i) = XA(i)*DEB(i) + XB(i)
         DEBl(i) = DEB(i) - DEBv(i)
         XD(i) = 0.5
         XEM(i) = 0.5
         HEM(i) = 0.5
         IF ( DABS(DEB(i)).GE.eps ) THEN
            XD(i) = DEBv(i)/DEB(i)
            HEM(i) = (HE(i,1)*DEBl(i)+HE(i,2)*DEBv(i))/DEB(i)
            XEM(i) = (HEM(i)-HFS)/HFGs
         ENDIF
      ENDDO
      DEBs = DEB(NC1)
      CONTINUE
      END
!*==S00017.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00017
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA88 / FLUp(22,2) , FLUv(22,2) , FLUl(22,2) , FLUiv(22,2)&
     &                , DEBil(22,2) , FLUil(22,2) , DEBi(22) , DEBhi(22)&
     &                , DEBiv(22,2)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      COMMON /AAA33 / VCO , XL0055 , D876 , DINt , DEXt , VOL002 ,      &
     &                VOL005 , LCO , NCRay
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA17 / DDPdt , DDH(22,2) , DDV(22,2) , DDM(22,2) ,       &
     &                DDU(22,2)
      COMMON /AAA18 / DDEb(21) , DDDeb(21) , DXAt(21) , DXAg(21) ,      &
     &                DXBt(21) , DXBg(21) , DNUg(21) , DNUf(21) ,       &
     &                DHE(22,2) , DDEbo(21)
      COMMON /AAA20 / DQP(22,2) , DFLul(22) , DFLuv(22) , DFLuil(22) ,  &
     &                DFLuiv(22) , DDEbil(22) , DDEbiv(22) , DQCei(2) , &
     &                DQCed(22,12) , DDTuo(22,12) , DDTga(22,12) ,      &
     &                DDTmi(2) , DDTmu(2)
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      COMMON /AAA16 / DBEnt(20) , TENt(20) , HENt(20) , DEBe , ENTe ,   &
     &                DBSor(20) , TSOr(20) , DEBs , DPDtt(20) ,         &
     &                TDPdt(20) , HSOr(20) , THSor(20) , ENTs , NDBent ,&
     &                NDPdt , NDBsor , NHSor
      DOUBLE PRECISION M , num , dalfa(22,2) , dtf(22,2)
      DOUBLE PRECISION r(22) , ta(22) , w(22) , dal(12) , xx(8) , s(22)
      DOUBLE PRECISION dml(21) , dmv(21)
      DOUBLE PRECISION ya(5) , tta(5) , cr(5)
      DOUBLE PRECISION e(22) , f(22) , al(12)
      DOUBLE PRECISION dalbb(21) , daltt(21)
      DOUBLE PRECISION dtvid(22,12) , dfluve(22) , dfluvs(22)
      coef = 144.D+00/778.D+00
      eps = 1.D-6
      DTSat = DTSat*DPDt
      tess = TEM + 1.D-4
      CALL S00934(TENt,DBEnt,HENt,tess,x1,x2,20)
      dqinj = (x1-DEB(1))/1.D-4
      DH(1,1) = (x2-H(1,1))/1.D-4
      CALL S00934(THSor,HSOr,HSOr,tess,entss,x,2)
      DH(NC2,1) = (entss-H(NC2,1))/1.D-4
      dhfs = DHFps*DPDt
      dhgs = DHGps*DPDt
      dnufs = DVVfps*DPDt
      dnugs = DVVgps*DPDt
      DO i = 1 , NC1
         ie = i
         is = i + 1
         dvfb = dnufs
         dvft = dnufs
         dvgb = dnugs
         dvgt = dnugs
         dhfb = dhfs
         dhft = dhfs
         dhgb = dhgs
         dhgt = dhgs
         DDEbo(i) = DDEb(i)
         idoduc = ITYp(ie)
         IF ( idoduc.EQ.2 ) THEN
            dhfb = DH(ie,1)
            dhgb = DH(ie,2)
            dvfb = DNU(ie,1)
            dvgb = DNU(ie,2)
         ELSEIF ( idoduc.EQ.3 ) THEN
            ica = IST(ie,2)
            IF ( ica.EQ.1 ) THEN
               dhfb = DH(ie,2)
               dvfb = DNU(ie,2)
            ELSEIF ( ica.EQ.3 ) THEN
               dhgb = DH(ie,2)
               dvgb = DNU(ie,2)
            ENDIF
         ELSE
            ica = IST(ie,1)
            IF ( ica.EQ.1 ) THEN
               dhfb = DH(ie,1)
               dvfb = DNU(ie,1)
            ELSEIF ( ica.EQ.3 ) THEN
               dhgb = DH(ie,1)
               dvgb = DNU(ie,1)
            ENDIF
         ENDIF
         IF ( ITYp(is).EQ.2 ) THEN
            dhft = DH(is,1)
            dhgt = DH(is,2)
            dvft = DNU(is,1)
            dvgt = DNU(is,2)
         ELSE
            ica = IST(is,1)
            IF ( ica.EQ.1 ) THEN
               dhft = DH(is,1)
               dvft = DNU(is,1)
            ELSEIF ( ica.EQ.3 ) THEN
               dhgt = DH(is,1)
               dvgt = DNU(is,1)
            ENDIF
         ENDIF
         DNUg(i) = (dvgb+dvgt)/2.
         DNUf(i) = (dvfb+dvft)/2.
         DHE(i,1) = dhfb
         DHE(i,2) = dhgb
         debll = DEBav(i)*(1.-XA(i)) - XB(i)
         debvv = DEBav(i)*XA(i) + XB(i)
         IF ( debll.LT.0. ) DHE(i,1) = dhft
         IF ( debvv.LT.0. ) DHE(i,2) = dhgt
      ENDDO
      vfdvg = VVFs/VVGs
      dvfdvg = (DVVfps-vfdvg*DVVgps)*DPDt
      dalfa(1,1) = 0.
      dalfa(NC2,1) = 0.
      DO i = 2 , NC1
         j1 = 2 - 1/ITYp(i)
         DO j = 1 , j1
            dtf(i,j) = DTDp(i,j)*DPDt + DTDh(i,j)*DH(i,j)
            dalfa(i,j) = 0.
            IF ( IST(i,j).EQ.2 ) THEN
               x = (H(i,j)-HFS)/HFGs
               dxdt = (DH(i,j)-(DHFps+DHFgs*x)*DPDt)/HFGs
               aldx = 1./(x+(1.-x)*vfdvg)
               aldx = aldx*aldx
               dalfa(i,j) = aldx*(vfdvg*dxdt-(1.-x)*x*dvfdvg/VVGs)
            ENDIF
         ENDDO
      ENDDO
      dalbb(1) = 0.
      daltt(NC1) = 0.
      DO i = 2 , NC1
         ni = NASl(i)
         DO j = 1 , ni
            al(j) = TVId(i,j)
         ENDDO
         CALL S33018(i,dalfa,dal,daljb,daljt)
         daltt(i-1) = daljb
         dalbb(i) = daljt
         DO j = 1 , ni
            dtvid(i,j) = dal(j)
         ENDDO
      ENDDO
      DO i = 1 , NC1
         dalb = dalbb(i)
         dalt = daltt(i)
         sec = SC(i)
         ik = ICZw(i)
         IF ( KGLiss.EQ.4 ) CALL S33022(i,sec,ik,dalb,dalt,x1,x2,x3,x4)
         DXAt(i) = x1
         DXAg(i) = x2
         DXBt(i) = x3
         DXBg(i) = x4
      ENDDO
      DO i = 2 , NC1
         ni = NASl(i)
         DO j = 1 , ni
            al(j) = TVId(i,j)
            dal(j) = dtvid(i,j)
         ENDDO
         CALL S00089(i,al,dal,dtf,xx)
         IF ( ITYp(i).EQ.2 ) THEN
            DFLul(i) = xx(1)
            DFLuv(i) = xx(2)
            DFLuil(i) = xx(3)
            DFLuiv(i) = xx(4)
            DDEbil(i) = xx(5)
            DDEbiv(i) = xx(6)
            dfluve(i) = xx(7)
            dfluvs(i) = xx(8)
         ELSE
            DQP(i,1) = xx(1)
            DQP(i,2) = xx(2)
         ENDIF
      ENDDO
      DO i = 2 , NC1
         ie = i - 1
         is = i
         IF ( ITYp(i).LT.2 ) THEN
            hlve = HE(ie,2) - HE(ie,1)
            hlvs = HE(is,2) - HE(is,1)
            vh = DVVh(i,1)
            dvp = DVVp(i,1)*DPDt
            dvvpp = DNUpp(i,1)*DPDt
            dvvhh = DNUhp(i,1)*DH(i,1)
            dvvhp = DNUhp(i,1)*DPDt
            dadt = vh*(DQP(i,1)+hlve*DXBt(ie)-hlvs*DXBt(is)             &
     &             +(DHE(ie,2)-DHE(ie,1))*XB(ie)-(DHE(is,2)-DHE(is,1))  &
     &             *XB(is)) + dvvhp*(FLUp(i,1)+HB(ie)-HB(is))
            dadme = vh*hlve*DXBg(ie)
            dadms = -vh*hlvs*DXBg(is)
            dbdt = vh*(hlve*DXAt(ie)+XA(ie)*DHE(ie,2)+(1.-XA(ie))       &
     &             *DHE(ie,1)) + dvp + (HA(ie)-H(i,1))*dvvhp
            dbdme = vh*hlve*DXAg(ie)
            dcdt = -vh*(hlvs*DXAt(is)+XA(is)*DHE(is,2)+(1.-XA(is))      &
     &             *DHE(is,1)) - dvp - (HA(is)-H(i,1))*dvvhp
            dcdms = -vh*hlvs*DXAg(is)
            w(i) = BB(i,1)
            r(i) = dvp*DM(i,1) + DPDt*(M(i,1)*(dvvpp+dvvhh)+coef*V(i,1) &
     &             *dvvhp)
         ELSEIF ( ITYp(i).GE.3 ) THEN
            vp1 = DVVp(i,1)*DPDt
            vp2 = DVVp(i,2)*DPDt
            vh1 = DVVh(i,1)
            vh2 = DVVh(i,2)
            dvvpp1 = DNUpp(i,1)*DPDt
            dvvhh1 = DNUhp(i,1)*DH(i,1)
            dvvhp1 = DNUhp(i,1)*DPDt
            dvvpp2 = DNUpp(i,2)*DPDt
            dvvhh2 = DNUhp(i,2)*DH(i,2)
            dvvhp2 = DNUhp(i,2)*DPDt
            hlve = HE(ie,2) - HE(ie,1)
            hlvs = HE(is,2) - HE(is,1)
            all = ALFa(i,1)
            ddebi = 0.
            ddebhi = 0.
            IF ( all.GT.1.D-6 ) ddebi = DEBi(i)/(all*(1.-all))          &
     &                                  *dalfa(i,1)
            ddebi = ddebi - DEBi(i)*DVVgps*DPDt/VVGs
            ddebhi = ddebi*HGS + DEBi(i)*DHGps*DPDt
            dadt = vh1*(DQP(i,1)+hlve*DXBt(ie)+XB(ie)                   &
     &             *(DHE(ie,2)-DHE(ie,1)))                              &
     &             + vh2*(DQP(i,2)-hlvs*DXBt(is)-XB(is)                 &
     &             *(DHE(is,2)-DHE(is,1))) + DEBi(i)                    &
     &             *(vp2-vp1-H(i,2)*dvvhp2+H(i,1)*dvvhp1)               &
     &             + ddebi*(DELh(i,2)-DELh(i,1)) + ddebhi*(vh2-vh1)     &
     &             + dvvhp1*(HB(ie)-DEBhi(i)+FLUp(i,1))                 &
     &             + dvvhp2*(DEBhi(i)-HB(is)+FLUp(i,2))
            dadme = vh1*hlve*DXBg(ie)
            dadms = -vh2*hlvs*DXBg(is)
            dbdt = vh1*(hlve*DXAt(ie)+XA(ie)*DHE(ie,2)+(1.-XA(ie))      &
     &             *DHE(ie,1)) + vp1 + (HA(ie)-H(i,1))*dvvhp1
            dbdme = vh1*hlve*DXAg(ie)
            dcdt = -vh2*(hlvs*DXAt(is)+XA(is)*DHE(is,2)+(1.-XA(is))     &
     &             *DHE(is,1)) - vp2 - (HA(is)-H(i,2))*dvvhp2
            dcdms = -vh2*hlvs*DXAg(is)
            w(i) = BB(i,1) + BB(i,2)
            r(i) = DM(i,1)*vp1 + DM(i,2)                                &
     &             *vp2 + DPDt*(coef*(DV(i,1)*(vh1-vh2)+V(i,1)          &
     &             *dvvhp1+V(i,2)*dvvhp2)+M(i,1)*(dvvpp1+dvvhh1)+M(i,2) &
     &             *(dvvpp2+dvvhh2))
         ELSE
            dflvme = dfluve(i)
            dflvms = dfluvs(i)
            vh1 = DVVh(i,1)
            vh2 = DVVh(i,2)
            vp1 = DVVp(i,1)*DPDt
            vp2 = DVVp(i,2)*DPDt
            yne1 = DELh(i,1) + vh1*HE(ie,1)
            yne2 = DELh(i,2) + vh2*HE(ie,2)
            yns1 = DELh(i,1) + vh1*HE(is,1)
            yns2 = DELh(i,2) + vh2*HE(is,2)
            dns = yns1 - yns2
            dne = yne1 - yne2
            yl1 = DELh(i,1) + vh1*H(i,1)
            yl2 = DELh(i,2) + vh2*H(i,2)
            ga1 = DELh(i,1) + vh1*HFS
            ga2 = DELh(i,2) + vh2*HGS
            ddns = vp1 - vp2 + vh1*DHE(is,1) - vh2*DHE(is,2)
            ddne = vp1 - vp2 + vh1*DHE(ie,1) - vh2*DHE(ie,2)
            dga2 = vp2 + vh2*DHGps*DPDt - DNU(i,1)
            dga1 = vp1 + vh1*DHFps*DPDt - DNU(i,2)
            dadt = dns*DXBt(is) - dne*DXBt(ie)                          &
     &             + vh1*(DFLul(i)+DFLuil(i)) + vh2*(DFLuv(i)+DFLuiv(i))&
     &             + DDEbil(i)*(-yl1+ga2) + DDEbiv(i)*(ga1-yl2) + XB(is)&
     &             *ddns - XB(ie)*ddne + DEBil(i,1)*dga2 + DEBiv(i,2)   &
     &             *dga1
            dadme = -dne*DXBg(ie) + vh2*dflvme
            dadms = dns*DXBg(is) + vh2*dflvms
            dbdt = -dne*DXAt(ie) + XA(ie)*(vp2+vh2*DHE(ie,2))           &
     &             + (1.-XA(ie))*(vp1+vh1*DHE(ie,1))
            dbdme = -dne*DXAg(ie)
            dcdt = dns*DXAt(is) - XA(is)*(vp2+vh2*DHE(is,2))            &
     &             - (1.-XA(is))*(vp1+vh1*DHE(is,1))
            dcdms = dns*DXAg(is)
            w(i) = BB(i,1) + BB(i,2)
            r(i) = DPDt*(DM(i,1)*DVVp(i,1)+DM(i,2)*DVVp(i,2)            &
     &             +coef*DV(i,1)*(vh1-vh2))
         ENDIF
         r(i) = r(i) + dadt + DEB(ie)*dbdt + DEB(is)*dcdt
         s(i) = dadme + DEB(ie)*dbdme + B(i)
         ta(i) = dadms + DEB(is)*dcdms + C(i)
         e(i) = -r(i)
      ENDDO
      IF ( KDEb.EQ.0 ) THEN
         DDEb(1) = dqinj
      ELSE
         ret = 1./15.
         DDEb(1) = DDEb(1)*(1.-ret*(1.+SDSc))                           &
     &             + ret*(SDSc*DDEb(NC1)+dqinj)
      ENDIF
      IF ( KPRess.EQ.0 ) THEN
         tess = TEM + 1.D-4
         CALL S00934(TSOr,DBSor,DBSor,tess,x1,x2,NDBsor)
         DDEb(NC1) = (x1-DEB(NC1))/1.D-4
         e(2) = e(2) - s(2)*DDEb(1)
         e(NC1) = e(NC1) - ta(NC1)*DDEb(NC1)
         f(2) = 1.
         num = 0.
         denom = 0.
         DO i = 2 , NC
            num = num + f(i)*e(i)
            denom = denom + f(i)*w(i)
            f(i+1) = -f(i)*ta(i)/s(i+1)
         ENDDO
         num = num + f(NC1)*e(NC1)
         denom = denom + f(NC1)*w(NC1)
         DDPdt = num/denom
         DDEb(2) = (e(2)-w(2)*DDPdt)/ta(2)
         DO i = 3 , NC
            DDEb(i) = (e(i)-s(i)*DDEb(i-1)-w(i)*DDPdt)/ta(i)
         ENDDO
      ELSE
         DDPdt = 0.
         DO i = 2 , NC1
            DDEb(i) = (-r(i)-w(i)*DDPdt-s(i)*DDEb(i-1))/ta(i)
         ENDDO
      ENDIF
      DO i = 1 , NC1
         dxmm = DDEb(i)
         xmm = DEB(i)
         xaa = XA(i)
         dxadm = DXAg(i)
         dxadt = DXAt(i)
         dxbdm = DXBg(i)
         dxbdt = DXBt(i)
         dml(i) = dxmm*(1.-xaa-xmm*dxadm-dxbdm) - xmm*dxadt - dxbdt
         dmv(i) = dxmm*(xaa+xmm*dxadm+dxbdm) + xmm*dxadt + dxbdt
         qsec = (dxmm-DDEbo(i))/DT
         y = DABS(dxmm)/(20.*DT)
         doduc = .01
         y = DMAX1(y,doduc)
         yy = DABS(qsec)
         IF ( yy.GE.y ) qsec = qsec*y/yy
         DDDeb(i) = qsec
      ENDDO
      DDDeb(1) = 0.
      DO i = 2 , NC1
         ie = i - 1
         is = i
         IF ( ITYp(i).LT.2 ) THEN
            DDV(i,2) = 0.
            DDH(i,2) = 0.
            DDM(i,2) = 0.
            DDU(i,2) = 0.
            DDM(i,1) = DDEb(ie) - DDEb(is)
            DDU(i,1) = dml(ie)*HE(ie,1) + dmv(ie)*HE(ie,2) - dml(is)    &
     &                 *HE(is,1) - dmv(is)*HE(is,2) + DEBv(ie)*DHE(ie,2)&
     &                 + DQP(i,1) + DEBl(ie)*DHE(ie,1) - DEBv(is)       &
     &                 *DHE(is,2) - DEBl(is)*DHE(is,1)
         ELSEIF ( ITYp(i).GE.3 ) THEN
            DDM(i,1) = DDEb(ie) - ddebi
            DDM(i,2) = ddebi - DDEb(is)
            DDU(i,1) = dml(ie)*HE(ie,1) + dmv(ie)*HE(ie,2) - ddebhi +   &
     &                 DQP(i,1) + DEBl(ie)*DHE(ie,1) + DEBv(ie)         &
     &                 *DHE(ie,2)
            DDU(i,2) = ddebhi - dml(is)*HE(is,1) - dmv(is)*HE(is,2)     &
     &                 - DEBl(is)*DHE(is,1) - DEBv(is)*DHE(is,2)        &
     &                 + DQP(i,2)
         ELSE
            DFLuv(i) = DFLuv(i) + dfluve(i)*DDEb(ie) + dfluvs(i)        &
     &                 *DDEb(is)
            DDM(i,1) = dml(ie) - dml(is) - DDEbil(i) + DDEbiv(i)
            DDM(i,2) = dmv(ie) - dmv(is) + DDEbil(i) - DDEbiv(i)
            DDU(i,1) = dml(ie)*HE(ie,1) - dml(is)*HE(is,1) + DFLul(i)   &
     &                 + DFLuil(i) + DEBl(ie)*DHE(ie,1) - DEBl(is)      &
     &                 *DHE(is,1) - DEBil(i,1)*DH(i,1) + DEBiv(i,2)     &
     &                 *DHFps*DPDt - H(i,1)*DDEbil(i) + HFS*DDEbiv(i)
            DDU(i,2) = dmv(ie)*HE(ie,2) - dmv(is)*HE(is,2) + DFLuv(i)   &
     &                 + DFLuiv(i) + DEBv(ie)*DHE(ie,2) - DEBv(is)      &
     &                 *DHE(is,2) + DEBil(i,1)*DHGps*DPDt - DEBiv(i,2)  &
     &                 *DH(i,2) + HGS*DDEbil(i) - H(i,2)*DDEbiv(i)
         ENDIF
         j1 = 2 - 1/ITYp(i)
         epsm = DELm*VC(i)/VVFs
         DO j = 1 , j1
            daj = DVVh(i,j)*(DDU(i,j)-H(i,j)*DDM(i,j)) + VV(i,j)        &
     &            *DDM(i,j)                                             &
     &            + (DVVp(i,j)*DM(i,j)+DNUhp(i,j)*(DUU(i,j)-H(i,j)      &
     &            *DM(i,j)))*DPDt
            dbj = DM(i,j)*DVVp(i,j) + M(i,j)                            &
     &            *(DNUpp(i,j)*DPDt+DNUhp(i,j)*DH(i,j))                 &
     &            + coef*(DVVh(i,j)*DV(i,j)+V(i,j)*DNUhp(i,j)*DPDt)
            DDV(i,j) = daj + dbj*DPDt + BB(i,j)*DDPdt
            ym = M(i,j)
            IF ( ym.GE.epsm ) THEN
               DDH(i,j) = (coef*(DV(i,j)*DPDt+V(i,j)*DDPdt)+DDU(i,j)    &
     &                    -H(i,j)*DDM(i,j)-2.*DH(i,j)*DM(i,j))/ym
            ELSE
               ym = epsm
               dym = 0.
               DDH(i,j) = (coef*(DV(i,j)*DPDt+V(i,j)*DDPdt)+DDU(i,j)    &
     &                    -H(i,j)*DDM(i,j)-DH(i,j)*(DM(i,j)+dym))/ym
            ENDIF
            IF ( DABS(DDH(i,j)).LT.1.D-4 ) DDH(i,j) = 0.
            IF ( DABS(DDV(i,j)).LT.1.D-4 ) DDV(i,j) = 0.
         ENDDO
      ENDDO
      ttem = TEM + 1.D-4
      CALL S00934(TPUi,QPUi,QPUi,ttem,RPUi,xx(1),6)
      qqtot = QINit*RPUi
      dqtdt = (qqtot-QTOtal)/1.D-4
      DO i = 2 , NC1
         n1 = NASl(i)
         ccc = HCG(i)*NCRay*XL(i)/n1
         DO j = 1 , n1
            dqfou = QREpa(i,j)*dqtdt
            dquga = (DT876(i,j)-DTGai(i,j))*ccc
            DDTuo(i,j) = (dqfou-dquga)/XMCuo(i,j)
            DDTga(i,j) = (dquga-DQCed(i,j))/XMCga(i,j)
         ENDDO
      ENDDO
      IF ( ICAt(2).LE.2 ) GOTO 1200
      i = NIV
      j = ISLni
      n1 = NASl(i)
      dqfou = QREpa(i,j)*dqtdt
      coeff = XLGai*sec*XL(i)/n1
      ccc = HCG(i)*NCRay*XL(i)/n1
      dri = n1*DV(i,1)/VC(i)
      IF ( i.EQ.2 .AND. j.EQ.1 ) THEN
         ya(1) = RI(1)*XL(i)/(2.*n1)
         ya(2) = (1.-RI(2)/2.)*XL(i)/n1
         tta(1) = DTMi(1)
         tta(2) = DTMi(2)
         ii = i
         jj = ISLni + 1
         IF ( n1.LE.1 ) THEN
            ii = NIV + 1
            jj = 1
         ENDIF
         ya(3) = XL(i)/n1 + .5*XL(ii)/NASl(ii)
         tta(3) = DTGai(ii,jj)
         der1 = 2.*(tta(2)-tta(1))/((ya(2)-ya(1))*(ya(2)+ya(1)))
         CALL S00099(3,ya,tta,cr)
         der2 = 2.*cr(1)
      ELSEIF ( i.NE.NC1 .OR. j.NE.n1 ) THEN
         ii = i
         jj = ISLni - 1
         IF ( jj.LT.1 ) THEN
            ii = i - 1
            jj = NASl(ii)
         ENDIF
         ya(1) = -.5*XL(ii)/NASl(ii)
         tta(1) = DTGai(ii,jj)
         ya(2) = .5*RI(1)*XL(i)/n1
         tta(2) = DTMi(1)
         tta(3) = DTMi(2)
         ya(3) = (1.-.5*RI(2))*XL(i)/n1
         ii = i
         jj = ISLni + 1
         IF ( jj.GT.n1 ) THEN
            ii = i + 1
            jj = 1
         ENDIF
         ya(4) = XL(i)/n1 + .5*XL(ii)/NASl(ii)
         tta(4) = DTGai(ii,jj)
         CALL S00099(4,ya,tta,cr)
         der1 = 6.*cr(1)*ya(2) + 2*cr(2)
         der2 = 6.*cr(1)*ya(3) + 2*cr(2)
      ELSE
         ya(1) = RI(2)*XL(i)/(2.*n1)
         ya(2) = (1.-RI(1)/2.)*XL(i)/n1
         tta(1) = DTMi(2)
         tta(2) = DTMi(1)
         ii = i
         jj = ISLni - 1
         IF ( n1.LE.1 ) THEN
            ii = i - 1
            jj = NASl(ii)
         ENDIF
         ya(3) = XL(i)/n1 + .5*XL(ii)/NASl(ii)
         tta(3) = DTGai(ii,jj)
         der2 = 2.*(tta(2)-tta(1))/((ya(2)-ya(1))*(ya(2)+ya(1)))
         CALL S00099(3,ya,tta,cr)
         der1 = 2.*cr(1)
      ENDIF
      dquce1 = HLCf*(TMU(1)-TMI(1))*XHTc(i,j)
      dquce2 = HVCff(i)*(TMU(2)-TMI(2))*XHTc(i,j)
      IF ( RI(1).GE.eps ) dquce1 = (DQCei(1)-QCEi(1)*dri/RI(1))/RI(1)
      IF ( RI(2).GE.eps ) dquce2 = (DQCei(2)+QCEi(2)*dri/RI(2))/RI(2)
      dqugi1 = ccc*(DTMu(1)-DTMi(1))
      dqugi2 = ccc*(DTMu(2)-DTMi(1))
      dqcon1 = coeff*der1
      dqcon2 = coeff*der2
      ymcga = XMCga(i,j)
      ymcuo = XMCuo(i,j)
      DDTmu(1) = (dqfou-dqugi1)/ymcuo
      DDTmu(2) = (dqfou-dqugi2)/ymcuo
      DDTmi(1) = (dqugi1+dqcon1-dquce1)/ymcga
      DDTmi(2) = (dqugi2+dqcon2-dquce2)/ymcga
      ylim = .05
      ys = XL(i)/n1
      y1 = RI(1)*ys
      y2 = RI(2)*ys
      dy1 = dri*ys
      dy2 = -dri*ys
      yz = DV(i,1)*(TMI(1)-TMI(2))/SCApa(i)
      dyz = (DDV(i,1)*(TMI(1)-TMI(2))+DV(i,1)*(DTMi(1)-DTMi(2)))        &
     &      /SCApa(i)
      IF ( DV(i,1).GE.0. ) THEN
         IF ( y1.LT.ylim ) THEN
            dterc = dyz/ylim
            IF ( RI(1).GE.1.D-3 ) DDTmi(1) = DDTmi(1) - dterc
            i1 = i
            j1 = ISLni - 1
            IF ( j1.LT.1 ) THEN
               i1 = i - 1
               j1 = NASl(i1)
            ENDIF
            IF ( i1.LT.IBCh .OR. i1.GT.IHCh ) GOTO 1200
            dterc = XMCga(i,j)*(dyz*(1.-y1/ylim)-yz*dy1/ylim)           &
     &              /(ys*XMCga(i1,j1))
            DDTga(i1,j1) = DDTga(i1,j1) - dterc
         ELSE
            DDTmi(1) = DDTmi(1) - (dyz-yz*dy1/y1)/y1
            GOTO 1200
         ENDIF
      ENDIF
      IF ( y2.LT.ylim ) THEN
         dterc = dyz/ylim
         IF ( RI(2).GE.1.D-3 ) DDTmi(2) = DDTmi(2) - dterc
         i1 = i
         j1 = ISLni + 1
         IF ( j1.GT.n1 ) THEN
            i1 = i + 1
            j1 = 1
         ENDIF
         IF ( i1.GE.IBCh .AND. i1.LE.IHCh ) THEN
            dterc = XMCga(i,j)*(dyz*(1.-y2/ylim)-yz*dy2/ylim)           &
     &              /(ys*XMCga(i1,j1))
            DDTga(i1,j1) = DDTga(i1,j1) - dterc
         ENDIF
      ELSE
         DDTmi(2) = DDTmi(2) - (dyz-yz*dy2/y2)/y2
      ENDIF
 1200 CONTINUE
      END
!*==S33018.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!*==b8b.for
      SUBROUTINE S33018(I,Dalf,Al,Daljb,Daljt)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      DOUBLE PRECISION M , Dalf(22,2) , ax(3) , ay(3) , Al(12)
      ni = NASl(I)
      alec = .01
      alin = alec
      alsu = 1. - alec
      xlm = XL(I)/2.
      dxlm = 0.
      IF ( ITYp(I).GT.2 ) THEN
         xlm = HNIv(I)/2.
         dxlm = DV(I,1)/SCApa(I)
      ENDIF
      ax(2) = ZCOt(I-1) + xlm
      alm = ALFa(I,1)
      dalm = Dalf(I,1)
      IF ( ITYp(I).EQ.2 ) THEN
         alm = V(I,2)/VC(I)
         dalm = DV(I,2)/VC(I)
      ENDIF
      der = dalm/alec
      fm = 0.
      dfm = 0.
      gm = 0.
      dgm = 0.
      hm = 1.
      dhm = 0.
      ay(2) = dalm
      IF ( alm.LT.alin ) THEN
         hm = alm/alec
         fm = 1. - hm
         dhm = der
         dfm = -der
      ELSEIF ( alm.GT.alsu ) THEN
         gm = (alm-alsu)/alec
         hm = 1. - gm
         dgm = der
         dhm = -der
      ENDIF
      j = I - 1
      ax(1) = ZCOt(I-1)
      idoduc = ITYp(j)
      IF ( idoduc.EQ.2 ) THEN
         xlb = -XL(j)/2.
         dxlb = 0.
         alb = V(j,2)/VC(j)
         dalb = DV(j,2)/VC(j)
      ELSEIF ( idoduc.EQ.3 ) THEN
         xlb = (HNIv(j)-XL(j))/2.
         dxlb = DV(j,1)/SCApa(j)
         alb = ALFa(j,2)
         dalb = Dalf(j,2)
      ELSE
         xlb = -XL(j)/2.
         IF ( j.EQ.1 ) xlb = -XL(I)/2.
         dalb = Dalf(j,1)
         alb = ALFa(j,1)
         dxlb = 0.
      ENDIF
      der = dalb/alec
      fb = 0.
      dfb = 0.
      gb = 0.
      dgb = 0.
      hb = 1.
      dhb = 0.
      IF ( alb.LT.alin ) THEN
         hb = alb/alec
         fb = 1. - hb
         dhb = der
         dfb = -der
      ELSEIF ( alb.GT.alsu ) THEN
         gb = (alb-alsu)/alec
         hb = 1. - gb
         dgb = der
         dhb = -der
      ENDIF
      xlhb = xlm - xlb
      aljo = (alb*xlm-alm*xlb)/xlhb
      tdd = hb + fm*fb + gm*gb
      dtdd = dhb + fm*dfb + dfm*fb + gm*dgb + dgm*gb
      tcc = hm*tdd
      dtcc = hm*dtdd + dhm*tdd
      daljo = (dalb*xlm-dalm*xlb+dxlm*(alb-aljo)-dxlb*(alm-aljo))/xlhb
      ay(1) = dalm + (daljo-dalm)*tcc + (aljo-alm)*dtcc
      Daljb = ay(1)
      p1 = (ay(2)-ay(1))/(ax(2)-ax(1))
      q1 = ay(1) - ax(1)*p1
      dax2 = 0.
      IF ( ITYp(I).GT.2 ) THEN
         ax(2) = ZCOt(I-1) + (XL(I)+HNIv(I))/2.
         alm = ALFa(I,2)
         dalm = Dalf(I,2)
         dax2 = DV(I,1)/SCApa(I)
         der = dalm/alec
         fm = 0.
         dfm = 0.
         gm = 0.
         dgm = 0.
         hm = 1.
         dhm = 0.
         IF ( alm.LT.alin ) THEN
            hm = alm/alec
            fm = 1. - hm
            dhm = der
            dfm = -der
         ELSEIF ( alm.GT.alsu ) THEN
            gm = (alm-alsu)/alec
            hm = 1. - gm
            dgm = der
            dhm = -der
         ENDIF
      ENDIF
      j = I + 1
      ax(3) = ZCOt(I-1) + XL(I)
      xlm = ax(2) - ax(3)
      dxlm = dax2
      idoduc = ITYp(j)
      IF ( idoduc.EQ.2 ) THEN
         xlh = XL(j)/2.
         dxlh = 0.
         alh = V(j,2)/VC(j)
         dalh = DV(j,2)/VC(j)
      ELSEIF ( idoduc.EQ.3 ) THEN
         xlh = HNIv(j)/2.
         dxlh = DV(j,1)/SCApa(j)
         alh = ALFa(j,1)
         dalh = Dalf(j,1)
      ELSE
         xlh = XL(j)/2.
         dxlh = 0.
         IF ( j.EQ.NC2 ) xlh = XL(I)/2.
         alh = ALFa(j,1)
         dalh = Dalf(j,1)
      ENDIF
      der = dalh/alec
      fh = 0.
      dfh = 0.
      gh = 0.
      dgh = 0.
      hh = 1.
      dhh = 0.
      IF ( alh.LT.alin ) THEN
         hh = alh/alec
         fh = 1. - hh
         dhh = der
         dfh = -der
      ELSEIF ( alh.GT.alsu ) THEN
         gh = (alh-alsu)/alec
         hh = 1. - gh
         dgh = der
         dhh = -der
      ENDIF
      xlhb = xlh - xlm
      aljo = (alm*xlh-alh*xlm)/xlhb
      daljo = (dalm*xlh-dalh*xlm+dxlh*(alm-aljo)-dxlm*(alh-aljo))/xlhb
      tdd = hh + fm*fh + gm*gh
      dtdd = dhh + fm*dfh + dfm*fh + gm*dgh + dgm*gh
      tcc = hm*tdd
      dtcc = hm*dtdd + dhm*tdd
      ay(3) = dalm + (daljo-dalm)*tcc + (aljo-alm)*dtcc
      Daljt = ay(3)
      p2 = (ay(3)-ay(2))/(ax(3)-ax(2))
      q2 = ay(2) - ax(2)*p2
      IF ( ICAt(2).LT.3 ) THEN
         Daljb = dalm
         Daljt = dalm
         DO j = 1 , ni
            xp = ZCOt(I-1) + (j-.5)*XL(I)/ni
            IF ( xp.GE.ax(2) ) THEN
               Al(j) = p2*xp + q2
            ELSE
               Al(j) = p1*xp + q1
            ENDIF
         ENDDO
         RETURN
      ELSEIF ( ITYp(I).GE.3 ) THEN
         Al(1) = Dalf(I,1)
         Al(2) = Dalf(I,2)
         RETURN
      ELSE
         DO j = 1 , ni
            Al(j) = dalm
         ENDDO
         RETURN
      ENDIF
      END
!*==S33019.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S33019(I,Sec)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      DOUBLE PRECISION M
      vf = VE(I,1)
      vg = VE(I,2)
      g = DEB(I)/Sec
      tjon = TJOnc(I)
      ichoix = KGLiss
      IF ( ichoix.EQ.2 ) THEN
         RETURN
      ELSEIF ( ichoix.EQ.3 ) THEN
      ELSEIF ( ichoix.EQ.4 ) THEN
         alb = ALFb(I)
         alt = ALFt(I)
         CALL F88345(g,alb,alt,vg,vf,tjon,x1,x2,x3,ALFad(I),x4,pe,cst,  &
     &               ic,gl1,gl2,ICZw(I))
      ELSE
         alf = ALFb(I)
         ic = 1
         IF ( g.LE.0. ) THEN
            alf = ALFt(I)
            ic = 3
         ENDIF
         xaa = alf*vf/(alf*vf+(1.-alf)*vg)
         xbb = 0.
         ALFad(I) = alf
         GLInf(I) = 0.
         GLSup(I) = 0.
         GOTO 100
      ENDIF
      GLInf(I) = gl2
      GLSup(I) = gl1
      VGJo(I) = x4
      xaa = pe
      xbb = cst*Sec
 100  ICAs(I) = ic
      XA(I) = xaa
      XB(I) = xbb
      dell = (1.-xaa)*DEB(I) - xbb
      devv = xaa*DEB(I) + xbb
      hel = HJOnt(I,1)
      hev = HJOnb(I,2)
      IF ( dell.GT.0. ) hel = HJOnb(I,1)
      IF ( devv.LT.0. ) hev = HJOnt(I,2)
      HE(I,1) = hel
      HE(I,2) = hev
      HA(I) = xaa*hev + (1.-xaa)*hel
      HB(I) = xbb*(hev-hel)
      CONTINUE
      END
!*==S33022.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S33022(I,Sec,Ik,Dalb,Dalt,X1,X2,X3,X4)
      IMPLICIT DOUBLE PRECISION(a-H,O-Z)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA18 / DDEb(21) , DDDeb(21) , DXAt(21) , DXAg(21) ,      &
     &                DXBt(21) , DXBg(21) , DNUg(21) , DNUf(21) ,       &
     &                DHE(22,2) , DDEbo(21)
      IF ( Ik.GE.1 ) THEN
         al = ALFad(I)
         eps = 1.D-7
         alsu = 1. - eps
         IF ( al.GE.eps .AND. al.LE.alsu ) THEN
            vf = VE(I,1)
            vg = VE(I,2)
            temp = TJOnc(I)
            sig = SIGMA(temp)
            rg = 1./vg
            rf = 1./vf
            tcc = sig*(rf-rg)
            tcc = vf*32.174*DSQRT(tcc)
            vbcr = 1.53*DSQRT(tcc)
            r = rg/rf
            dvf = DNUf(I)
            dvg = DNUg(I)
            drf = -dvf*rf*rf
            drg = -dvg*rg*rg
            rp = r*(dvf/vf-dvg/vg)
            alo = .925*(vf/vg)**.239
            a = .67
            IF ( al.GE.alo ) a = .47
            vgj = VGJo(I)
            alop = alo*rp*.239/r
            vbcrp = .25*vbcr*(dvf/vf-rp/(1.-r))
            alb = ALFb(I)
            alt = ALFt(I)
            tao = alop/(a*alo)
            tcr = vbcrp/vbcr
            tl1 = 0.
            tl2 = 0.
            IF ( alb.GT.eps ) tl1 = Dalb/(a*alb) - tao + drg/rg + tcr
            IF ( alt.GT.eps .AND. alt.LT.alsu )                         &
     &           tl2 = ((1./a-1.)/alt-1./(1.-alt))*Dalt + drf/rf - tao +&
     &           tcr
            GLInfp(I) = tl2*GLInf(I)
            V55198(I) = GLSup(I)*tl1
            tc = .25*dvf/vf - rp*(.25/(1.-r)+.239/(a*r))
            td = -1./(1.-al) + (1.-a)/(a*al)
            vgjt = vgj*tc
            vgja = vgj*td
            vgjaa = vgj*(1.-a)*(-2./(1.-al)+1./a)/(a*al*al)
            vgjat = vgj*tc*td
            IF ( Ik.EQ.3 ) THEN
               alst = (rg*alt+rf*alb)/(rg+rf)
               z1 = (1.-al)*(alt-al) + al*(al-alb)
               z2 = (rg+rf)*al*(alst-al)
               z1t = Dalt*(1.-al) - al*Dalb
               z1a = 4.*al - 1. - alb - alt
               z2t = al*(drg*(alt-al)+drf*(alb-al)+rg*Dalt+rf*Dalb)
               z2a = rg*alt + rf*alb - 2.*al*(rg+rf)
               g = DEBav(I)/Sec
               daldt = (z2*vgjt+vgj*z2t-g*z1t)/(g*z1a-z2*vgja-vgj*z2a)
               sd = z2a*vgj + z2*vgja - g*z1a
               s = z1/sd
               rm = al*rg + (1.-al)*rf
               t = al/rm
               u = rf*(al*vgja*rm+g+rf*vgj)/(rm*rm)
               z1aa = 4.
               z2aa = -2.*(rg+rf)
               ta = rf/(rm*rm)
               sda = z2aa*vgj + 2.*z2a*vgja + z2*vgjaa - g*z1aa
               sa = (z1a-s*sda)/sd
               ua = rf*(2.*vgja+al*vgjaa+2.*u*(1.-r))/rm
               dpal = rg*(ta+u*sa+ua*s)
               dpg = rg*s*(u*(z1a/sd+sa)+s*ua+ta+rf/(rm*rm))
               rmp = al*drg + (1.-al)*drf
               ut = (drf/rf-2.*rmp/rm)                                  &
     &              *u + rf*(al*(rmp*vgja+rm*vgjat)+drf*vgj+rf*vgjt)    &
     &              /(rm*rm)
               tt = -t*rmp/rm
               z1at = -Dalb - Dalt
               z2at = drg*(alt-2.*al) + drf*(alb-2.*al) + rg*Dalt +     &
     &                rf*Dalb
               sdt = z2at*vgj + z2a*vgjt + z2t*vgja + z2*vgjat - g*z1at
               st = (z1t-s*sdt)/sd
               dpt = dpal*daldt + drg*(t+u*s) + rg*(tt+u*st+ut*s)
               djgt = u*daldt +                                         &
     &                al*(vgj*(rm*drf-rf*rmp)+rm*rf*vgjt-g*rmp)/(rm*rm)
               dqg = -dpg*g
               dqt = drg*al*(g+rf*vgj)/rm + rg*djgt - g*dpt
            ELSE
               zz = 1. - al*(1.-r)
               dal = Dalb
               IF ( Ik.EQ.2 ) dal = Dalt
               IF ( Ik.EQ.4 ) dal = (Dalb+Dalt)/2.
               dpt = (rp*al*(1.-al)+r*dal)/(zz*zz)
               dpg = 0.
               vgjp = vgjt + vgja*dal
               dqt = (vgj*(dal/zz-al*(dvg/vg+rp*al/zz))+al*vgjp)/(vg*zz)
               dqg = 0.
            ENDIF
            X1 = dpt
            X2 = dpg/Sec
            X3 = Sec*dqt
            X4 = dqg
            RETURN
         ENDIF
      ENDIF
      X1 = 0.
      X2 = 0.
      X3 = 0.
      X4 = 0.
      CONTINUE
      END
!*==S44553.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S44553(I,J,K,Alff,Hcoef,Hray)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      DOUBLE PRECISION M
      DATA y1/0.D+00/ , y2/0.D0/
      hvcf = HVCff(I)
      IF ( ITYp(I).EQ.2 ) hvcf = HVCfo
      IF ( I.LT.NIV ) iopt = 2
      IF ( I.GT.NIV ) iopt = 0
      IF ( I.EQ.NIV .AND. J.EQ.1 ) iopt = 2
      IF ( I.EQ.NIV .AND. J.EQ.2 ) iopt = 0
      IF ( ICAt(2).LE.2 ) iopt = 3
      tmur = TGAi(I,K)
      IF ( I.EQ.NIV .AND. K.EQ.ISLni ) tmur = TMI(J)
      tflui = TS
      IF ( ITYp(I).NE.2 ) tflui = T(I,J)
      Hray = F00113(ICAt(2),P,DIAhy,tmur,tflui,Alff)
      xx = TS - tflui
      xts = .025 + 2.34/(2.4+xx*xx)
      IF ( xx.LT.0. ) xts = 1.
      xsi = 1.
      IF ( tflui.GT.tmur ) xsi = .1 + 1.8/(tflui-tmur+2.)
      kk = iopt + 1
      IF ( kk.EQ.2 ) THEN
         PRINT 99001
99001    FORMAT (' S44553/ECART/ERREUR ')
!         v66077 = hlcf*dabs(tmur-tflui)/ecart
         GOTO 100
      ELSEIF ( kk.EQ.3 ) THEN
         v66077 = HLCf
         GOTO 100
      ELSEIF ( kk.EQ.4 ) THEN
         hmil = 100./3600.
         zz1 = ZCOt(IBCh-1)
         zz2 = ZCOt(IHCh)
         zz = ZCOt(I-1) + (XL(I)*(J-.5))/NASl(I)
         xx = ((zz-zz1)/(zz2-zz1)) - .5
         tbord = 700.
         tmil = 900.
         aco = (tmil-tbord)*4.
         tmoy = tmil - aco*xx*xx
         tmin = tmoy - 100.
         tmax = tmoy + 100.
         ect = tmax - tmin
         z1 = (tmax-tmur)/ect
         z2 = (tmur-tmin)/ect
         IF ( tmur.LE.tmax ) THEN
            IF ( Alff.GT.1.D-4 ) THEN
               pt1 = -.1
               pt2 = 2.*(hvcf-hmil)/hvcf
               qt1 = 1.
               qt2 = 1. - pt2
               al1 = .625
               al2 = .95
               IF ( Alff.LE.al1 ) THEN
                  y1 = (pt1*Alff+qt1)*HLCf
               ELSEIF ( Alff.GT.al2 ) THEN
                  y1 = (pt2*Alff+qt2)*hvcf
               ELSE
                  xla = al2 - al1
                  x1 = pt1*al1 + qt1
                  x2 = pt2*al2 + qt2
                  h1 = x1*HLCf
                  h2 = x2*hvcf
                  u1 = -pt1/x1
                  u2 = -pt2/x2
                  p1 = (Alff-al2)*(Alff-al2)                            &
     &                 *((2.-u1*xla)*(Alff-al1)+xla)/(xla*xla*xla)
                  p2 = (Alff-al1)*(Alff-al1)                            &
     &                 *((2.+u2*xla)*(al2-Alff)+xla)/(xla*xla*xla)
                  y1 = (h1*p1+h2*p2)
               ENDIF
            ELSE
               y1 = HLCf
            ENDIF
            IF ( tmur.LE.tmin ) THEN
               z2 = 0.
               z1 = 1.
               v66077 = z1*y1 + z2*y2
               GOTO 100
            ENDIF
         ELSE
            z1 = 0.
            z2 = 1.
         ENDIF
      ELSE
         IF ( Alff.LE.0. ) THEN
            v66077 = HLCf
         ELSEIF ( Alff.LE..5 ) THEN
            x = 4.*DLOG(60.*HLCf)
            za = 240.*hvcf + x - 4.
            zb = -120.*hvcf - x + 2.
            v66077 = HLCf*DEXP((za*Alff+zb)*Alff)
         ELSEIF ( Alff.GT.1. ) THEN
            v66077 = hvcf
         ELSE
            v66077 = (2.*hvcf-1./30.)*Alff + 1./30. - hvcf
         ENDIF
         GOTO 100
      ENDIF
      IF ( Alff.GT..5 ) THEN
         y2 = 2.*(hvcf-hmil)*(Alff-1.) + hvcf
         y2 = 2.*(hvcf-hmil)*(Alff-1.) + hvcf
      ELSE
         beta = (hmil-hvcf)/(HLCf-hvcf)
         zb = (.5-beta)/(.5*(1.-beta))
         za = zb*zb/(1.-zb)
         zc = -1./(za+zb)
         y2 = 1./(za*Alff+zb) + zc
         y2 = y2*(HLCf-hvcf) + hvcf
      ENDIF
      v66077 = z1*y1 + z2*y2
 100  Hcoef = v66077
      IF ( ITYp(I).EQ.2 ) RETURN
      Hcoef = Hcoef*xsi*xts
      RETURN
      END
!*==F00111.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      DOUBLE PRECISION FUNCTION F00111(I)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      DOUBLE PRECISION M
      F00111 = H(I,1)
      IF ( ITYp(I).GE.2 ) F00111 = (H(I,1)*M(I,1)+H(I,2)*M(I,2))        &
     &                             /(M(I,1)+M(I,2))
      CONTINUE
      END
!*==S00022.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00022
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      DOUBLE PRECISION M
      IF ( ICAt(2).GT.2 ) THEN
         z1 = XL(NIV)/2.
         z2 = HNIv(NIV)/2.
         IF ( NIV.GT.2 ) z1 = XL(NIV-1)/2.
         hj = F00111(NIV-1)
         x = (hj-HFS)/(HGS-HFS)
         al3 = x/(x+(1.-x)*VVFs/VVGs)
         IF ( al3.LT..05 ) z1 = z1*al3/.05
         PP1 = (ALFa(NIV,1)-al3)/(z1+z2)
         IF ( PP1.LT.0. ) PP1 = 0.
         PP2 = PP1
      ENDIF
      CALL S00012
      DO i = 1 , NC1
         DEBav(i) = 0.
         XA(i) = 0.
         XB(i) = 0.
      ENDDO
      CALL S00013
      DO i = 1 , NC1
         idoduc = ITYp(i)
         IF ( idoduc.EQ.2 ) THEN
            HA(i) = F00111(i)
            XA(i) = (HA(i)-H(i,1))/(H(i,2)-H(i,1))
            HE(i,1) = H(i,1)
            HE(i,2) = H(i,2)
            ALFad(i) = (ALFa(i,1)*V(i,1)+ALFa(i,2)*V(i,2))/VC(i)
         ELSEIF ( idoduc.EQ.3 ) THEN
            HA(i) = H(i,2)
            XA(i) = (HA(i)-HF(i,2))/(HG(i,2)-HF(i,2))
            ALFad(i) = ALFa(i,2)
            HE(i,1) = HF(i,2)
            HE(i,2) = HG(i,2)
         ELSE
            HA(i) = H(i,1)
            XA(i) = (HA(i)-HF(i,1))/(HG(i,1)-HF(i,1))
            ALFad(i) = ALFa(i,1)
            HE(i,1) = HF(i,1)
            HE(i,2) = HG(i,1)
         ENDIF
         HB(i) = 0.
         XB(i) = 0.
      ENDDO
      CALL S00014
      CONTINUE
      END
!*==S33055.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S33055(Ii)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      DOUBLE PRECISION sto(12,12) , ag(12) , bg(12) , au(12) , bu(12) , &
     &                 x(12)
      DOUBLE PRECISION xn(12)
      IF ( NMUltr.LE.1 ) RETURN
      n1 = NSL(Ii)
      DO i = 1 , n1
         sto(i,1) = TGAi(Ii,i)
         sto(i,2) = TGAii(Ii,i)
         sto(i,3) = T876(Ii,i)
         sto(i,4) = TUMax(Ii,i)
         sto(i,5) = TUMin(Ii,i)
         sto(i,6) = XMCga(Ii,i)
         sto(i,7) = XMCuo(Ii,i)
         sto(i,8) = PL(Ii,i)
         sto(i,9) = QREpa(Ii,i)
         sto(i,10) = XHTc(Ii,i)
      ENDDO
      NASl(Ii) = NSL(Ii)*NMUltr
      IF ( Ii.GT.2 .AND. Ii.LT.NC1 ) THEN
         x1 = -XL(Ii-1)/NASl(Ii-1)/2.
         x2 = XL(Ii)/2.
         x3 = XL(Ii) + XL(Ii+1)/NASl(Ii+1)/2.
         yg1 = TGAi(Ii-1,NASl(Ii-1))
         yg3 = TGAi(Ii+1,1)
         yu1 = T876(Ii-1,NASl(Ii-1))
         yu3 = T876(Ii+1,1)
         n1 = NASl(Ii)
         tg2 = 0.
         tu2 = 0.
         n2 = NSL(Ii)
         DO j = 1 , n2
            tg2 = tg2 + TGAi(Ii,j)
            tu2 = tu2 + T876(Ii,j)
         ENDDO
         yg2 = tg2/n2
         yu2 = tu2/n2
         yx21 = (yg2-yg1)/(x2-x1)
         yx32 = (yg3-yg2)/(x3-x2)
         ga = (yx21-yx32)/(x1-x3)
         gb = (yg2-yg1-ga*(x2*x2-x1*x1))/(x2-x1)
         gc = yg1 - ga*x1*x1 - gb*x1
         yx21 = (yu2-yu1)/(x2-x1)
         yx32 = (yu3-yu2)/(x3-x2)
         ua = (yx21-yx32)/(x1-x3)
         ub = (yu2-yu1-ua*(x2*x2-x1*x1))/(x2-x1)
         uc = yu1 - ua*x1*x1 - ub*x1
         n2 = NSL(Ii)
         i2 = 0
         DO i = 1 , n2
            i1 = i2 + 1
            i2 = i*NMUltr
            DO j = i1 , i2
               y = XL(Ii)/n1*(j-0.5)
               TGAi(Ii,j) = ga*y*y + gb*y + gc
               T876(Ii,j) = ua*y*y + ub*y + uc
               TGAii(Ii,j) = sto(i,2)
               TUMax(Ii,j) = sto(i,4)
               TUMin(Ii,j) = sto(i,5)
               XMCga(Ii,j) = sto(i,6)/NMUltr
               XMCuo(Ii,j) = sto(i,7)/NMUltr
               PL(Ii,j) = sto(i,8)
               QREpa(Ii,j) = sto(i,9)/NMUltr
               XHTc(Ii,j) = sto(i,10)/NMUltr
            ENDDO
         ENDDO
         RETURN
      ELSE
         IF ( n1.EQ.1 ) THEN
            n2 = n1*NMUltr
            DO i = 2 , n2
               TGAi(Ii,i) = TGAi(Ii,1)
               T876(Ii,i) = T876(Ii,1)
            ENDDO
         ELSE
            x(1) = XL(Ii)/n1/2.
            DO i = 2 , n1
               x(i) = x(i-1) + XL(Ii)/n1
               bg(i) = (TGAi(Ii,i)-TGAi(Ii,i-1))/(x(i)-x(i-1))
               ag(i) = TGAi(Ii,i) - x(i)*bg(i)
               bu(i) = (T876(Ii,i)-T876(Ii,i-1))/(x(i)-x(i-1))
               au(i) = T876(Ii,i) - x(i)*bu(i)
            ENDDO
            x(1) = 0.
            x(n1) = XL(Ii)
            xn(1) = XL(Ii)/n1/2./NMUltr
            n2 = n1*NMUltr
            DO i = 2 , n2
               xn(i) = xn(i-1) + XL(Ii)/n1/NMUltr
            ENDDO
            i2 = 0
            DO i = 1 , n1
               i1 = i2 + 1
               i2 = i*NMUltr
               DO k = i1 , i2
                  ij = i
                  IF ( xn(k).GE.x(i) ) ij = i + 1
                  TGAi(Ii,k) = ag(ij) + bg(ij)*xn(k)
                  T876(Ii,k) = au(ij) + bu(ij)*xn(k)
               ENDDO
            ENDDO
         ENDIF
         i2 = 0
         DO i = 1 , n1
            i1 = i2 + 1
            i2 = i*NMUltr
            DO k = i1 , i2
               TGAii(Ii,k) = sto(i,2)
               TUMax(Ii,k) = sto(i,4)
               TUMin(Ii,k) = sto(i,5)
               XMCga(Ii,k) = sto(i,6)/NMUltr
               XMCuo(Ii,k) = sto(i,7)/NMUltr
               PL(Ii,k) = sto(i,8)
               QREpa(Ii,k) = sto(i,9)/NMUltr
               XHTc(Ii,k) = sto(i,10)/NMUltr
            ENDDO
         ENDDO
         RETURN
      ENDIF
      END
!*==S00011.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00011
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA33 / VCO , XL0055 , D876 , DINt , DEXt , VOL002 ,      &
     &                VOL005 , LCO , NCRay
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      DOUBLE PRECISION M , x(3) , y(3) , xy(3)
      DOUBLE PRECISION bidon(2000)
      pi = 3.14159
      I6Ft = NC1/2
      I8Ft = 2*NC1/3
      J6Ft = 1
      J8Ft = 1
      z6 = XL0055/2.
      z8 = 2.*XL0055/3.
      DO i = 2 , NC1
         ni = NASl(i)
         zminc = ZCOt(i-1)
         zint = XL(i)/ni
         DO j = 1 , ni
            zmin = zminc + (j-1.)*zint
            zmax = zmin + zint
            IF ( z6.LE.zmax ) THEN
               IF ( z6.GE.zmin ) THEN
                  I6Ft = i
                  J6Ft = j
               ENDIF
               IF ( z8.LE.zmax ) THEN
                  I8Ft = i
                  J8Ft = j
                  GOTO 200
               ENDIF
            ENDIF
         ENDDO
      ENDDO
 200  sec = pi*(DEXt*DEXt-DINt*DINt)*NCRay/4.
      VOL002 = sec*XL0055
      VOL005 = pi*D876*D876/4.*XL0055*NCRay
      XMGai = RGAi*VOL002
      XM876 = R876*VOL005
      XMCgai = CPGai*XMGai
      XMC876 = CP876*XM876
      perim = NCRay*pi*DEXt
      DIAhy = 4.*SCApa(2)/perim
      DO i = 2 , NC1
         n1 = NSL(i)
         xmg = XMCgai*XL(i)/XL0055/n1
         xmu = XMC876*XL(i)/XL0055/n1
         SHTc(i) = XL(i)*DEXt*pi
         SHTc(i) = SHTc(i)*NCRay
         xht = SHTc(i)/n1
         DO j = 1 , n1
            XMCga(i,j) = xmg
            XMCuo(i,j) = xmu
            XHTc(i,j) = xht
         ENDDO
      ENDDO
      DO i = 2 , NC1
         n1 = NSL(i)
         DO j = 1 , n1
            QREpa(i,j) = PL(i,j)*XL(i)/n1/XL0055
         ENDDO
      ENDDO
      PLMoy = QINit/NCRay/XL0055
      CALL S00934(TPUi,QPUi,QPUi,TEM,RPUi,xx,6)
      PLMoy = PLMoy*RPUi
      DO i = 2 , NC1
         hgap = HGApp(i)
         HCG(i) = 2.*pi/(DLOG(DEXt/DINt)/XLGai+2./(D876*hgap)           &
     &            +1./(4.*XL876))
      ENDDO
      IF ( KTGai.NE.0 ) THEN
         DO i = 2 , NC1
            n1 = NSL(i)
            DO j = 1 , n1
               TGAi(i,j) = T(i,1)
               IF ( ITYp(i).EQ.2 ) TGAi(i,j) = T(i,2)
            ENDDO
         ENDDO
      ENDIF
      tpe = TGAi(IBCh,1)
      x(3) = XL(2)/(2.*NSL(2))
      x(2) = -x(3)
      x(1) = -3.*x(3)
      y(3) = TGAi(2,1)
      y(2) = T(1,1)
      y(1) = 2.*y(2) - TS + 20.
      NSL(1) = 1
      NASl(1) = 1
      CALL S00099(3,x,y,xy)
      QCOn(1,1) = 2.*xy(1)*XLGai*sec*XL(2)/NSL(2)
      CALL S66832
      ISLni = 0
      CALL S33055(IBTr)
      CALL S33055(IHTr)
      IF ( ICAt(2).GT.2 ) THEN
         i = NIV
         n1 = NASl(i)
         ISLni = HNIv(i)/XL(i)*n1 + 1
         DO j = 1 , ISLni
            TGAi(i,j) = T(i,1)
         ENDDO
         IF ( ISLni.NE.n1 ) THEN
            isl2 = ISLni + 1
            DO j = isl2 , n1
               TGAi(i,j) = T(i,2)
            ENDDO
         ENDIF
         TMI(1) = T(i,1)
         TMI(2) = T(i,2)
      ENDIF
      IF ( KT876.NE.0 ) THEN
         DO i = 2 , NC1
            hcgg = HCG(i)
            n1 = NASl(i)
            DO j = 1 , n1
               T876(i,j) = TGAi(i,j) + PL(i,j)*PLMoy/hcgg
            ENDDO
         ENDDO
         i = NIV
         j = ISLni
         hcgg = HCG(i)
         TMU(1) = TMI(1) + PL(i,j)*PLMoy/hcgg
         TMU(2) = TMI(2) + PL(i,j)*PLMoy/hcgg
      ENDIF
      DO i = 2 , NC1
         n1 = NASl(i)
         DO j = 1 , n1
            pi = 3.14159
            TUMax(i,j) = T876(i,j) + PL(i,j)*PLMoy/(8.*pi*XL876)
            TUMin(i,j) = T876(i,j) - PL(i,j)*PLMoy/(8.*pi*XL876)
            TGAii(i,j) = TGAi(i,j) + PL(i,j)*DLOG(DEXt/DINt)            &
     &                   *PLMoy/(2.*pi*XLGai)
         ENDDO
      ENDDO
      stot = 0.
      DO i = IBCh , IHCh
         stot = stot + SHTc(i)
      ENDDO
      ecart = QINit/(stot*HLCf)
      DO i = 2 , NC1
         xg = HGApp(i)*3600.
      ENDDO
      DO ii = 2 , NC1
         i = NC1 + 2 - ii
         n1 = NSL(i)
         DO jj = 1 , n1
            j = n1 + 1 - jj
         ENDDO
      ENDDO
      CONTINUE
      END
!*==S00934.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00934(X,Y,Z,Xa,Ya,Za,Npoint)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION X(20) , Y(20) , Z(20)
      IF ( Xa.GT.X(1) ) THEN
         IF ( Xa.GE.X(Npoint) ) THEN
            Ya = Y(Npoint)
            Za = Z(Npoint)
         ENDIF
         DO i = 2 , Npoint
            IF ( Xa.LE.X(i) ) THEN
               rap = (Xa-X(i-1))/(X(i)-X(i-1))
               Ya = Y(i-1) + (Y(i)-Y(i-1))*rap
               Za = Z(i-1) + (Z(i)-Z(i-1))*rap
               GOTO 100
            ENDIF
         ENDDO
 100     RETURN
      ELSE
         Ya = Y(1)
         Za = Z(1)
         RETURN
      ENDIF
      END
!*==S00099.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00099(Np,X,T,C)
      IMPLICIT DOUBLE PRECISION(A-H,O-z)
      DOUBLE PRECISION X(Np) , T(Np) , y(5) , z(5) , C(Np)
      DO i = 1 , Np
         C(i) = 0.
      ENDDO
      IF ( Np.LE.3 ) THEN
         DO i = 2 , 3
            y(i) = (T(i)-T(1))/(X(i)-X(1))
         ENDDO
         C(1) = (y(3)-y(2))/(X(3)-X(2))
         C(2) = y(2) - (X(1)+X(2))*C(1)
         C(3) = T(1) - X(1)*C(2) - X(1)*X(1)*C(1)
         RETURN
      ELSEIF ( Np.GT.4 ) THEN
         IF ( Np.GT.5 ) RETURN
         RETURN
      ELSE
         DO i = 2 , 4
            y(i) = (T(i)-T(1))/(X(i)-X(1))
         ENDDO
         DO i = 3 , 4
            z(i) = (y(i)-y(2))/(X(i)-X(2))
         ENDDO
         C(1) = (z(4)-z(3))/(X(4)-X(3))
         C(2) = z(3) - (X(1)+X(2)+X(3))*C(1)
         C(3) = y(2) - C(1)*(X(2)*X(2)+X(1)*X(2)+X(1)*X(1)) - C(2)      &
     &          *(X(1)+X(2))
         C(4) = T(1) - C(1)*X(1)*X(1)*X(1) - C(2)*X(1)*X(1) - C(3)*X(1)
         RETURN
      ENDIF
      END
!*==S00004.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00004(Ica,Nivo,Iorg)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA33 / VCO , XL0055 , D876 , DINt , DEXt , VOL002 ,      &
     &                VOL005 , LCO , NCRay
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      DOUBLE PRECISION M , hnivo(22)
      islnio = ISLni
      ibtro = IBTr
      ihtro = IHTr
      DO i = 2 , NC1
         hnivo(i) = HNIv(i)
      ENDDO
      IF ( ICAt(2).GT.2 ) HNIv(NIV) = V(NIV,1)/SCApa(NIV)
      CALL S66832
      IF ( IBTr.LT.ibtro ) THEN
         CALL S33055(IBTr)
         CALL S00055(ihtro)
      ENDIF
      IF ( IHTr.GT.ihtro ) THEN
         CALL S33055(IHTr)
         CALL S00055(ibtro)
      ENDIF
      IF ( ICAt(2).LE.2 ) RETURN
      ZNIv = 0.
      i = 1
 200  i = i + 1
      IF ( ITYp(i).GE.3 ) THEN
         HNIv(i) = V(i,1)/SCApa(i)
         ZNIv = ZNIv + HNIv(i)
         ISLni = HNIv(i)/XL(i)*NASl(i) + 1
         IF ( i.LT.NC1 ) THEN
            indice = i + 1
            DO i = indice , NC1
               HNIv(i) = 0.
            ENDDO
         ENDIF
         ll = 12
         indio = ll*Nivo + islnio
         indi = ll*NIV + ISLni
         IF ( indi.EQ.indio ) RETURN
         i1 = Nivo
         j1 = islnio
         i2 = NIV
         j2 = ISLni
         h1 = XL(i1)/NASl(i1)
         z3 = XL(i2)/NASl(i2)
         dt3 = DTGai(i2,j2)
         t3 = TGAi(i2,j2)
         dtu3 = DT876(i2,j2)
         tu3 = T876(i2,j2)
         IF ( ISS.EQ.2 ) THEN
            z2 = hnivo(i1) - (j1-1.)*XL(i1)/NASl(i1)
            ind1 = 2
            ind2 = 1
            vite = VITess/SCApa(i1)
            h2 = -HNIv(i2) + j2*XL(i2)/NASl(i2)
            h3 = HNIv(i2) - (j2-1.)*XL(i2)/NASl(i2)
            i4 = i2
            j4 = j2 - 1
            IF ( j4.LT.1 ) THEN
               i4 = i2 - 1
               j4 = NASl(i4)
            ENDIF
         ELSE
            z2 = -hnivo(i1) + j1*XL(i1)/NASl(i1)
            ind1 = 1
            ind2 = 2
            vite = -VITess/SCApa(i1)
            h2 = HNIv(i2) - (j2-1.)*XL(i2)/NASl(i2)
            h3 = -HNIv(i2) + j2*XL(i2)/NASl(i2)
            i4 = i2
            j4 = j2 + 1
            IF ( j4.GT.NASl(i2) ) THEN
               i4 = i2 + 1
               j4 = 1
            ENDIF
         ENDIF
         IF ( i4.GE.IBCh .AND. i4.LE.IHCh ) THEN
            h4 = XL(i4)/NASl(i4)
            t4 = TGAi(i4,j4)
            tu4 = T876(i4,j4)
         ENDIF
         t1 = TMI(ind1)
         tu1 = TMU(ind1)
         IF ( Iorg.NE.1 ) THEN
            dt2 = DTMi(ind2)
            t2 = TMI(ind2) - dt2*DT
            t3 = t3 - dt3*DT
            dtu2 = DTMu(ind2)
            tu2 = TMU(ind2) - dtu2*DT
            tu3 = tu3 - dtu3*DT
            u2 = (z2*tu2+z3*tu3)/(z2+z3)
            dtu2 = (vite*(tu2-u2)+z2*dtu2+z3*dtu3)/(z2+z3)
            tu3 = u2 + dtu2*DT
            u2 = (z2*t2+z3*t3)/(z2+z3)
            dtu2 = (vite*(t2-u2)+z2*dt2+z3*dt3)/(z2+z3)
            t3 = u2 + dtu2*DT
         ENDIF
         IF ( i4.GE.IBCh .AND. i4.LE.IHCh ) THEN
            t2 = (t1*h3+(h1+h2)/(h3+h4)*((h2+h3+h4)*t3-h2*t4))          &
     &           /(h1+h2+h3)
            t2min = DMIN1(t1,t3)
            t2max = DMAX1(t1,t3)
            IF ( t2.LT.t2min ) t2 = t2min
            IF ( t2.GT.t2max ) t2 = t2max
            tu2 = (tu1*h3+(h1+h2)/(h3+h4)*((h2+h3+h4)*tu3-h2*tu4))      &
     &            /(h1+h2+h3)
            t2min = DMIN1(tu1,tu3)
            t2max = DMAX1(tu1,tu3)
            tu2 = DMIN1(tu2,t2max)
            tu2 = DMAX1(tu2,t2min)
         ELSE
            t2 = (t1+t3)/2.
            tu2 = (tu1+tu3)/2.
         ENDIF
         TGAi(i1,j1) = t1
         TMI(ind1) = t2
         TMI(ind2) = t3
         T876(i1,j1) = tu1
         TMU(ind1) = tu2
         TMU(ind2) = tu3
         RETURN
      ELSE
         ZNIv = ZNIv + XL(i)
         HNIv(i) = 0.
         GOTO 200
      ENDIF
      END
!*==S66832.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S66832
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      DOUBLE PRECISION M
      nivo = NIV
      znivo = ZNIv
      ibtro = IBTr
      ihtro = IHTr
      IF ( ICAt(2).LE.2 ) THEN
         TGAi(1,1) = TGAi(2,1)
         qt = QCOn(1,1)
         im = 1
         jm = 1
         DO i = 2 , NC1
            n2 = NASl(i)
            DO j = 1 , n2
               IF ( QCOn(i,j).GT.qt ) THEN
                  nn = i
                  jj = j - 1
                  IF ( jj.LT.1 ) THEN
                     nn = i - 1
                     jj = NASl(nn)
                  ENDIF
                  IF ( (TGAi(i,j)-TGAi(nn,jj)).LT.50. ) THEN
                     im = i
                     jm = j
                     qt = QCOn(i,j)
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         n1 = NASl(im)
         NIV = im
         IF ( jm.LE.(n1/2) ) THEN
            IBTr = im - 1
            IHTr = im
         ELSE
            IBTr = im
            IHTr = im + 1
         ENDIF
         IF ( IBTr.LT.2 ) THEN
            IBTr = 2
            IHTr = MIN0(3,NC1)
         ELSEIF ( IHTr.GT.NC1 ) THEN
            IBTr = MAX0(NC,2)
            IHTr = NC1
         ENDIF
         ZNIv = 0.
         IF ( NIV.GE.2 ) THEN
            IF ( NIV.NE.2 ) THEN
               n1 = NIV - 1
               DO i = 2 , n1
                  ZNIv = ZNIv + XL(i)
               ENDDO
            ENDIF
            ZNIv = ZNIv + (jm-.5)*XL(NIV)/NASl(NIV)
         ENDIF
         IF ( IHTr.GT.(ihtro+1) ) THEN
            ZNIv = znivo
            IHTr = ihtro + 1
            IBTr = IHTr - 1
            NIV = 0
            RETURN
         ELSEIF ( IBTr.LT.(ibtro-1) ) THEN
            ZNIv = znivo
            IBTr = ibtro - 1
            IHTr = IBTr + 1
            NIV = 0
            RETURN
         ELSE
            RETURN
         ENDIF
      ELSEIF ( NIV.EQ.2 ) THEN
         IBTr = 2
         IHTr = 3
         RETURN
      ELSEIF ( HNIv(NIV).GT.XL(NIV)/2. ) THEN
         IF ( NIV.NE.NC1 ) THEN
            IBTr = NIV
            IHTr = NIV + 1
            RETURN
         ENDIF
      ENDIF
      IBTr = NIV - 1
      IHTr = NIV
      CONTINUE
      END
!*==S00020.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00020
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA88 / FLUp(22,2) , FLUv(22,2) , FLUl(22,2) , FLUiv(22,2)&
     &                , DEBil(22,2) , FLUil(22,2) , DEBi(22) , DEBhi(22)&
     &                , DEBiv(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA33 / VCO , XL0055 , D876 , DINt , DEXt , VOL002 ,      &
     &                VOL005 , LCO , NCRay
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      DOUBLE PRECISION M , x(5) , y(5) , xy(5) , der(2)
      DOUBLE PRECISION xg(132) , tg(132) , xf(132) , dtg(132)
      DOUBLE PRECISION xg1(132) , xg2(132) , tg1(132) , tg2(132)
      pi = 3.14159
      eps = 1.D-3
      TERc(1) = 0.
      TERc(2) = 0.
      DTMi(1) = 0.
      DTMi(2) = 0.
      sec = pi/4.*(DEXt*DEXt-DINt*DINt)
      sec = sec*NCRay
      CALL S00934(TPUi,QPUi,QPUi,TEM,RPUi,x(1),6)
      QTOtal = QINit*RPUi
      PLMoy = QTOtal/XL0055/NCRay
      IF ( ICAt(2).GT.2 ) THEN
         TGAi(NIV,ISLni) = TMI(1)*RI(1) + TMI(2)*RI(2)
         T876(NIV,ISLni) = TMU(1)*RI(1) + TMU(2)*RI(2)
      ENDIF
      IF ( KCOn.NE.0 ) THEN
         xf(1) = 0.
         k = 1
         DO i = 2 , NC1
            n1 = NASl(i)
            DO j = 1 , n1
               TERco(i,j) = 0.
               k = k + 1
               xf(k) = xf(k-1) + XL(i)/n1
               xg(k-1) = xf(k-1) + (xf(k)-xf(k-1))/2.
               tg(k-1) = TGAi(i,j)
               xg1(k-1) = xg(k-1)
               xg2(k-1) = xg(k-1)
               tg1(k-1) = tg(k-1)
               tg2(k-1) = tg(k-1)
               IF ( i.EQ.NIV .AND. j.EQ.ISLni ) THEN
                  xg1(k-1) = xf(k-1) + (xf(k)-xf(k-1))/2.*RI(1)
                  xg2(k-1) = xf(k) - (xf(k)-xf(k-1))/2.*RI(2)
                  tg1(k-1) = TMI(1)
                  tg2(k-1) = TMI(2)
                  kniv = k - 1
               ENDIF
            ENDDO
         ENDDO
         kmax = k
         kmax1 = kmax - 1
         DO k = 2 , kmax1
            dtg(k) = (tg1(k)-tg2(k-1))/(xg1(k)-xg2(k-1))
         ENDDO
         dtg(1) = 0.
         dtg(kmax) = 0.
         k = 0
         DO i = 2 , NC1
            n1 = NASl(i)
            DO j = 1 , n1
               k = k + 1
               IF ( k.EQ.1 ) THEN
                  x(1) = xg(1)
                  x(2) = xg1(2)
                  y(1) = tg(1)
                  y(2) = tg1(2)
                  xo = 0.
                  xy(1) = (y(2)-y(1))/(x(2)-x(1))*(x(1)+x(2)-2*xo)
                  QCOn(i,j) = 2.*xy(1)*XLGai*sec*XL(i)/n1
               ELSEIF ( k.NE.kmax1 ) THEN
                  x(1) = xg2(k-1)
                  x(2) = xg(k)
                  x(3) = xg1(k+1)
                  y(1) = tg2(k-1)
                  y(2) = tg(k)
                  y(3) = tg1(k+1)
                  CALL S00099(3,x,y,xy)
                  QCOn(i,j) = 2.*xy(1)*XLGai*sec*XL(i)/n1
               ELSE
                  x(1) = xg(kmax1)
                  x(2) = xg2(kmax1-1)
                  y(1) = tg(kmax1)
                  y(2) = tg2(kmax1-1)
                  xo = xf(kmax)
                  xy(1) = (y(2)-y(1))/(x(2)-x(1))*(x(1)+x(2)-2*xo)
                  QCOn(i,j) = 2.*xy(1)*XLGai*sec*XL(i)/n1
               ENDIF
            ENDDO
         ENDDO
         x(3) = XL(2)/(2.*NASl(2))
         x(2) = -x(3)
         x(1) = -3.*x(3)
         y(3) = TGAi(2,1)
         y(2) = T(1,1)
         y(1) = 2.*y(2) - TS + 20.
         CALL S00099(3,x,y,xy)
         QCOn(1,1) = 2.*xy(1)*XLGai*sec*XL(2)/NASl(2)
      ENDIF
      DO i = 2 , NC1
         n1 = NASl(i)
         hcgg = HCG(i)
         DO j = 1 , n1
            QFOu(i,j) = QREpa(i,j)*QTOtal
            QUGa(i,j) = (T876(i,j)-TGAi(i,j))*hcgg*XL(i)*NCRay/n1
            DT876(i,j) = (QFOu(i,j)-QUGa(i,j))/XMCuo(i,j)
            DTGai(i,j) = (QUGa(i,j)-QCEd(i,j)+QCOn(i,j))/XMCga(i,j)
         ENDDO
      ENDDO
      IF ( ICAt(2).LE.2 ) GOTO 600
      IF ( NIV.LE.IHCh .AND. NIV.GE.IBCh ) THEN
         i = NIV
         j = ISLni
         n1 = NASl(i)
         ccc = XL(i)*NCRay*HCG(i)/n1
         QUGii(1) = (TMU(1)-TMI(1))*ccc*RI(1)
         QUGii(2) = (TMU(2)-TMI(2))*ccc*RI(2)
         IF ( kniv.EQ.1 ) THEN
            x(1) = xg1(1)
            x(2) = xg2(1)
            x(3) = xg(2)
            y(1) = tg1(1)
            y(2) = tg2(1)
            y(3) = tg(2)
            xo = 0.
            ind1 = 1
            ind2 = 2
         ELSEIF ( kniv.NE.kmax1 ) THEN
            x(1) = xg(kniv-1)
            x(2) = xg1(kniv)
            x(3) = xg2(kniv)
            x(4) = xg(kniv+1)
            y(1) = tg(kniv-1)
            y(2) = tg1(kniv)
            y(3) = tg2(kniv)
            y(4) = tg(kniv+1)
            CALL S00099(4,x,y,xy)
            der(1) = 6.*xy(1)*x(2) + 2.*xy(2)
            der(2) = 6.*xy(1)*x(3) + 2.*xy(2)
            GOTO 400
         ELSE
            x(1) = xg2(kmax1)
            x(2) = xg1(kmax1)
            x(3) = xg(kmax1-1)
            y(1) = tg2(kmax1)
            y(2) = tg1(kmax1)
            y(3) = tg(kmax1-1)
            xo = xf(kmax)
            ind1 = 2
            ind2 = 1
         ENDIF
      ELSE
         DO ij = 1 , 3
            QUGii(ij) = 0.
            QCOni(ij) = 0.
         ENDDO
         GOTO 600
      ENDIF
      der(ind1) = 2.*(y(2)-y(1))/((x(2)-x(1))*(x(2)+x(1)-2.*xo))
      CALL S00099(3,x,y,xy)
      der(ind2) = 2.*xy(1)
 400  coef = XLGai*sec*XL(i)/n1
      QCOni(1) = coef*RI(1)*der(1)
      QCOni(2) = coef*RI(2)*der(2)
      DO ij = 1 , 2
         IF ( RI(ij).GE.eps ) THEN
            DTMi(ij) = (QUGii(ij)-QCEi(ij)+QCOni(ij))                   &
     &                 /(XMCga(i,j)*RI(ij))
            DTMu(ij) = (QFOu(i,j)-QUGii(ij)/RI(ij))/XMCuo(i,j)
         ENDIF
      ENDDO
      zlim = .05
      zs = XL(i)/n1
      z1 = RI(1)*zs
      z2 = RI(2)*zs
      zz = VITess*(TMI(1)-TMI(2))/SCApa(i)
      zzu = VITess*(TMU(1)-TMU(2))/SCApa(i)
      IF ( ISS.EQ.2 ) THEN
         IF ( z2.LT.zlim ) THEN
            TERc(1) = zz/zlim
            tercu = zzu/zlim
            IF ( RI(2).GE.eps ) THEN
               DTMi(2) = DTMi(2) - TERc(1)
               DTMu(2) = DTMu(2) - tercu
            ENDIF
            IF ( ISLni.EQ.n1 ) THEN
               i1 = i + 1
               j1 = 1
            ELSE
               i1 = i
               j1 = ISLni + 1
            ENDIF
            IF ( i1.GE.IBCh .AND. i1.LE.IHCh ) THEN
               TERco(i1,j1) = zz*XMCga(i,j)*(1.-z2/zlim)                &
     &                        /(zs*XMCga(i1,j1))
               DTGai(i1,j1) = DTGai(i1,j1) - TERco(i1,j1)
               tercu = zzu*XMCuo(i,j)*(1.-z2/zlim)/(zs*XMCuo(i1,j1))
               DT876(i1,j1) = DT876(i1,j1) - tercu
            ENDIF
         ELSE
            TERc(1) = zz/z2
            DTMi(2) = DTMi(2) - TERc(1)
            tercu = zzu/z2
            DTMu(2) = DTMu(2) - tercu
         ENDIF
      ELSEIF ( z1.LT.zlim ) THEN
         TERc(1) = zz/zlim
         tercu = zzu/zlim
         IF ( RI(1).GE.eps ) THEN
            DTMi(1) = DTMi(1) - TERc(1)
            DTMu(1) = DTMu(1) - tercu
         ENDIF
         IF ( ISLni.EQ.1 ) THEN
            i1 = i - 1
            j1 = NASl(i-1)
         ELSE
            i1 = i
            j1 = ISLni - 1
         ENDIF
         IF ( i1.GE.IBCh .AND. i1.LE.IHCh ) THEN
            TERco(i1,j1) = zz*XMCga(i,j)*(1.-z1/zlim)/(zs*XMCga(i1,j1))
            DTGai(i1,j1) = DTGai(i1,j1) - TERco(i1,j1)
            tercu = zzu*XMCuo(i,j)*(1.-z1/zlim)/(zs*XMCuo(i1,j1))
            DT876(i1,j1) = DT876(i1,j1) - tercu
         ENDIF
      ELSE
         TERc(1) = zz/z1
         DTMi(1) = DTMi(1) - TERc(1)
         tercu = zzu/z1
         DTMu(1) = DTMu(1) - tercu
      ENDIF
 600  CONTINUE
      END
!*==S00018.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00018(I12,I21,Iorg)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      COMMON /AAA17 / DDPdt , DDH(22,2) , DDV(22,2) , DDM(22,2) ,       &
     &                DDU(22,2)
      COMMON /AAA16 / DBEnt(20) , TENt(20) , HENt(20) , DEBe , ENTe ,   &
     &                DBSor(20) , TSOr(20) , DEBs , DPDtt(20) ,         &
     &                TDPdt(20) , HSOr(20) , THSor(20) , ENTs , NDBent ,&
     &                NDPdt , NDBsor , NHSor
      COMMON /AAA18 / DDEb(21) , DDDeb(21) , DXAt(21) , DXAg(21) ,      &
     &                DXBt(21) , DXBg(21) , DNUg(21) , DNUf(21) ,       &
     &                DHE(22,2) , DDEbo(21)
      DOUBLE PRECISION M
      DTBef = 1.25*DT
      is = ISS
      I12 = 0
      I21 = 0
      eps = 1.D-8
      Iorg = 0
      doduc = .8
      ddc = 10.*V0012/NC1
      coedh = DMIN1(doduc,ddc)
      DT = V0012
      DTTemp = V0012
      DTDis = V0012
      DTChan = V0012
      DTTemp = V0012
      DTPhy = V0012
      DTOrg = V0012
      DTVoi = V0012
      sens = V00001
      dtp = V0012
      IF ( DABS(DPDt).GT.eps ) dtp = sens*P/DABS(DPDt)
      DO i = 2 , NC1
         j1 = 2 - (1/ITYp(i))
         dth = V0012
         dtvol = V0012
         sens = V00001
         DO j = 1 , j1
            x = V0012
            y = DABS(DH(i,j))
            IF ( y.LE.1.D-3 ) y = 1.D-3
            yy = DABS(DDH(i,j))
            IF ( yy.LE.1.D-2 ) yy = 1.D-2
            dt2 = DSQRT(sens*H(i,j)/yy)
            dt3 = sens*H(i,j)/y
            dth = DMIN1(x,dt2,dt3)
         ENDDO
         IF ( ITYp(i).EQ.2 ) THEN
            y = DABS(DV(i,1))
            yo = VC(i)*1.D-3
            IF ( y.LE.yo ) y = yo
            yy = DABS(DDV(i,1))
            yo = yo*10.
            IF ( yy.LE.yo ) yy = yo
            xla = .5
            ymax = VC(i)
            IF ( y.LT.ymax ) xla = .5*ymax/y
            dt1 = xla*y/yy
            dt2 = DSQRT(sens*VC(i)/(10.*yy))
            dt3 = sens*VC(i)/y
            dtvol = DMIN1(x,dt1,dt2,dt3)
         ENDIF
         DTPhy = DMIN1(dth,dtvol,DTPhy)
         sens = V22202
         n1 = NASl(i)
         DO j = 1 , n1
            dtpar = V0012
            IF ( i.EQ.NIV .AND. j.EQ.ISLni ) THEN
               k1 = 2
               DO k = 1 , k1
                  dtpa = V0012
                  IF ( DABS(DTMi(k)).GT.eps )                           &
     &                 dtpa = 2.*sens*DABS(TMI(k)/DTMi(k))
                  dtpar = DMIN1(dtpar,dtpa)
               ENDDO
            ELSEIF ( DABS(DTGai(i,j)).GT.eps ) THEN
               dtpar = sens*DABS(TGAi(i,j)/DTGai(i,j))
            ENDIF
            DTTemp = DMIN1(DTTemp,dtpar)
         ENDDO
         IF ( ITYp(i).NE.2 ) THEN
            j1 = 2 - (1/ITYp(i))
            DO j = 1 , j1
               IF ( DABS(DH(i,j)).GE.1.D-6 ) THEN
                  dtch1 = .6*(HFS-H(i,j))/DH(i,j)
                  dtch2 = .6*(HGS-H(i,j))/DH(i,j)
                  IF ( dtch1.LT.eps ) dtch1 = V0012
                  IF ( dtch2.LT.eps ) dtch2 = V0012
                  DTChan = DMIN1(dtch1,dtch2,DTChan)
               ENDIF
            ENDDO
         ENDIF
         IF ( ICAt(2).GT.2 ) THEN
            dtch1 = V0012
            dtch2 = V0012
            IF ( ITYp(i).EQ.3 ) THEN
               xh1 = H(i,1)
               xh2 = H(i,2)
               dxh1 = DH(i,1)
               dxh2 = DH(i,2)
            ELSE
               xh1 = F00111(i)
               xh2 = F00111(i)
               dxh1 = DH(i,1)
               dxh2 = DH(i,2)
               IF ( ITYp(i).NE.1 ) THEN
                  xmt = M(i,1) + M(i,2)
                  dxh1 = (DM(i,1)*(H(i,1)-xh1)+DM(i,2)*(H(i,2)-xh1)     &
     &                   +M(i,1)*DH(i,1)+M(i,2)*DH(i,2))/(xmt*xmt)
                  dxh2 = dxh1
               ENDIF
            ENDIF
            j = i - 1
            idoduc = 0
            IF ( idoduc.EQ.2 ) THEN
               hinf = F00111(j)
               xmt = M(j,1) + M(j,2)
               dhinf = (DM(j,1)*(H(j,1)-hinf)+DM(j,2)*(H(j,2)-hinf)     &
     &                 +M(j,1)*DH(j,1)+M(j,2)*DH(j,2))/(xmt*xmt)
            ELSEIF ( idoduc.EQ.3 ) THEN
               hinf = H(j,2)
               dhinf = DH(j,2)
            ELSE
               hinf = H(j,1)
               dhinf = DH(j,1)
            ENDIF
            j = i + 1
            IF ( ITYp(j).EQ.2 ) THEN
               hsup = F00111(j)
               xmt = M(j,1) + M(j,2)
               dhsup = (DM(j,1)*(H(j,1)-hsup)+DM(j,2)*(H(j,2)-hsup)     &
     &                 +M(j,1)*DH(j,1)+M(j,2)*DH(j,2))/(xmt*xmt)
            ELSE
               hsup = H(j,1)
               dhsup = DH(j,1)
            ENDIF
            IF ( i.NE.2 ) THEN
               ect = xh1 - hinf
               dect = dxh1 - dhinf
               IF ( DABS(dect).GE.1.D-4 ) THEN
                  dtch1 = coedh*ect/dect
                  IF ( dtch1.LT.eps ) dtch1 = V0012
               ENDIF
            ENDIF
            IF ( i.NE.NC1 ) THEN
               ect = xh2 - hsup
               dect = dxh2 - dhsup
               IF ( DABS(dect).GE.1.D-4 ) THEN
                  dtch2 = coedh*ect/dect
                  IF ( dtch2.LT.eps ) dtch2 = V0012
               ENDIF
            ENDIF
            DTVoi = DMIN1(DTVoi,dtch1,dtch2)
         ENDIF
      ENDDO
      IF ( ICAt(2).GT.2 ) THEN
         sens = V22201
         roo = 1.
         vite = DABS(VITess)
         DTDis = V0012
         vniv = vite/SCApa(NIV)
         vinj = DEB(1)
         vinj = vinj*VVFs/SCApa(NIV)
         IF ( vniv.GE..01 ) THEN
            IF ( vinj.GE..01 ) roo = vinj/vniv
            DTDis = sens*XL(NIV)/vniv*DMIN1(1.D0,roo)
         ENDIF
      ENDIF
      DTPre = dtp
      DT = DMIN1(dtp,DTPhy,DTTemp,DTChan,DTDis,DTBef,DTVoi)
      dto = V0012
      DO i = 2 , NC1
         IF ( ITYp(i).NE.1 ) THEN
            DO j = 1 , 2
               yc = V(i,j)
               IF ( yc.GT.1.D-7 ) THEN
                  ya = DDV(i,j)
                  yb = DV(i,j)
                  vf = yc + (yb+ya*DT)*DT
                  IF ( vf.LT.0. ) THEN
                     IF ( DABS(ya).GE.1.D-6 ) THEN
                        delt = yb*yb - 4.*ya*yc
                        IF ( delt.GT.1.D-8 ) THEN
                           tc = DSQRT(delt)
                           dt1 = (-yb-tc)/(2.*ya)
                           dt2 = (-yb+tc)/(2.*ya)
                           dt3 = DMIN1(dt1,dt2)
                           dt4 = DMAX1(dt1,dt2)
                           dt1 = dt3
                           IF ( dt3.LE.0. ) dt1 = dt4
                           dt1 = .9*dt1
                           dto = DMIN1(dt1,dto)
                        ENDIF
                     ELSEIF ( DABS(yb).GE.1.D-6 ) THEN
                        dt1 = -yc/yb
                        dt1 = .9*dt1
                        dto = DMIN1(dt1,dto)
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      DTOrg = dto
      DT = DMIN1(DT,DTOrg)
      DT = DMAX1(V0011,DT)
      IF ( ICAt(2).GT.2 ) THEN
         i = NIV
         ecv = DT*(DDV(i,1)*DT+DV(i,1))
         vl1 = V(i,1) + ecv
         vl2 = V(i,2) - ecv
         IF ( vl1.LT.0. ) Iorg = 1
         IF ( vl2.LT.0. ) Iorg = 1
      ENDIF
      dt3 = V0012
      DO i = 1 , NDBent
         tbut = TENt(i) + 1.D-4
         dt2 = tbut - TEM
         IF ( dt2.GT.0. ) THEN
            IF ( dt2.LT.DT ) dt3 = DMIN1(dt3,dt2)
         ENDIF
      ENDDO
      DTOrg = DMIN1(DTOrg,dt3)
      DT = DMIN1(DT,DTOrg)
      DT = DMAX1(V0011,DT)
      DO i = 2 , NC1
         IF ( DABS(DEBl(i-1)).LT.1.D-6 ) THEN
            vmin = 1.D-7*VC(i)
            IF ( V(i,1).LT.vmin ) THEN
               H(i,1) = HF(i-1,1)
               GOTO 600
            ENDIF
         ENDIF
      ENDDO
 600  DO i = 1 , NC1
         albt = DABS(ALFb(i)-ALFt(i))
         IF ( albt.GE.1.D-2 ) THEN
            sec = SC(i)
            g = DEB(i)/sec
            dg = DDEb(i)/sec
            dtgsu = V0012
            denom = V55198(i) - dg
            IF ( DABS(denom).GT.1.D-3 ) THEN
               dtgsu = (g-GLSup(i))/denom
               IF ( dtgsu.LE.1.D-4 ) dtgsu = V0012
            ENDIF
            dtgin = V0012
            denom = GLInfp(i) - dg
            IF ( DABS(denom).GT.1.D-3 ) THEN
               dtgin = (g-GLInf(i))/denom
               IF ( dtgin.LE.1.D-4 ) dtgin = V0012
            ENDIF
            DTVoi = DMIN1(DTVoi,dtgsu,dtgin)
         ENDIF
      ENDDO
      DT = DMIN1(DTVoi,DT)
      DT = DMAX1(DT,V0011)
      CONTINUE
      END
!*==S00001.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!*==b8c.for
      SUBROUTINE S00001
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA16 / DBEnt(20) , TENt(20) , HENt(20) , DEBe , ENTe ,   &
     &                DBSor(20) , TSOr(20) , DEBs , DPDtt(20) ,         &
     &                TDPdt(20) , HSOr(20) , THSor(20) , ENTs , NDBent ,&
     &                NDPdt , NDBsor , NHSor
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      DOUBLE PRECISION M
      coef = 144.D+00/778.D+00
      CALL S00096(P,HFS,HGS,TS,VVFs,VVGs,DVVfps,DVVgps,DHFps,DHGps,     &
     &            DTSat)
      h1 = HFS - 5.
      CALL S55199(h1,P,T1,r,x1,x2,dtdh1,dtdp1)
      VV1 = 1./r
      h2 = HGS + 10.
      CALL S55198(h2,P,T2,r,x1,x2,dtdh2,dtdp2)
      VV2 = 1./r
      h3 = 1850.
      h4 = 1860.
      CALL S55198(h3,P,t3,r,x1,x2,x3,x4)
      vv3 = 1./r
      dvp3 = x2
      dtp3 = x4
      CALL S55198(h4,P,t4,r,x1,x2,x3,x4)
      vv4 = 1./r
      dvp4 = x2
      dtp4 = x4
      HFGs = HGS - HFS
      DHFgs = DHGps - DHFps
      vfgs = VVGs - VVFs
      dvfgs = DVVgps - DVVfps
      CALL S00934(TENt,DBEnt,HENt,TEM,DEBe,H(1,1),20)
      CALL S00934(THSor,HSOr,HSOr,TEM,ENTs,x,2)
      H(NC2,1) = ENTs
      IF ( KPRess.EQ.1 ) THEN
         CALL S00934(TDPdt,DPDtt,DPDtt,TEM,P,x,13)
      ELSE
         CALL S00934(TSOr,DBSor,DBSor,TEM,DEBs,x,NDBsor)
      ENDIF
      DO i = 1 , NC2
         j1 = 2
         IF ( ITYp(i).EQ.1 ) j1 = 1
         IF ( ITYp(i).EQ.2 ) THEN
            IF ( H(i,1).GE.HFS ) H(i,1) = HFS - 1.D-6
            IF ( H(i,2).LE.HGS ) H(i,2) = HGS + 1.D-6
            vmin = 1.D-7*VC(i)
            IF ( V(i,1).LT.vmin ) THEN
               V(i,1) = 0.
               V(i,2) = VC(i)
            ELSEIF ( V(i,2).LT.vmin ) THEN
               V(i,1) = VC(i)
               V(i,2) = 0.
            ENDIF
         ENDIF
         DO j = 1 , j1
            IF ( H(i,j).LT.10. ) H(i,j) = 10.
            XT(i,j) = (H(i,j)-HFS)/HFGs
            xti = XT(i,j)
            IF ( xti.LT.0. ) THEN
               IST(i,j) = 1
               ALFa(i,j) = 0.
               XM(i,j) = 0.
               HF(i,j) = H(i,j)
               HG(i,j) = HGS
               CALL S55199(H(i,j),P,T(i,j),r,DVVh(i,j),DVVp(i,j),       &
     &                     DTDh(i,j),DTDp(i,j))
               DNUhp(i,j) = 0.
               DNUpp(i,j) = 0.
               VV(i,j) = 1./r
               IF ( H(i,j).GT.h1 ) THEN
                  x = 1. - (HFS-H(i,j))/5.
                  T(i,j) = (1.-x)*T1 + x*TS
                  VV(i,j) = (1.-x)*VV1 + x*VVFs
                  DVVh(i,j) = (VVFs-VV1)/5.
                  dxdh = 1./5.
                  DTDh(i,j) = (TS-T1)*dxdh
               ENDIF
            ELSEIF ( xti.GT.1. ) THEN
               IST(i,j) = 3
               ALFa(i,j) = 1.
               XM(i,j) = 1.
               HF(i,j) = HFS
               HG(i,j) = H(i,j)
               CALL S55198(H(i,j),P,T(i,j),r,DVVh(i,j),DVVp(i,j),       &
     &                     DTDh(i,j),DTDp(i,j))
               DNUhp(i,j) = 0.
               DNUpp(i,j) = 0.
               VV(i,j) = 1./r
               IF ( H(i,j).LT.h2 ) THEN
                  x = (H(i,j)-HGS)/10.
                  T(i,j) = (1.-x)*TS + x*T2
                  VV(i,j) = (1.-x)*VVGs + x*VV2
                  DVVh(i,j) = (VV2-VVGs)/10.
                  dxdh = 1./10.
                  DTDh(i,j) = (T2-TS)*dxdh
               ELSEIF ( H(i,j).GT.h4 ) THEN
                  x = (H(i,j)-1860.)/10.
                  T(i,j) = t4 + (t4-t3)*x
                  VV(i,j) = vv4 + (vv4-vv3)*x
                  DTDh(i,j) = (t4-t3)/10.
                  DVVh(i,j) = (vv4-vv3)/10.
                  DTDp(i,j) = dtp4 + (dtp4-dtp3)*x
                  DVVp(i,j) = dvp4 + (dvp4-dvp3)*x
               ENDIF
            ELSE
               IST(i,j) = 2
               dvfgs = DVVgps - DVVfps
               vfgs = VVGs - VVFs
               T(i,j) = TS
               DTDp(i,j) = DTSat
               DTDh(i,j) = 0.
               XM(i,j) = xti
               ALFa(i,j) = xti/((1.-xti)*VVFs/VVGs+xti)
               VV(i,j) = (1.-xti)*VVFs + xti*VVGs
               dxdp = -(DHFps+DHFgs*xti)/HFGs
               dvvdpx = DVVfps + xti*dvfgs
               DVVp(i,j) = vfgs*dxdp + dvvdpx
               DVVh(i,j) = vfgs/HFGs
               xx = vfgs*DHFgs/HFGs
               DNUhp(i,j) = (dvfgs-xx)/HFGs
               DNUpp(i,j) = dxdp*(-2.*xx+dvfgs)
               HF(i,j) = HFS
               HG(i,j) = HGS
            ENDIF
            M(i,j) = V(i,j)/VV(i,j)
            U(i,j) = M(i,j)*H(i,j) - coef*P*V(i,j)
            DELh(i,j) = VV(i,j) - H(i,j)*DVVh(i,j)
            DELp(i,j) = DVVp(i,j) + VV(i,j)*DVVh(i,j)*coef
            BB(i,j) = M(i,j)*DELp(i,j)
         ENDDO
      ENDDO
      CONTINUE
      END
!*==S00012.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00012
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      DOUBLE PRECISION M , ax(5) , ay(5)
      TVId(1,1) = ALFa(1,1)
      TVId(NC2,1) = ALFa(NC2,1)
      ALFb(1) = TVId(1,1)
      ALFt(NC1) = TVId(NC2,1)
      alec = .01
      alin = alec
      alsu = 1. - alec
      DO i = 2 , NC1
         ni = NASl(i)
         xl2 = XL(i)/2.
         IF ( ITYp(i).EQ.3 ) xl2 = HNIv(i)/2.
         ax(2) = ZCOt(i-1) + xl2
         ay(2) = ALFa(i,1)
         IF ( ITYp(i).EQ.2 ) ay(2) = V(i,2)/VC(i)
         xlm = xl2
         alm = ay(2)
         fm = 0.
         hm = 1.
         gm = 0.
         IF ( alm.LT.alin ) THEN
            hm = alm/alec
            fm = 1. - hm
         ELSEIF ( alm.GT.alsu ) THEN
            gm = (alm-alsu)/alec
            hm = 1. - gm
         ENDIF
         j = i - 1
         ax(1) = ZCOt(i-1)
         xlb = -XL(j)/2.
         IF ( j.EQ.1 ) xlb = -XL(i)/2.
         alb = ALFa(j,1)
         IF ( ITYp(j).EQ.2 ) alb = V(j,2)/VC(j)
         IF ( ITYp(j).EQ.3 ) THEN
            alb = ALFa(j,2)
            xlb = (HNIv(j)-XL(j))/2.
         ENDIF
         aljo = (alb*xlm-alm*xlb)/(xlm-xlb)
         fb = 0.
         gb = 0.
         hbb = 1.
         IF ( alb.LT.alin ) THEN
            hbb = alb/alec
            fb = 1. - hbb
         ELSEIF ( alb.GT.alsu ) THEN
            gb = (alb-alsu)/alec
            hbb = 1. - gb
         ENDIF
         tcc = hm*(hbb+fm*fb+gm*gb)
         ay(1) = alm + (aljo-alm)*tcc
         aljb = ay(1)
         p1 = (ay(2)-ay(1))/(ax(2)-ax(1))
         q1 = ay(1) - ax(1)*p1
         j = i + 1
         IF ( ITYp(i).GT.2 ) THEN
            ax(2) = ZCOt(i-1) + (XL(i)+HNIv(i))/2.
            ay(2) = ALFa(i,2)
            alm = ay(2)
            xlm = (HNIv(i)-XL(i))/2.
            fm = 0.
            hm = 0.
            gm = 0.
            IF ( alm.LT.alin ) THEN
               hm = alm/alec
               fm = 1. - hm
            ELSEIF ( alm.GT.alsu ) THEN
               gm = (alm-alsu)/alec
               hm = 1. - gm
            ENDIF
         ENDIF
         ax(3) = ZCOt(i-1) + XL(i)
         xlm = ax(2) - ax(3)
         xlh = XL(j)/2.
         IF ( ITYp(j).EQ.3 ) xlh = HNIv(j)/2.
         IF ( j.EQ.NC2 ) xlh = XL(i)/2.
         alh = ALFa(j,1)
         IF ( ITYp(j).EQ.2 ) alh = V(j,2)/VC(j)
         aljo = (alh*xlm-alm*xlh)/(xlm-xlh)
         fh = 0.
         gh = 0.
         hh = 1.
         IF ( alh.LT.alin ) THEN
            hh = alh/alec
            fh = 1. - hh
         ELSEIF ( alh.GT.alsu ) THEN
            gh = (alh-alsu)/alec
            hh = 1. - gh
         ENDIF
         tcc = hm*(hh+fm*fh+gm*gh)
         ay(3) = alm + (aljo-alm)*tcc
         aljt = ay(3)
         p2 = (ay(3)-ay(2))/(ax(3)-ax(2))
         q2 = ay(2) - ax(2)*p2
         IF ( ICAt(2).GE.3 ) THEN
            ALFb(i) = aljt
            ALFt(i-1) = aljb
            IF ( ITYp(i).GE.3 ) THEN
               TVId(i,1) = ALFa(i,1)
               TVId(i,2) = ALFa(i,2)
            ELSE
               DO j = 1 , ni
                  TVId(i,j) = alm
               ENDDO
            ENDIF
         ELSE
            ALFb(i) = alm
            ALFt(i-1) = alm
            DO j = 1 , ni
               xp = ZCOt(i-1) + (j-.5)*XL(i)/ni
               IF ( xp.GE.ax(2) ) THEN
                  TVId(i,j) = p2*xp + q2
               ELSE
                  TVId(i,j) = p1*xp + q1
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      CONTINUE
      END
!*==S00005.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00005(Iorg)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      KIMp = 0
      i = IZOne
      IF ( i.LT.NZOne ) THEN
         IF ( TEM.GE.TIMimp(i+1) ) i = i + 1
      ENDIF
      dfre = DIFref(i)
      ifr = IFRe(i)
      dmic = DIFmic(i)
      np1 = NPAs1(i)
      np2 = NPAs2(i)
      V0011 = V0011n(i)
      V0012 = V0012x(i)
      IF ( i.EQ.IZOne ) THEN
         ect = TEM - TEMimp
         IF ( ect.LT.dfre ) THEN
            IF ( ifr.LT.10 ) GOTO 100
            IF ( IIMp.GE.np1 ) THEN
               KIMp = 1
            ELSEIF ( ICO.GE.np2 ) THEN
               KIMp = 1
               ICO = 0
            ELSEIF ( ifr.GE.20 ) THEN
               IF ( Iorg.EQ.1 ) KIMp = 1
            ENDIF
         ELSE
            KIMp = 1
            IF ( ifr.LT.10 ) GOTO 100
         ENDIF
      ELSE
         KIMp = 1
         ICO = 0
         IZOne = i
         IF ( ifr.LT.10 ) GOTO 100
      ENDIF
      dtmi = V0011 + 1.D-6
      IF ( DT.GE.dtmi ) THEN
         ICO = 0
      ELSEIF ( ICO.GT.1 ) THEN
         ICO = ICO + 1
      ELSEIF ( Iorg.NE.1 ) THEN
         KIMp = 1
         ICO = ICO + 1
      ENDIF
 100  IIMp = IIMp + 1
      IF ( KIMp.EQ.0 ) THEN
         ect = TEM - TEMmic
         IF ( ect.LT.dmic ) RETURN
         x = TEM/dmic
         ix = INT(x)
         num = NITera - NIMic
         IF ( num.LT.1 ) num = 1
         dtmoy = (TEM-TEMmic)/num
         TEMmic = ix*dmic
         NIMic = NITera
         RETURN
      ELSE
         iok = 0
         IF ( ISOrt.NE.0 ) THEN
            NSTar = NSTar + 1
            IF ( NSTar.GE.N00011(i) ) THEN
               NSTar = 0
               NECrit = NECrit + 1
               iok = 1
            ENDIF
         ENDIF
         IIMp = 0
         x = TEM/dfre
         ix = INT(x)
         TEMimp = ix*dfre
         num = NITera - NIMic
         IF ( num.LT.1 ) num = 1
         dtmoy = (TEM-TEMmic)/num
         TEMmic = TEMimp
         NIMic = NITera
         k = ifr - 10*(ifr/10)
         PRINT 99001 , TEM , NITera
99001    FORMAT (' TEMPS = ',F15.8,' , NITERA : ',I10)
         RETURN
      ENDIF
      END
!*==S55199.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S55199(H,P,T,Rho,Dvdhp,Dvdph,Dtdh,Dtdp)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA a00 , a01 , a02 , a03 , a04 , a05 , a06 , a07 , a08 , a09 ,  &
     &     a010 , a011 , a012/1.0421485616135D-02 ,                     &
     &     3.7989819258871D-04 , -1.0809520514321D-05 ,                 &
     &     1.7199071099232D-07 , -1.7003453661702D-09 ,                 &
     &     1.1093079504746D-11 , -4.9305639341012D-14 ,                 &
     &     1.5141687819488D-16 , -3.2078176823915D-19 ,                 &
     &     4.5968793700556D-22 , -4.2500133969565D-25 ,                 &
     &     2.2858451991157D-28 , -5.4289561756668D-32/
      DATA a10 , a11 , a12 , a13 , a14 , a15 , a16 , a17 , a18 , a19 ,  &
     &     a110 , a111 , a112/7.1558907160166D-06 ,                     &
     &     -4.8989819387822D-07 , 1.3966895742219D-08 ,                 &
     &     -2.2153628233107D-10 , 2.1877549314087D-12 ,                 &
     &     -1.4259859281335D-14 , 6.3323408405408D-17 ,                 &
     &     -1.9428241441369D-19 , 4.1119914445076D-22 ,                 &
     &     -5.8869455392068D-25 , 5.4376738094709D-28 ,                 &
     &     -2.9220477630923D-31 , 6.9343597264571D-35/
      DATA a20 , a21 , a22 , a23 , a24 , a25 , a26 , a27 , a28 , a29 ,  &
     &     a210 , a211 , a212/ - 3.0284448323971D-09 ,                  &
     &     2.0572908444785D-10 , -5.8631773583883D-12 ,                 &
     &     9.2947600855704D-14 , -9.1733812676273D-16 ,                 &
     &     5.9751462062171D-18 , -2.6513621151927D-20 ,                 &
     &     8.1280449373158D-23 , -1.7188520355861D-25 ,                 &
     &     2.4587005585713D-28 , -2.2691496114378D-31 ,                 &
     &     1.2183868386189D-34 , -2.8891726411793D-38/
      DATA a30 , a31 , a32 , a33 , a34 , a35 , a36 , a37 , a38 , a39 ,  &
     &     a310 , a311 , a312/4.1603162475165D-13 ,                     &
     &     -2.8253313464467D-14 , 8.0498643149541D-16 ,                 &
     &     -1.2756275155692D-17 , 1.2583652213261D-19 ,                 &
     &     -8.1918809930522D-22 , 3.6327419331754D-24 ,                 &
     &     -1.1129150871768D-26 , 2.3518675463763D-29 ,                 &
     &     -3.3618179351310D-32 , 3.1004787014482D-35 ,                 &
     &     -1.6636314001942D-38 , 3.9424525577961D-38/
      DATA b00 , b01 , b02 , b03 , b04 , b05 , b06 , b07 , b08 , b09 ,  &
     &     b010 , b011 , b012/3.4799516636947D+01 ,                     &
     &     8.0207756895533D-01 , 5.7134938990324D-03 ,                  &
     &     -9.0888455214870D-05 , 9.0343489356475D-07 ,                 &
     &     -5.9586500435765D-09 , 2.6874936213899D-11 ,                 &
     &     -8.4045091912731D-14 , 1.8181932356958D-16 ,                 &
     &     -2.6657692257362D-19 , 2.5243994056927D-22 ,                 &
     &     -1.3912049726064D-25 , 3.3848430431244D-29/
      DATA b10 , b11 , b12 , b13 , b14 , b15 , b16 , b17 , b18 , b19 ,  &
     &     b110 , b111 , b112/ - 3.3870909274776D-03 ,                  &
     &     3.3749976806899D-05 , -8.3841532461841D-07 ,                 &
     &     1.3908406425265D-08 , -1.5041874666294D-10 ,                 &
     &     1.1068510439562D-12 , -5.6404562404400D-15 ,                 &
     &     1.9963935983267D-17 , -4.8613722666270D-20 ,                 &
     &     7.9478277453504D-23 , -8.3010018206858D-26 ,                 &
     &     4.9898102992625D-29 , -1.3103660877630D-32/
      DATA b20 , b21 , b22 , b23 , b24 , b25 , b26 , b27 , b28 , b29 ,  &
     &     b210 , b211 , b212/ - 2.2063755868780D-07 ,                  &
     &     1.4268968388826D-08 , -3.9416351705981D-10 ,                 &
     &     5.9606582726984D-12 , -5.4871340648895D-14 ,                 &
     &     3.2304069082462D-16 , -1.2421935589492D-18 ,                 &
     &     3.1127728952968D-21 , -4.9182825188505D-24 ,                 &
     &     4.4404482566015D-27 , -1.5665047149903D-30 ,                 &
     &     -5.6075256366923D-34 , 4.6890466767990D-37/
      c0 = a00 +                                                        &
     &     H*(a01+H*(a02+H*(a03+H*(a04+H*(a05+H*(a06+H*(a07+H*(a08+     &
     &     H*(a09+H*(a010+H*(a011+H*a012)))))))))))
      c1 = a10 +                                                        &
     &     H*(a11+H*(a12+H*(a13+H*(a14+H*(a15+H*(a16+H*(a17+H*(a18+     &
     &     H*(a19+H*(a110+H*(a111+H*a112)))))))))))
      c2 = a20 +                                                        &
     &     H*(a21+H*(a22+H*(a23+H*(a24+H*(a25+H*(a26+H*(a27+H*(a28+     &
     &     H*(a29+H*(a210+H*(a211+H*a212)))))))))))
      c3 = a30 +                                                        &
     &     H*(a31+H*(a32+H*(a33+H*(a34+H*(a35+H*(a36+H*(a37+H*(a38+     &
     &     H*(a39+H*(a310+H*(a311+H*a312)))))))))))
      d0 = b00 +                                                        &
     &     H*(b01+H*(b02+H*(b03+H*(b04+H*(b05+H*(b06+H*(b07+H*(b08+     &
     &     H*(b09+H*(b010+H*(b011+H*b012)))))))))))
      d1 = b10 +                                                        &
     &     H*(b11+H*(b12+H*(b13+H*(b14+H*(b15+H*(b16+H*(b17+H*(b18+     &
     &     H*(b19+H*(b110+H*(b111+H*b112)))))))))))
      d2 = b20 +                                                        &
     &     H*(b21+H*(b22+H*(b23+H*(b24+H*(b25+H*(b26+H*(b27+H*(b28+     &
     &     H*(b29+H*(b210+H*(b211+H*b212)))))))))))
      dd0 = b01 +                                                       &
     &      H*(2.*b02+H*(3.*b03+H*(4.*b04+H*(5.*b05+H*(6.*b06+H*(7.*b07+&
     &      H*(8.*b08+H*(9.*b09+H*(10.*b010+H*(11.*b011+12.*b012*H))))))&
     &      ))))
      dd1 = b11 +                                                       &
     &      H*(2.*b12+H*(3.*b13+H*(4.*b14+H*(5.*b15+H*(6.*b16+H*(7.*b17+&
     &      H*(8.*b18+H*(9.*b19+H*(10.*b110+H*(11.*b111+12.*b112*H))))))&
     &      ))))
      dd2 = b21 +                                                       &
     &      H*(2.*b22+H*(3.*b23+H*(4.*b24+H*(5.*b25+H*(6.*b26+H*(7.*b27+&
     &      H*(8.*b28+H*(9.*b29+H*(10.*b210+H*(11.*b211+12.*b212*H))))))&
     &      ))))
      dvdh1 = a01 +                                                     &
     &        H*(2.*a02+H*(3.*a03+H*(4.*a04+H*(5.*a05+H*(6.*a06+H*(7.*  &
     &        a07+                                                      &
     &        H*(8.*a08+H*(9.*a09+H*(10.*a010+H*(11.*a011+H*12.*a012))))&
     &        ))))))
      dvdh2 = a11 +                                                     &
     &        H*(2.*a12+H*(3.*a13+H*(4.*a14+H*(5.*a15+H*(6.*a16+H*(7.*  &
     &        a17+                                                      &
     &        H*(8.*a18+H*(9.*a19+H*(10.*a110+H*(11.*a111+H*12.*a112))))&
     &        ))))))
      dvdh3 = a21 +                                                     &
     &        H*(2.*a22+H*(3.*a23+H*(4.*a24+H*(5.*a25+H*(6.*a26+H*(7.*  &
     &        a27+                                                      &
     &        H*(8.*a28+H*(9.*a29+H*(10.*a210+H*(11.*a211+H*12.*a212))))&
     &        ))))))
      dvdh4 = a31 +                                                     &
     &        H*(2.*a32+H*(3.*a33+H*(4.*a34+H*(5.*a35+H*(6.*a36+H*(7.*  &
     &        a37+                                                      &
     &        H*(8.*a38+H*(9.*a39+H*(10.*a310+H*(11.*a311+H*12.*a312))))&
     &        ))))))
      Dvdhp = dvdh1 + P*(dvdh2+P*(dvdh3+P*dvdh4))
      Dvdph = c1 + 2.*P*c2 + 3.*P**2*c3
      v = c0 + P*(c1+P*(c2+P*c3))
      T = d0 + P*(d1+P*d2)
      Dtdh = dd0 + P*(dd1+P*dd2)
      Dtdp = d1 + 2.*d2*P
      Rho = 1./v
      CONTINUE
      END
!*==S55198.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S55198(H,Pp,T,Rho,Dvdhp,Dvdph,Dtdh,Dtdp)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA a00 , a01 , a02 , a03 , a04 , a05 , a06 , a07 , a08 ,        &
     &     a09/6170.78663883D+00 , 489.886451913D+00 ,                  &
     &     -1.71011901617D+00 , .00310665805845D+00 ,                   &
     &     -34.6610591433D-07 , 24.1062845537D-10 , -10.411445494D-13 , &
     &     27.0989223969D-17 , -38.9010136647D-21 , 23.654728597D-25/
      DATA a10 , a11 , a12 , a13 , a14 , a15 , a16 , a17 , a18 ,        &
     &     a19/ - 27.2234538756D+00 , -1.55945023546D+00 ,              &
     &     .0055688166592D+00 , -10.1907487805D-06 , 11.3988461203D-09 ,&
     &     -79.2905387618D-13 , 34.2019458067D-16 , -88.8161818151D-20 ,&
     &     12.7093552905D-23 , -76.9751114847D-28/
      DATA a20 , a21 , a22 , a23 , a24 , a25 , a26 , a27 , a28 ,        &
     &     a29/.0400334696045D+00 , .0019832294859D+00 ,                &
     &     -72.2377802606D-07 , 13.3067830456D-09 , -14.917384566D-12 , &
     &     10.3760380082D-15 , -44.6915456261D-19 , 11.5763883026D-22 , &
     &     -16.5086089907D-26 , 99.5533322201D-31/
      DATA a30 , a31 , a32 , a33 , a34 , a35 , a36 , a37 , a38 ,        &
     &     a39/ - 26.7334101475D-06 , -12.5868723956D-07 ,              &
     &     46.6552793801D-10 , -86.4549289781D-13 , 97.1047694917D-16 , &
     &     -67.5258399441D-19 , 29.036844039D-22 , -75.0092619426D-26 , &
     &     10.6572651116D-29 , -63.9675459557D-34/
      DATA a40 , a41 , a42 , a43 , a44 , a45 , a46 , a47 , a48 ,        &
     &     a49/87.7523620691D-10 , 39.8481772183D-11 ,                  &
     &     -15.0016095749D-13 , 27.947539761D-16 , -31.4412831D-19 ,    &
     &     21.8544044613D-22 , -93.8062091533D-26 , 24.1619703535D-29 , &
     &     -34.1939584316D-33 , 20.421351707D-37/
      DATA a50 , a51 , a52 , a53 , a54 , a55 , a56 , a57 , a58 ,        &
     &     a59/ - 11.4250142505D-13 , -50.3259455874D-15 ,              &
     &     19.2105899709D-17 , -35.9600016574D-20 , 40.5103597804D-23 , &
     &     -28.1410621514D-26 , 12.0553990633D-29 , -30.9558667049D-33 ,&
     &     43.6260674D-37 , -25.9155193335D-38/
      DATA b00 , b01 , b02 , b03 , b04 , b05 , b06 ,                    &
     &     b07/445.665390762D+00 , -2.22263915916D+00 ,                 &
     &     .00472884372253D+00 , -55.6382273478D-07 ,                   &
     &     39.0978422275D-10 , -16.4098861643D-13 , 38.0911781785D-17 , &
     &     -37.7244080484D-21/
      DATA b10 , b11 , b12 , b13 , b14 , b15 , b16 ,                    &
     &     b17/ - 1093412.10859D+00 , 5392.48720906D+00 ,               &
     &     -11.3504501908D+00 , .0132165571029D+00 ,                    &
     &     -91.9374308338D-07 , 38.2114755965D-10 , -87.8718361625D-14 ,&
     &     86.2568295293D-18/
      DATA b20 , b21 , b22 , b23 , b24 , b25 , b26 ,                    &
     &     b27/473986630.09D+00 , -2303848.6116D+00 ,                   &
     &     4783.56767793D+00 , -5.50018860399D+00 ,                     &
     &     .00378235279254D+00 , -15.556306052D-07 , 35.4309477645D-11 ,&
     &     -34.4728839248D-15/
      DATA b30 , b31 , b32 , b33 , b34 , b35 , b36 ,                    &
     &     b37/ - 59101208761.4D+00 , 285687397.896D+00 ,               &
     &     -590083.283098D+00 , 675.104420712D+00 , -.46204835791D+00 , &
     &     .000189172522631D+00 , -42.8993080892D-09 ,                  &
     &     41.5668966327D-13/
      DATA b40 , b41 , b42 , b43 , b44 , b45 , b46 ,                    &
     &     b47/3147752810980.D+00 , -15146899832.4D+00 ,                &
     &     31147138.3352D+00 , -35480.7250254D+00 , 24.1808570239D+00 , &
     &     -.00985946157921D+00 , 22.2693241999D-07 ,                   &
     &     -21.4941089448D-11/
      DATA b50 , b51 , b52 , b53 , b54 , b55 , b56 ,                    &
     &     b57/ - 75347916606600.D+00 , 361189325093.D+00 ,             &
     &     -739923492.971D+00 , 839728.397005D+00 , -570.189938069D+00 ,&
     &     .231648962465D+00 , -52.1369298193D-06 , 50.1481876596D-10/
      DATA b60 , b61 , b62 , b63 , b64 , b65 , b66 ,                    &
     &     b67/663617137215000.D+00 , -3171123872380.D+00 ,             &
     &     6475932094.22D+00 , -7326561.7806D+00 , 4959.5175126D+00 ,   &
     &     -2.00874787721D+00 , .000450750907579D+00 ,                  &
     &     -43.2282398141D-09/
      p = Pp
      IF ( p.LT.25. ) p = 25.D+00
      a0 = a00 +                                                        &
     &     p*(a01+p*(a02+p*(a03+p*(a04+p*(a05+p*(a06+p*(a07+p*(a08+     &
     &     p*(a09)))))))))
      a1 = a10 +                                                        &
     &     p*(a11+p*(a12+p*(a13+p*(a14+p*(a15+p*(a16+p*(a17+p*(a18+     &
     &     p*(a19)))))))))
      a2 = a20 +                                                        &
     &     p*(a21+p*(a22+p*(a23+p*(a24+p*(a25+p*(a26+p*(a27+p*(a28+     &
     &     p*(a29)))))))))
      a3 = a30 +                                                        &
     &     p*(a31+p*(a32+p*(a33+p*(a34+p*(a35+p*(a36+p*(a37+p*(a38+     &
     &     p*(a39)))))))))
      a4 = a40 +                                                        &
     &     p*(a41+p*(a42+p*(a43+p*(a44+p*(a45+p*(a46+p*(a47+p*(a48+     &
     &     p*(a49)))))))))
      a5 = a50 +                                                        &
     &     p*(a51+p*(a52+p*(a53+p*(a54+p*(a55+p*(a56+p*(a57+p*(a58+     &
     &     p*(a59)))))))))
      da0 = a01 +                                                       &
     &      p*(2.*a02+p*(3.*a03+p*(4.*a04+p*(5.*a05+p*(6.*a06+p*(7.*a07+&
     &      p*(8.*a08+9.*a09*p)))))))
      da1 = a11 +                                                       &
     &      p*(2.*a12+p*(3.*a13+p*(4.*a14+p*(5.*a15+p*(6.*a16+p*(7.*a17+&
     &      p*(8.*a18+9.*a19*p)))))))
      da2 = a21 +                                                       &
     &      p*(2.*a22+p*(3.*a23+p*(4.*a24+p*(5.*a25+p*(6.*a26+p*(7.*a27+&
     &      p*(8.*a28+9.*a29*p)))))))
      da3 = a31 +                                                       &
     &      p*(2.*a32+p*(3.*a33+p*(4.*a34+p*(5.*a35+p*(6.*a36+p*(7.*a37+&
     &      p*(8.*a38+9.*a39*p)))))))
      da4 = a41 +                                                       &
     &      p*(2.*a42+p*(3.*a43+p*(4.*a44+p*(5.*a45+p*(6.*a46+p*(7.*a47+&
     &      p*(8.*a48+9.*a49*p)))))))
      da5 = a51 +                                                       &
     &      p*(2.*a52+p*(3.*a53+p*(4.*a54+p*(5.*a55+p*(6.*a56+p*(7.*a57+&
     &      p*(8.*a58+9.*a59*p)))))))
      T = a0 + H*(a1+H*(a2+H*(a3+H*(a4+H*(a5)))))
      Dtdh = a1 + H*(2.*a2+H*(3.*a3+H*(4.*a4+5.*a5*H)))
      Dtdp = da0 + H*(da1+H*(da2+H*(da3+H*(da4+H*da5))))
      b0 = b00 + H*(b01+H*(b02+H*(b03+H*(b04+H*(b05+H*(b06+H*(b07)))))))
      b1 = b10 + H*(b11+H*(b12+H*(b13+H*(b14+H*(b15+H*(b16+H*(b17)))))))
      b2 = b20 + H*(b21+H*(b22+H*(b23+H*(b24+H*(b25+H*(b26+H*(b27)))))))
      b3 = b30 + H*(b31+H*(b32+H*(b33+H*(b34+H*(b35+H*(b36+H*(b37)))))))
      b4 = b40 + H*(b41+H*(b42+H*(b43+H*(b44+H*(b45+H*(b46+H*(b47)))))))
      b5 = b50 + H*(b51+H*(b52+H*(b53+H*(b54+H*(b55+H*(b56+H*(b57)))))))
      b6 = b60 + H*(b61+H*(b62+H*(b63+H*(b64+H*(b65+H*(b66+H*(b67)))))))
      db0 = b01 +                                                       &
     &      H*(2.*b02+H*(3.*b03+H*(4.*b04+H*(5.*b05+H*(6.*b06+H*(7.*b07)&
     &      )))))
      db1 = b11 +                                                       &
     &      H*(2.*b12+H*(3.*b13+H*(4.*b14+H*(5.*b15+H*(6.*b16+H*(7.*b17)&
     &      )))))
      db2 = b21 +                                                       &
     &      H*(2.*b22+H*(3.*b23+H*(4.*b24+H*(5.*b25+H*(6.*b26+H*(7.*b27)&
     &      )))))
      db3 = b31 +                                                       &
     &      H*(2.*b32+H*(3.*b33+H*(4.*b34+H*(5.*b35+H*(6.*b36+H*(7.*b37)&
     &      )))))
      db4 = b41 +                                                       &
     &      H*(2.*b42+H*(3.*b43+H*(4.*b44+H*(5.*b45+H*(6.*b46+H*(7.*b47)&
     &      )))))
      db5 = b51 +                                                       &
     &      H*(2.*b52+H*(3.*b53+H*(4.*b54+H*(5.*b55+H*(6.*b56+H*(7.*b57)&
     &      )))))
      db6 = b61 +                                                       &
     &      H*(2.*b62+H*(3.*b63+H*(4.*b64+H*(5.*b65+H*(6.*b66+H*(7.*b67)&
     &      )))))
      r = 1./p
      v = b0 + r*(b1+r*(b2+r*(b3+r*(b4+r*(b5+r*(b6))))))
      Dvdph = -                                                         &
     &        r**2*(b1+r*(2.*b2+r*(3.*b3+r*(4.*b4+r*(5.*b5+r*(6.*b6)))))&
     &        )
      Dvdhp = db0 + r*(db1+r*(db2+r*(db3+r*(db4+r*(db5+r*(db6))))))
      IF ( Pp.LT.25. ) THEN
         p = Pp
         IF ( p.LT.0.1 ) p = 0.1
         v = v*25./p
         Dvdhp = Dvdhp*25./p
         Dvdph = Dvdph*(25./p)**2
         Rho = 1./v
         RETURN
      ELSE
         Rho = 1./v
         RETURN
      ENDIF
      END
!*==F88345.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE F88345(G,Alfb,Alft,Vg,Vf,T,Sig,Fluxg,Fluxf,Alj,Vgj,P,Q,&
     &                  Icas,Gl1,Gl2,Izw)
      IMPLICIT DOUBLE PRECISION(a-H,O-Z)
      Z1(x) = (1.-x)*(Alft-x) + x*(x-Alfb)
      Z2(x) = x*(Alft-x)/Vg - x*(x-Alfb)/Vf
      rf = 1./Vf
      rg = 1./Vg
      grav = 32.174
      gc = 32.174
      a1 = .67
      a2 = .47
      b1 = 1./a1
      b2 = 1./a2
      n = 0
      nitm = 15
      Izw = 0
      vfdvg = Vf/Vg
      Sig = SIGMA(T)
      tcc = Sig*gc*grav*(rf-rg)
      tcc = Vf*DSQRT(tcc)
      vbcr = 1.53*DSQRT(tcc)
      alo = .925*(Vf/Vg)**.239D+00
      alsu = 1. - 1.D-7
      Gl1 = 1.D+6
      Gl2 = -1.D+6
      IF ( G.GE.0. ) THEN
         Icas = 1
         Izw = 1
         Alj = Alfb
      ELSE
         Icas = 3
         Izw = 2
         Alj = Alft
      ENDIF
      Vgj = S00098(vbcr,alo,Alj)
      b = b1
      IF ( Alfb.GE.alo ) b = b2
      Gl1 = ((Alfb/alo)**b)*rg*vbcr
      Gl2 = -rf*S00098(vbcr,alo,Alft)
      IF ( Alfb.GE.1.D-7 .AND. Alft.LE.alsu ) THEN
         IF ( G.LT.Gl1 .AND. G.GT.Gl2 ) THEN
            Icas = 2
            Izw = 3
            n = 1
            alfst = (rg*Alft+rf*Alfb)/(rg+rf)
            albt = DABS(Alft-Alfb)
            IF ( albt.GE.1.D-3 ) THEN
               al1 = DMIN1(Alfb,Alft)
               al2 = DMAX1(Alfb,Alft)
               vg1 = S00098(vbcr,alo,al1)
               vg2 = S00098(vbcr,alo,al2)
               vgst = S00098(vbcr,alo,alfst)
               x = al1
               f1 = (vg1*Z2(x)-G*Z1(x))/albt
               x = al2
               f2 = (vg2*Z2(x)-G*Z1(x))/albt
               x = alfst
               fst = (vgst*Z2(x)-G*Z1(x))/albt
               IF ( DABS(fst).GT.1.D-04 ) THEN
                  IF ( fst.GT.0. ) THEN
                     al1 = alfst
                     f1 = fst
                  ELSE
                     al2 = alfst
                     f2 = fst
                  ENDIF
 5                n = n + 1
                  x = (al1*f2-al2*f1)/(f2-f1)
                  Vgj = S00098(vbcr,alo,x)
                  IF ( (al2-al1).GE.1.D-7 ) THEN
                     f = (Vgj*Z2(x)-G*Z1(x))/albt
                     IF ( DABS(f).GT.1.D-04 ) THEN
                        IF ( f.GT.0. ) THEN
                           al1 = x
                           f1 = f
                        ELSE
                           al2 = x
                           f2 = f
                        ENDIF
                        IF ( n.LT.nitm ) GOTO 5
                        Alj = (al1*f2-al2*f1)/(f2-f1)
                        Vgj = S00098(vbcr,alo,x)
                        GOTO 100
                     ELSE
                        Alj = x
                        IF ( DABS(f).GE.1.D-8 ) THEN
                           IF ( f.GT.0. ) THEN
                              al1 = x
                              f1 = f
                           ELSE
                              al2 = x
                              f2 = f
                           ENDIF
                           Alj = (al1*f2-al2*f1)/(f2-f1)
                           Vgj = S00098(vbcr,alo,x)
                        ENDIF
                        GOTO 100
                     ENDIF
                  ELSE
                     Alj = x
                     GOTO 100
                  ENDIF
               ELSE
                  Alj = alfst
                  Vgj = vgst
                  GOTO 100
               ENDIF
            ELSE
               Alj = (Alfb+Alft)/2.D+00
               Vgj = S00098(vbcr,alo,Alj)
               Izw = 4
               GOTO 100
            ENDIF
         ENDIF
      ENDIF
      rhob = Alj*rg + (1.-Alj)*rf
      Vgj = S00098(vbcr,alo,Alj)
      Fluxg = (G+rf*Vgj)*Alj/rhob
      Fluxf = (G*(1.-Alj)-rg*Vgj*Alj)/rhob
      P = rg*Alj/rhob
      Q = rf*rg*Vgj*Alj/rhob
      IF ( G.GE.0. .AND. Fluxf.LT.0. ) THEN
         Izw = 0
         Icas = 1
         IF ( G.LT.0. ) THEN
            Gl2 = -1.D+6
            Icas = 3
         ENDIF
         Alj = 1.D+00
         Fluxg = G*Vg
         Fluxf = 0.D+00
         P = 1.D+00
         Q = 0.D+00
         Vgj = 0.D+00
         RETURN
      ELSEIF ( G.LT.0. .AND. Fluxg.GT.0. ) THEN
         Izw = 0
         Gl1 = 1.D+6
         Icas = 1
         IF ( G.LT.0. ) Icas = 3
         Alj = 0.D+00
         Fluxg = 0.D+00
         Fluxf = G*Vf
         P = 0.D+00
         Q = 0.D+00
         Vgj = 0.D+00
         RETURN
      ELSE
         RETURN
      ENDIF
 100  rhob = Alj*rg + (1.-Alj)*rf
      Fluxg = (G+rf*Vgj)*Alj/rhob
      Fluxf = (G*(1.-Alj)-rg*Vgj*Alj)/rhob
      x = Alj
      dz2 = -2.*x*(rg+rf) + Alft*rg + Alfb*rf
      dz1 = 4.*x - 1. - Alfb - Alft
      djgg = x/rhob
      a = a1
      IF ( x.GE.alo ) a = a2
      eb = 1./a - 1.D+00
      xvgja = vbcr*(x/alo)**eb*(-x+(1.-x)*(1.-a)/a)/alo
      djgal = rf*(xvgja+(G+rf*Vgj)/rhob)/rhob
      z2vgja = ((Alft-x)/Vg+(Alfb-x)/Vf)*xvgja
      dalg = Z1(x)/(dz2*Vgj+z2vgja-G*dz1)
      P = rg*(djgg+djgal*dalg)
      Q = rg*Fluxg - P*G
      CONTINUE
      END
!*==SI.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      DOUBLE PRECISION FUNCTION SI(Xtbl,Ytbl,Xx,Nn,Ind)
      DOUBLE PRECISION Xx
      DOUBLE PRECISION Xtbl(32) , Ytbl(32)
      Ind = 0
      x = Xx
      n = Nn
      IF ( x.LT.Xtbl(1) ) THEN
         Ind = 1
         ii = 2
      ELSEIF ( x.EQ.Xtbl(1) ) THEN
         ii = 2
      ELSEIF ( Xtbl(n).LT.x ) THEN
         Ind = 2
         ii = n
      ELSEIF ( Xtbl(n).EQ.x ) THEN
         ii = n
      ELSE
         DO ik = 2 , n
            ii = ik
            IF ( Xtbl(ik).GE.x ) GOTO 100
         ENDDO
      ENDIF
 100  x1 = Xtbl(ii-1)
      x2 = Xtbl(ii)
      y1 = Ytbl(ii-1)
      y2 = Ytbl(ii)
      SI = y1 + (y2-y1)*(x-x1)/(x2-x1)
      CONTINUE
      END
!*==S00093.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      DOUBLE PRECISION FUNCTION S00093(Pwl,Twall)
      IMPLICIT DOUBLE PRECISION(a-H,O-Z)
      DOUBLE PRECISION tabpwl(32) , taba(32) , tabb(32)
      DOUBLE PRECISION a , b
      DATA tabpwl/0.000 , 0.007 , 0.010 , 0.012 , 0.015 , 0.020 ,       &
     &     0.025 , 0.030 , 0.035 , 0.040 , 0.050 , 0.060 , 0.070 ,      &
     &     0.080 , 0.100 , 0.120 , 0.150 , 0.200 , 0.250 , 0.300 ,      &
     &     0.400 , 0.500 , 0.600 , 0.800 , 1.000 , 1.200 , 1.500 ,      &
     &     2.000 , 3.000 , 5.000 , 10.000 , 20.000/
      DATA taba/0.000950 , 0.000936 , 0.000890 , 0.000898 , 0.000890 ,  &
     &     0.000856 , 0.000808 , 0.000778 , 0.000769 , 0.000758 ,       &
     &     0.000720 , 0.000689 , 0.000678 , 0.000671 , 0.000620 ,       &
     &     0.000600 , 0.000565 , 0.000523 , 0.000501 , 0.000486 ,       &
     &     0.000452 , 0.000447 , 0.000416 , 0.000392 , 0.000384 ,       &
     &     0.000371 , 0.000354 , 0.000335 , 0.000322 , 0.000307 ,       &
     &     0.000287 , 0.000275/
      DATA tabb/0.029 , 0.037 , 0.047 , 0.056 , 0.067 , 0.080 , 0.090 , &
     &     0.096 , 0.108 , 0.120 , 0.132 , 0.144 , 0.156 , 0.168 ,      &
     &     0.184 , 0.200 , 0.220 , 0.245 , 0.270 , 0.295 , 0.325 ,      &
     &     0.355 , 0.375 , 0.410 , 0.450 , 0.480 , 0.510 , 0.550 ,      &
     &     0.620 , 0.700 , 0.800 , 0.910/
      a = SI(tabpwl(1),taba(1),Pwl,32,ind)
      b = SI(tabpwl(1),tabb(1),Pwl,32,ind)
      S00093 = b*DEXP(-1.0*a*Twall)
      CONTINUE
      END
!*==SIGMA.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      DOUBLE PRECISION FUNCTION SIGMA(T)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA b0 , b1 , b2 , b3 , b4 , b5/0.83D+00 , 1.160936807D-1 ,      &
     &     1.121404688D-3 , -5.752805180D-6 , 1.286274650D-8 ,          &
     &     -1.149719290D-11/
      tc = (T-32.)*5./9.D+00
      tk = tc + 273.15
      dtk = 647.3D+00 - tk
      dtk = DMAX1(1.D-6,dtk)
      SIGMA = b1*dtk*dtk/(1.+b0*dtk)
      SIGMA = SIGMA + dtk*dtk*(b2+dtk*(b3+dtk*(b4+dtk*b5)))
      SIGMA = SIGMA*12.*14.5037738D-6/2.540005
      CONTINUE
      END
!*==S00098.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      DOUBLE PRECISION FUNCTION S00098(Vbcr,Alo,Al)
      IMPLICIT DOUBLE PRECISION(a-H,O-Z)
      a = .67
      IF ( Al.GE.Alo ) a = .47
      b = 1./a
      c = b - 1.D+00
      rap = (Al/Alo)**c
      S00098 = Vbcr*(1.-Al)*rap/Alo
      CONTINUE
      END
!*==S00002.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00002
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      COMMON /AAA33 / VCO , XL0055 , D876 , DINt , DEXt , VOL002 ,      &
     &                VOL005 , LCO , NCRay
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA88 / FLUp(22,2) , FLUv(22,2) , FLUl(22,2) , FLUiv(22,2)&
     &                , DEBil(22,2) , FLUil(22,2) , DEBi(22) , DEBhi(22)&
     &                , DEBiv(22,2)
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(22,12) , HVCff(22)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA16 / DBEnt(20) , TENt(20) , HENt(20) , DEBe , ENTe ,   &
     &                DBSor(20) , TSOr(20) , DEBs , DPDtt(20) ,         &
     &                TDPdt(20) , HSOr(20) , THSor(20) , ENTs , NDBent ,&
     &                NDPdt , NDBsor , NHSor
      COMMON /AAA17 / DDPdt , DDH(22,2) , DDV(22,2) , DDM(22,2) ,       &
     &                DDU(22,2)
      COMMON /AAA18 / DDEb(21) , DDDeb(21) , DXAt(21) , DXAg(21) ,      &
     &                DXBt(21) , DXBg(21) , DNUg(21) , DNUf(21) ,       &
     &                DHE(22,2) , DDEbo(21)
      COMMON /AAA20 / DQP(22,2) , DFLul(22) , DFLuv(22) , DFLuil(22) ,  &
     &                DFLuiv(22) , DDEbil(22) , DDEbiv(22) , DQCei(2) , &
     &                DQCed(22,12) , DDTuo(22,12) , DDTga(22,12) ,      &
     &                DDTmi(2) , DDTmu(2)
      CHARACTER*2 ititre(8)
      DOUBLE PRECISION M
      READ (5,99001) (ititre(i),i=1,8)
99001 FORMAT (8A2)
      READ (5,99006) KPRess , KDEb , KCOn , KTGai , KT876 , KGLiss ,    &
     &               (ICAt(i),i=1,3)
      READ (5,99014) NZOne
      DO i = 1 , NZOne
         READ (5,99007) IFRe(i) , TIMimp(i) , DIFref(i) , DIFmic(i) ,   &
     &                  NPAs1(i) , NPAs2(i) , V0011n(i) , V0012x(i)
99007    FORMAT (I2,8X,3F10.4,2(5X,I5),2F10.6)
      ENDDO
      READ (5,99008) (N00011(i),i=1,NZOne)
99008 FORMAT (40I2)
      V0011 = V0011n(1)
      V0012 = V0012x(1)
      READ (5,99010) DELm , DELv , DELvi , DELvs
      READ (5,99010) V00001 , V22201 , V22202 , V22203
      READ (5,99010) TLIm , ZLImin , ZLImax
      READ (5,99006) NMUltr
      DELm = DELm/100.D+00
      DELv = DELv/100.D+00
      DELvi = DELvi/100.D+00
      DELvs = DELvs/100.D+00
      VFIx = 1./12.D+00
      READ (5,99012) NCRay
99012 FORMAT (I5)
      READ (5,99010) R876 , XL876 , CP876
      READ (5,99010) RGAi , XLGai , CPGai
      READ (5,99010) D876 , DINt , DEXt
      D876 = D876/12.D+00
      DINt = DINt/12.D+00
      DEXt = DEXt/12.D+00
      READ (5,99013) TEM , P , QINit , NC , IBCh , IHCh
99013 FORMAT (3F10.4,3(I2,8X))
      NC1 = NC + 1
      NC2 = NC + 2
      XL0055 = 0.D+00
      ITYp(1) = 1
      DO i = 2 , NC1
         READ (5,99015) ITYp(i) , H(i,1) , H(i,2) , xl1 , xl2 , SCApa(i)
99015    FORMAT (I1,9X,6F10.4)
         IF ( ITYp(i).EQ.2 ) THEN
            XL(i) = xl1
            VC(i) = xl1*SCApa(i)
            V(i,1) = VC(i)*(1.-xl2)
            V(i,2) = VC(i)*xl2
         ELSEIF ( ITYp(i).EQ.3 ) THEN
            XL(i) = xl1 + xl2
            V(i,1) = xl1*SCApa(i)
            V(i,2) = xl2*SCApa(i)
            VC(i) = V(i,1) + V(i,2)
            HNIv(i) = xl1
         ELSE
            XL(i) = xl1
            V(i,1) = xl1*SCApa(i)
            VC(i) = V(i,1)
         ENDIF
      ENDDO
      ITYp(NC2) = 1
      H(NC2,2) = 0.D+00
      VC(NC2) = 1.D+00
      V(NC2,2) = 1.D+00
      VC(1) = 1.D+00
      V(1,1) = 1.D+00
      DO i = IBCh , IHCh
         XL0055 = XL0055 + XL(i)
      ENDDO
      LCO = XL0055
      DO ii = 1 , NC2
         i = NC2 + 1 - ii
         h1 = H(i,1)
         h2 = H(i,2)
         h3 = 0.D+00
         v1 = V(i,1)
         v2 = V(i,2)
         v3 = 0.D+00
      ENDDO
      IF ( KDEb.EQ.1 ) READ (5,99016) SDSc
      READ (5,99014) NDBent
      READ (5,99011) (TENt(i),DBEnt(i),HENt(i),i=1,NDBent)
99011 FORMAT (3F10.4)
      READ (5,99014) NHSor
      READ (5,99016) (THSor(i),HSOr(i),i=1,NHSor)
      IF ( KPRess.EQ.1 ) THEN
         READ (5,99014) NDPdt
         READ (5,99016) (TDPdt(i),DPDtt(i),i=1,NDPdt)
      ELSE
         READ (5,99014) NDBsor
         READ (5,99016) (TSOr(i),DBSor(i),i=1,NDBsor)
      ENDIF
      READ (5,99014) NPUi
      READ (5,99016) (TPUi(i),QPUi(i),i=1,NPUi)
      READ (5,99005) (HGApp(i),i=2,NC1)
      DO i = 2 , NC1
         HGApp(i) = HGApp(i)/3600.D+00
      ENDDO
      READ (5,99017) (NSL(i),i=2,NC1)
99017 FORMAT (8(I2,8X))
      DO i = 2 , NC1
         n1 = NSL(i)
         NASl(i) = NSL(i)
         DO j = 1 , n1
            READ (5,99003) PL(i,j) , T876(i,j) , TGAi(i,j)
99003       FORMAT (3F10.4)
         ENDDO
      ENDDO
      READ (5,99004) HLCf , hvcf , HVAp , HCOn , HVIn , HLIn
99004 FORMAT (6F10.4)
      HLCf = HLCf/3600.D+00
      hvcf = hvcf/3600.D+00
      HVAp = HVAp/3600.D+00
      HCOn = HCOn/3600.D+00
      HVIn = HVIn/3600.D+00
      HLIn = HLIn/3600.
      HVCfo = hvcf
      TEMimp = TEM - 10000.
      TEMmic = TEMimp
      IZOne = 0
      IF ( TEM.GE.TIMimp(1) ) THEN
 650     IZOne = IZOne + 1
         IF ( IZOne.LT.NZOne ) THEN
            IF ( TEM.GT.TIMimp(IZOne+1) ) GOTO 650
         ENDIF
      ENDIF
      IIMp = 0
      ICO = 0
      DT = V0011
      NIV = 0
      ISLni = 0
      hen = H(IBCh,1)
      READ (5,99005) (SC(i),i=1,NC1)
      VCO = 0.
      DO i = IBCh , IHCh
         VCO = VCO + VC(i)
      ENDDO
      ZNIv = 0.
      ica = ICAt(2)
      IF ( ica.GT.2 ) THEN
         DO i = 2 , NC1
            IF ( ITYp(i).GE.3 ) THEN
               ZNIv = ZNIv + HNIv(i)
               NIV = i
               GOTO 800
            ELSE
               ZNIv = ZNIv + XL(i)
            ENDIF
         ENDDO
      ENDIF
 800  ZCOt(IBCh-1) = 0
      IF ( IBCh.GT.2 ) THEN
         ibc = IBCh - 1
         DO i = 2 , ibc
            j = IBCh - i
            ZCOt(j) = ZCOt(j+1) - XL(j+1)
         ENDDO
      ENDIF
      DO i = IBCh , NC1
         ZCOt(i) = ZCOt(i-1) + XL(i)
      ENDDO
      ZNIv = ZNIv + ZCOt(1)
      RETURN
99002 FORMAT (/,10X,8A2)
99005 FORMAT (8F10.4)
99006 FORMAT (6(I1,9X),3I1)
99009 FORMAT (I1,4X,2I5,5X,3F10.4)
99010 FORMAT (5F10.4)
99014 FORMAT (I2)
99016 FORMAT (2F10.4)
99018 FORMAT (2I3,3X,F7.4,5(2X,F8.4),3(2X,F10.4))
99019 FORMAT (18X,F10.4,4X,F10.4)
99020 FORMAT ((14X,3(4X,F10.4)))
99021 FORMAT ((4X,I2,4X,3F10.4,2(3X,I5,2X),2F10.6))
      END
!*==X21Y21.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE X21Y21(X,Y)
      DOUBLE PRECISION X(21) , Y(21)
      DOUBLE PRECISION xx(21) , yy(21)
      DATA xx/0.2 , 200. , 400. , 600. , 800. , 1000. , 1500. , 2000. , &
     &     2500. , 3000. , 3500. , 4000. , 6000. , 8000. , 10000. ,     &
     &     15000. , 20000. , 25000. , 30000. , 40000. , 50000./
      DATA yy/0. , 69.31 , 120.68 , 166.92 , 210.12 , 251.19 , 347.43 , &
     &     437.34 , 522.82 , 604.92 , 684.31 , 761.46 , 1053.22 ,       &
     &     1325.78 , 1584.89 , 2192.16 , 2759.46 , 3298.77 , 3816.78 ,  &
     &     4804.5 , 5743.49/
      DO i = 1 , 21
         X(i) = xx(i)
         Y(i) = yy(i)
      ENDDO
      CONTINUE
      END
!*==S00096.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00096(Pp,Hf,Hg,T,Vf,Vg,Dvfdp,Dvgdp,Dhfdp,Dhgdp,Dtsat)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA a0 , a1 , a2 , a3 , a4 , a5/0.0168896951589D+00 ,            &
     &     89.7563708097D-7 , -95.3392512069D-10 , 76.4433456620D-13 ,  &
     &     -28.1881717776D-16 , 40.5307653492D-20/
      DATA b0 , b1 , b2 , b3 , b4 , b5 , b6 , b7/300.12985208D+00 ,     &
     &     -252.638119855D+00 , 97.9981947188D+00 , -22.4207151267D+00 ,&
     &     3.22500188803D+00 , -0.288260253918D+00 ,                    &
     &     .0146831531571D+00 , -32.6352964583D-5/
      DATA c0 , c1 , c2 , c3 , c4/71.5171806566D+00 ,                   &
     &     56.4133540077D+00 , -3.73181288419D+00 , .737563606575D+00 , &
     &     .00809293492177D+00/
      DATA d0 , d1 , d2 , d3 , d4 , d5/ - 501.911804844D+00 ,           &
     &     643.013652461D+00 , -254.053681062D+00 , 53.4172368776D+00 , &
     &     -5.47296691662D+00 , .227574393371D+00/
      DATA e0 , e1 , e2 , e3 , e4 , e5 , e6/ - 983.676282822D+00 ,      &
     &     2649.64781997D+00 , -1365.75884763D+00 , 373.419845898D+00 , &
     &     -56.7876969779D+00 , 4.56687019374D+00 , -.152528030277D+00/
      DATA dd0 , dd1 , dd2 , dd3/ - 310295.234948D+00 ,                 &
     &     121447.323002D+00 , -15841.5422032D+00 , 690.18449767D+00/
      DATA ee0 , ee1 , ee2 , ee3/464392.898586D+00 ,                    &
     &     -181216.034989D+00 , 23651.1207875D+00 , -1029.9067531D+00/
      DATA cc0 , cc1 , cc2 , cc3 , cc4 , cc5/ - 0.603754D+00 ,          &
     &     -0.195861D+00 , 1.90488D-2 , -8.98731D-4 , 1.93828D-5 ,      &
     &     -1.59293D-7/
      DATA aa0 , aa1 , aa2 , aa3 , aa4 , aa5/ - 9.62997D-6 ,            &
     &     -6.26013D-7 , 5.86957D-8 , -2.84574D-9 , 6.23361D-11 ,       &
     &     -5.23200D-13/
      DATA bb0 , bb1 , bb2 , bb3/0.834155D+00 , 392.019D+00 ,           &
     &     -173.129D+00 , 161.554D+00/
      DATA df0 , df1 , df2 , df3 , df4 , df5/ - 0.640476D+00 ,          &
     &     -0.194453D+00 , 1.89171D-2 , -8.92998D-4 , 1.92670D-5 ,      &
     &     -1.58399D-7/
      DATA eg0 , eg1 , eg2 , eg3 , eg4 , eg5/ - 9.33315D-2 ,            &
     &     -8.71131D-2 , 8.43181D-3 , -3.95457D-4 , 8.50339D-6 ,        &
     &     -6.97062D-8/
      DATA ta , vfa , hfa , hga/281.03D+00 , 0.017269D+00 , 250.24D+00 ,&
     &     1174.4D+0/
      p = Pp
      IF ( p.GE.41. ) THEN
         w = DLOG(DABS(p))
         rp = 1./p
         IF ( p.GE.2005. ) THEN
            Hf = dd0 + w*(dd1+w*(dd2+w*dd3))
            Hg = ee0 + w*(ee1+w*(ee2+w*ee3))
         ELSE
            Hf = d0 + w*(d1+w*(d2+w*(d3+w*(d4+w*d5))))
            Hg = e0 + w*(e1+w*(e2+w*(e3+w*(e4+w*(e5+w*e6)))))
            IF ( p.GT.1995. ) THEN
               fctr = (p-1995.)/10.
               hf1 = dd0 + w*(dd1+w*(dd2+w*dd3))
               hg1 = ee0 + w*(ee1+w*(ee2+w*ee3))
               Hf = Hf*(1.-fctr) + hf1*fctr
               Hg = Hg*(1.-fctr) + hg1*fctr
            ENDIF
         ENDIF
         Vf = a0 + p*(a1+p*(a2+p*(a3+p*(a4+p*a5))))
         Vg = b0 + w*(b1+w*(b2+w*(b3+w*(b4+w*(b5+w*(b6+w*b7))))))
         T = c0 + w*(c1+w*(c2+w*(c3+w*c4)))
         Dtsat = (c1+w*(2.*c2+w*(3.*c3+4.*c4*w)))/p
         Dvfdp = a1 + p*(2.*a2+p*(3.*a3+p*(4.*a4+p*5.*a5)))
         Dvgdp = rp*                                                    &
     &           (b1+w*(2.*b2+w*(3.*b3+w*(4.*b4+w*(5.*b5+w*(6.*b6+w*7.* &
     &           b7))))))
         IF ( p.GE.2005. ) THEN
            Dhfdp = rp*(dd1+w*(2.*dd2+w*3.*dd3))
            Dhgdp = rp*(ee1+w*(2.*ee2+w*3.*ee3))
         ELSE
            Dhfdp = rp*(d1+w*(2.*d2+w*(3.*d3+w*(4.*d4+w*5.*d5))))
            Dhgdp = rp*                                                 &
     &              (e1+w*(2.*e2+w*(3.*e3+w*(4.*e4+w*(5.*e5+w*6.*e6)))))
            IF ( p.GT.1995. ) THEN
               fctr = (p-1995.)/10.
               dhfdp1 = rp*(dd1+w*(2.*dd2+w*3.*dd3))
               dhgdp1 = rp*(ee1+w*(2.*ee2+w*3.*ee3))
               Dhfdp = Dhfdp*(1.-fctr) + dhfdp1*fctr
               Dhgdp = Dhgdp*(1.-fctr) + dhgdp1*fctr
            ENDIF
         ENDIF
         RETURN
      ELSEIF ( p.LT.40 ) THEN
         x = 50. - Pp
         rp = 1./Pp
         T = ta + x*(cc0+x*(cc1+x*(cc2+x*(cc3+x*(cc4+x*cc5)))))
         Dtsat = -                                                      &
     &           (cc0+x*(2.*cc1+x*(3.*cc2+x*(4.*cc3+x*(5.*cc4+6.*cc5*x))&
     &           )))
         Vf = vfa + x*(aa0+x*(aa1+x*(aa2+x*(aa3+x*(aa4+x*aa5)))))
         Vg = bb0 + rp*(bb1+rp*(bb2+rp*bb3))
         Hf = hfa + x*(df0+x*(df1+x*(df2+x*(df3+x*(df4+x*df5)))))
         Hg = hga + x*(eg0+x*(eg1+x*(eg2+x*(eg3+x*(eg4+x*eg5)))))
         Dvfdp = -aa0 -                                                 &
     &           x*(2.*aa1+x*(3.*aa2+x*(4.*aa3+x*(5.*aa4+x*6.*aa5))))
         Dvgdp = -rp**2*(bb1+rp*(2.*bb2+rp*3.*bb3))
         Dhfdp = -df0 -                                                 &
     &           x*(2.*df1+x*(3.*df2+x*(4.*df3+x*(5.*df4+x*6.*df5))))
         Dhgdp = -eg0 -                                                 &
     &           x*(2.*eg1+x*(3.*eg2+x*(4.*eg3+x*(5.*eg4+x*6.*eg5))))
         RETURN
      ELSE
         fctr = p - 40.
         w = DLOG(DABS(p))
         rp = 1.0/p
         x = 50. - Pp
         hf1 = d0 + w*(d1+w*(d2+w*(d3+w*(d4+w*d5))))
         hg1 = e0 + w*(e1+w*(e2+w*(e3+w*(e4+w*(e5+w*e6)))))
         vf1 = a0 + p*(a1+p*(a2+p*(a3+p*(a4+p*a5))))
         vg1 = b0 + w*(b1+w*(b2+w*(b3+w*(b4+w*(b5+w*(b6+w*b7))))))
         t1 = c0 + w*(c1+w*(c2+w*(c3+w*c4)))
         T = ta + x*(cc0+x*(cc1+x*(cc2+x*(cc3+x*(cc4+x*cc5)))))
         t2 = T
         Vf = vfa + x*(aa0+x*(aa1+x*(aa2+x*(aa3+x*(aa4+x*aa5)))))
         Vg = bb0 + rp*(bb1+rp*(bb2+rp*bb3))
         Hf = hfa + x*(df0+x*(df1+x*(df2+x*(df3+x*(df4+x*df5)))))
         Hg = hga + x*(eg0+x*(eg1+x*(eg2+x*(eg3+x*(eg4+x*eg5)))))
         T = T*(1.0-fctr) + t1*fctr
         dt1dp = (c1+w*(2.*c2+w*(3.*c3+4.*c4*w)))/p
         dt2dp = -                                                      &
     &           (cc0+x*(2.*cc1+x*(3.*cc2+x*(4.*cc3+x*(5.*cc4+6.*cc5*x))&
     &           )))
         Dtsat = dt1dp*fctr + (1.-fctr)*dt2dp + t1 - t2
         Vf = Vf*(1.0-fctr) + vf1*fctr
         Vg = Vg*(1.0-fctr) + vg1*fctr
         Hf = Hf*(1.0-fctr) + hf1*fctr
         Hg = Hg*(1.0-fctr) + hg1*fctr
         dvfdp1 = a1 + p*(2.*a2+p*(3.*a3+p*(4.*a4+p*5.*a5)))
         dvgdp1 = rp*                                                   &
     &            (b1+w*(2.*b2+w*(3.*b3+w*(4.*b4+w*(5.*b5+w*(6.*b6+w*7.*&
     &            b7))))))
         dhfdp1 = rp*(d1+w*(2.*d2+w*(3.*d3+w*(4.*d4+w*5.*d5))))
         dhgdp1 = rp*(e1+w*(2.*e2+w*(3.*e3+w*(4.*e4+w*(5.*e5+w*6.*e6))))&
     &            )
         Dvfdp = -aa0 -                                                 &
     &           x*(2.*aa1+x*(3.*aa2+x*(4.*aa3+x*(5.*aa4+x*6.*aa5))))
         Dvgdp = -rp**2*(bb1+rp*(2.*bb2+rp*3.*bb3))
         Dhfdp = -df0 -                                                 &
     &           x*(2.*df1+x*(3.*df2+x*(4.*df3+x*(5.*df4+x*6.*df5))))
         Dhgdp = -eg0 -                                                 &
     &           x*(2.*eg1+x*(3.*eg2+x*(4.*eg3+x*(5.*eg4+x*6.*eg5))))
         Dvfdp = Dvfdp*(1.0-fctr) + dvfdp1*fctr
         Dvgdp = Dvgdp*(1.0-fctr) + dvgdp1*fctr
         Dhfdp = Dhfdp*(1.0-fctr) + dhfdp1*fctr
         Dhgdp = Dhgdp*(1.0-fctr) + dhgdp1*fctr
         RETURN
      ENDIF
!      PRINT11111,PP,TA,CC0,CC1,VFA,AA0,AA1,BB0,HFA,HGA,DF0,EG0
99001 FORMAT (10F13.3)
      END
!*==S00019.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00019
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA22 / DH(22,2) , DM(22,2) , DPDt , DUU(22,2) , DV(22,2) &
     &                , DNU(22,2)
      COMMON /AAA10 / V(22,2) , VV(22,2) , H(22,2) , HF(22,2) , HG(22,2)&
     &                , DNUhp(22,2) , U(22,2) , XM(22,2) , XT(22,2) ,   &
     &                ALFa(22,2) , T(22,2) , DVVh(22,2) , DELh(22,2) ,  &
     &                DVVp(22,2) , DELp(22,2) , AA(22,2) , BB(22,2) ,   &
     &                DTDh(22,2) , DTDp(22,2) , DTSat , DNUpp(22,2) ,   &
     &                A(22) , B(22) , C(22) , TVId(22,12) , M(22,2) ,   &
     &                IST(22,2)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(22,12) , PL(22,12) , QREpa(22,12) ,          &
     &                XMCga(22,12) , XMCuo(22,12) , QCEd(22,12) ,       &
     &                QCOn(22,12) , QUGa(22,12) , QFOu(22,12) , QCEi(2) &
     &                , QCOni(2) , QUGii(2) , TMI(2) , DTMi(2) ,        &
     &                QTOtal , QTUg , QTGf , TERc(2) , TGAi(22,12) ,    &
     &                TGAii(22,12) , T876(22,12) , TUMax(22,12) ,       &
     &                TUMin(22,12) , DTGai(22,12) , DT876(22,12) ,      &
     &                TERco(22,12) , TMU(2) , DTMu(2) , NPUi , NASl(22) &
     &                , NSL(22)
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(21,2) ,&
     &                HEM(21) , VE(21,2) , XA(21) , XB(21) , XD(21) ,   &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(21,2) ,        &
     &                HJOnt(21,2) , DEBav(21) , GLInf(21) , GLSup(21) , &
     &                PP1 , PP2 , ALNiv , VGJo(21) , GLInfp(21) ,       &
     &                V55198(21) , ICAs(21) , ICZw(21)
      COMMON /AAA17 / DDPdt , DDH(22,2) , DDV(22,2) , DDM(22,2) ,       &
     &                DDU(22,2)
      COMMON /AAA18 / DDEb(21) , DDDeb(21) , DXAt(21) , DXAg(21) ,      &
     &                DXBt(21) , DXBg(21) , DNUg(21) , DNUf(21) ,       &
     &                DHE(22,2) , DDEbo(21)
      COMMON /AAA20 / DQP(22,2) , DFLul(22) , DFLuv(22) , DFLuil(22) ,  &
     &                DFLuiv(22) , DDEbil(22) , DDEbiv(22) , DQCei(2) , &
     &                DQCed(22,12) , DDTuo(22,12) , DDTga(22,12) ,      &
     &                DDTmi(2) , DDTmu(2)
      DOUBLE PRECISION M
      TEM = TEM + DT
      P = P + DT*(DPDt+DDPdt*DT)
      DO i = 2 , NC1
         j1 = 2
         IF ( ITYp(i).EQ.1 ) j1 = 1
         DO j = 1 , j1
            xdh = DH(i,j)
            ydh = DABS(xdh)
            xddh = DDH(i,j)
            yddh = DABS(xddh)
            ydh = DMAX1(1.D0,ydh)
            yddht = yddh*DT
            IF ( yddht.GT.(ydh/2.) ) THEN
               xddh = (ydh/2.)*xddh/yddh
               yddh = DABS(xddh)
            ENDIF
            IF ( ydh.GT.1.D+5 .OR. yddh.GT.1.D+8 ) THEN
               IF ( ydh.GE.1.D+5 ) xdh = xdh*1.D+5/ydh
               IF ( yddh.GE.1.D+8 ) xddh = xddh*1.D+8/yddh
            ENDIF
            H(i,j) = H(i,j) + DT*(xdh+DT*xddh)
            V(i,j) = V(i,j) + DT*(DV(i,j)+DT*DDV(i,j))
         ENDDO
      ENDDO
      DO i = 2 , NC1
         n1 = NASl(i)
         DO j = 1 , n1
            T876(i,j) = T876(i,j) + DT*(DT876(i,j)+DT*DDTuo(i,j))
            TGAi(i,j) = TGAi(i,j) + DT*(DTGai(i,j)+DT*DDTga(i,j))
         ENDDO
      ENDDO
      DO k = 1 , 2
         TMI(k) = TMI(k) + DT*(DTMi(k)+DT*DDTmi(k))
         TMU(k) = TMU(k) + DT*(DTMu(k)+DT*DDTmu(k))
      ENDDO
      DO i = 1 , NC1
         DEB(i) = DEB(i) + DT*(DDEb(i)+DT*DDDeb(i))
      ENDDO
      CONTINUE
      END
!*==S00000.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE S00000(Ini)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /AAA11 / V0011n(20) , V0012x(20) , V0011 , V0012 , TLIm ,  &
     &                ZLImin , ZLImax , DTVoi , TIMimp(20) , DIFref(20) &
     &                , DIFmic(20) , V00001 , V22202 , TEMimp , TEMmic ,&
     &                DELm , DELv , DELvi , DELvs , V22201 , V22203 ,   &
     &                DTPre , DTPhy , DTTemp , DTChan , DTDis , DTBef , &
     &                DTOrg , NIMic , NMUltr , IZOne , N00011(20) ,     &
     &                ISOrt , NECrit , NSTar , IPLot , NZOne , IFRe(20) &
     &                , ICO , IIMp , NPAs1(20) , NPAs2(20)
      COMMON /AAA26 / DEB(21) , DEBv(21) , DEBl(21) , ALFad(21) ,       &
     &                ALFb(21) , ALFt(21) , HA(21) , HB(21) , HE(42) ,  &
     &                HEM(21) , VE(42) , XA(21) , XB(21) , XD(21) ,     &
     &                XEM(21) , SDSc , TJOnc(21) , HJOnb(42) , HJOnt(42)&
     &                , DEBav(21) , GLInf(21) , GLSup(21) , PP1 , PP2 , &
     &                ALNiv , VGJo(21) , GLInfp(21) , V55198(21) ,      &
     &                ICAs(21) , ICZw(21)
      COMMON /AAA22 / DH(44) , DM(44) , DPDt , DUU(44) , DV(44) ,       &
     &                DNU(44)
      COMMON /AAA33 / VCO , XL0055 , D876 , DINt , DEXt , VOL002 ,      &
     &                VOL005 , LCO , NCRay
      COMMON /AAA44 / SC(21) , SCApa(22) , SHTc(22) , VC(22) , XL(22) , &
     &                VFIx , ZCOt(21) , I6Ft , J6Ft , I8Ft , J8Ft , NC ,&
     &                NC1 , NC2 , IBCh , IHCh
      COMMON /AAA55 / KPRess , KDEb , KCOn , KTGai , KT876 , ICAt(3) ,  &
     &                KIMp , KGLiss
      COMMON /AAA66 / TS , HFS , HGS , HFGs , DHFps , DHGps , VVFs ,    &
     &                VVGs , DVVfps , DVVgps , DHFgs , VV1 , VV2 ,      &
     &                DVV1p , DVV2p , T1 , T2
      COMMON /AAA77 / QINit , TPUi(20) , QPUi(20) , RPUi , PLMoy ,      &
     &                XHTc(264) , PL(264) , QREpa(264) , XMCga(264) ,   &
     &                XMCuo(264) , QCEd(264) , QCOn(264) , QUGa(264) ,  &
     &                QFOu(264) , QCEi(2) , QCOni(2) , QUGii(2) , TMI(2)&
     &                , DTMi(2) , QTOtal , QTUg , QTGf , TERc(2) ,      &
     &                TGAi(264) , TGAii(264) , T876(264) , TUMax(264) , &
     &                TUMin(264) , DTGai(264) , DT876(264) , TERco(264) &
     &                , TMU(2) , DTMu(2) , NPUi , NASl(22) , NSL(22)
      COMMON /AAA88 / FLUp(44) , FLUv(44) , FLUl(44) , FLUiv(44) ,      &
     &                DEBil(44) , FLUil(44) , DEBi(22) , DEBhi(22) ,    &
     &                DEBiv(44)
      COMMON /AAA99 / HLCf , HVCfo , HVAp , HCOn , HVIn , HLIn , R876 , &
     &                RGAi , XM876 , XMGai , XL876 , XLGai , CP876 ,    &
     &                CPGai , XMC876 , XMCgai , DIAhy , HGApp(22) ,     &
     &                HCG(22) , HTC(264) , HVCff(22)
      COMMON /AAA10 / V(44) , VV(44) , H(44) , HF(44) , HG(44) ,        &
     &                DNUhp(44) , U(44) , XM(44) , XT(44) , ALFa(44) ,  &
     &                T(44) , DVVh(44) , DELh(44) , DVVp(44) , DELp(44) &
     &                , AA(44) , BB(44) , DTDh(44) , DTDp(44) , DTSat , &
     &                DNUpp(44) , A(22) , B(22) , C(22) , TVId(264) ,   &
     &                M(44) , IST(44)
      COMMON /AAA13 / TEM , P , DT , HNIv(22) , VITess , ZNIv , RI(2) , &
     &                ISS , NITera , NIV , ISLni , IBTr , IHTr ,        &
     &                ITYp(22)
      COMMON /AAA16 / DBEnt(20) , TENt(20) , HENt(20) , DEBe , ENTe ,   &
     &                DBSor(20) , TSOr(20) , DEBs , DPDtt(20) ,         &
     &                TDPdt(20) , HSOr(20) , THSor(20) , ENTs , NDBent ,&
     &                NDPdt , NDBsor , NHSor
      COMMON /AAA17 / DDPdt , DDH(44) , DDV(44) , DDM(44) , DDU(44)
      COMMON /AAA18 / DDEb(21) , DDDeb(21) , DXAt(21) , DXAg(21) ,      &
     &                DXBt(21) , DXBg(21) , DNUg(21) , DNUf(21) ,       &
     &                DHE(44) , DDEbo(21)
      COMMON /AAA20 / DQP(44) , DFLul(22) , DFLuv(22) , DFLuil(22) ,    &
     &                DFLuiv(22) , DDEbil(22) , DDEbiv(22) , DQCei(2) , &
     &                DQCed(264) , DDTuo(264) , DDTga(264) , DDTmi(2) , &
     &                DDTmu(2)
      DOUBLE PRECISION M
      DO i = 1 , 20
         DBEnt(i) = Ini
      ENDDO
      DO i = 1 , 20
         TENt(i) = Ini
      ENDDO
      DO i = 1 , 20
         HENt(i) = Ini
      ENDDO
      DEBe = Ini
      ENTe = Ini
      DO i = 1 , 20
         DPDtt(i) = Ini
      ENDDO
      DO i = 1 , 20
         TDPdt(i) = Ini
      ENDDO
      DO i = 1 , 20
         DBSor(i) = Ini
      ENDDO
      DO i = 1 , 20
         TSOr(i) = Ini
      ENDDO
      DEBs = Ini
      DO i = 1 , 20
         HSOr(i) = Ini
      ENDDO
      DO i = 1 , 20
         THSor(i) = Ini
      ENDDO
      ENTs = Ini
      NDBent = Ini
      NDPdt = Ini
      NDBsor = Ini
      NHSor = Ini
      DO i = 1 , 20
         V0011n(i) = Ini
      ENDDO
      DO i = 1 , 20
         V0012x(i) = Ini
      ENDDO
      V0011 = Ini
      V0012 = Ini
      DTVoi = Ini
      DO i = 1 , 20
         TIMimp(i) = Ini
      ENDDO
      DO i = 1 , 20
         DIFref(i) = Ini
      ENDDO
      DO i = 1 , 20
         DIFmic(i) = Ini
      ENDDO
      TEMimp = Ini
      TEMmic = Ini
      DELm = Ini
      DELv = Ini
      DELvi = Ini
      DELvs = Ini
      TLIm = Ini
      ZLImin = Ini
      ZLImax = Ini
      V00001 = Ini
      V22202 = Ini
      V22201 = Ini
      V22203 = Ini
      DTPre = Ini
      DTPhy = Ini
      DTTemp = Ini
      DTChan = Ini
      DTDis = Ini
      DTBef = Ini
      DTOrg = Ini
      NIMic = Ini
      NMUltr = Ini
      IZOne = Ini
      DO i = 1 , 20
         N00011(i) = Ini
      ENDDO
      ISOrt = Ini
      NECrit = Ini
      NSTar = Ini
      IPLot = Ini
      NZOne = Ini
      DO i = 1 , 20
         IFRe(i) = Ini
      ENDDO
      ICO = Ini
      IIMp = Ini
      DO i = 1 , 20
         NPAs1(i) = Ini
      ENDDO
      DO i = 1 , 20
         NPAs2(i) = Ini
      ENDDO
      DO i = 1 , 21
         DEB(i) = Ini
      ENDDO
      DO i = 1 , 21
         DEBv(i) = Ini
      ENDDO
      DO i = 1 , 21
         DEBl(i) = Ini
      ENDDO
      DO i = 1 , 21
         ALFad(i) = Ini
      ENDDO
      DO i = 1 , 21
         ALFb(i) = Ini
      ENDDO
      DO i = 1 , 21
         ALFt(i) = Ini
      ENDDO
      DO i = 1 , 21
         HA(i) = Ini
      ENDDO
      DO i = 1 , 21
         HB(i) = Ini
      ENDDO
      DO i = 1 , 42
         HE(i) = Ini
      ENDDO
      DO i = 1 , 21
         HEM(i) = Ini
      ENDDO
      DO i = 1 , 42
         VE(i) = Ini
      ENDDO
      DO i = 1 , 21
         XA(i) = Ini
      ENDDO
      DO i = 1 , 21
         XB(i) = Ini
      ENDDO
      DO i = 1 , 21
         XD(i) = Ini
      ENDDO
      DO i = 1 , 21
         XEM(i) = Ini
      ENDDO
      SDSc = Ini
      DO i = 1 , 21
         TJOnc(i) = Ini
      ENDDO
      DO i = 1 , 42
         HJOnb(i) = Ini
      ENDDO
      DO i = 1 , 42
         HJOnt(i) = Ini
      ENDDO
      DO i = 1 , 21
         DEBav(i) = Ini
      ENDDO
      DO i = 1 , 21
         GLInf(i) = Ini
      ENDDO
      DO i = 1 , 21
         GLSup(i) = Ini
      ENDDO
      PP1 = Ini
      PP2 = Ini
      ALNiv = Ini
      DO i = 1 , 21
         VGJo(i) = Ini
      ENDDO
      DO i = 1 , 21
         GLInfp(i) = Ini
      ENDDO
      DO i = 1 , 21
         V55198(i) = Ini
      ENDDO
      DO i = 1 , 21
         ICAs(i) = Ini
      ENDDO
      DO i = 1 , 21
         ICZw(i) = Ini
      ENDDO
      DO i = 1 , 44
         DH(i) = Ini
      ENDDO
      DO i = 1 , 44
         DM(i) = Ini
      ENDDO
      DPDt = Ini
      DO i = 1 , 44
         DUU(i) = Ini
      ENDDO
      DO i = 1 , 44
         DV(i) = Ini
      ENDDO
      DO i = 1 , 44
         DNU(i) = Ini
      ENDDO
      DDPdt = Ini
      DO i = 1 , 44
         DDH(i) = Ini
      ENDDO
      DO i = 1 , 44
         DDV(i) = Ini
      ENDDO
      DO i = 1 , 44
         DDM(i) = Ini
      ENDDO
      DO i = 1 , 44
         DDU(i) = Ini
      ENDDO
      DO i = 1 , 21
         DDEb(i) = Ini
      ENDDO
      DO i = 1 , 21
         DDDeb(i) = Ini
      ENDDO
      DO i = 1 , 21
         DXAt(i) = Ini
      ENDDO
      DO i = 1 , 21
         DXAg(i) = Ini
      ENDDO
      DO i = 1 , 21
         DXBt(i) = Ini
      ENDDO
      DO i = 1 , 21
         DXBg(i) = Ini
      ENDDO
      DO i = 1 , 21
         DNUg(i) = Ini
      ENDDO
      DO i = 1 , 21
         DNUf(i) = Ini
      ENDDO
      DO i = 1 , 44
         DHE(i) = Ini
      ENDDO
      DO i = 1 , 21
         DDEbo(i) = Ini
      ENDDO
      DO i = 1 , 44
         DQP(i) = Ini
      ENDDO
      DO i = 1 , 22
         DFLul(i) = Ini
      ENDDO
      DO i = 1 , 22
         DFLuv(i) = Ini
      ENDDO
      DO i = 1 , 22
         DFLuil(i) = Ini
      ENDDO
      DO i = 1 , 22
         DFLuiv(i) = Ini
      ENDDO
      DO i = 1 , 22
         DDEbil(i) = Ini
      ENDDO
      DO i = 1 , 22
         DDEbiv(i) = Ini
      ENDDO
      DO i = 1 , 2
         DQCei(i) = Ini
      ENDDO
      DO i = 1 , 264
         DQCed(i) = Ini
      ENDDO
      DO i = 1 , 264
         DDTuo(i) = Ini
      ENDDO
      DO i = 1 , 264
         DDTga(i) = Ini
      ENDDO
      DO i = 1 , 2
         DDTmi(i) = Ini
      ENDDO
      DO i = 1 , 2
         DDTmu(i) = Ini
      ENDDO
      VCO = Ini
      XL0055 = Ini
      D876 = Ini
      DINt = Ini
      DEXt = Ini
      VOL002 = Ini
      VOL005 = Ini
      LCO = Ini
      NCRay = Ini
      DO i = 1 , 21
         SC(i) = Ini
      ENDDO
      DO i = 1 , 22
         SCApa(i) = Ini
      ENDDO
      DO i = 1 , 22
         SHTc(i) = Ini
      ENDDO
      DO i = 1 , 22
         VC(i) = Ini
      ENDDO
      DO i = 1 , 22
         XL(i) = Ini
      ENDDO
      VFIx = Ini
      DO i = 1 , 21
         ZCOt(i) = Ini
      ENDDO
      I6Ft = Ini
      J6Ft = Ini
      I8Ft = Ini
      J8Ft = Ini
      NC = Ini
      NC1 = Ini
      NC2 = Ini
      IBCh = Ini
      IHCh = Ini
      KPRess = Ini
      KDEb = Ini
      KCOn = Ini
      KTGai = Ini
      KT876 = Ini
      DO i = 1 , 3
         ICAt(i) = Ini
      ENDDO
      KIMp = Ini
      KGLiss = Ini
      TS = Ini
      HFS = Ini
      HGS = Ini
      HFGs = Ini
      DHFps = Ini
      DHGps = Ini
      VVFs = Ini
      VVGs = Ini
      DVVfps = Ini
      DVVgps = Ini
      DHFgs = Ini
      VV1 = Ini
      VV2 = Ini
      DVV1p = Ini
      DVV2p = Ini
      T1 = Ini
      T2 = Ini
      QINit = Ini
      DO i = 1 , 20
         TPUi(i) = Ini
      ENDDO
      DO i = 1 , 20
         QPUi(i) = Ini
      ENDDO
      RPUi = Ini
      PLMoy = Ini
      DO i = 1 , 264
         XHTc(i) = Ini
      ENDDO
      DO i = 1 , 264
         PL(i) = Ini
      ENDDO
      DO i = 1 , 264
         QREpa(i) = Ini
      ENDDO
      DO i = 1 , 264
         XMCga(i) = Ini
      ENDDO
      DO i = 1 , 264
         XMCuo(i) = Ini
      ENDDO
      DO i = 1 , 264
         QCEd(i) = Ini
      ENDDO
      DO i = 1 , 264
         QCOn(i) = Ini
      ENDDO
      DO i = 1 , 264
         QUGa(i) = Ini
      ENDDO
      DO i = 1 , 264
         QFOu(i) = Ini
      ENDDO
      DO i = 1 , 2
         QCEi(i) = Ini
      ENDDO
      DO i = 1 , 2
         QCOni(i) = Ini
      ENDDO
      DO i = 1 , 2
         QUGii(i) = Ini
      ENDDO
      DO i = 1 , 2
         TMI(i) = Ini
      ENDDO
      DO i = 1 , 2
         DTMi(i) = Ini
      ENDDO
      QTOtal = Ini
      QTUg = Ini
      QTGf = Ini
      DO i = 1 , 2
         TERc(i) = Ini
      ENDDO
      DO i = 1 , 264
         TGAi(i) = Ini
      ENDDO
      DO i = 1 , 264
         TGAii(i) = Ini
      ENDDO
      DO i = 1 , 264
         T876(i) = Ini
      ENDDO
      DO i = 1 , 264
         TUMax(i) = Ini
      ENDDO
      DO i = 1 , 264
         TUMin(i) = Ini
      ENDDO
      DO i = 1 , 264
         DTGai(i) = Ini
      ENDDO
      DO i = 1 , 264
         DT876(i) = Ini
      ENDDO
      DO i = 1 , 264
         TERco(i) = Ini
      ENDDO
      DO i = 1 , 2
         TMU(i) = Ini
      ENDDO
      DO i = 1 , 2
         DTMu(i) = Ini
      ENDDO
      NPUi = Ini
      DO i = 1 , 22
         NASl(i) = Ini
      ENDDO
      DO i = 1 , 22
         NSL(i) = Ini
      ENDDO
      DO i = 1 , 44
         FLUp(i) = Ini
      ENDDO
      DO i = 1 , 44
         FLUv(i) = Ini
      ENDDO
      DO i = 1 , 44
         FLUl(i) = Ini
      ENDDO
      DO i = 1 , 44
         FLUiv(i) = Ini
      ENDDO
      DO i = 1 , 44
         FLUil(i) = Ini
      ENDDO
      DO i = 1 , 22
         DEBi(i) = Ini
      ENDDO
      DO i = 1 , 22
         DEBhi(i) = Ini
      ENDDO
      DO i = 1 , 44
         DEBiv(i) = Ini
      ENDDO
      DO i = 1 , 44
         DEBil(i) = Ini
      ENDDO
      HLCf = Ini
      HVCfo = Ini
      HVAp = Ini
      HCOn = Ini
      HVIn = Ini
      HLIn = Ini
      R876 = Ini
      RGAi = Ini
      XM876 = Ini
      XMGai = Ini
      XL876 = Ini
      XLGai = Ini
      CP876 = Ini
      CPGai = Ini
      XMC876 = Ini
      XMCgai = Ini
      DIAhy = Ini
      DO i = 1 , 22
         HGApp(i) = Ini
      ENDDO
      DO i = 1 , 22
         HCG(i) = Ini
      ENDDO
      DO i = 1 , 264
         HTC(i) = Ini
      ENDDO
      DO i = 1 , 22
         HVCff(i) = Ini
      ENDDO
      DO i = 1 , 44
         V(i) = Ini
      ENDDO
      DO i = 1 , 44
         VV(i) = Ini
      ENDDO
      DO i = 1 , 44
         H(i) = Ini
      ENDDO
      DO i = 1 , 44
         HF(i) = Ini
      ENDDO
      DO i = 1 , 44
         HG(i) = Ini
      ENDDO
      DO i = 1 , 44
         DNUhp(i) = Ini
      ENDDO
      DO i = 1 , 44
         U(i) = Ini
      ENDDO
      DO i = 1 , 44
         XM(i) = Ini
      ENDDO
      DO i = 1 , 44
         XT(i) = Ini
      ENDDO
      DO i = 1 , 44
         ALFa(i) = Ini
      ENDDO
      DO i = 1 , 44
         T(i) = Ini
      ENDDO
      DO i = 1 , 44
         DVVh(i) = Ini
      ENDDO
      DO i = 1 , 44
         DELh(i) = Ini
      ENDDO
      DO i = 1 , 44
         DVVp(i) = Ini
      ENDDO
      DO i = 1 , 44
         DELp(i) = Ini
      ENDDO
      DO i = 1 , 44
         AA(i) = Ini
      ENDDO
      DO i = 1 , 44
         BB(i) = Ini
      ENDDO
      DO i = 1 , 44
         DTDh(i) = Ini
      ENDDO
      DO i = 1 , 44
         DTDp(i) = Ini
      ENDDO
      DTSat = Ini
      DO i = 1 , 44
         DNUpp(i) = Ini
      ENDDO
      DO i = 1 , 22
         A(i) = Ini
      ENDDO
      DO i = 1 , 22
         B(i) = Ini
      ENDDO
      DO i = 1 , 22
         C(i) = Ini
      ENDDO
      DO i = 1 , 264
         TVId(i) = Ini
      ENDDO
      DO i = 1 , 44
         M(i) = Ini
      ENDDO
      DO i = 1 , 44
         IST(i) = Ini
      ENDDO
      TEM = Ini
      P = Ini
      DT = Ini
      DO i = 1 , 22
         HNIv(i) = Ini
      ENDDO
      VITess = Ini
      ZNIv = Ini
      DO i = 1 , 2
         RI(i) = Ini
      ENDDO
      ISS = Ini
      NITera = Ini
      NIV = Ini
      ISLni = Ini
      IBTr = Ini
      IHTr = Ini
      DO i = 1 , 22
         ITYp(i) = Ini
      ENDDO
      CONTINUE
      END
