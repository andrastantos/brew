!*==AA0001.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
!   SPAG options set to convert source form only
!     =======================================================================
!     PROGRAM: The autocorrelation test for pseudorandom numbers using the
!              2-d Ising model with the Wolff algorithm.
!     -----------------------------------------------------------------------
!     for    testing of pseudorandom numbers; based on calculating some
!            physical quantities of the 2-d Ising model as well as their
!            integrated autocorrelation functions.
!     by     I. Vattulainen, vattulai@convex.csc.fi
!     alg    autocorrelation, the Ising model, Wolff updating
!     ref    Phys. Rev. Lett. 73, 2513 (1994).
!     title  acorrt_iw.f
!     size
!     prec   single/double
!     lang   Fortran77
!     -----------------------------------------------------------------------
!     The author of this software is I. Vattulainen.  Copyright (c) 1993.
!     Permission to use, copy, modify, and distribute this software for
!     any purpose without fee is hereby granted, provided that this entire
!     notice is included in all copies of any software which is or includes
!     a copy or modification of this software and in all copies of the
!     supporting documentation for such software.
!     THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
!     WARRANTY. IN PARTICULAR, NEITHER THE AUTHOR NOR AT&T MAKE ANY
!     REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
!     OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
!     -----------------------------------------------------------------------
!     In the autocorrelation test, we calculate averages of some physical
!     quantities such as the energy, the susceptibility, and the updated
!     cluster size in the 2-d Ising model. Additionally, their autocorrelation
!     functions and corresponding integrated autocorrelation values are
!     determined. The chosen temperature corresponds to the critical point,
!     and the Wolff updating method is utilized. The results of the
!     simulation (test) are a function of the pseudorandom number generator
!     used. Since the exact value is known only for the energy, other results
!     must be compared with corresponding results of other generators;
!     i.e. the test must be performed comparatively between several
!     pseudorandom number generators.
!     -----------------------------------------------------------------------
!     Main parameters in the test are as follows:
!     L         linear size of the lattice.
!     DK        interaction / temperature*Boltzmann constant.
!     MCBEG     number of passed initial configurations
!               (initialization time; a minimum of 10000 is suggested).
!     ISCANS    number of independent samples.
!     ICSAMP    maximum detected autocorrelation time.
!     INEWEST   tells the index of the latest (newest) configuration.
!     IZ        contains the 2-d lattice.
!     D*ROO     contains the autocorrelation function.
!     D*TAU     gives the integrated autocorrelation time, where
!     D*SIGMA   is its' error estimate.
!     ------------------------------------------------------
!     ******************
!     PRACTICAL DETAILS:
!     ******************
!
!     Required modification to this code:
!                   Initialization and implementation of the pseudorandom
!                   number generator (PRNG), which will be tested. For
!                   initialization, there is a reserved space in the code
!                   below. The PRNG may be appended to the end of the file,
!                   or may be called separately during compiling. In this
!                   version, the default generator is GGL.
!
!                   During the test, random numbers are called in two cases:
!                   in the formation of the random (initial) lattice and
!                   during its updating. The former is in the main program
!                   and the latter in the subroutine WOLFF. As a consequence,
!                   if you are testing a generator which produces a sequence
!                   of random numbers at a time (this is not the case with
!                   GGL), the code must be modified correspondingly.
!
!     Output files: general.dat         General information of the test.
!                   susc_data.dat       Autocorrelation function and its
!                                       integrated value for susceptibility
!                                       (including the error estimate).
!                   ener_data.dat       Corresponding results for the energy.
!                   clus_data.dat       Corresponding results for the
!                                       flipped cluster sizes.
!                   clus_distr.dat      Normalized probability distribution
!                                       for the flipped cluster size (as a
!                                       function of cluster size).
!
!     Keep in mind that the given results are *NOT* final. This is due to
!     the calculation of errors in a Monte Carlo simulation, in which the
!     integrated autocorrelation time plays an important role. In order to
!     find the correct error estimates, perform the following steps after
!     the test:
!        (1) First of all, determine the estimates for the integrated
!            autocorrelation times. To do that, for each of the quantities
!            energy, susceptibility, and flipped cluster size, you must find
!            the regime (plateau) in which the results have already converged
!            but where the statistical errors are not yet very large. An
!            estimate for the corresponding integrated autocorrelation time
!            (tau) is then an average over this plateau.
!        (2) The given values for the error estimates of energy,
!            susceptibility and the average flipped cluster size (given in
!            a file general.dat) must then be modified: when the
!            corresponding integrated autocorrelation time (tau) is known,
!            the error estimate must then be multiplied by tau. A square
!            root of the product then gives the correct error estimate.
!
!     For further practical details of the method see U. Wolff [Phys. Lett. B
!     228, 379 (1989) or I. Vattulainen [cond-mat@babbage.sissa.it No.
!     9411062].
!     =======================================================================
 
      IMPLICIT NONE
      DOUBLE PRECISION DK
      INTEGER L , L2 , MCBEG , ISCANS , ICSAMP
      PARAMETER (L=16,L2=L*L)
!     -------------------------------------
!     Coefficients at criticality (some empirical estimates for a finite
!     system are also given):
!     For an infinite lattice (L --> infinity):
!      PARAMETER( DK = 0.44068679350977D0 )
!     For a lattice size L = 64:
!      PARAMETER( DK = 0.43821982133122D0 )
!     For a lattice size L = 16:
!      PARAMETER( DK = 0.43098188945039D0 )
!     -------------------------------------
      PARAMETER (DK=0.44068679350977D0)
!     Please choose MCBEG > 9999 (time for equilibration).
      PARAMETER (MCBEG=10000)
!     Good statistics require ISCANS > 1e6.
      PARAMETER (ISCANS=1000000)
      PARAMETER (ICSAMP=25)
 
!     ===============================================================
 
      INTEGER iz(L,L) , in1(L) , ip1(L) , istack(L2,2) , ix , iy ,      &
     &        itrsum , isum , ipos(ICSAMP) , inewest , idt , ilocal ,   &
     &        icnt , ifin(2) , ifac , iclsiz , icldst(L2) , i , j ,     &
     &        itries , is , idiff , iks
 
!     Susceptibility variables:
!     Energy variables:
!     Cluster variables:
      DOUBLE PRECISION prob , dsauto(ICSAMP) , dsus , dsusold(ICSAMP) , &
     &                 dsusave , dsus2 , dsroo(0:ICSAMP) ,              &
     &                 dskappa(ICSAMP) , dsrw(ICSAMP) , dstau(ICSAMP) , &
     &                 dsxw(ICSAMP) , dsyw(ICSAMP) , dssigma(ICSAMP) ,  &
     &                 dsaverage , dserror , deauto(ICSAMP) , dene ,    &
     &                 deneold(ICSAMP) , deneave , dene2 ,              &
     &                 deroo(0:ICSAMP) , dekappa(ICSAMP) , derw(ICSAMP) &
     &                 , detau(ICSAMP) , dexw(ICSAMP) , deyw(ICSAMP) ,  &
     &                 desigma(ICSAMP) , deaverage , deerror ,          &
     &                 dclauto(ICSAMP) , dcl , dclold(ICSAMP) , dclave ,&
     &                 dcl2 , dclroo(0:ICSAMP) , dclkappa(ICSAMP) ,     &
     &                 dclrw(ICSAMP) , dcltau(ICSAMP) , dclxw(ICSAMP) , &
     &                 dclyw(ICSAMP) , dclsigma(ICSAMP) , dclavr ,      &
     &                 dclerror , dclrat
 
      REAL*8 dseed , diseed
 
      REAL ran , GGL
 
      PRINT * , 'Opening files'
      OPEN (20,STATUS='UNKNOWN',FILE='suscdata.dat',FORM='FORMATTED',   &
     &      ACCESS='SEQUENTIAL')
      OPEN (21,STATUS='UNKNOWN',FILE='enerdata.dat',FORM='FORMATTED',   &
     &      ACCESS='SEQUENTIAL')
!      OPEN(22,STATUS='UNKNOWN',FILE='general.dat',FORM='FORMATTED',
!     +     ACCESS='SEQUENTIAL')
      OPEN (23,STATUS='UNKNOWN',FILE='clusdata.dat',FORM='FORMATTED',   &
     &      ACCESS='SEQUENTIAL')
      OPEN (24,STATUS='UNKNOWN',FILE='clusdist.dat',FORM='FORMATTED',   &
     &      ACCESS='SEQUENTIAL')
 
!     --------------------------
!     Initialize some variables:
!     --------------------------
      PRINT * , 'Initializing variables'
      isum = 0
      itrsum = 0
!
      dsus = 0.D0
      dsusave = 0.D0
      dsus2 = 0.D0
!
      dene = 0.D0
      deneave = 0.D0
      dene2 = 0.D0
!
      dcl = 0.D0
      dclave = 0.D0
      dclrat = 0.D0
      dcl2 = 0.D0
!
      DO i = 1 , ICSAMP
         dsusold(i) = 0.D0
         deneold(i) = 0.D0
         dclold(i) = 0.D0
      ENDDO
      DO i = 1 , ICSAMP
         dsauto(i) = 0.D0
         deauto(i) = 0.D0
         dclauto(i) = 0.D0
      ENDDO
      DO i = 1 , L
         in1(i) = i - 1
         ip1(i) = i + 1
      ENDDO
      in1(1) = L
      ip1(L) = 1
      DO i = 1 , L2
         icldst(i) = 0
      ENDDO
 
!     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
!     First of all, initialize the pseudorandom number generator.
!     In the case of GGL, only one number is needed:
 
      dseed = 14159.D0
!     ```````````````````````````````````````````````````````````
      diseed = dseed
 
!     ----------------------------------------------------------
!     Probability PROB to unite equal lattice sites as clusters:
!     ----------------------------------------------------------
      prob = 1.D0 - DEXP(-2.D0*DK)
 
!     --------------------------------------
!     Create a random initial configuration:
!     --------------------------------------
      PRINT * , 'Creating random initial configuration'
      DO iy = 1 , L
         DO ix = 1 , L
            ran = GGL(dseed)
            IF ( ran.LT.0.5D0 ) THEN
               iz(iy,ix) = -1
            ELSE
               iz(iy,ix) = 1
            ENDIF
         ENDDO
      ENDDO
 
!     ------------------------------------------------------------------
!     Perform the dynamic part to achieve equilibrium.
!     ------------------------------------------------------------------
!     Pass a total of MCBEG configurations. Determine MCBEG by studying
!     the order parameter of the system (as function of T), for example.
!     ------------------------------------------------------------------
 
      PRINT * , 'Equilibrating'
      DO i = 1 , MCBEG
         itries = 0
 711     CALL WOLFF(L,L2,iz,istack,in1,ip1,prob,dseed,isum,iclsiz)
         itries = itries + isum
         IF ( itries.LT.L2 ) GOTO 711
      ENDDO
 
!     ----------------------------------------------
!     Take a number of ICSAMP samples to start with:
!     ----------------------------------------------
      PRINT * , 'Taking samples'
      DO i = 1 , ICSAMP
         CALL WOLFF(L,L2,iz,istack,in1,ip1,prob,dseed,isum,iclsiz)
         CALL SUSCEP(L,iz,dsus)
         dsusold(i) = dsus
         CALL ENERG(L,L2,iz,dene,in1,ip1)
         deneold(i) = dene
         dclold(i) = DBLE(iclsiz)
      ENDDO
 
!     -------------------------------------------------------------------
!     Set the initial state for the vector IPOS, which gives the order of
!     appearance of the configurations. IPOS(1) gives the newest, IPOS(2)
!     the second newest, ..., and IPOS(IC) the oldest configuration.
!     -------------------------------------------------------------------
      PRINT * , 'Setting initial state'
      inewest = ICSAMP
      DO i = 1 , ICSAMP
         ipos(i) = ICSAMP - i + 1
      ENDDO
 
!     =================================================
!     !!!!!!!!!!!!!! MAIN PROGRAM STARTS !!!!!!!!!!!!!!
!     =================================================
!     Take a number of ISCANS independent measurements.
!     Each measurement corresponds to flipping of a
!     single cluster.
!     =================================================
 
      DO icnt = 1 , ISCANS
 
         CALL WOLFF(L,L2,iz,istack,in1,ip1,prob,dseed,isum,iclsiz)
!     -----------------------------------------------------
!     Calculate the susceptibility autocorrelation function
!     corresponding to the latest sample.
!     -----------------------------------------------------
         CALL SUSCEP(L,iz,dsus)
         dsusave = dsusave + dsus
         dsus2 = dsus2 + dsus*dsus
         DO is = 1 , ICSAMP
            dsauto(is) = dsauto(is) + dsusold(ipos(is))*dsus
         ENDDO
!     ---------------------------------------------
!     Calculate the energy autocorrelation function
!     corresponding to the latest sample.
!     ---------------------------------------------
         CALL ENERG(L,L2,iz,dene,in1,ip1)
         deneave = deneave + dene
         dene2 = dene2 + dene*dene
         DO is = 1 , ICSAMP
            deauto(is) = deauto(is) + deneold(ipos(is))*dene
         ENDDO
!     ---------------------------------------------------
!     Then calculate the distribution of updated clusters
!     and their autocorrelation function.
!     ---------------------------------------------------
         dcl = DBLE(iclsiz)
         dclave = dclave + dcl
         dcl2 = dcl2 + dcl*dcl
         DO is = 1 , ICSAMP
            dclauto(is) = dclauto(is) + dclold(ipos(is))*dcl
         ENDDO
         icldst(iclsiz) = icldst(iclsiz) + 1
!     -----------------------------------------------------------------
!     Replace the latest lattice with the second latest and so forth...
!     -----------------------------------------------------------------
         inewest = inewest + 1
         IF ( inewest.GT.ICSAMP ) inewest = 1
         dsusold(inewest) = dsus
         deneold(inewest) = dene
         dclold(inewest) = dcl
!     -----------------------------------
!     Update the order of configurations.
!     -----------------------------------
         DO i = 1 , ICSAMP
            ipos(i) = ipos(i) + 1
            IF ( ipos(i).GT.ICSAMP ) ipos(i) = 1
         ENDDO
 
      ENDDO
 
!     ==================
!     End of sampling...
!     ==================
!     ===============================================
!     !!!!!!!!!!!!!! MAIN PROGRAM ENDS !!!!!!!!!!!!!!
!     ===============================================
 
!     -----------------------------------------------
!     Susceptibility case:
!     Calculate the estimator for the integrated
!     autocorrelation time. In addition, determine
!     the error estimate. For further details see
!     [U. Wolff, Phys. Lett. B {\bf 228}, 379 (1989).
!     -----------------------------------------------
 
      dsusave = dsusave/ISCANS
!     Average of samples:
      dsaverage = dsusave
      dsusave = dsusave*dsusave
!     Average of squares of samples:
      dsus2 = dsus2/ISCANS
!     Monte Carlo error estimate squared divided by \tau:
      dserror = (dsus2-dsusave)*2.D0/DBLE(ISCANS)
 
      DO i = 1 , ICSAMP
         dsauto(i) = dsauto(i)/ISCANS
      ENDDO
      dsroo(0) = 1.D0
      DO i = 1 , ICSAMP
         dsroo(i) = (dsauto(i)-dsusave)/(dsus2-dsusave)
      ENDDO
      DO i = 1 , ICSAMP
         dskappa(i) = dsroo(i)/(dsroo(i-1))
      ENDDO
      DO i = 1 , ICSAMP
         dsrw(i) = dsroo(i)/(1.D0-dskappa(i))
      ENDDO
      DO i = 1 , ICSAMP
         dstau(i) = 0.5D0 + dsrw(i)
         IF ( i.GE.2 ) THEN
            DO j = 1 , (i-1)
               dstau(i) = dstau(i) + dsroo(j)
            ENDDO
         ENDIF
      ENDDO
!     Error:
      DO i = 1 , ICSAMP
         dsxw(i) = 0.5D0
         dsyw(i) = 0.5D0
         IF ( i.GE.2 ) THEN
            DO j = 1 , (i-1)
               dsxw(i) = dsxw(i) + dsroo(j)*dsroo(j)
               dsyw(i) = dsyw(i) + (dsroo(j)-dsroo(j-1))                &
     &                   *(dsroo(j)-dsroo(j-1))
            ENDDO
         ENDIF
      ENDDO
      DO i = 1 , ICSAMP
!         DSSIGMA(I) =
!     +        DSTAU(I)*DSTAU(I)*(DBLE(I) - 0.5D0 +
!     +        (1.D0 + DSKAPPA(I))/(1.D0 - DSKAPPA(I))) +
!     +        DSKAPPA(I)*DSXW(I)/((1.D0 - DSKAPPA(I))**2.D0) +
!     +        (DSKAPPA(I)**2.D0)*DSYW(I)/((1.D0 - DSKAPPA(I))**4.D0)
!     The above lines were changed to those below by K. G. Hamilton,
!     2-Sep-95, in order to avoid exponentiation errors that can
!     occur with some compilers when taking X**Y and X<0.
         dssigma(i) = dstau(i)*dstau(i)                                 &
     &                *(DBLE(i)-0.5D0+(1.D0+dskappa(i))/(1.D0-dskappa(i)&
     &                )) + dskappa(i)*dsxw(i)/((1.D0-dskappa(i))**2)    &
     &                + (dskappa(i)**2)*dsyw(i)/((1.D0-dskappa(i))**4)
      ENDDO
      DO i = 1 , ICSAMP
         dssigma(i) = dssigma(i)*4.D0/DBLE(ISCANS)
      ENDDO
      DO i = 1 , ICSAMP
         dssigma(i) = DSQRT(dssigma(i))
      ENDDO
 
!     -----------------------------------------------
!     Energy case:
!     Calculate the estimator for the integrated
!     autocorrelation time. In addition, determine
!     the error estimate. For further details see
!     [U. Wolff, Phys. Lett. B {\bf 228}, 379 (1989).
!     -----------------------------------------------
 
      deneave = deneave/ISCANS
      deaverage = deneave
      deneave = deneave*deneave
      dene2 = dene2/ISCANS
      deerror = (dene2-deneave)*2.D0/DBLE(ISCANS)
 
      DO i = 1 , ICSAMP
         deauto(i) = deauto(i)/ISCANS
      ENDDO
      deroo(0) = 1.D0
      DO i = 1 , ICSAMP
         deroo(i) = (deauto(i)-deneave)/(dene2-deneave)
      ENDDO
      DO i = 1 , ICSAMP
         dekappa(i) = deroo(i)/(deroo(i-1))
      ENDDO
      DO i = 1 , ICSAMP
         derw(i) = deroo(i)/(1.D0-dekappa(i))
      ENDDO
      DO i = 1 , ICSAMP
         detau(i) = 0.5D0 + derw(i)
         IF ( i.GE.2 ) THEN
            DO j = 1 , (i-1)
               detau(i) = detau(i) + deroo(j)
            ENDDO
         ENDIF
      ENDDO
!     Error:
      DO i = 1 , ICSAMP
         dexw(i) = 0.5D0
         deyw(i) = 0.5D0
         IF ( i.GE.2 ) THEN
            DO j = 1 , (i-1)
               dexw(i) = dexw(i) + deroo(j)*deroo(j)
               deyw(i) = deyw(i) + (deroo(j)-deroo(j-1))                &
     &                   *(deroo(j)-deroo(j-1))
            ENDDO
         ENDIF
      ENDDO
      DO i = 1 , ICSAMP
!         DESIGMA(I) =
!     +        DETAU(I)*DETAU(I)*(DBLE(I) - 0.5D0 +
!     +        (1.D0 + DEKAPPA(I))/(1.D0 - DEKAPPA(I))) +
!     +        DEKAPPA(I)*DEXW(I)/((1.D0 - DEKAPPA(I))**2.D0) +
!     +        (DEKAPPA(I)**2.D0)*DEYW(I)/((1.D0 - DEKAPPA(I))**4.D0)
!     The above lines were changed to those below by K. G. Hamilton,
!     2-Sep-95, in order to avoid exponentiation errors that can
!     occur with some compilers when taking X**Y and X<0.
         desigma(i) = detau(i)*detau(i)                                 &
     &                *(DBLE(i)-0.5D0+(1.D0+dekappa(i))/(1.D0-dekappa(i)&
     &                )) + dekappa(i)*dexw(i)/((1.D0-dekappa(i))**2)    &
     &                + (dekappa(i)**2)*deyw(i)/((1.D0-dekappa(i))**4)
      ENDDO
      DO i = 1 , ICSAMP
         desigma(i) = desigma(i)*4.D0/DBLE(ISCANS)
      ENDDO
      DO i = 1 , ICSAMP
         desigma(i) = DSQRT(desigma(i))
      ENDDO
 
!     -------------
!     Cluster case:
!     -------------------------------------------------------
      dclave = dclave/ISCANS
      dclavr = dclave
      dclrat = dclavr/L2
      dclave = dclave*dclave
      dcl2 = dcl2/ISCANS
      dclerror = (dcl2-dclave)*2.D0/DBLE(ISCANS)
 
      DO i = 1 , ICSAMP
         dclauto(i) = dclauto(i)/ISCANS
      ENDDO
      dclroo(0) = 1.D0
      DO i = 1 , ICSAMP
         dclroo(i) = (dclauto(i)-dclave)/(dcl2-dclave)
      ENDDO
 
      DO i = 1 , ICSAMP
         dclkappa(i) = dclroo(i)/(dclroo(i-1))
      ENDDO
      DO i = 1 , ICSAMP
         dclrw(i) = dclroo(i)/(1.D0-dclkappa(i))
      ENDDO
      DO i = 1 , ICSAMP
         dcltau(i) = 0.5D0 + dclrw(i)
         IF ( i.GE.2 ) THEN
            DO j = 1 , (i-1)
               dcltau(i) = dcltau(i) + dclroo(j)
            ENDDO
         ENDIF
      ENDDO
!     Error:
      DO i = 1 , ICSAMP
         dclxw(i) = 0.5D0
         dclyw(i) = 0.5D0
         IF ( i.GE.2 ) THEN
            DO j = 1 , (i-1)
               dclxw(i) = dclxw(i) + dclroo(j)*dclroo(j)
               dclyw(i) = dclyw(i) + (dclroo(j)-dclroo(j-1))            &
     &                    *(dclroo(j)-dclroo(j-1))
            ENDDO
         ENDIF
      ENDDO
      DO i = 1 , ICSAMP
!         DCLSIGMA(I) =
!     +        DCLTAU(I)*DCLTAU(I)*(DBLE(I) - 0.5D0 +
!     +        (1.D0 + DCLKAPPA(I))/(1.D0 - DCLKAPPA(I))) +
!     +        DCLKAPPA(I)*DCLXW(I)/((1.D0 - DCLKAPPA(I))**2.D0) +
!     +        (DCLKAPPA(I)**2.D0)*DCLYW(I)/((1.D0 - DCLKAPPA(I))**4.D0)
!     The above lines were changed to those below by K. G. Hamilton,
!     2-Sep-95, in order to avoid exponentiation errors that can
!     occur with some compilers when taking X**Y and X<0.
         dclsigma(i) = dcltau(i)*dcltau(i)                              &
     &                 *(DBLE(i)-0.5D0+(1.D0+dclkappa(i))               &
     &                 /(1.D0-dclkappa(i))) + dclkappa(i)*dclxw(i)      &
     &                 /((1.D0-dclkappa(i))**2) + (dclkappa(i)**2)      &
     &                 *dclyw(i)/((1.D0-dclkappa(i))**4)
      ENDDO
      DO i = 1 , ICSAMP
         dclsigma(i) = dclsigma(i)*4.D0/DBLE(ISCANS)
      ENDDO
      DO i = 1 , ICSAMP
         dclsigma(i) = DSQRT(dclsigma(i))
      ENDDO
 
!     ------------------
!     Write the results:
!     ------------------
!     General data:
      WRITE (*,*) ' Parameters in the autocorrelation test: '
      WRITE (*,*) '      Initial seed:                      ' , diseed
      WRITE (*,*) '      Lattice size (L):                  ' , L
      WRITE (*,*) '      Number of samples (ISCANS):        ' , ISCANS
      WRITE (*,*) '      Coupling constant (DK):            ' , DK
      WRITE (*,*) '      Number of omitted confs. (MCBEG):  ' , MCBEG
      WRITE (*,*)
      WRITE (*,*) ' Average energy of the system:           ' ,         &
     &            deaverage
      WRITE (*,*) ' and its squared (error estimate / tau): ' , deerror
      WRITE (*,*)
      WRITE (*,*) ' Average susceptibility of the system:   ' ,         &
     &            dsaverage/DBLE(L2)
      WRITE (*,*) ' and its squared (error estimate / tau): ' ,         &
     &            dserror/DBLE(L2)
      WRITE (*,*)
!     The average cluster size is given here in a non-normalized form.
!     (not divided by the system size L2).
      WRITE (*,*) ' Average flipped cluster size:           ' , dclavr
      WRITE (*,*) ' and its squared (error estimate / tau): ' , dclerror
      WRITE (*,*) ' ----------------------------------------' ,         &
     &            '--------------------------'
      WRITE (*,*) ' The notation <squared (error estimate / tau)>' ,    &
     &            ' means the following:'
      WRITE (*,*) ' to get the correct error estimate, multiply' ,      &
     &            ' the given value by'
      WRITE (*,*) ' the integrated autocorrelation time tau (see' ,     &
     &            ' comments in the code '
      WRITE (*,*) ' for details), and then take a square root.'
      WRITE (*,*) ' ========================================' ,         &
     &            '=========================='
!      CLOSE(22)
!     Susceptibility results:
      WRITE (20,*) '      ==================================='
      WRITE (20,*) '      AUTO-CORRELATION OF SUSCEPTIBILITY:'
      WRITE (20,*) '      ==================================='
      ifin(1) = 0
      ifin(2) = 1
      WRITE (20,*) ifin(1) , DBLE(ifin(2))
      DO i = 1 , ICSAMP
         WRITE (20,*) i , dsroo(i)
      ENDDO
      WRITE (20,*) '      ====================================='
      WRITE (20,*) '      INTEGRATED AC-TIME OF SUSCEPTIBILITY:'
      WRITE (20,*) '      ====================================='
      DO i = 1 , ICSAMP
         WRITE (20,*) i , dstau(i)*dclrat , dssigma(i)*dclrat
      ENDDO
      CLOSE (20)
!     Energy results:
      WRITE (21,*) '      ==========================='
      WRITE (21,*) '      AUTO-CORRELATION OF ENERGY:'
      WRITE (21,*) '      ==========================='
      WRITE (21,*) ifin(1) , DBLE(ifin(2))
      DO i = 1 , ICSAMP
         WRITE (21,*) i , deroo(i)
      ENDDO
      WRITE (21,*) '      ============================='
      WRITE (21,*) '      INTEGRATED AC-TIME OF ENERGY:'
      WRITE (21,*) '      ============================='
      DO i = 1 , ICSAMP
         WRITE (21,*) i , detau(i)*dclrat , desigma(i)*dclrat
      ENDDO
      CLOSE (21)
!     Cluster results:
      WRITE (23,*) '      ====================================='
      WRITE (23,*) '      AUTO-CORRELATION OF FLIPPED CLUSTERS:'
      WRITE (23,*) '      ====================================='
      WRITE (23,*) ifin(1) , DBLE(ifin(2))
      DO i = 1 , ICSAMP
         WRITE (23,*) i , dclroo(i)
      ENDDO
      WRITE (23,*) '      ======================================='
      WRITE (23,*) '      INTEGRATED AC-TIME OF FLIPPED CLUSTERS:'
      WRITE (23,*) '      ======================================='
      DO i = 1 , ICSAMP
         WRITE (23,*) i , dcltau(i)*dclrat , dclsigma(i)*dclrat
      ENDDO
      CLOSE (23)
 
!     Distribution of flipped clusters:
      DO i = 1 , L2
         iks = icldst(i)
         IF ( iks.NE.0 ) THEN
            WRITE (24,*) i , DBLE(icldst(i))/ISCANS
         ENDIF
      ENDDO
      CLOSE (24)
 
      CONTINUE
      END
!*==WOLFF.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
!     ============================================================
!     Wolff Monte Carlo algorithm (discrete case). The idea is by
!     U. Wolff [Phys. Rev. Lett. 60 (1988) 1461] and the program
!     is a modification of Ref. [Physica A 167 (1990) 565] by
!     J. S. Wang and R. H. Swendsen.
!     ------------------------------
!     Ilpo Vattulainen, Tampere univ. of tech., Funland.
!     14.4.1993
!     ============================================================
 
      SUBROUTINE WOLFF(L,L2,Iz,Istack,In1,Ip1,Prob,Dseed,Isum,Iclsiz)
 
      INTEGER L , L2 , Iz(L,L) , Istack(L2,2) , In1(L) , Ip1(L) ,       &
     &        nn(4,2) , isize , i , j , ipt , ky , kx , Isum , ishere , &
     &        nny , nnx , Iclsiz
      DOUBLE PRECISION Prob
      REAL*8 Dseed
      REAL ran , rans , ranx , rany
 
      ipt = 0
      Isum = 0
 
!     ------------------------------------------------------------------
!     Start a Monte Carlo loop to update a total of MC*L2 lattice sites.
!     ------------------------------------------------------------------
 
!     ISIZE calculates the number of tried updates.
!     ICLSIZ calculates the size of the flipped cluster.
 
      isize = 1
      Iclsiz = 1
 
!     ------------------------------------------------------------------------
!     Choose a starting position and change its state immediately. Other parts
!     of a cluster will also be changed immediately after recognition.
!     ------------------------------------------------------------------------
      rany = GGL(Dseed)
      ranx = GGL(Dseed)
      ky = rany*L + 1
      IF ( ky.GT.L ) ky = L
      kx = ranx*L + 1
      IF ( kx.GT.L ) kx = L
      ishere = Iz(ky,kx)
      Iz(ky,kx) = -ishere
!     -----------------------------
!     Periodic boundary conditions:
!     -----------------------------
 120  nn(1,1) = ky
      nn(1,2) = In1(kx)
      nn(2,1) = ky
      nn(2,2) = Ip1(kx)
      nn(3,1) = In1(ky)
      nn(3,2) = kx
      nn(4,1) = Ip1(ky)
      nn(4,2) = kx
!     ------------------------
!     Check nearest neighbors:
!     ------------------------
      DO j = 1 , 4
         nny = nn(j,1)
         nnx = nn(j,2)
         IF ( ishere.NE.Iz(nny,nnx) ) GOTO 125
         isize = isize + 1
         rans = GGL(Dseed)
         IF ( rans.GT.Prob ) GOTO 125
         Iz(nny,nnx) = -Iz(nny,nnx)
         Iclsiz = Iclsiz + 1
!     -----------------------------------------------------------------
!     Increment the stack of lattice sites which are part of a cluster,
!     but whose nearest neighbors have not yet been checked:
!     -----------------------------------------------------------------
         ipt = ipt + 1
         Istack(ipt,1) = nny
         Istack(ipt,2) = nnx
 125  ENDDO
!     --------------------------------------------------------------
!     When IPT equals zero the cluster has reached its maximum size.
!     Otherwise go back and check the remaining possibilities.
!     --------------------------------------------------------------
      IF ( ipt.EQ.0 ) GOTO 140
      ky = Istack(ipt,1)
      kx = Istack(ipt,2)
      ipt = ipt - 1
      GOTO 120
!     --------------------------------------------
!     Calculate the total amount of tried updates:
!     --------------------------------------------
 140  Isum = isize
 
      CONTINUE
      END
!*==SUSCEP.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
!     =============================================================
!
!     Calculate the susceptibility based on the definition of Wolff
!     [U. Wolff, Phys. Lett. B {\bf 228}, 379 (1989).
!
!     =============================================================
      SUBROUTINE SUSCEP(L,Iz,Dsus)
 
      INTEGER L , Iz(L,L) , iznum
      DOUBLE PRECISION Dsus
 
      iznum = 0
      DO iy = 1 , L
         DO ix = 1 , L
            iznum = iznum + Iz(iy,ix)
         ENDDO
      ENDDO
      Dsus = DBLE(iznum)
      Dsus = Dsus*Dsus
      Dsus = Dsus/(L*L)
 
      CONTINUE
      END
!*==ENERG.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
!     ==============================================================
!
!     Calculate the energy of the system (in variables of U/J).
!
!     ==============================================================
 
      SUBROUTINE ENERG(L,L2,Iz,Dene,In1,Ip1)
 
      INTEGER L , L2 , Iz(L,L) , In1(L) , Ip1(L) , iznum
      DOUBLE PRECISION Dene
 
      iznum = 0
      DO iy = 1 , L
         DO ix = 1 , L
            in1x = In1(ix)
            in1y = In1(iy)
            ip1x = Ip1(ix)
            ip1y = Ip1(iy)
            iznum = iznum + Iz(iy,ix)                                   &
     &              *(Iz(in1y,ix)+Iz(iy,in1x)+Iz(ip1y,ix)+Iz(iy,ip1x))
!            IZNUM = IZNUM + IZ(IY,IX)*(IZ(IN1(IY),IX) +
!     +           IZ(IY,IN1(IX)) + IZ(IP1(IY),IX) + IZ(IY,IP1(IX)))
         ENDDO
      ENDDO
      Dene = DBLE(iznum)/(2*L2)
 
      CONTINUE
      END
!*==GGL.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
 
!     ==============================================================
!     A pseudorandom number generator GGL
!     ------------------------------------------------------------
 
      REAL FUNCTION GGL(Ds)
 
!     +     D1,
      DOUBLE PRECISION Ds , d2
!      DATA D1/2147483648.D0/
      DATA d2/2147483647.D0/
      Ds = DMOD(16807.D0*Ds,d2)
!     Generate U(0,1] distributed random numbers:
      GGL = Ds/d2
 
      CONTINUE
      END
