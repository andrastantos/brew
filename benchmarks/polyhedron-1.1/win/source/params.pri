!*==AA0005.spg  processed by SPAG 6.55Dc at 09:50 on 23 Sep 2005
!
! --- COMMON BLOCK /NUMPARM/ Variables:
!
!         GRAVI - real    - Acceleration due to gravity (m/s**2)
!          RGAS - real    - Gas constant (m**2/s**2/deg. K)
!          ZMIN - real    - Minimum plume centerline height (m)
!            DS - real    - Step size (m) in the numerical plume
!                           rise algorithm
!         NSTEP - integer - Internal save frequency of plume rise
!                           calculations (i.e., every DS*NSTEP meters)
!                           (NOTE: this the frequency with which the
!                           results are saved internally -- not that
!                           passed back from the NUMRISE routine)
!         SLAST - real    - Termination distance (m) of the plume rise
!                           calculation
!            RP - real    - Radiation coefficient (kg/m**2/deg. K**3/s)
!   ALPHAP(mxent) - real array - Perturbed entrainment coefficients
!                                (parallel)
!    BETAP(mxent) - real array - Perturbed entrainment coefficients
!                                (normal)
!   XCAT(mxentp1) - real array - Downwind distances (m) for which each
!                                perturbed entrainment coefficient
!                                (ALPHAP, BETAP) is valid (NENT+1 values
!                                for NENT entrainment coefficients).
!            NENT - integer    - Number of perturbed entrainment
!                                coefficients entered
!----------------------------------------------------------------------
! --- PARAMETER statements                                        PRIME
!----------------------------------------------------------------------
! --- Specify model version
      CHARACTER*12 MVER , MLEVEL
      PARAMETER (MVER='alpha',MLEVEL='950610')
!
! --- Specify parameters
      INTEGER , PARAMETER :: MXNZ = 100
      INTEGER , PARAMETER :: MXNTR = 50
      INTEGER , PARAMETER :: MXNW = 5000
      INTEGER , PARAMETER :: MXENT = 10
!TMP      INTEGER, PARAMETER :: io5=5,io6=6
      INTEGER , PARAMETER :: IO5 = 7 , IO6 = 8
!
! --- Compute derived parameters
      INTEGER , PARAMETER :: MXNZP1 = MXNZ + 1
      INTEGER , PARAMETER :: MXENTP1 = MXENT + 1
!
! --- GENERAL PARAMETER definitions:
!          MXNZ - Maximum number of vertical layers in
!                 the meteorological data
!         MXNTR - Maximum number of downwind distances for which
!                 numerical plume rise will be reported
!          MXNW - Maximum number of downwind distances for numerical
!                 plume rise integration (should be set equal to
!                 SLAST/DS)
!         MXENT - Maximum number of perturbed entrainment coefficients
!                 entered
!
! --- FORTRAN I/O unit numbers:
!           IO5 - Control file                  - input  - formatted
!           IO6 - List file                     - output - formatted
