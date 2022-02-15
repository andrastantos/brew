!*==AA0002.spg  processed by SPAG 6.55Dc at 09:50 on 23 Sep 2005
!
! --- COMMON BLOCK /WAKEDAT/ Variables:
!
!            HB - real    - Building height (m)
!            WB - real    - Building width (crosswind) - (m)
!           XLB - real    - Building length (alongwind) - (m)
!            RB - real    - Scale length (m)
!            HR - real    - Maximum cavity height (m) above ground
!           XLR - real    - Length of downwind cavity (m) from
!                           downwind face of building
!           XLC - real    - Length of roof cavity (m)
!         XBADJ - real    - Distance along the wind from the stack to
!                           the origin of the building (upwind center
!                           of effective building)
!         YBADJ - real    - Distance crosswind from the stack to
!                           the origin of the building (upwind center
!                           of effective building)
!            Ub - real    - Wind speed (m/s) at the height of bldg
!           Urh - real    - Wind speed (m/s) at release height
!
!          NWAK - integer - Number of downwind distances at which
!                           wake properties are tabulated (LE mxntr)
!   XWAK(mxntr) - real    - Downwind distance (m) from source
!  SZWAK(mxntr) - real    - Sigma-z (m) at position XWAK
!  SYWAK(mxntr) - real    - Sigma-y (m) at position XWAK
!  DRWAK(mxntr) - real    - Plume growth rate at position XWAK expressed
!                           as d/dx(plume radius) for equivalent top-hat
!          NCAV - integer - Number of downwind distances at which
!                           wake properties of cavity source are
!                           tabulated (LE mxntr)
!   XCAV(mxntr) - real    - Downwind distance (m) from primary source
!  SZCAV(mxntr) - real    - Sigma-z (m) for cavity source
!  SYCAV(mxntr) - real    - Sigma-y (m) for cavity source
!         FQCAV - real    - Fraction of plume mass captured by cavity
!         ISTAB - integer - PG stability class
!         LRURL - logical - Rural dispersion when .TRUE.
!         VSIGZ - real    - Virtual source sigma (m) for sigma-z beyond wake
!         VSIGY - real    - Virtual source sigma (m) for sigma-y beyond wake
!        VSIGZC - real    - Virtual source sigma (m) for sigma-z beyond wake
!                           for cavity source
!        VSIGYC - real    - Virtual source sigma (m) for sigma-y beyond wake
!                           for cavity source
 
!----------------------------------------------------------------------
! --- COMMON BLOCK /AMBIENT/ -- Selected met. data at one         PRIME
!                               grid cell;  used in numerical
!                               plume rise computation
!----------------------------------------------------------------------
!
      COMMON /AMBIENT/ NZA , UAMB(MXNZ) , RAMB(MXNZ) , DEDZ(MXNZP1) ,   &
     &                 TAMB(MXNZ) , ZFACEA(MXNZP1) , ZGPTA(MXNZ) ,      &
     &                 TAMB0 , RAMB0 , ADIA , PTGRAD0
