!*==AA0004.spg  processed by SPAG 6.55Dc at 09:50 on 23 Sep 2005
!
! --- COMMON BLOCK /DFSN/ Variables:
!
!          AFAC - real    - Diffusion transitions to ambient (with
!                           virtual source) when wake turbulence decays
!                           to AFAC*(ambient turbulence intensity) for
!                           PG classes 4, 5, and 6
!       XBYRMAX - real    - Upper limit on distance from upwind face
!                           of bldg to transition point for ambient
!                           diffusion
!       WIZ,WIY - real    - Base Turbulence intensities in wake
!       WFZ,WFY - real    - Scaling factors for sigmaz and sigmay
!        DUA_UA - real    - [Ua-U]/Ua in wake at downwind face of bldg
!                                U: average speed in wake
!                               Ua: ambient speed
!         DECAY - real    - Exponent for turbulence intensity change
!                           with distance from downwind face of bldg
!        DECAYI - real    - 1/DECAY
!     RURLIZ(6) - real    - Rural turbulence intensities in z
!     RURLIY(6) - real    - Rural turbulence intensities in y
!     URBNIZ(6) - real    - Urban turbulence intensities in z
!     URBNIY(6) - real    - Urban turbulence intensities in y
! --- Ambient turbulence intensities are inferred from Briggs (1973)
! --- "Diffusion estimation for small emissions", ATDL-106;
!----------------------------------------------------------------------
! --- COMMON BLOCK /NUMPARM/ -- Parameters used in the            PRIME
!                               numerical plume rise algorithm
!----------------------------------------------------------------------
!
      COMMON /NUMPARM/ GRAVI , RGAS , ZMIN , DS , NSTEP , SLAST , RP ,  &
     &                 ALPHAP(MXENT) , BETAP(MXENT) , XCAT(MXENTP1) ,   &
     &                 NENT
