!*==AA0003.spg  processed by SPAG 6.55Dc at 09:50 on 23 Sep 2005
!
! --- COMMON BLOCK /AMBIENT/ Variables:
!
!                    NZA - integer - Number of layers
!             UAMB(mxnz) - real    - Wind speed profile (m/s) - winds
!                                    defined at cell CENTERS
!             RAMB(mxnz) - real    - Ambient air density profile
!                                    (kg/m**3) - defined at cell CENTERS
!           DEDZ(mxnzp1) - real    - Pot. temperature gradient profile
!                                    (deg. K/m) - defined at cell FACES
!             TAMB(mxnz) - real    - Temperature profile (deg .K) -
!                                    defined at cell CENTERS
!         ZFACEA(mxnzp1) - real    - Heights of layer faces (m)
!            ZGPTA(mxnz) - real    - Heights of layer centers (m)
!                  TAMB0 - real    - Surface air temperature (deg. K)
!                  RAMB0 - real    - Surface air density (kg/m**3)
!                   ADIA - real    - Dry adiabatic lapse rate (deg. K/m)
!                PTGRAD0 - real    - Minimum potential temperature lapse
!                                    rate (deg. K/m)
!----------------------------------------------------------------------
! --- COMMON BLOCK /DFSN/ -- Parameters used in the            PRIME
!                            PRIME turbulence and diffusion
!                            subroutines
!----------------------------------------------------------------------
!
      REAL RURLIZ(6) , RURLIY(6) , URBNIZ(6) , URBNIY(6)
      COMMON /DFSN  / AFAC , XBYRMAX , WIZ0 , WIY0 , WFZ , WFY ,        &
     &                DUA_UA , XDECAY , XDECAYI , RURLIZ , RURLIY ,     &
     &                URBNIZ , URBNIY
