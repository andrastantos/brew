! ______________________________________________________________________
!     rnfprm.h :  Parameters for RNFLOW
!     author   :  Michel Olagnon 
!     date     :  4 May 1993
! ______________________________________________________________________
!
!
! .......  unites logiques ...............................
!
      integer, parameter :: luctr = 0    ! unite controle
      integer, parameter :: luinp = 5    ! unite entree
      integer, parameter :: luout = 6    ! unite resultats
      integer, parameter :: luimp = luout, luecr = luctr, luclv = luinp
      integer, parameter :: lufic1 = 21, lufic2 = 22
!
! .......  taille de chaines .............................
!
      integer, parameter :: lficm = 64
      integer, parameter :: largm = 160
!
! .......  taille du probleme ............................
!
      integer, parameter :: ndonm = 1000001 ! nombre de donnees
      integer, parameter :: npicm =   ndonm ! nombre maximal d'extrema
      integer, parameter :: ncls  =     256 ! nombre de classes
      integer, parameter :: nsim  =      64 ! nombre de simulations
      real, parameter    :: xmin0 =     0.0 ! gamme de valeurs
      real, parameter    :: xmax0 =    32.0 ! gamme de valeurs
! ______________________________________________________________________
