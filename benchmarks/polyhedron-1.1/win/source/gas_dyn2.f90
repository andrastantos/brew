      MODULE Vcimage
!...Created by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93  
      CHARACTER (LEN=80), SAVE :: CARD, FIELD
      END MODULE Vcimage
      MODULE Vcoord
!...Created by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93  
      INTEGER, PARAMETER :: IMAXP1 = 3000001     ! 60x previous
      REAL, DIMENSION(0:IMAXP1), SAVE :: BAREA
      REAL, DIMENSION(IMAXP1), SAVE :: CAREA, VOL
      REAL, SAVE ::  RRIGHT
      END MODULE Vcoord
      MODULE Vimage
!...Created by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93  
      INTEGER, SAVE :: ICPNT
      LOGICAL, SAVE ::  EOFF
      END MODULE Vimage
module ints
!!--AREA.INT
!
   INTERFACE
      REAL FUNCTION AREA (RADIUS)
      USE Vcoord
      REAL RADIUS
      END FUNCTION AREA
   END INTERFACE
!
!!--CHOZDT.INT
!
   INTERFACE
      SUBROUTINE CHOZDT(NODES, VEL, SOUND, DX, DT, STABF)
      INTEGER NODES
      REAL DT, STABF
      REAL, DIMENSION(NODES) :: VEL, SOUND, DX
      END SUBROUTINE CHOZDT
   END INTERFACE
!
!!--DRAG.INT
!
   INTERFACE
     SUBROUTINE DRAG(AREA, SIGMA, TEMP, PRES, DENS, VEL, REY)
     REAL :: AREA, SIGMA, TEMP, PRES, DENS, VEL, REY
     END SUBROUTINE DRAG
   END INTERFACE
!
!!--EOS.INT
!
   INTERFACE
       SUBROUTINE EOS(NODES, IENER, DENS, PRES, TEMP, GAMMA, CS, SHEAT, CGAMMA, WT)
!
      INTEGER NODES
      REAL SHEAT, CGAMMA, WT
      REAL, DIMENSION(NODES) :: IENER, DENS, PRES, TEMP, GAMMA, CS
      END SUBROUTINE EOS
   END INTERFACE
!
!!--KEEL.INT
!
   INTERFACE
      SUBROUTINE KEEL(NODES, DX, RADIUS, RBOUND, VEL, DENS, IENER,      &
     &    SCREEN, NS, NSNODE, SAREA)
      USE Vcoord
      INTEGER NODES, NS
      LOGICAL SCREEN
      INTEGER, DIMENSION(IMAXP1) :: NSNODE
      REAL, DIMENSION(NODES) :: DX, RADIUS
      REAL, DIMENSION(0:IMAXP1) :: RBOUND
      REAL, DIMENSION(NODES) :: VEL, DENS, IENER
      REAL, DIMENSION(IMAXP1) :: SAREA
      END SUBROUTINE KEEL
   END INTERFACE
!
!!--NEXT.INT
!
   INTERFACE
      SUBROUTINE NEXT
      USE Vcimage
      USE Vimage
      END SUBROUTINE NEXT
   END INTERFACE
!
!!--NOZZLE.INT
!
   INTERFACE
      SUBROUTINE NOZZLE(TIME, P0, D0, E0, C0, PRES, DENS, VEL, ENER, FLOW, GAMMA)
      USE Vcimage
      USE Vimage
      REAL :: TIME, P0, FLOW
      REAL, OPTIONAL :: D0, E0, C0, PRES, DENS, VEL, ENER, GAMMA
      END SUBROUTINE NOZZLE
   END INTERFACE
!
!!--READIN.INT
!
   INTERFACE
      SUBROUTINE READIN(PROB, TITLE, CSTOP, FCYCLE, DCYCLE, DHIST, VHIST&
     &    , IMAX, PHIST, DEBUG, NSTAT, STATS, MAXSTA, NCORE, PPLOT,     &
     &    DPLOT, VPLOT, TPLOT, SLIST, D0, E0, NODES, SHEAT, GAMMA, COLD &
     &    , THIST, NVISC, SCREEN, WEIGHT, TSTOP, STABF)
      USE Vcimage
      USE Vimage
      INTEGER CSTOP,FCYCLE,DCYCLE,IMAX,NSTAT,MAXSTA,NCORE,NODES,NVISC
      REAL PROB, D0, E0, SHEAT, GAMMA, COLD, WEIGHT, TSTOP, STABF
      LOGICAL DHIST, VHIST, PHIST, DEBUG, PPLOT, DPLOT, VPLOT, TPLOT,   &
     &    SLIST, THIST, SCREEN
      CHARACTER TITLE*80
      INTEGER, DIMENSION(MAXSTA) :: STATS
      END SUBROUTINE READIN
   END INTERFACE
!
!!--TERROR.INT
!
   INTERFACE
      SUBROUTINE TERROR(WANTED, FIELD, GOT)
      INTEGER WANTED, GOT
      CHARACTER FIELD*80
      END SUBROUTINE TERROR
   END INTERFACE
!
!!--TSOLVE.INT
!
   INTERFACE
      SUBROUTINE TSOLVE(NODES, ENER, TEMP)
      INTEGER NODES
      REAL, DIMENSION(NODES) :: ENER, TEMP
      END SUBROUTINE TSOLVE
   END INTERFACE
!
!!--VALUE.INT
!
   INTERFACE
      SUBROUTINE VALUE(RESULT, ITYPE)
      USE Vcimage
      USE Vimage
      INTEGER ITYPE
      REAL RESULT
      END SUBROUTINE VALUE
   END INTERFACE
end module ints
!
      REAL FUNCTION AREA (RADIUS)
!
!        GIVEN A POSITION (RADIUS) IN THE CHAMBER, CALCULATE THE
!        CROSS SECTIONAL AREA AT THAT POINT
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE Vcoord
      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      REAL RADIUS
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: POINTS = 38
      INTEGER, PARAMETER :: IMAX = 3000000    ! RJA - 60x previous. This is the limit for the max problem size and is specified in several places. All need to be changed to update the max size.
      REAL, PARAMETER :: PI = 3.14159
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: I, N
      REAL, DIMENSION(0:POINTS) :: X, Y
      REAL :: X1, X2, Y1, Y2, SLOPE, B, XRIGHT
      LOGICAL :: FIRST = .TRUE.
      SAVE X,Y,XRIGHT,FIRST
!-----------------------------------------------
      DATA X/0.1524, 0.1651, 0.1778, 0.1905, 0.2032, 0.2159, 0.2286,    &
     &    0.2413, 0.2540, 0.2667, 0.2794, 0.2921, 0.3048, 0.3175, 0.3302&
     &    , 0.3429, 0.3556, 0.3683, 0.3810, 0.3937, 0.4064, 0.4191,     &
     &    0.4318, 0.4445, 0.4572, 0.4699, 0.4826, 0.4953, 0.5080, 0.5207&
     &    , 2.4084, 13.4988, 40.4863, 43.4108, 45.3158, 52.3008, 54.2058&
     &    , 54.8408, 78.5809/
      DATA Y/0.0206, 0.0208, 0.0211, 0.0216, 0.0222, 0.0231, 0.0242,    &
     &    0.0255, 0.0271, 0.0289, 0.0310, 0.0335, 0.0363, 0.0249, 0.0552&
     &    , 0.0893, 0.1237, 0.1564, 0.1868, 0.2149, 0.2410, 0.2657,     &
     &    0.2897, 0.3136, 0.3380, 0.3633, 0.3896, 0.4169, 0.4448, 0.4725&
     &    , 0.4980, 5.2099, 40.0101, 40.0101, 41.8825, 41.8825, 41.8825 &
     &    , 41.8825, 41.8825/
!
!        FIND TABLE ENTRY AND LINEARLY INTERPOLATE FOR CORRECT AREA.
!        IF RADIUS IS LESS THAN BEGINNING OF DATA OR LARGER THAN END
!        OF DATA, MAKE THE AREA THE SAME AS THE NEAREST DATA POINT
!
      IF (RADIUS <= X(0)/100.0) THEN
          AREA = Y(0)*1.E-4
      ELSE
          DO N = 0, POINTS - 1
              I = N
              IF (RADIUS>=X(N)/100.0 .AND. RADIUS<=X(N+1)/100.0) EXIT 
          END DO
   20     CONTINUE
          X1 = X(I)/100.0
          X2 = X(I+1)/100.0
          IF (X1 == X2) THEN
              I = I + 1
              IF (I > POINTS) THEN
                  AREA = Y(POINTS)*1.E-4
                  RETURN 
              ENDIF
              GO TO 20
          ENDIF
          Y1 = Y(I)*1.E-4
          Y2 = Y(I+1)*1.E-4
          SLOPE = (Y2 - Y1)/(X2 - X1)
          B = Y2 - SLOPE*X2
          AREA = RADIUS*SLOPE + B
      ENDIF
!
      END FUNCTION AREA
      SUBROUTINE CHOZDT(NODES, VEL, SOUND, DX, DT, STABF)
!                                      *********************************
!                                      CHOOSE TIME STEP
!
!                                      STABF IS A STABILITY FACTOR
!                                      *********************************
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER NODES
      REAL DT, STABF
      REAL, DIMENSION(NODES) :: VEL, SOUND, DX
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: IMAX = 3000000    ! RJA - 60x previous.
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: ISET(1)
      REAL :: VSET, SSET
      REAL, DIMENSION (NODES) :: DTEMP
!-----------------------------------------------
      DTEMP = DX/(ABS(VEL) + SOUND)
      ISET = MINLOC (DTEMP)
      DT = DTEMP(ISET(1))
      VSET = VEL(ISET(1))
      SSET = SOUND(ISET(1))
      WRITE (16,                                                         &
     &'('' CELL SETTING DT IS '',I5,'', V='',1PE12.5,                   &
     &  '', CS='',E12.5)') ISET(1), VSET, SSET
      DT = STABF*DT
      RETURN 
      END SUBROUTINE CHOZDT
     SUBROUTINE DRAG(AREA, SIGMA, TEMP, PRES, DENS, VEL, REY)
!
!        CALCULATE PRESSURE DROP DUE TO DRAG ACROSS SCREENS
!
!        INPUT:
!
!        AREA     REAL       AREA OF SCREEN
!        SIGMA    REAL       CONSTANT EQUAL TO 3*SQRT((PI*M)/(8*K))
!                              WHERE M=MOLECULE MASS, K=BOLTZMANN CONST
!        PRES     REAL       FLOW PRESSURE
!        DENS     REAL       FLOW DENSITY
!        TEMP     REAL       FLOW TEMPERATURE
!        VEL      REAL       FLOW VELOCITY
!
!        OUTPUT:
!
!        PRES     REAL       ADJUSTED FLOW PRESSURE
!        REY      REAL       REYNOLD'S NUMBER
!
!        WE ASSUME DRAG IS 0.5*CD*AREA*VEL**2
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      REAL :: AREA, SIGMA, TEMP, PRES, DENS, VEL, REY
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      REAL :: COEFF
!-----------------------------------------------
!
!        CALCULATE REYNOLD'S NUMBER
!
      REY = SIGMA*VEL/(DENS*SQRT(TEMP))
!
!        GET DRAG COEFFICIENT
!
      COEFF = CD(REY)
!
!        CALCULATE DRAG
!
      PRES = PRES + 0.5*COEFF*DENS*AREA*VEL*VEL
!
!-------------------------------------------------
!   I N T E R N A L  F U N C T I O N S
!-------------------------------------------------
!
      CONTAINS
          REAL FUNCTION CD (REY)
!
!        CALCULATE DRAG COEFFICIENT AS A FUNCTION OF REYNOLDS
!        NUMBER FOR A RIGHT CIRCULAR CYLINDER (REF:  PAGE 190
!        OF AIP 50TH ANNIVERSARY PHYSICS VADE MECUM
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
          IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          REAL REY
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
          INTEGER, PARAMETER :: NDATA = 11
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          INTEGER :: IOLD, N, NN, I
          REAL, DIMENSION(NDATA) :: R, DRAG
          REAL :: X1, Y1, X2, Y2, SLOPE, B
          LOGICAL :: FIRST = .TRUE.
          SAVE FIRST,IOLD,R,DRAG
!-----------------------------------------------
          DATA R/0.1,1.0,10.0,100.0,1.E3,2.E3,1.E4,1.E5,2.E5,5.E5,1.E6/
          DATA DRAG/60.0,10.0,3.0,1.8,1.0,1.0,1.2,1.2,1.2,0.3,0.38/
          IF (FIRST) THEN
              FIRST = .FALSE.
              IOLD = NDATA - 1
          ENDIF
!
!        INTERPOLATE ON REYNOLD'S NUMBER
!
          IF (REY >= R(NDATA)) THEN
              N = NDATA - 1
          ELSE IF (REY < R(1)) THEN
              N = 1
          ELSE
              NN = NDATA - 1
              IF (REY < R(IOLD+1)) NN = IOLD
              DO I = NN, 1, -1
                  N = I
                  IF (REY>=R(I) .AND. REY<R(I+1)) GO TO 20
              END DO
          ENDIF
   20     CONTINUE
          X1 = R(N)
          Y1 = DRAG(N)
          X2 = R(N+1)
          Y2 = DRAG(N+1)
          IOLD = N
          SLOPE = (Y2 - Y1)/(X2 - X1)
          B = Y2 - SLOPE*X2
          CD = REY*SLOPE + B
          END FUNCTION CD
!
      END SUBROUTINE DRAG
       SUBROUTINE EOS(NODES, IENER, DENS, PRES, TEMP, GAMMA, CS, SHEAT,  &
     &    CGAMMA, WT)
!
!        EQUATION OF STATE
!
!        INPUT:
!
!        NODES     INTEGER     NUMBER OF CELLS IN MESH
!        IENER     REAL A.     INTERNAL SPECIFIC ENERGY (J/KG)
!        DENS      REAL A.     DENSITY (KG/M**3)
!        SHEAT     REAL        CONSTANT SPECIFIC HEAT TO BE USED
!        CGAMMA    REAL        CONSTANT GAMMA TO BE USED
!
!        OUTPUT:
!
!        PRES      REAL A.     PRESSURE (PASCALS)
!        TEMP      REAL A.     TEMPERATURE (DEG K)
!        GAMMA     REAL A.     THERMODYNAMIC GAMMA
!        CS        REAL A.     SOUND SPEED (M/S)
!
!        NOTE:  THE ENTIRE MESH IS CALCULATED AT ONCE, SO THESE ARRAYS
!               CONTAIN THE VARIABLES FOR EACH CELL
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
      USE INTS , ONLY : TSOLVE
      IMPLICIT NONE
!      INCLUDE 'tsolve.int'
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER NODES
      REAL SHEAT, CGAMMA, WT
      REAL, DIMENSION(NODES) :: IENER, DENS, PRES, TEMP, GAMMA, CS
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      REAL, PARAMETER :: RGAS = 8.314
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      REAL :: CONST
!-----------------------------------------------
!     PARAMETER (RGAS=8.314)
!
!        CONSTANT SPECIFIC HEAT AND GAMMA CALCULATIONS
!
      IF (SHEAT>0.0 .AND. CGAMMA>0.0) THEN
          TEMP(:NODES) = IENER(:NODES)/SHEAT
          PRES(:NODES) = (CGAMMA - 1.0)*DENS(:NODES)*IENER(:NODES)
          GAMMA(:NODES) = CGAMMA
          CS(:NODES) = SQRT(CGAMMA*PRES(:NODES)/DENS(:NODES))
      ELSE
!
!        NON-CONSTANT SPECIFIC HEAT AND GAMMA CALCULATIONS
!
!                                      *********************************
!                                      SOLVE FOR TEMPERATURE
!                                      *********************************
          CONST = RGAS/WT
          CALL TSOLVE (NODES, IENER, TEMP)
!                                      *********************************
!                                      SOLVE FOR PRESSURE
!                                      *********************************
          PRES(:NODES) = CONST*DENS(:NODES)*TEMP(:NODES)
!                                      *********************************
!                                      SOLVE FOR GAMMA
!                                      *********************************
          GAMMA(:NODES) = 1.0 + PRES(:NODES)/(DENS(:NODES)*IENER(:NODES))
!                                      *********************************
!                                      SOLVE FOR SOUND SPEED
!                                      *********************************
          CS(:NODES) = SQRT(GAMMA(:NODES)*PRES(:NODES)/DENS(:NODES))
      ENDIF
!
      END SUBROUTINE EOS
      SUBROUTINE KEEL(NODES, DX, RADIUS, RBOUND, VEL, DENS, IENER,      &
     &    SCREEN, NS, NSNODE, SAREA)
!
!        CALCULATE MESH INITIAL CONDITIONS
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE Vcoord
      USE INTS,only : area
      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER NODES, NS
      LOGICAL SCREEN
      INTEGER, DIMENSION(IMAXP1) :: NSNODE
      REAL, DIMENSION(NODES+1) :: DX, RADIUS
      REAL, DIMENSION(0:IMAXP1) :: RBOUND
      REAL, DIMENSION(NODES+1) :: VEL, DENS, IENER
      REAL, DIMENSION(IMAXP1) :: SAREA
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: IMAX = 3000000    ! RJA - 60x previous.
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: NP1, N, NM1, NST, NN
      REAL, DIMENSION(IMAXP1) :: NAREA, SRAD
      REAL :: DXX, RLEFT, VSET, ESET, DSET, TRANS, XRIGHT
!      INCLUDE 'area.int'
!
      DATA XRIGHT/78.5809E-2/
!
      RRIGHT = XRIGHT
!
!-----------------------------------------------
!
      NS = 0
!
!        CALCULATE SCREEN INFORMATION IF NECESSARY
!
      IF (SCREEN) CALL SGEOM (NS, SRAD, SAREA, NAREA)
!
!        CONSTRUCT A UNIFORM GRID
!
      NP1 = NODES + 1
      DXX = RRIGHT/REAL(NODES - 1)
!
!        CALCULATE RADIUS OF CELL CENTERS
!
      RBOUND(0) = -DXX
      DO N = 1, NP1
          RBOUND(N) = RBOUND(N-1) + DXX
      END DO
      RADIUS(:NP1) = RBOUND(:NP1-1) + DXX/2.0
!
!        CELL WIDTHS
!
      NM1 = NODES - 1
      NST = 1
      IF (SCREEN) THEN
          DO N = 1, NP1
              DX(N) = RBOUND(N) - RBOUND(N-1)
!
!        IF SCREENS PRESENT, SEE IF ONE FITS IN THIS CELL
!
   15         CONTINUE
              IF (NST <= NS) THEN
                  RLEFT = RBOUND(N-1)
                  RRIGHT = RBOUND(N)
                  IF (RLEFT<=SRAD(NST) .AND. RRIGHT>SRAD(NST)) THEN
                      NSNODE(NST) = N
                      NST = NST + 1
                      GO TO 15
                  ENDIF
              ENDIF
          END DO
      ELSE
          DX(:NP1) = RBOUND(1:NP1) - RBOUND(:NP1-1)
!
!        IF SCREENS PRESENT, SEE IF ONE FITS IN THIS CELL
!
      ENDIF
!
!        MAKE SURE ALL SCREENS HAVE BEEN ACCOUNTED FOR
!
      IF (SCREEN) THEN
          IF (NST <= NS) THEN
              NST = NST - 1
              IF (NST <= 0) THEN
                  WRITE (16, '(//'' NO SCREENS WERE ACCOUNTED FOR'')')
              ELSE
                   WRITE (16,                                            &
     &'(//'' NOT ALL SCREENS WERE ACCOUNTED FOR''/                      &
     &    '' SCREENS HAVE BEEN LOCATED AT:''/)')
                   WRITE (16,                                            &
     &'('' N='',I5,'' RADIUS='',1PE12.5,                                &
     &  '' LOCATED IN NODE ''                                           &
     & ,I5)') (NN,SRAD(NN),NSNODE(NN),NN=1,NST)
              ENDIF
              NST = NST + 1
              WRITE (16, '(//'' UNACCOUNTED FOR SCREENS ARE AT:''/)')
               WRITE (16, '('' RADIUS='',1PE12.5)') (SRAD(NN),NN=NST,NS)
              STOP 
          ENDIF
      ENDIF
!
!        CELL BOUNDARY AREAS
!
      NST = 1
      DO N = 0, NP1
          BAREA(N) = AREA(RBOUND(N))
          IF (N == NP1) BAREA(N) = BAREA(N-1)
      END DO
!
!        CELL CENTER AREAS
!
      DO N = 1, NP1
          CAREA(N) = AREA(RADIUS(N))
          IF (N == NP1) CAREA(N) = CAREA(N-1)
      END DO
!
!        MODIFY CELL BOUNDARY AREAS FOR SCREENS IF NECESSARY
!
      IF (SCREEN) THEN
          DO N = 1, NS
              NN = NSNODE(N)
              SAREA(N) = SAREA(N)*BAREA(NN)
              BAREA(NN) = BAREA(NN)*NAREA(N)
          END DO
      ENDIF
!
!        CELL VOLUMES.  ASSUME THE CELL IS A FRUSTRUM OF A RIGHT
!        CIRCULAR CONE
!
      VOL(:NP1) = DX(:NP1)/3.0*(BAREA(:NP1-1)+SQRT(BAREA(:NP1-1)*BAREA(1&
     &    :NP1))+BAREA(1:NP1))
!
!        FILL MESH WITH AMBIENT CONDITIONS OUTSIDE CHAMBER.
!        THESE ARE 70 MICRONS OF MERCURY PRESSURE AND A TEMPERATURE
!        OF 300 DEG KELVIN.  WE ASSUME AN IDEAL GAS.  THE MOLECULAR
!        WEIGHT OF STEAM IS ASSUMED TO BE 18.016 G/MOLE.  THE GAS
!        CONSTANT IS 8.314 J/DEG K/MOLE.
!
!
!        SET CHAMBER INTERNAL ENERGY TO AMBIENT
!
!        I=2.765E5 FOR GAMMA 1.4
!        I=4.1477E5 FOR STEAM
!
      VSET = 0.0
      ESET = 2.765E5
      DSET = 9.629E-7
      VEL(2:NODES) = VSET
      DENS(2:NODES) = DSET
      IENER(2:NODES) = ESET
!
!        PRINT OUT MESH GEOMETRY
!
      WRITE (16, '(///'' MESH GEOMETRY:''//)')
       WRITE (16,                                                        &
     &'('' CELL='',I5,'' R='',E12.5,'' DR='',                           &
     &  E12.5,'' VOL='',                                                &
     &  E12.5,'' LEFT AREA='',E12.5,                                    &
     &  '' RIGHT AREA='',E12.5)') (N,RADIUS(N),DX(N),VOL(N),BAREA(N-1), &
     &    BAREA(N),N=1,NODES)
      IF (SCREEN) THEN
          WRITE (16, '(///''1SCREEN LOCATIONS:''//)')
           DO N = 1, NS
              TRANS = 100.0*NAREA(N)
              NN = NSNODE(N)
              WRITE (16,                                                 &
     &'('' CELL='',I5,'' SCREEN RADIUS='',1PE12.5,                      &
     &  '' CELL RADIUS='',E12.5,'' SCREEN AREA='',                      &
     &   E12.5,'' PERCENT TRANSMISSION='',E12.5)') NN, SRAD(N), RADIUS( &
     &            NN), SAREA(N), TRANS
          END DO
      ENDIF
!
!-----------------------------------------------------
!       INTERNAL SUBROUTINE
!-----------------------------------------------------
!
      CONTAINS
          SUBROUTINE SGEOM (NS, RADIUS, AREA, TRANS)
!
!        CALCULATE SCREEN LOCATIONS AND AREAS
!
!        OUTPUT
!
!        NS          INTEGER        NUMBER OF SCREENS
!        RADIUS      REAL A.        LOCATIONS OF SCREENS
!        AREA        REAL A.        FRACTION OF ORIGINAL NOZZLE AREA
!                                   TAKEN UP BY SCREENS
!        TRANS       REAL A.        FRACTION OF ORIGINAL NOZZLE AREA
!                                   STILL OPEN FOR FLOW (TRANSMISSION
!  !...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
          IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          INTEGER NS
          REAL, DIMENSION(NODES) :: RADIUS, AREA, TRANS
!-----------------------------------------------
          NS = 5
          RADIUS(1) = 0.434108
          RADIUS(2) = 0.45315
          RADIUS(3) = 0.523
          RADIUS(4) = 0.542
          RADIUS(5) = 0.548
          AREA(1) = 0.10
          AREA(2) = 0.10
          AREA(3) = 0.1412
          AREA(4) = 0.1412
          AREA(5) = 0.1412
          TRANS(:5) = 1.0 - AREA(:5)
          END SUBROUTINE SGEOM
!
      END SUBROUTINE KEEL
      SUBROUTINE NEXT
!
!        SEQUENTIALLY SCAN 80 COLUMN CARD IMAGES FROM UNIT 5 FOR
!        NON-DELIMETER SUBSTRINGS.  OUTPUT IS PASSED THROUGH
!        COMMONS /CIMAGE/ AND /IMAGE/ AS EXPLAINED BELOW.
!        SCANNING STOPS AT THE END OF THE SUBSTRING OR AT AN END
!        OF FILE.  IF AN END OF FILE IS ENCOUNTERED, THE FILE IS
!        LEFT POSITIONED AFTER THE END OF FILE.
!
!        OUTPUT:
!
!             FIELD     CHARACTER*80
!                       BLANK FILLED STRING CONTAINING THE NEXT
!                       VALID SUBSTRING ENCOUNTERED, LEFT
!                       JUSTIFIED AND BLANK FILLED.  A VALID
!                       SUBSTRING IS ANY SET OF CONTIGUOUS
!                       NON-DELIMETERS.  DELIMETERS ARE DEFINED
!                       IN THE DECLARATIONS AT THE BEGINNING OF THE
!                       ROUTINE.  RETURNED IN COMMON /CIMAGE/.
!
!             EOFF      LOGICAL
!                       TRUE=END OF FILE ENCOUNTERED, NO VALID SUB-
!                            STRING WAS FOUND
!                       FALSE=VALID SUB-STRING FOUND
!                       THIS FLAG IS RETURNED IN COMMON /IMAGE/.
!
!             CARD      CHARACTER*80
!                       STRING CONTAINING CONTENTS OF LAST RECORD
!                       READ.  THE CONTENTS OF THIS STRING SHOULD
!                       NOT BE CHANGED IF SUBSEQUENT CALLS TO NEXT
!                       ARE GOING TO BE MADE.  THIS STRING IS PASSED
!                       IN COMMON /CIMAGE/.
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE Vcimage
      USE Vimage
      IMPLICIT NONE
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: ICEND = 80
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: NODEL, I, ISTART, KMIN, J, K, IEND, ICPNT_INITIAL
      CHARACTER, DIMENSION(3) :: DELIM
      LOGICAL :: FIRST
      SAVE FIRST
!-----------------------------------------------
      DATA NODEL, DELIM/3, ' ', ',', '='/
      DATA ICPNT_INITIAL/81/
      DATA FIRST /.TRUE./
!
      IF (FIRST) THEN
           FIRST = .FALSE.
           ICPNT = ICPNT_INITIAL
      ENDIF
!                                      *******************************
!                                      IF ICPNT>ICEND, READ THE NEXT
!                                      RECORD OFF UNIT 5 INTO THE
!                                      STRING 'CARD'.  NEXT VERIFY
!                                      THAT THIS IS A NON- BLANK
!                                      CARD.  IF IT IS BLANK, SKIP IT
!                                      AND GET THE NEXT RECORD
!                                      *******************************
   10 CONTINUE
      EOFF = .FALSE.
      IF (ICPNT > ICEND) THEN
          READ (15, '(A)', END=50) CARD
          IF (CARD(1:ICEND) == ' ') GO TO 10
          ICPNT = 1
      ENDIF
!                                      *******************************
!                                      GET THE NEXT SUB-STRING.  WE
!                                      DO THIS AS FOLLOWS.  WE LOOK
!                                      FOR THE NEXT DELIMETER.  IF IT
!                                      IS AS THE SAME POSITION AS THE
!                                      CURRENT POINTER, WE ADVANCE
!                                      THE POINTER AND TRY AGAIN.  IF
!                                      NOT, THEN THE POINTER IS AT
!                                      THE BEGINNING OF A SUB-STRING
!                                      AND THE DELIMETER IS TRAILING
!                                      THIS SUB-STRING.  NOTE THAT WE
!                                      LOOK FOR ALL THE DELIMETERS
!                                      POSSIBLE BEFORE TAKING ANY
!                                      ACTION.
!                                      *******************************
      DO I = ICPNT, ICEND
          ISTART = I
          KMIN = 0
          DO J = 1, NODEL
              K = INDEX(CARD(I:ICEND),DELIM(J))
!                                      *******************************
!                                      INDEX RETURNS POSITIONS
!                                      RELATIVE THE BEGINNING OF THE
!                                      SUB-STRING.  HENCE WE ADD IN
!                                      THE APPROPIATE OFF-SET TO GIVE
!                                      THE INDEX RELATIVE TO THE
!                                      BEGINNING OF THE STRING CARD,
!                                      NOT JUST THE SUB-STRING
!                                      CARD(I:ICEND).
!                                      *******************************
              IF (K /= 0) THEN
                  K = K + I - 1
                  IF (KMIN == 0) THEN
                      KMIN = K
                  ELSE
                      KMIN = MIN(K,KMIN)
                  ENDIF
              ENDIF
          END DO
!                                      *******************************
!                                      IF KMIN IS NOT EQUAL TO THE
!                                      CURRENT POINTER POSITION, THEN
!                                      IT MUST BE POINTING AT THE
!                                      TRAILING DELIMETER OF A VALID
!                                      SUB-STRING.
!                                      *******************************
          IF (KMIN == I) THEN
              CYCLE 
          ELSE IF (KMIN > 0) THEN
              IEND = KMIN - 1
              GO TO 40
          ENDIF
!                                      *******************************
!                                      IF WE FALL THROUGH, THERE WAS
!                                      NO DELIMETER FOUND ON THE
!                                      REMAINDER OF THIS RECORD.
!                                      THIS MEANS THE ENTIRE
!                                      REMAINDER OF THIS RECORD IS A
!                                      VALID SUB-STRING
!                                      *******************************
          IEND = ICEND
          GO TO 40
      END DO
!                                      *******************************
!                                      IF WE FALL THROUGH THIS LOOP,
!                                      THERE WERE NO MORE NON-
!                                      DELIMETERS ON THIS RECORD.  GO
!                                      GET NEXT RECORD
!                                      *******************************
      ICPNT = ICEND + 1
      GO TO 10
!                                      *******************************
!                                      PUT THE SUB-STRING INTO THE
!                                      STRING 'FIELD'.  NOTE THAT
!                                      FORTRAN 77 PADS THE STRING
!                                      WITH BLANKS
!                                      *******************************
   40 CONTINUE
      FIELD = CARD(ISTART:IEND)
      ICPNT = IEND + 2
      RETURN 
!                                      *******************************
!                                      END OF FILE ENCOUNTERED, SET
!                                      FLAG AND RETURN
!                                      *******************************
   50 CONTINUE
      EOFF = .TRUE.
      ICPNT = ICEND + 1
      RETURN 
      END SUBROUTINE NEXT
      SUBROUTINE NOZZLE(TIME, P0, D0, E0, C0, PRES, DENS, VEL, ENER,    &
     &    FLOW, GAMMA)
!
!        DEFINE TIME DEPENDENT FLOW CONDITIONS AT THE NOZZLE EXIT
!
!
!        INPUT:
!
!        TIME      REAL      TIME (S) TO DEFINE CONDITIONS AT
!        P0        REAL      INITIAL CHAMBER PRESSURE (PASCALS)
!        D0        REAL      INITIAL CHAMBER DENSITY (KG/M**3)
!                              NOT REQUIRED IF FLOW NOT SET
!        E0        REAL      INITIAL CHAMBER SPECIFIC INTERNAL ENERGY
!                                  (J/KG)
!        C0        REAL      INITIAL CHAMBER SOUND VELOCITY (M/S)
!                              NOT REQUIRED IF FLOW NOT SET
!        FLOW      REAL      IF NON-NEGATIVE, THIS DEFINES THE MASS
!                              FLOW RATE THROUGH THE NOZZLE.  IF SET,
!                              FLOW WILL BE CONSTANT IN TIME.
!        GAMMA     REAL      INITIAL CHAMBER GAMMA (NOT REQUIRED IF
!                              FLOW IS GIVEN)
!
!        OUTPUT:
!
!        PRES      REAL      PRESSURE (PASCALS)
!        DENS      REAL      DENSITY (KG/M**3)
!        VEL       REAL      VELOCITY (M/S)
!        ENER      REAL      INTERNAL SPECIFIC ENERGY (J/KG)
!
!        IF FLOW IS NOT SET, THEN
!        WE ASSUME CRITICAL FLOW ACROSS THE NOZZLE AND CALCULATE THE
!        DECAY IN THE CHAMBER ASSUMING ISOTHERMAL CONDITIONS.
!
!
!        AREA = NOZZLE AREA (M**2)
!        VOL  = VOLUME OF CHAMBER (M**3)
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE Vcimage
      USE Vimage
      USE INTS,only : terror,value
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      REAL TIME, P0, FLOW
      REAL, OPTIONAL :: D0, E0, C0, PRES, DENS, VEL, ENER, GAMMA
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      REAL, PARAMETER :: AREAt = 5.1725E-7
      REAL, PARAMETER :: VOL = 1.24E-5
      INTEGER, PARAMETER :: IMAX = 3000000    ! RJA - 60x previous.
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: TYPE, ICNT, IJ, IOLD, N, NN, I
      REAL, DIMENSION(IMAX) :: P, T
      REAL :: X1, Y1, X2, Y2, SLOPE, B
      LOGICAL :: FIRST = .TRUE.
      CHARACTER :: INFIL*30
      SAVE FIRST,ICNT,T,P, IOLD
!      INCLUDE 'terror.int'
!      INCLUDE 'value.int'
!-----------------------------------------------
!
!        IF FLOW NOT SET, USE INPUTED PRESSURE-TIME HISTORY
!
      IF (FLOW <= 0.0) THEN
          IF (FIRST) THEN
              ICPNT = 999
              ICNT = 1
              WRITE (*, '(//'' ENTER P-T HISTORY FILE NAME: '')')
               READ (*, '(A)') INFIL
              OPEN(UNIT=15, FILE=INFIL, STATUS='UNKNOWN')
              REWIND 15
    3         CONTINUE
              CALL VALUE (T(ICNT), TYPE)
              IF (.NOT.EOFF) THEN
                  IF (TYPE /= 1) THEN
                      CALL TERROR (1, FIELD, TYPE)
                      WRITE (16, '('' ABORT IN NOZZLE'')')
                       STOP 
                  ENDIF
                  CALL VALUE (P(ICNT), TYPE)
                  IF (EOFF) THEN
                      WRITE (16, '('' UNEXPECTED EOF IN NOZZLE, ABORT'')'&
     &                    )
                       STOP 
                  ELSE IF (TYPE /= 1) THEN
                      CALL TERROR (1, FIELD, TYPE)
                      WRITE (16, '('' ABORT IN NOZZLE'')')
                       STOP 
                  ENDIF
                  ICNT = ICNT + 1
                  GO TO 3
              ELSE
                  ICNT = ICNT - 1
                  IF (ICNT <= 0) THEN
                      WRITE (16, '('' NO NOZZLE P/T HISTORY, ABORT'')')
                       CLOSE(UNIT=15)
                      STOP 
                  ENDIF
                  CLOSE(UNIT=15)
                  WRITE (16, '(''1PRESSURE-TIME HISTORY FOR CHAMBER''/)')
                   DO IJ = 1, ICNT
                      P(IJ) = P(IJ)/1.727865
                      WRITE (16, '('' TIME='',1PE12.5,'' PRES='',E12.5)')&
     &                     T(IJ), P(IJ)
                  END DO
                  P0 = P(1)
                  FIRST = .FALSE.
                  IOLD = ICNT - 1
                  RETURN 
              ENDIF
          ENDIF
          IF (TIME >= T(ICNT)) THEN
              N = ICNT - 1
          ELSE IF (TIME < T(1)) THEN
              N = 1
          ELSE
              NN = ICNT - 1
              IF (TIME < T(IOLD+1)) NN = IOLD
              DO I = NN, 1, -1
                  N = I
                  IF (TIME>=T(I) .AND. TIME<T(I+1)) GO TO 20
              END DO
          ENDIF
   20     CONTINUE
          X1 = T(N)
          Y1 = P(N)
          X2 = T(N+1)
          Y2 = T(N+2)
          IOLD = N
          SLOPE = (Y2 - Y1)/(X2 - X1)
          B = Y2 - SLOPE*X2
          PRES = TIME*SLOPE + B
          ENER = E0
          DENS = PRES/((GAMMA - 1.0)*ENER)
          VEL = C0
      ELSE
!
!        FLOW IS SET, CALCULATE CONSTANT FLOW
!
          VEL = FLOW/(D0*AREAt)
          ENER = E0
          DENS = D0
          PRES = P0
      ENDIF
      END SUBROUTINE NOZZLE
      SUBROUTINE READIN(PROB, TITLE, CSTOP, FCYCLE, DCYCLE, DHIST, VHIST&
     &    , IMAX, PHIST, DEBUG, NSTAT, STATS, MAXSTA, NCORE, PPLOT,     &
     &    DPLOT, VPLOT, TPLOT, SLIST, D0, E0, NODES, SHEAT, GAMMA, COLD &
     &    , THIST, NVISC, SCREEN, WEIGHT, TSTOP, STABF)
!
!        FREE FORMAT INPUT DRIVER
!        JOHN K. PRENTICE,  11 JULY 1986
!
!        THIS ROUTINE READS THE FREE FORMAT INPUT FILE TO DETERMINE
!        PARAMETERS FOR A CALCULATION.  BELOW IS GIVEN A LIST OF THE
!        INPUT PARAMETERS SHOWING THE USER INPUT WORD(S), THE FORTRAN
!        VARIABLE NAME LOADED, AND THE DESCRIPTION OF THE PARAMETER.
!
!        INPUT WORD(S)           FORTRAN    TYPE         DESCRIPTION
!                                VARIABLE
!                                NAME
!        -------------           --------   ----   ---------------------
!
!        PROBLEM_NUMBER            PROB      R     PROBLEM NUMBER
!            -OR-
!        PROB
!
!        TITLE                     TITLE     !     80 CHARACTER TITLE
!                                                  FOR THIS CALCULATION.
!                                                  WHEN THE WORD TITLE
!                                                  IS ENCOUNTERED IN
!                                                  INPUT, SCANNING WILL
!                                                  STOP FOR THAT LINE
!                                                  AND THE ENTIRE NEXT
!                                                  LINE WILL BE READ AS
!                                                  THE TITLE.
!
!        NUMBER_OF_NODES           NODES     I     NUMBER OF CELLS IN
!              -OR-                                MESH
!        NODES
!
!        CYCLE_STOP                CSTOP     I     LAST CYCLE TO RUN
!             -OR-                                 CALCUALATION TO
!        CSTOP
!
!        TIME_STOP                 TSTOP     R     LAST TIME TO RUN
!             -OR-                                 CALCULATION TO.
!        TSTOP                                     OPTIONAL.
!
!        STABILITY_FACTOR          STABF     R     COURANT CONDITION
!             -OR-                                 STABILITY FACTOR
!        STABF
!
!        FIRST_DUMP                FCYCLE    I     FIRST CYCLE TO
!             -OR-                                 DUMP RESULTS AT.
!        FCYCLE                                    DEFAULT IS 1
!
!        DUMP_INTERVAL             DCYCLE    I     CYCLE INTERVAL TO
!             -OR-                                 DUMP RESULTS.
!        DCYCLE                                    DEFAULT IS 1
!
!        SPECIFIC_HEAT             SHEAT     R     SPECIFIC HEAT OF GAS.
!              -OR-                                IF NOT SPECIFIED, THEN
!        SHEAT                                     A VALUE WILL BE CHOSEN
!                                                  APPROPIATE TO STEAM.
!                                                  IF SET, CONSTANT VALUE
!                                                  WILL BE ASSUMED. MUST
!                                                  SET GAMMA IS SPECIFIC
!                                                  HEAT IS SET.
!
!        GAMMA                     GAMMA     R     THERMODYNAMIC GAMMA OF
!                                                  GAS.  IF NOT SPECIFIED,
!                                                  A VALUE APPROPIATE TO
!                                                  STEAM WILL BE USED.
!                                                  IF SET, CONSTANT VALUE
!                                                  WILL BE ASSUMED.  MUST
!                                                  SET SPECIFIC HEAT IF
!                                                  GAMMA IS SET.
!
!        MOLECULAR_WEIGHT          WEIGHT    R     MOLEULAR WEIGHT IN
!             -OR-                                 KG/MOLE.  IF NOT
!        WEIGHT                                    SPECIFIED, A VALUE
!                                                  APPROPIATE TO WATER
!                                                  WILL BE USED
!
!        INCLUDE_SCREENS           SCREEN    B     IF SET, SCREENS WILL
!                                                  BE INCLUDED IN
!                                                  CALCULATION.  DEFAULT
!                                                  IS NO SCREENS
!
!        CONSTANT_FLOW             COLD      R     IF SET, CONSTANT FLOW
!                                                  THROUGH THE NOZZLE
!                                                  WILL BE ASSUMED AT THE
!                                                  MASS RATE GIVEN
!
!        CHAMBER_DENSITY           D0        R     INITIAL CHAMBER
!             -OR-                                 DENSITY (KG/M**3)
!        D0
!
!        CHAMBER_ENERGY            E0        R     INITIAL CHAMBER
!             -OR-                                 SPECIFIC INTERNAL
!        E0                                        ENERGY (J/KG)
!
!        TEMPERATURE_HISTOGRAM     THIST     B     CAUSE TEMPERATURE
!             -OR-                                 HISTOGRAM TO BE
!        THIST                                     PRODUCED.  DEFAULT
!                                                  IS NO PLOT
!
!        DENSITY_HISTOGRAM         DHIST     B     CAUSE
!             -OR-                                 DENSITY HISTOGRAM TO
!        DHIST                                     BE PRODUCED. DEFAULT
!                                                  IS NO PLOT
!
!        PRESSURE_HISTOGRAM        PHIST     B     CAUSE
!             -OR-                                 PRESSURE HISTOGRAM TO
!        PHIST                                     BE PRODUCED.  DEFAULT
!                                                  IS NO PLOT
!
!        VELOCITY_HISTOGRAM        VHIST     B     CAUSE
!             -OR-                                 VELOCITY HISTOGRAM TO
!        VHIST                                     BE PRODUCED.  DEFAULT
!                                                  IS NO PLOT
!
!        DEBUG                     DEBUG     B     CAUSE
!                                                  DEBUG INFORMATION TO
!                                                  BE PRODUCED EACH DUMP
!                                                  DEFAULT NO DEBUG INFO
!
!        STATIONS                  STATS     I     LIST OF CELL NUMBERS
!          -OR-                                    FOR WHICH TO STORE
!        STATS                                     STATION DATA.  NOTE
!                                                  THAT FORTRAN VARIABLE
!                                                  STATS IS AN INTEGER
!                                                  ARRAY.  THE LENGTH
!                                                  IS COMPUTED IN THIS
!                                                  ROUTINE TO BE NSTAT
!                                                  AND THIS VALUE IS
!                                                  PASSED BACK.  DEFAULT
!                                                  IS NSTAT=0.
!                                                  NOTE:  THE NUMBER OF
!                                                  WORDS OF
!                                                  CM NEEDED FOR EACH
!                                                  STATION IS NCORE
!
!        *** IF NSTAT > 0, THEN THE FOLLOWING TYPES OF STATION PLOTS
!            MAY BE REQUESTED.  DEFAULT IN ALL CASES IS NO PLOT
!
!        PRESSURE_PLOT             PPLOT     B     PRESSURE -VS- TIME
!             -OR-                                 PLOT FOR EACH
!        PPLOT                                     STATION
!
!        DENSITY_PLOT              DPLOT     B     DENSITY -VS- TIME
!             -OR-                                 PLOT FOR EACH
!        DPLOT                                     STATION
!
!        VELOCITY_PLOT             VPLOT     B     VELOCITY -VS- TIME
!             -OR-                                 PLOT FOR EACH
!        VPLOT                                     STATION
!
!        TEMPERATURE_PLOT          TPLOT     B     TEMPERATURE -VS- TIME
!             -OR-                                 PLOT FOR EACH STATION
!        TPLOT
!
!        PRINT_STATION_DATA        SLIST     B     PRINT STATION DATA
!             -OR-                                 FOR EACH STATION
!        SLIST
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE Vcimage
      USE Vimage
      USE INTS,only:terror,value
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER CSTOP,FCYCLE,DCYCLE,IMAX,NSTAT,MAXSTA,NCORE,NODES,NVISC
      REAL PROB, D0, E0, SHEAT, GAMMA, COLD, WEIGHT, TSTOP, STABF
      LOGICAL DHIST, VHIST, PHIST, DEBUG, PPLOT, DPLOT, VPLOT, TPLOT,   &
     &    SLIST, THIST, SCREEN
      CHARACTER TITLE*80
      INTEGER, DIMENSION(MAXSTA) :: STATS
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: TYPE, NCYCLE, I
      REAL :: RNODES, RCSTOP, RCYCLE, AVISC, RSTAT
      LOGICAL :: ABORT
!      INCLUDE 'terror.int'
!      INCLUDE 'value.int'
!-----------------------------------------------
!                                      *********************************
!                                      DEFAULTS
!                                      *********************************
      NODES = -999
      COLD = -999.0
      SHEAT = -999.0
      GAMMA = -999.0
      TSTOP = 1.E10
      STABF = 0.5
      WEIGHT = 18.016E-3
      DCYCLE = 1
      FCYCLE = 1
      NVISC = 0
      SCREEN = .FALSE.
      THIST = .FALSE.
      DHIST = .FALSE.
      PHIST = .FALSE.
      VHIST = .FALSE.
      DEBUG = .FALSE.
      PPLOT = .FALSE.
      DPLOT = .FALSE.
      VPLOT = .FALSE.
      TPLOT = .FALSE.
      SLIST = .FALSE.
      NSTAT = 0
      
      
      
!                                      *********************************
!                                      PARSE INPUT AND COMPARE TO
!                                      EXPECTED STRINGS
!                                      *********************************
   10 CONTINUE
      CALL NEXT
   20 CONTINUE
      IF (.NOT.EOFF) THEN
!                                      *********************************
!                                      PROBLEM NUMBER
!                                      *********************************
          IF (FIELD=='PROB' .OR. FIELD=='PROBLEM_NUMBER') THEN
              CALL VALUE (PROB, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      TITLE
!                                      *********************************
          ELSE IF (FIELD == 'TITLE') THEN
              READ (15, '(A)', END=30) TITLE
              ICPNT = 9999
              GO TO 10
   30         CONTINUE
              WRITE (16,                                                 &
     &'('' END OF FILE ENCOUNTERED WHILE TRYING TO '',                  &
     &  ''READ THE TITLE, ABORT'')')
               STOP 
!                                      *********************************
!                                      NUMBER OF NODES
!                                      *********************************
          ELSE IF (FIELD=='NUMBER_OF_NODES' .OR. FIELD=='NODES') THEN
              CALL VALUE (RNODES, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
              NODES = INT(RNODES)
!
!        TIME STOP
!
          ELSE IF (FIELD=='TIME_STOP' .OR. FIELD=='TSTOP') THEN
              CALL VALUE (TSTOP, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!
!        STABILITY FACTOR
!
          ELSE IF (FIELD=='STABILITY_FACTOR' .OR. FIELD=='STABF') THEN
              CALL VALUE (STABF, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      CYCLE STOP
!                                      *********************************
          ELSE IF (FIELD=='CSTOP' .OR. FIELD=='CYCLE_STOP') THEN
              CALL VALUE (RCSTOP, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
              CSTOP = INT(RCSTOP)
!                                      *********************************
!                                      FIRST DUMP
!                                      *********************************
          ELSE IF (FIELD=='FCYCLE' .OR. FIELD=='FIRST_DUMP') THEN
              CALL VALUE (RCYCLE, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
              FCYCLE = INT(RCYCLE)
!                                      *********************************
!                                      DUMP INTERVAL
!                                      *********************************
          ELSE IF (FIELD=='DCYCLE' .OR. FIELD=='DUMP_INTERVAL') THEN
              CALL VALUE (RCYCLE, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
              DCYCLE = INT(RCYCLE)
!                                      *********************************
!                                      SPECIFIC HEAT
!                                      *********************************
          ELSE IF (FIELD=='SPECIFIC_HEAT' .OR. FIELD=='SHEAT') THEN
              CALL VALUE (SHEAT, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      MOLECULAR WEIGHT
!                                      *********************************
          ELSE IF (FIELD=='MOLECULAR_WEIGHT' .OR. FIELD=='WEIGHT') THEN
              CALL VALUE (WEIGHT, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      GAMMA
!                                      *********************************
          ELSE IF (FIELD == 'GAMMA') THEN
              CALL VALUE (GAMMA, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      CONSTANT MASS FLOW
!                                      *********************************
          ELSE IF (FIELD == 'CONSTANT_FLOW') THEN
              CALL VALUE (COLD, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      MEAN FREE PATH IN SHOCK
!                                      *********************************
          ELSE IF (FIELD == 'MEAN_FREE_PATH') THEN
              CALL VALUE (AVISC, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
              NVISC = INT(AVISC)
              IF (NVISC < 0) THEN
                  WRITE (16,                                             &
     &'(//'' ILLEGAL MEAN FREE PATH OF '',I5,                       '', &
     &ABORT.''//)') NVISC
                  STOP 
              ENDIF
!                                      *********************************
!                                      INITIAL CHAMBER DENSITY
!                                      *********************************
          ELSE IF (FIELD=='CHAMBER_DENSITY' .OR. FIELD=='D0') THEN
              CALL VALUE (D0, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      INITIAL CHAMBER ENERGY
!                                      *********************************
          ELSE IF (FIELD=='CHAMBER_ENERGY' .OR. FIELD=='E0') THEN
              CALL VALUE (E0, TYPE)
              IF (TYPE /= 1) THEN
                  CALL TERROR (1, FIELD, TYPE)
                  STOP 
              ENDIF
!                                      *********************************
!                                      INCLUDE SCREENS
!                                      *********************************
          ELSE IF (FIELD == 'INCLUDE_SCREENS') THEN
              SCREEN = .TRUE.
!                                      *********************************
!                                      TEMPERATURE HISTOGRAM
!                                      *********************************
          ELSE IF (FIELD=='THIST' .OR. FIELD=='TEMPERATURE_HISTOGRAM')  &
     &            THEN
              THIST = .TRUE.
!                                      *********************************
!                                      DENSITY HISTOGRAM
!                                      *********************************
          ELSE IF (FIELD=='DHIST' .OR. FIELD=='DENSITY_HISTOGRAM') THEN
              DHIST = .TRUE.
!                                      *********************************
!                                      PRESSURE HISTOGRAM
!                                      *********************************
          ELSE IF (FIELD=='PHIST' .OR. FIELD=='PRESSURE_HISTOGRAM') THEN
              PHIST = .TRUE.
!                                      *********************************
!                                      VELOCITY HISTOGRAM
!                                      *********************************
          ELSE IF (FIELD=='VHIST' .OR. FIELD=='VELOCITY_HISTOGRAM') THEN
              VHIST = .TRUE.
!                                      *********************************
!                                      DEBUG
!                                      *********************************
          ELSE IF (FIELD == 'DEBUG') THEN
              DEBUG = .TRUE.
!                                      *********************************
!                                      STATIONS
!                                      *********************************
          ELSE IF (FIELD=='STATS' .OR. FIELD=='STATIONS') THEN
   40         CONTINUE
              NSTAT = NSTAT + 1
              IF (NSTAT > IMAX) THEN
                  WRITE (16,                                             &
     &'('' TOO MANY STATIONS HAVE BEEN DEFINED.''/                      &
     &  '' THE MAXIMUM ALLOWED IS '',I5/                                &
     &  '' CHANGE IMAX IN THE'',                                        &
     &  '' PROGRAM AND RECOMPILE'')') IMAX
                  STOP 
              ELSE
                  CALL VALUE (RSTAT, TYPE)
                  IF (TYPE /= 1) THEN
                      NSTAT = NSTAT - 1
                      EOFF = .FALSE.
                      IF (TYPE == (-1)) EOFF = .TRUE.
                      GO TO 20
                  ELSE
                      STATS(NSTAT) = INT(RSTAT)
                      GO TO 40
                  ENDIF
              ENDIF
!                                      *********************************
!                                      STATION PRESSURE PLOT
!                                      *********************************
          ELSE IF (FIELD=='PPLOT' .OR. FIELD=='PRESSURE_PLOT') THEN
              PPLOT = .TRUE.
!                                      *********************************
!                                      STATION DENSITY PLOT
!                                      *********************************
          ELSE IF (FIELD=='DPLOT' .OR. FIELD=='DENSITY_PLOT') THEN
              DPLOT = .TRUE.
!                                      *********************************
!                                      STATION VELOCITY PLOT
!                                      *********************************
          ELSE IF (FIELD=='VPLOT' .OR. FIELD=='VELOCITY_PLOT') THEN
              VPLOT = .TRUE.
!                                      *********************************
!                                      STATION TEMPERATURE PLOT
!                                      *********************************
          ELSE IF (FIELD=='TPLOT' .OR. FIELD=='TEMPERATURE_PLOT') THEN
              TPLOT = .TRUE.
!                                      *********************************
!                                      PRINT STATION DATA
!                                      *********************************
          ELSE IF (FIELD=='SLIST' .OR. FIELD=='PRINT_STATION_DATA') THEN
              SLIST = .TRUE.
!                                      *********************************
!                                      UNRECOGNIZED WORD
!                                      *********************************
          ELSE
              WRITE (16,                                                 &
     &'('' UNRECOGNIZED WORD ENCOUNTERED IN INPUT''/                    &
     &  '' THE WORD WAS ----> '',A/'' ABORT''//)') FIELD
              STOP 
          ENDIF
      ELSE
!                                      *********************************
!                                      MAKE SURE THE NUMBER OF NODES IS
!                                      A VALID NUMBER
!                                      *********************************
          IF (NODES == -999) THEN
              WRITE (16,                                                 &
     &'('' THE NUMBER OF NODES WAS NOT SPECIFIED IN '',                 &
     &  ''INPUT, ABORT'')')
               STOP 
          ELSE IF (NODES <= 0) THEN
              WRITE (16,                                                 &
     &'('' AN ILLEGAL VALUE WAS GIVEN FOR THE NUMBER '',                &
     &  ''OF NODES, ABORT'')')
               STOP 
          ELSE IF (NODES > IMAX + 1) THEN
                WRITE (16,                                                 &
     &'('' THE NUMBER OF NODES REQUESTED WAS '',I5/                     &
     &  '' THE MAXIMUM ALLOWED IS '',I5/                                &
     &  '' TO RUN MORE THAN THE MAXIMUM, CHANGE IMAX'',                 &
     &  '' IN THE CODE AND RECOMPILE. ABORT.''//)') NODES, IMAX
              STOP 
          ENDIF
!                                      *********************************
!                                      CHECK WHETHER BOTH GAMMA AND
!                                      SPECIFIC HEAT WERE GIVEN
!                                      *********************************
          IF (GAMMA>0.0 .OR. SHEAT>0.0) THEN
              IF (GAMMA>0.0 .AND. SHEAT<=0.0) THEN
                  WRITE (16,                                             &
     &'(//'' IF GAMMA IS SPECIFIED, SO MUST BE THE''                    &
     &   ,'' THE SPECIFIC HEAT.  ABORT'')')
                   STOP 
              ELSE IF (GAMMA<0.0 .AND. SHEAT>0.0) THEN
                  WRITE (16,                                             &
     &'(//'' IF SPECIFIC HEAT IS SPECIFIED, SO '',                    ''&
     &MUST BE GAMMA.  ABORT'')')
                   STOP 
              ENDIF
          ENDIF
!                                      *********************************
!                                      SORT THE STATIONS AND BE SURE
!                                      THAT THERE IS ENOUGH STORAGE FOR
!                                      THE STATIONS
!                                      *********************************
          IF (NSTAT > 0) THEN
              NCYCLE = CSTOP - FCYCLE + 1
              NCORE = NCYCLE*NSTAT
              IF (NCORE > MAXSTA) THEN
                  WRITE (16,                                            &
     &'('' THERE IS NOT ENOUGH ARRAY STORAGE TO STORE '',               &
     &''ALL THE STATION DATA FOR THE STATIONS DEFINED''/                &
     &'' YOU NEED AT LEAST '',I8,'' WORDS, BUT HAVE'',                  &
     &'' ONLY '',I8/'' CHANGE PARAMETER MAXSTA IN THE '',               &
     &''MAIN ROUTINE TO AT LEAST '',I8,'' AND RECOMPILE''/              &
     &/)') NCORE, MAXSTA, NCORE
                  STOP 
              ENDIF
              NCORE = NCYCLE
              ABORT = .FALSE.
              CALL QSORT (STATS(1:NSTAT))
              DO I = 1, NSTAT
                  IF (STATS(I)<1 .OR. STATS(I)>NODES) WRITE (16,         &
     &'(/                  '' STATION AT CELL '',I5,'' IS OUTSIDE MESH''&
     &)') STATS(I)
              END DO
          ENDIF
!                                      *********************************
!                                      PRINT OUT INPUT VALUES
!                                      *********************************
          WRITE (16,                                                     &
     &'(''1CORTESA ONE DIMENSIONAL GAS DYNAMICS CODE''//1X,A//             &
     &  '' PARAMETERS FOR THIS RUN ARE:''/)') TITLE
          WRITE (16,                                                     &
     &'('' PROBLEM NUMBER = '',F10.5/                                   &
     &  '' NUMBER OF NODES = '',I5/                                     &
     &  '' CYCLE STOP = '',I5/                                          &
     &  '' TIME STOP = '',1PE12.5/                                      &
     &  '' COURANT CONDITION STABILITY FACTOR = '',0PF5.2/              &
     &  '' FIRST CYCLE TO DUMP AT = '',I5/                              &
     &  '' DUMP INTERVAL = '',I5/)') PROB, NODES, CSTOP, TSTOP, STABF,  &
     &        FCYCLE, DCYCLE
          IF (COLD < 0.0) THEN
              WRITE (16,                                                 &
     &'(//'' CALCULATE NOZZLE FLOW USING PRESSURE-TIME HISTORY FROM INPU&
     &T''/)')
          ELSE
               WRITE (16,                                                &
     &'(//'' CALCULATE NOZZLE FLOW AS CONSTANT AT '',                 1P&
     &E12.5,'' KG/SEC''/)') COLD
          ENDIF
          IF (SCREEN) THEN
              WRITE (16, '(/'' INCLUDE SCREENS IN CALCULATION''/)')
          ELSE
               WRITE (16,                                                &
     &            '(/'' NO SCREENS ARE PRESENT IN CALCULATION''/)')
          ENDIF
           IF (SHEAT > 0.0) THEN
              WRITE (16,                                                 &
     &'(//'' USE CONSTANT SPECIFIC HEAT OF '',1PE12.5,                 '&
     &' JOULES/DEG KELVIN/KILOGRAM'')') SHEAT
              WRITE (16, '('' USE CONSTANT GAMMA OF '',1PE12.5)') GAMMA
          ELSE
              WRITE (16,                                                 &
     &            '(//'' USE THERMODYNAMICS APPROPIATE TO STEAM'')')
          ENDIF
           WRITE (16, '('' MOLECULAR WEIGHT = '',1PE12.5,'' KG/MOLE''/)')&
     &         WEIGHT
          IF (DHIST) WRITE (16, '('' PLOT DENSITY HISTOGRAM ''/)')
           IF (PHIST) WRITE (16, '('' PLOT PRESSURE HISTOGRAM ''/)')
           IF (VHIST) WRITE (16, '('' PLOT VELOCITY HISTOGRAM ''/)')
           IF (DEBUG) WRITE (16, '('' PRINT DEBUG INFORMATION ''/)')
           IF (NSTAT > 0) THEN
              IF (PPLOT) WRITE (16, '('' PLOT STATION PRESSURE ''/)')
               IF (DPLOT) WRITE (16, '('' PLOT STATION DENSITY ''/)')
               IF (VPLOT) WRITE (16, '('' PLOT STATION VELOCITY ''/)')
               IF (TPLOT) WRITE (16, '('' PLOT STATION TEMPERATURE ''/)')
               IF (SLIST) WRITE (16, '('' PRINT STATION DATA ''/)')
               WRITE (16, '(///'' STATIONS ARE LOCATED AT CELLS:''/)')
               WRITE (16, '(''       '',I5/)') (STATS(I),I=1,NSTAT)
          ELSE IF (PPLOT .OR. DPLOT .OR. VPLOT .OR. TPLOT) THEN
              WRITE (16,                                                 &
     &'(//'' STATION PLOTS HAVE BEEN REQUESTED, BUT NO'',               &
     &    '' STATIONS HAVE BEEN DEFINED.  ABORT''//)')
               STOP 
          ELSE IF (SLIST) THEN
              WRITE (16,                                                 &
     &'(//'' YOU HAVE REQUESTED A PRINTOUT OF THE STATION'',            &
     &    '' DATA, BUT NO STATIONS HAVE BEEN DEFINED. '',               &
     &    '' ABORT''//)')
               STOP 
          ENDIF
          RETURN 
      ENDIF
      GO TO 10
!
!-------------------------------------------------
!     INTERNAL SUBROUTINE
!-------------------------------------------------
!
      CONTAINS
          RECURSIVE SUBROUTINE QSORT (LIST)
!
!         RECURSIVE QUICK SORT OF THE INTEGER ARRAY LIST
!
!         THIS ROUTINE FROM "PROGRAMMER'S GUIDE TO FORTRAN 90",
!         W. S. BRAINERD, C. H. GOLDBERG, AND J. C. ADAMS, McGRAW-HILL,
!         1990, PAGE 147
!
          IMPLICIT NONE
          INTEGER, DIMENSION(:), INTENT(INOUT) :: LIST
          INTEGER, DIMENSION(SIZE(LIST)) :: SMALLER,LARGER
          INTEGER :: I,NUMBER_SMALLER,NUMBER_EQUAL,NUMBER_LARGER
          REAL :: CHOSEN
!
          IF (SIZE(LIST) > 1) THEN
              CHOSEN = LIST(1)
              NUMBER_SMALLER = 0
              NUMBER_EQUAL = 1
              NUMBER_LARGER = 0
!
              DO I = 2,SIZE(LIST)
                  IF (LIST(I) < CHOSEN) THEN
                      NUMBER_SMALLER = NUMBER_SMALLER + 1
                      SMALLER(NUMBER_SMALLER) = LIST(I)
                  ELSE IF (LIST(I) == CHOSEN) THEN
                      NUMBER_EQUAL = NUMBER_EQUAL + 1
                  ELSE
                      NUMBER_LARGER = NUMBER_LARGER + 1
                      LARGER(NUMBER_LARGER) = LIST(I)
                  END IF
              END DO
!
              CALL QSORT (SMALLER(1:NUMBER_SMALLER))
              LIST(1:NUMBER_SMALLER) = SMALLER(1:NUMBER_SMALLER)
              LIST(NUMBER_SMALLER+1:NUMBER_SMALLER+NUMBER_EQUAL) = CHOSEN
              CALL QSORT (LARGER(1:NUMBER_LARGER))
              LIST(NUMBER_SMALLER+NUMBER_EQUAL+1:) = LARGER(1:NUMBER_LARGER)
          END IF
!
          END SUBROUTINE QSORT
!
      END SUBROUTINE READIN
      SUBROUTINE TERROR(WANTED, FIELD, GOT)
!
!        PRINT OUT AN ERROR MESSAGE
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER WANTED, GOT
      CHARACTER FIELD*80
!-----------------------------------------------
!
      IF (WANTED == 1) THEN
          IF (GOT == 0) THEN
              WRITE (16,                                                 &
     &'('' SCANNING INPUT FOR A NUMERIC FIELD.''/                       &
     &  '' INSTEAD THE ALPHANUMERIC STRING ---> '',                     &
     &  A,'' <--- WAS FOUND.''/'' ABORT''//)') FIELD
          ELSE
              WRITE (16,                                                 &
     &'('' SCANNING INPUT FOR A NUMERIC FIELD.''/                       &
     &  '' INSTEAD THE END OF FILE WAS HIT.''/                          &
     &  '' YOU ARE MISSING A NUMERICAL VALUE AT THE'',                  &
     &  '' END OF YOUR LAST INPUT STRING.  ABORT''//)')
          ENDIF
      ELSE
           WRITE (16, '('' UNKNOWN PROBLEM WITH INPUT, ABORT''//)')
      ENDIF
       END SUBROUTINE TERROR
       SUBROUTINE TSOLVE(NODES, ENER, TEMP)
!
!        SOLVE FOR THE TEMPERATURE GIVEN THE SPECIFIC INTERNAL
!        ENERGY, ASSUMING IDEAL GAS BEHAVIOR.
!
!        INPUT:
!
!        NODES      INTEGER     NUMBER OF CELLS
!        ENER       REAL A.     INTERNAL SPECIFIC ENERGY (J/KG)
!
!        OUTPUT:
!
!        TEMP       REAL A.     TEMPERATURE (DEG KELVIN)
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER NODES
      REAL, DIMENSION(NODES) :: ENER, TEMP
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: N, NN, I
      REAL, DIMENSION(28) :: TDATA, EDATA
      REAL :: ENERGY, X1, Y1, X2, Y2, SLOPE, B
      LOGICAL :: FIRST = .TRUE.
      SAVE FIRST,TDATA,EDATA
!-----------------------------------------------
!
!        CONVERT ENTHALPY IN TABLE FROM CAL/MOLE TO J/KG
!
      IF (FIRST) THEN
!
!        FAKE A T/E DATA FILE, FOR PERFORMANCE TESTS ONLY
!
          DO N = 1, NODES
              TDATA(N) = REAL(N)
              EDATA(N)  = REAL(N)
          ENDDO
!
          EDATA = 4.1840/18.016E-3*EDATA
          FIRST = .FALSE.
      ENDIF
!
!        INTERPOLATE ON INTERNAL ENERGY (ENTHALPY)
!
      DO N = 1, NODES
          ENERGY = ENER(N)
          IF (ENERGY >= EDATA(28)) THEN
              NN = 27
          ELSE IF (ENERGY < EDATA(1)) THEN
              NN = 1
          ELSE
              DO I = 27, 1, -1
                  NN = I
                  IF (ENERGY>=EDATA(I) .AND. ENERGY<EDATA(I+1)) GO TO 30
              END DO
          ENDIF
   30     CONTINUE
          X1 = EDATA(NN)
          Y1 = TDATA(NN)
          X2 = EDATA(NN+1)
          Y2 = TDATA(NN+1)
          SLOPE = (Y2 - Y1)/(X2 - X1)
          B = Y2 - SLOPE*X2
          TEMP(N) = ENERGY*SLOPE + B
      END DO
      END SUBROUTINE TSOLVE
      SUBROUTINE VALUE(RESULT, ITYPE)
!
!        FREE-FORMAT NUMERIC/ALPHANUMERIC INPUT.  WHEN THIS ROUTINE IS
!        CALLED, IT WILL CALL SUBROUTINE NEXT TO FETCH THE NEXT
!        VALID FIELD OFF UNIT 5.  THIS FIELD WILL THEN BE PASSED
!        TO VALUE IN THE COMMON BLOCK /CIMAGE/ AS AN INTERNAL FILE.
!        VALUE THEN ATTEMPTS A READ ON THE FILE. IF THE FIELD WAS
!        INTEGER, THEN VALUE RETURNS A FLOATED RESULT.  IN THE
!        EVENT NO FURTHER DATA IS AVAILABLE, THE LOGICAL VARIABLE
!        EOFF IN COMMON /IMAGE/ WILL BE SET TRUE.  IN ALL OTHER CASES
!        IT WILL BE FALSE.  ALSO, ITYPE=-1 WILL INDICATE AN END OF
!        FILE WAS ENCOUNTERED.  FINALLY, THE FIELD BEING PROCESSED IS
!        UNCHANGED BY VALUE AND CAN BE USED BY OTHER ROUTINES SINCE
!        IT IS PASSED IN COMMON.
!
!        OUTPUT:
!
!             RESULT    REAL
!                       NUMERICAL (OR LOGICAL) VALUE OF THE FIELD.
!                       IF THE FIELD WAS AN INTEGER, RESULT IS THE
!                       FLOATED VALUE.  IF THE FIELD WAS ALPHA-
!                       NUMERIC, THIS VARIABLE IS NOT SET.
!
!             ITYPE     INTEGER
!                       IDENTIFIER SHOWING TYPE OF FIELD READ.
!                       -1 = END OF FILE ENCOUNTERED, NO DATA READ.
!                        0 = ALPHANUMERIC DATA, NO VALUE RETURNED IN
!                            RESULT.
!                        1 = INTEGER OR REAL
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE Vcimage
      USE Vimage
      use ints,only:next
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER ITYPE
      REAL RESULT
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!      INCLUDE 'next.int'
!-----------------------------------------------
!                                      *******************************
!                                      GET NEXT FIELD OFF UNIT 5
!                                      *******************************
      CALL NEXT
      IF (EOFF) THEN
          ITYPE = -1
          RETURN 
      ENDIF
!                                      *******************************
!                                      READ FIELD AS A NUMERIC
!                                      *******************************
      READ (FIELD, FMT='(BN,F20.0)', ERR=10) RESULT
      ITYPE = 1
      RETURN 
!                                      *******************************
!                                      THE ONLY POSSIBILITY LEFT IS
!                                      THAT THIS WAS AN ALPHANUMERIC
!                                      *******************************
   10 CONTINUE
      ITYPE = 0
      RETURN 
      END SUBROUTINE VALUE
      program CORTESA 
!
!        THIS PROGRAM SOLVES THE CONTINUITY EQUATIONS FOR MASS,
!        MOMENTUM, AND ENERGY TO MODEL THE FLOW OF A GAS IN ONE
!        DIMENSION.  THE SOLUTION TECHNIQUE IS A ONE DIMENSIONAL
!        VERSION OF THE HULL DIFFERENCING SCHEME, MODIFIED FOR VARIABLE
!        AREA.  WE FURTHER MODIFY THE SCHEME SO THAT THE HYDRODYNAMIC
!        FRONT IS MOVED BY FREE MOLECULAR FLOW, ASSUMING A NEAR VACUUM
!        DOWNSTREAM OF THE FRONT.
!
!        JOHN K. PRENTICE                17 SEPTEMBER 1986
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!	Original F77 code written by John Prentice to model the
!	hydrodynamics in a plasma switch.  Modified for use in F90
!	performance benchmark project.  Modifications include removing
!	calls to plot packages and hardwiring the input/output file
! 	names.
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!
!...Translated by Pacific-Sierra Research VAST-90 1.02A2  13:53:43   1/12/93   -
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE Vcoord
      USE INTS
      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!                                      *********************************
!                                      IMAX   = MAXIMUM NUMBER OF CELLS
!                                               IN THE MESH
!                                      MAXSTA = MAXIMUM AMOUNT OF
!                                               STATION DATA POSSIBLE
!                                      *********************************
      INTEGER, PARAMETER :: IMAX = 3000000     ! RJA - 60x previous
      INTEGER, PARAMETER :: MAXSTA = 3000000   ! RJA - 60x previous
      REAL, PARAMETER :: BOLTS = 1.3806E-23
      REAL, PARAMETER :: AVOG = 6.0222E23
      REAL, PARAMETER :: DIAM = 4.E-10
      REAL, PARAMETER :: PI = 3.14159
      REAL, PARAMETER :: VMIN = 0.1
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: DCYCLE, CSTOP, FCYCLE, CYCLE
      INTEGER, DIMENSION(IMAXP1) :: STATS
      INTEGER, DIMENSION(MAXSTA) :: SCYCLE
      INTEGER, DIMENSION(IMAXP1) :: NSNODE
      INTEGER :: NLOC, I, NSTAT, NCORE, NODES, NVISC, NS, N, IFRONT,    &
     &    MCYCLE, IDC, IFLOW, NLAST, IDONOR, INEW, NN, M, NSTART, NSTOP
      REAL,DIMENSION(IMAXP1)::VEL,PRES,DENS,TENER,IENER,MASS,TEMP
      REAL, DIMENSION(MAXSTA) :: PSTAT, DSTAT, VSTAT, TSTAT, STIME
      REAL, DIMENSION(IMAXP1) :: RADIUS, ZERO
      REAL :: ILEFT
      REAL, DIMENSION(IMAXP1) :: SOUND, DX, GAMMA, MINUS1, SAREA, DBND, &
     &    IBND, VBND, PBND, GBND, RT, VT, PT, PV, VOLD, EOLD, OLDR
      REAL, DIMENSION(0:IMAXP1) :: RBOUND
      REAL :: TIME, PROB, D0(1), E0(1), SHEAT,                                &
     &    CGAMMA, COLD, WEIGHT, TSTOP, STABF, COEFF, AMASS, SIGMA,      &
     &    SIGMA2, T0(1), P0(1),                                         &
     &    CON, GAMMA0(1), C0(1), DT, XFRONT, DTHALF, PLEFT, DLEFT, VLEFT,   &
     &    FML, FEL, VMOML, DXFRNT, RFRNT, AFRONT, VFRONT, VOLSAV, RADSAV&
     &    , ASAV, DXSAV, VAR, FMR, VMOMR, FER, DAR, VAW, RMOVE, AMOVE,  &
     &    VMOVE, TMASS, TVMOM, TENGY, FMASS, FMOM, FENGY, OLDX, ANEW,   &
     &    VNEW, REYN, TMIN, TMAX, CMIN, CMAX
      LOGICAL :: PHIST, DHIST, VHIST, DEBUG, PPLOT, DPLOT, VPLOT, TPLOT &
     &    , SCREEN, SLIST, THIST, STOPIT
      CHARACTER :: INFIL*15, OUTFIL*15, TITLE*80
      REAL , ALLOCATABLE :: AMAC1U(:)
!      INCLUDE 'readin.int'
!      INCLUDE 'nozzle.int'
!      INCLUDE 'tsolve.int'
!      INCLUDE 'eos.int'
!      INCLUDE 'keel.int'
!      INCLUDE 'chozdt.int'
!      INCLUDE 'area.int'
!-----------------------------------------------
!                                      *********************************
!                                      INITIALIZATIONS
!                                      *********************************
      TIME = 0.
      NLOC = 0
      CYCLE = 0
       sarea=0.0
      STIME(:MAXSTA) = 0.0
      PSTAT(:MAXSTA) = 0.0
      DSTAT(:MAXSTA) = 0.0
      VSTAT(:MAXSTA) = 0.0
      TSTAT(:MAXSTA) = 0.0
      INFIL = 'gas_dyn.in'
      OUTFIL = 'gas_dyn.out'
      OPEN(UNIT=15, FILE=INFIL, STATUS='OLD', FORM='FORMATTED')
      OPEN(UNIT=16, FILE=OUTFIL, STATUS='UNKNOWN', FORM='FORMATTED')
!                                      *********************************
!                                      READ IN THE INITIAL
!                                      CONDITIONS.
!                                      *********************************
      CALL READIN (PROB, TITLE, CSTOP, FCYCLE, DCYCLE, DHIST, VHIST,    &
     &    IMAX, PHIST, DEBUG, NSTAT, STATS, MAXSTA, NCORE, PPLOT, DPLOT &
     &    , VPLOT, TPLOT, SLIST, D0(1), E0(1), NODES, SHEAT, CGAMMA, COLD,    &
     &    THIST, NVISC, SCREEN, WEIGHT, TSTOP, STABF)
      CLOSE(UNIT=15)

!
!     call area to initialize some arrays.  use AFRONT as a dummy variable
!
      AFRONT = AREA(0.0)      
!                                      *********************************
!                                      CALCULATE NOZZLE INITIAL
!                                      CONDITIONS
!                                      *********************************
      COEFF = 0.4*(18.016E-3/8.314)
      AMASS = WEIGHT/AVOG
      SIGMA = SQRT(BOLTS/(3.0*AMASS))
      SIGMA2 = 3.0*SQRT((PI*AMASS)/(8.0*BOLTS))
      T0(1) = COEFF*E0(1)
      IF (T0(1) > 6000.0) T0(1) = 5212.0
!
!        IF COLD NOT SET, CALL NOZZLE TO INITIALIZE
!
      IF (COLD <= 0.0) THEN
          CALL NOZZLE (0.0, P0(1), FLOW = 0.0)
!
!        ASSUME ISOTHERMAL CONDITIONS AT NOZZLE, T=300 DEG KELVIN
!
          IF (SHEAT<=0.0 .OR. CGAMMA<=0.0) THEN
              CALL TSOLVE (1, E0, T0)
              CON = 8.314/WEIGHT
              D0(1) = P0(1)/(CON*T0(1))
          ELSE
              D0(1) = P0(1)/((CGAMMA - 1.0)*E0(1))
          ENDIF
      ENDIF
      CALL EOS (1, E0, D0, P0, T0, GAMMA0, C0, SHEAT, CGAMMA, WEIGHT)
!                                      *********************************
!                                      INITIALIZE THE MESH
!                                      *********************************
      CALL NOZZLE (0.0, P0(1), D0(1), E0(1), C0(1), PRES(1), DENS(1), VEL(1), IENER(1), &
     &    COLD, GAMMA0(1)) 
      CALL KEEL (NODES, DX, RADIUS, RBOUND, VEL, DENS, IENER, SCREEN, NS,   &
     &    NSNODE, SAREA)
      TENER(:NODES) = VEL(:NODES)**2/2.0 + IENER(:NODES)
      ZERO(:NODES) = 0.0
      MINUS1(:NODES) = -1.0
!                                      *********************************
!                                      ESTIMATE INITIAL TEMPERATURE
!                                      WITH IDEAL GAS LAW AND GAMMA 1.4
!                                      *********************************
      TEMP(:NODES) = COEFF*IENER(:NODES)
      WHERE (TEMP(:NODES) > 6000.0) TEMP(:NODES) = 5212.0
!                                      *********************************
!                                      MAKE INITIAL CALL TO EOS AND
!                                      CHOZDT
!                                      *********************************
      CALL EOS (NODES, IENER, DENS, PRES, TEMP, GAMMA, SOUND, SHEAT,    &
     &    CGAMMA, WEIGHT)
!                                      *********************************
!                                      PRINT INITIAL CONDITIONS
!                                      *********************************
      WRITE (16, '(//'' INITIAL HYDRODYNAMIC CONDITIONS IN MESH:''//)')
       WRITE (16,                                                        &
     &'('' X='',1PE12.5,'' P='',1PE12.5,'' D='',E12.5,                  &
     &  '' U='',E12.5,'' I='',E12.5,'' T='',E12.5,'' CS='',             &
     &  E12.5,'' GAMMA='',E12.5)') (RADIUS(N),PRES(N),DENS(N),VEL(N),   &
     &    IENER(N),TEMP(N),SOUND(N),GAMMA(N),N=1,NODES)
      CALL CHOZDT (NODES, VEL, SOUND, DX, DT, STABF)
!***********************************************************************
!
!                       MAIN LOOP
!
!***********************************************************************
      WRITE (16, '(''1BEGINNING CALCULATION AT TIME='',1PE12.5//)') TIME
      IFRONT = 1
      XFRONT = RBOUND(1)
      STOPIT = .FALSE.
      MCYCLE = MIN(50,CSTOP/2)
      DO I = 1, CSTOP
          IF (I == CSTOP) STOPIT = .TRUE.
          IF (TIME + DT >= TSTOP) STOPIT = .TRUE.
          IF (I==1 .OR. MOD(I,MCYCLE)==0) THEN
              WRITE (*, '('' NOW AT CYCLE '',I5,'' AND TIME '',1PE12.5)'&
     &            ) I, TIME
              WRITE (16,'('' ----->  CYCLE='',I5,'' TIME='',1PE12.5,'' DT='', &
     &                 E12.5)') I, TIME, DT
          ENDIF
!                                      *********************************
!                                      ADVANCE TO T+DT/2
!                                      *********************************
          DTHALF = DT/2.0
!                                      *********************************
!                                      CALCULATE BOUNDARY CONDITIONS.
!                                      THE LEFT BOUNDARY IS DRIVEN BY
!                                      THE NOZZLE, THE RIGHT IS
!                                      PERFECTLY REFLECTIVE.  DEFINE
!                                      CELLS 1 AND 'NODES' TO THESE
!                                      VALUES, AS WELL AS THE LEFT AND
!                                      RIGHT BOUNDARIES.  THIS
!                                      ELIMINATES A PROBLEM IN ETBFCT
!                                      WITH ZERO VELOCITY CELLS
!                                      ADJACENT TO THE BOUNDARY
!                                      *********************************
          IF (I == 1) CALL NOZZLE (TIME, P0(1), D0(1), E0(1), C0(1), PLEFT, DLEFT,  &
     &        VLEFT, ILEFT, COLD, GAMMA0(1))
          IDC = 1
          IF(STOPIT.OR.FCYCLE<=I.AND.MOD(I,DCYCLE)==0.OR.I==1)IDC=0
          IF (DEBUG .OR. IDC==0) WRITE (16,                              &
     &'(//'' NOZZLE CONDITIONS AT BEGINNING OF TIME '',                 &
     &    ''STEP:''/                                                    &
     &    ''   PRESSURE='',1PE12.5/                                     &
     &    ''   DENSITY ='',1PE12.5/                                     &
     &    ''   ENERGY  ='',1PE12.5/                                     &
     &    ''   VELOCITY='',1PE12.5/)') PLEFT, DLEFT, ILEFT, VLEFT
!
!        LOAD NOZZLE CONDITIONS INTO CELL 1
!
          DENS(1) = DLEFT
          PRES(1) = PLEFT
          VEL(1) = VLEFT
          TENER(1) = ILEFT + VLEFT**2/2.0
          IENER(1) = ILEFT
!
!        LOAD INITIAL LEFT HAND FLOW CONDITIONS
!
          IFLOW = 0
          IF (IFLOW /= 2) THEN
              FML = 0.0
              FEL = 0.0
              VMOML = 0.0
          ELSE
              FML = DLEFT*VLEFT*DT*BAREA(1)
              FEL = FML*TENER(1)
              VMOML = FML*VLEFT
          ENDIF
!
!        IF FLOW AT END OF MESH, LOAD REFLECTIVE CONDITIONS
!
          IF (IFRONT > NODES) THEN
              IFRONT = NODES + 1
              DENS(IFRONT) = DENS(NODES)*VOL(NODES)/VOL(IFRONT)
              PRES(IFRONT) = PRES(NODES)
              VEL(IFRONT) = -VEL(NODES)
              TENER(IFRONT) = TENER(NODES)
              IENER(IFRONT) = IENER(NODES)
          ENDIF
!
          NLAST = IFRONT - 1
          IF (IFRONT <= NODES) THEN
              DXFRNT = XFRONT - RBOUND(IFRONT-1)
              RFRNT = RBOUND(NLAST) + 0.5*DXFRNT
              AFRONT = AREA(RFRNT)
              VFRONT = DXFRNT/3.0*(BAREA(NLAST)+SQRT(BAREA(NLAST)*AFRONT&
     &            )+AFRONT)
          ENDIF
!
!        DIFFERENCE ONLY IF FRONT HAS PROPAGATED AT LEAST INTO CELL 3
!
          IF (IFRONT >= 3) THEN
              VOLSAV = VOL(IFRONT)
              RADSAV = RADIUS(IFRONT)
              ASAV = CAREA(IFRONT)
              DXSAV = DX(IFRONT)
              VOL(IFRONT) = VFRONT
              RADIUS(IFRONT) = RFRNT
              CAREA(IFRONT) = AFRONT
              DX(IFRONT) = DXFRNT
!
!        CALCULATE MASSES FOR ACTIVE CELLS
!
              MASS(:IFRONT) = DENS(:IFRONT)*VOL(:IFRONT)
!
!        ***** PHASE 1 *****
!
!        IN PHASE 1 WE UPDATE INTERNAL ENERGY WITH WORK TERM DUE TO
!        VELOCITIES CALCULATED AT LAST TIME STEP.  WE THEN UPDATE
!        VELOCITIES WITH ACCELERATIONS DUE TO PRESSURE GRADIENTS
!
!        DENSITY,VELOCITY,ENERGY, AND PRESSURE AT
!        CELL BOUNDARY AT INITIAL TIME
!
              DBND(:NLAST) = (MASS(:NLAST)+MASS(2:NLAST+1))/(VOL(:NLAST)&
     &            +VOL(2:NLAST+1))
              IBND(:NLAST) = (IENER(:NLAST)+IENER(2:NLAST+1))/2.0
              VBND(:NLAST) = (VEL(:NLAST)+VEL(2:NLAST+1))/2.0
              PBND(:NLAST) = (PRES(:NLAST)+PRES(2:NLAST+1))/2.0
!
!        HALF TIME ADVANCED DENSITY ON CELL BOUNDARY
!
              RT(:NLAST) = DBND(:NLAST)*(1.0 - DTHALF/BAREA(1:NLAST)*(  &
     &            CAREA(2:NLAST+1)*VEL(2:NLAST+1)-CAREA(:NLAST)*VEL(:   &
     &            NLAST))/(RADIUS(2:NLAST+1)-RADIUS(:NLAST)))
!
!        HALF TIME ADVANCED VELOCITIES AND EFFECTIVE GAMMA
!
              VT(:NLAST) = VBND(:NLAST) - DTHALF/RT(:NLAST)*(PRES(2:    &
     &            NLAST+1)-PRES(:NLAST))/(RADIUS(2:NLAST+1)-RADIUS(:    &
     &            NLAST))
              GBND(:NLAST) = 1.0 + PBND(:NLAST)/(IBND(:NLAST)*DBND(:    &
     &            NLAST))
!
!        HALF TIME ADVANCED PRESSURE ON CELL BOUNDARY
!
              PT(:NLAST) = PBND(:NLAST)*(1.0 - DTHALF*GBND(:NLAST)/BAREA&
     &            (1:NLAST)*(CAREA(2:NLAST+1)*VEL(2:NLAST+1)-CAREA(:    &
     &            NLAST)*VEL(:NLAST))/(RADIUS(2:NLAST+1)-RADIUS(:NLAST))&
     &            )
              WHERE (ABS(VT(:NLAST)) <= VMIN) VT(:NLAST) = 0.0
              PV(:NLAST) = PT(:NLAST)*VT(:NLAST)
              VOLD(:NLAST) = VEL(:NLAST)
              EOLD(:NLAST) = TENER(:NLAST)
              OLDR(:NLAST) = DENS(:NLAST)
!
!        CALCULATE NEW VELOCITIES
!
              VEL(2:NLAST) = VOLD(2:NLAST) - DT/OLDR(2:NLAST)*(PT(2:    &
     &            NLAST)-PT(:NLAST-1))/DX(2:NLAST)
              WHERE (ABS(VEL(2:NLAST)) <= VMIN) VEL(2:NLAST) = 0.0
!
!        UPDATE ENERGY
!
              TENER(2:NLAST) = EOLD(2:NLAST) - DT/(OLDR(2:NLAST)*CAREA(2&
     &            :NLAST))*(PV(2:NLAST)*BAREA(2:NLAST)-PV(:NLAST-1)*    &
     &            BAREA(1:NLAST-1))/DX(2:NLAST)
!
!        UPDATE CELL 1 WITH NOZZLE CONDITIONS
!
              CALL NOZZLE (TIME, P0(1), D0(1), E0(1), C0(1), PLEFT, DLEFT, VLEFT,   &
     &            ILEFT, COLD, GAMMA0(1))
              VEL(1) = VLEFT
              TENER(1) = ILEFT + 0.5*VLEFT**2
!
!        ***** PHASE 2 *****
!
!        IN PHASE 2 WE TRANSPORT MASS AND INTERNAL ENERGY WHILE
!        CONSERVING MOMENTUM AND TOTAL ENERGY
!
              FML = 0.0
              FEL = 0.0
              VMOML = 0.0
              DO N = 1, NLAST
!
!        CALCULATE WEIGHTED VELOCITY ON RIGHT CELL BOUNDARY
!
                  VAR = 0.5*(VEL(N)+VEL(N+1))
                  IF (FML==0.0 .AND. VAR==0.0) THEN
!
!        INACTIVE CELL
!
                      FMR = 0.0
                      VMOMR = 0.0
                      FER = 0.0
!
!        ACTIVE CELL
!
                  ELSE
!
!        SET DONOR CELL DENSITY
!
                      IDONOR = N
                      IF (VAR < 0.0) IDONOR = N + 1
                      DAR = OLDR(IDONOR)
                      VAW = DAR*(1.0 - DT/BAREA(N)*(CAREA(N+1)*VEL(N+1)-&
     &                    CAREA(N)*VEL(N))/(RADIUS(N+1)-RADIUS(N)))
!
!        COMPUTE MASS FLUX TO RIGHT
!
                      RMOVE = RBOUND(N) + VAR*DT
                      AMOVE = AREA(RMOVE)
                      VMOVE = (RMOVE - RBOUND(N))/3.0*(BAREA(N)+SQRT(   &
     &                    BAREA(N)*AMOVE)+AMOVE)
                      FMR = VMOVE*VAW
                      VMOMR = FMR*VEL(IDONOR)
                      FER = FMR*TENER(IDONOR)
!
!        SKIP FLUXES INTO CELL 1
!
                      IF (N /= 1) THEN
                          TMASS = MASS(N) + FML - FMR
                          TVMOM = VEL(N)*MASS(N) + VMOML - VMOMR
                          TENGY = MASS(N)*TENER(N) + FEL - FER
!
!        COMPUTE FINAL VELOCITIES, ENERGY, AND PRESSURE
!
                          VEL(N) = TVMOM/TMASS
                          TENER(N) = TENGY/TMASS
                          IENER(N) = TENER(N) - 0.5*VEL(N)**2
                          MASS(N) = TMASS
                          DENS(N) = MASS(N)/VOL(N)
!
!        CHECK FOR NEGATIVE ENERGY
!
                          IF (IENER(N) <= 0.0) THEN
                              WRITE (16,                                 &
     &'('' NEGATIVE ENERGY IN CELL '',                                I5&
     &,'' I='',1PE12.5,'' FEL='',E12.5,                               ''&
     & FER='',E12.5)') N, IENER(N), FEL, FER
                              STOP 
                          ENDIF
                      ENDIF
                      FML = FMR
                      FEL = FER
                      VMOML = VMOMR
                  ENDIF
              END DO
!
!        RELOAD CELL 1
!
              DENS(1) = DLEFT
              PRES(1) = PLEFT
              VEL(1) = VLEFT
              TENER(1) = ILEFT + 0.5*VLEFT**2
              IENER(1) = ILEFT
              VOL(IFRONT) = VOLSAV
              RADIUS(IFRONT) = RADSAV
              CAREA(IFRONT) = ASAV
              DX(IFRONT) = DXSAV
          ENDIF
!
!        ADVANCE HYDRODYNAMIC FRONT BY FREE MOLECULAR FLOW
!
          IF (IFRONT <= NODES) THEN
              FMR = 0.0
              FER = 0.0
              VMOMR = 0.0
              FMASS = DENS(IFRONT)*VFRONT
              FMOM = FMASS*VEL(IFRONT)
              FENGY = FMASS*TENER(IFRONT)
!
!        CALCULATE ANY FLUXES INTO NEXT CELL
!
              INEW = IFRONT
              RMOVE = VEL(IFRONT)*DT + XFRONT
              IF (RMOVE > RBOUND(IFRONT)) THEN
                  INEW = IFRONT + 1
                  IF (IFRONT > NODES) THEN
                      RMOVE = RBOUND(NODES)
                  ELSE
                      AMOVE = AREA(RMOVE)
                      VMOVE = (RMOVE - RBOUND(IFRONT))/3.0*(BAREA(IFRONT&
     &                    )+SQRT(BAREA(IFRONT)*AMOVE)+AMOVE)
                      FMR = VMOVE*DENS(IFRONT)
                      VMOMR = FMR*VEL(IFRONT)
                      FER = FMR*TENER(IFRONT)
                      FMASS = FMASS - FMR
                      FMOM = FMOM - VMOMR
                      FENGY = FENGY - FER
                  ENDIF
              ENDIF
!
!        NOW ADD OLD FLUXES FROM LEFT BOUNDARY INTO OLD FRONT CELL
!
              FMASS = FMASS + FML
              FMOM = FMOM + VMOML
              FENGY = FENGY + FEL
!
!        UPDATE HYDRO FOR OLD FRONT CELL
!
              OLDX = MIN(RMOVE,RBOUND(IFRONT))
              ANEW = AREA(OLDX)
              VNEW = (OLDX - RBOUND(IFRONT-1))/3.0*(BAREA(IFRONT-1)+    &
     &            SQRT(BAREA(IFRONT-1)*ANEW)+ANEW)
              DENS(IFRONT) = FMASS/VNEW
              VEL(IFRONT) = FMOM/FMASS
              TENER(IFRONT) = FENGY/FMASS
              IENER(IFRONT) = TENER(IFRONT) - 0.5*VEL(IFRONT)**2
              IF (IENER(IFRONT) <= 0.0) THEN
                  WRITE (16,                                             &
     &'('' NEGATIVE ENERGY IN CELL '',I5,'' I='',                     1P&
     &E12.5/'' UPDATING OLD FRONT CELL'')') IFRONT, IENER(IFRONT)
                  STOP 
              ENDIF
!
!        MOVE FRONT TO NEXT CELL IF APPROPIATE
!
              IF (IFRONT /= INEW) THEN
                  IFRONT = INEW
                  IF (ABS(FMR) > 0.0) THEN
                      DENS(IFRONT) = FMR/VMOVE
                      VEL(IFRONT) = VMOMR/FMR
                      TENER(IFRONT) = FER/FMR
                      IENER(IFRONT) = TENER(IFRONT) - 0.5*VEL(IFRONT)**2
                      IF (IENER(IFRONT) <= 0.0) THEN
                          WRITE (16,                                     &
     &'('' NEGATIVE ENERGY IN CELL '',I5,                       '' I='',&
     &1PE12.5/'' UPDATING NEW FRONT CELL''                      )')     &
     &                        IFRONT, IENER(IFRONT)
                          STOP 
                      ENDIF
                  ENDIF
              ENDIF
              XFRONT = RMOVE
          ENDIF
          WRITE (16,                                                     &
     &'('' HYDRODYNAMIC FRONT AT X='',1PE12.5,'' IN CELL '',            &
     &      I5)') XFRONT, IFRONT
!                                      *********************************
!                                      CALCULATE PRESSURE FROM EQUATION
!                                      OF STATE
!                                      *********************************
          CALL EOS (NODES, IENER, DENS, PRES, TEMP, GAMMA, SOUND, SHEAT &
     &        , CGAMMA, WEIGHT)
!                                      *********************************
!                                      CALCULATE DRAG ACROSS SCREENS
!                                      *********************************
          IF (SCREEN) THEN
              DO N = 1, NS
                  NN = NSNODE(N)
                  CALL DRAG (SAREA(NN), SIGMA2, TEMP(NN),   &
     &                PRES(NN), DENS(NN), VEL(NN), REYN)
              END DO
          ENDIF
          TIME = TIME + DT
          CYCLE = CYCLE + 1
!          IF(IFRONT.GT.NODES) THEN
!              WRITE(16,'(//'' HYDRODYNAMIC FRONT HAS IMPACTED END OF '',
!     *                    ''CHAMBER, STOP CALCULATION''//)')
!              STOPIT=.TRUE.
!          ENDIF
          IDC = MOD(CYCLE,DCYCLE)
          IF (STOPIT .OR. CYCLE<FCYCLE) IDC = 1
          IF (DEBUG .OR. IDC==0) THEN
              TMIN = 1.E10
              TMAX = -1.E10
              CMIN = 1.E10
              CMAX = -1.E10
              ALLOCATE (AMAC1U(NODES))
              TMIN = MIN(TMIN,MINVAL(TEMP(:NODES)))
              TMAX = MAX(TMAX,MAXVAL(TEMP(:NODES)))
              AMAC1U = VEL(:NODES)/SOUND(:NODES)
              CMIN = MIN(MINVAL(AMAC1U),CMIN)
              CMAX = MAX(MAXVAL(AMAC1U),CMAX)
              DEALLOCATE (AMAC1U)
              WRITE (16,                                                 &
     &'('' MINIMUM TEMPERATURE = '',F10.5,'' DEG K''/                   &
     &  '' MAXIMUM TEMPERATURE = '',F10.5,'' DEG K''/                   &
     &  '' MINIMUM MACH NUMBER = '',F10.5,                              &
     &  '' MAXIMUM MACH NUMBER = '',F10.5/)') TMIN, TMAX, CMIN, CMAX
              WRITE (16,                                                 &
     &'(//'' MESH AT CYCLE '',I5,'' AND TIME '',1PE12.5)        ') CYCLE&
     &            , TIME
              WRITE (16,                                                 &
     &'('' X='',1PE12.5,'' P='',E12.5,'' D='',E12.5,                    &
     &  '' U='',E12.5,'' I='',E12.5,'' T='',E12.5,'' CS='', E12.5,      &
     &  '' GAMMA='',E12.5)') (RADIUS(N),PRES(N),DENS(N),VEL(N),IENER(N),&
     &            TEMP(N),SOUND(N),GAMMA(N),N=1,NODES)
          ENDIF
!                                      *********************************
!                                      SAVE STATION DATA
!                                      *********************************
          IF (NSTAT>0 .AND. CYCLE>=FCYCLE) THEN
              NLOC = NLOC + 1
              SCYCLE(NLOC:(NSTAT-1)*NCORE+NLOC:NCORE) = CYCLE
              STIME(NLOC:(NSTAT-1)*NCORE+NLOC:NCORE) = TIME
              PSTAT(NLOC:(NSTAT-1)*NCORE+NLOC:NCORE) = PRES(STATS(:NSTAT))
              DSTAT(NLOC:(NSTAT-1)*NCORE+NLOC:NCORE) = DENS(STATS(:NSTAT))
              VSTAT(NLOC:(NSTAT-1)*NCORE+NLOC:NCORE)=VEL(STATS(:NSTAT))
              TSTAT(NLOC:(NSTAT-1)*NCORE+NLOC:NCORE) = TEMP(STATS(:NSTAT))
          ENDIF
!                                      *********************************
!                                      CALCULATE NEW TIME STEP
!                                      *********************************
          CALL CHOZDT (NODES, VEL, SOUND, DX, DT, STABF)
          IDC = MOD(CYCLE,DCYCLE)
          IF (STOPIT) IDC = 0
          IF (TIME<TSTOP .AND. I/=CSTOP .AND. CYCLE<FCYCLE) IDC = 1
          IF (STOPIT) EXIT 
      END DO
!
!***********************************************************************
!
!                          END OF MAIN LOOP
!
!***********************************************************************
      IF (SLIST) THEN
!                                      *********************************
!                                      PRINT OUT STATION DATA
!                                      *********************************
          IF (NLOC <= 0) THEN
              WRITE (16, '(//'' NO STATION DATA FOR THIS RUN''//)')
          ELSE
               DO M = 1, NSTAT
                  I = STATS(M)
                  NSTART = (M - 1)*NCORE + 1
                  NSTOP = NSTART + NLOC - 1
                  WRITE (16,                                             &
     &'(''1STATION DATA FOR STATION NUMBER '',I5,                       &
     &  '' AT RADIUS '',1PE12.5,'' METERS''//)') I, RADIUS(I)
                  WRITE (16,                                             &
     &'('' CYCLE='',I5,'' TIME='',1PE12.5,                              &
     &  '' PRESSURE='',E12.5,                                           &
     &  '' DENSITY='',E12.5,'' VELOCITY='',E12.5,                       &
     &  '' TEMPERATURE='',E12.5)') (SCYCLE(I),STIME(I),PSTAT(I),DSTAT(I)&
     &                ,VSTAT(I),TSTAT(I),I=NSTART,NSTOP)
              END DO
          ENDIF
      ENDIF
!                                      *********************************
!                                      FORMATS
!                                      *********************************
 1001 FORMAT(1X,I3,61PD15.5,2X,1PD10.3)
 1002 FORMAT(2X,I4,2X,I3,2X,9(1PD10.3,2X),1PD10.3)
 1003 FORMAT(1X,///T2,'I',T8,'NODES',T16,'TIME',T24,'MODEN',T41,'VEL',  &
     & T53,'PRES',T65,'EDEN',T77,'RHO',T90,'TEMP',T100,'MACH NO.')
 1004 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER ',           &
     &                  'FIRST CALL TO FCT1D'/)
 1005 FORMAT(1X,I3,8(1PD12.5,2X))
 1006 FORMAT(1X,'>',I4,2X,I3,2X,9(1PD10.3,2X),1PD10.3)
 1007 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER ',           &
     &                  'FIRST CALL TO BOUND'/)
 1008 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER ',           &
     &                  'LOOP 30',/)
 1009 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER              &
     &SECOND CALL TO FCT1D',/)
 1010 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY SECOND CALL TO     &
     &BOUND'/)
 1011 FORMAT(//,'RESULTS OF CALCULATIONS IMMEDIATELY AFTER LOOP 50'/)
 1012 FORMAT('                               RADIUS (METERS)')
 1013 FORMAT('     DENSITY VERSUS RADIUS AT ',1PE12.5,' SEC')
 1014 FORMAT('                            DENSITY (KG/METER**3)')
 1015 FORMAT('     VELOCITY VERSUS RADIUS AT ',1PE12.5,' SEC')
 1016 FORMAT('                             VELOCITY (METERS/SEC)')
 1017 FORMAT('     PRESSURE VERSUS RADIUS AT ',1PE12.5,' SEC')
 1018 FORMAT('                             PRESSURE (PASCALS)')
 1019 FORMAT('                                TIME (SEC)')
 1020 FORMAT('PRESSURE VERSUS TIME AT RADIUS = ',1PE12.5,' METERS')
 1021 FORMAT('DENSITY VERSUS TIME AT RADIUS = ',1PE12.5,' METERS')
 1022 FORMAT('VELOCITY VERSUS TIME AT RADIUS = ',1PE12.5,' METERS')
 1023 FORMAT('TEMPERATURE VERSUS TIME AT RADIUS = ',1PE12.5,' METERS')
 1024 FORMAT('                        TEMPERATURE (DEGREES KELVIN)')
 1025 FORMAT('      TEMPERATURE VERSUS RADIUS AT ',1PE12.5,' SEC')
      END program CORTESA

