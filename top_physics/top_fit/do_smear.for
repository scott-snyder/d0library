      SUBROUTINE DO_SMEAR(POL,SIG_E,SIG_ETA,SIG_PHI,MASS,CART)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TAKE A POLAR VECTOR (ENERGY,ETA,PHI) AND
C-   SMEAR IT WITH RESOLUTIONS SIG_E,SIG_ETAPHI(2) AND OUT PUT A CARTESIAN
C-   4 VECTOR OF MASS MAS
C-
C-   Inputs  : POL(3) = ENERGY,ETA,PHI
C-             SIG_E = ENERGY RESOLUTION
C-             SIG_ETA,SIG_PHI RESOLUTION IN ETA AND PHI
C-             MASS = MASS OF VECTOR
C-   Outputs : CART(1:4) = 4 VECTOR OF SMEARED OBJECT_ET,ETA,PHI
C-   Controls:
C-
C-   Created  22-FEB-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION    POL(*),CART(*)
      DOUBLE PRECISION    SIG_E,SIG_ETA,SIG_PHI,MASS
      REAL R1,R2,R3,R4
      DOUBLE PRECISION POLS(3)
      INTEGER IER
      REAL    JET_ET_CUT
      LOGICAL first
      SAVE first
      DATA first / .true. /

C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_FIT_RCP')
        CALL EZGET('JET_ET_CUT',JET_ET_CUT,IER)
        CALL EZRSET
      ENDIF
   10 CONTINUE
      CALL RANNOR(R1,R2)
      CALL RANNOR(R3,R4)
C
      POLS(1) = POL(1)+R1*SIG_E
      POLS(2) = POL(2)+R2*SIG_ETA
      POLS(3) = POL(3)+R3*SIG_PHI
C
      CALL GET_CART(POLS,CART,MASS)
      IF ( CART(5).LT.JET_ET_CUT ) THEN
        GO TO 10
C REGENERATE
      ENDIF
  999 RETURN
      END
