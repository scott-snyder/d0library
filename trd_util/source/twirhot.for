      LOGICAL FUNCTION TWIRHOT(WIRE,CHAMBER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set TWIRHOT to to .TRUE. if wire in chamber 
C-                         has been recognized as a "hot" wire (only for
C-                         COSMIC1 runs)
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-MAY-1991   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IUCOMP,CHAMBER,NHOT,WIRE
      INTEGER LOUT,TRUNIT
      PARAMETER( NHOT = 3 )
      INTEGER HOTW(NHOT,3)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        CALL VZERO(HOTW,NHOT*3)
        HOTW(1,1)=107
        HOTW(1,2)=34
        HOTW(2,2)=66
        HOTW(3,2)=127
        HOTW(1,3)=78
        HOTW(2,3)=200
        LOUT=TRUNIT()
        FIRST=.FALSE.
      END IF
      TWIRHOT=.FALSE.
      IF(IUCOMP(WIRE,HOTW(1,CHAMBER),NHOT).LE.0)GO TO 999
      TWIRHOT=.TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
