      SUBROUTINE CALETA( IETA, CENETA, DELETA, IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : THIS SUBROUTINE RETURNS THE CENTRAL TOWER
C-                         VALUE AND RANGE OF THE PSEUDO RAPIDITY 
C-                         FOR THE NOMINAL "PHYSICS" ETA.
C-
C-   Inputs  :    IETA     "PHYSICS" ETA
C-   Outputs :    CENETA   eta at the center of the tower
C-                DELETA   range of eta spanned by the tower
C-                IERR     error code -- 0: OK
C-                                       1: invalid eta
C-   Controls:    NONE
C-
C-   Created  24-OCT-1988   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  IETA, IE, IERR
      REAL CENETA, SGN, DELETA
C
      IERR = 0
      IE = ABS(IETA)
      SGN = SIGN(1,IETA)
C
      IF ( IE .GE. 1 .AND. IE .LE. 32) THEN         ! nominal region
         CENETA = (IE-0.5) * 0.1
         DELETA = 0.1
      ELSE IF ( IE .EQ. 33) THEN
         CENETA = 3.31              ! eta 3.2 => 3.42
         DELETA = 0.22
      ELSE IF ( IE .EQ. 34) THEN
         CENETA = 3.56              ! eta 3.42 => 3.7
         DELETA = 0.28
      ELSE IF ( IE .EQ. 35) THEN
         CENETA = 3.9               ! eta 3.7 => 4.1
         DELETA = 0.4
      ELSE IF ( IE .EQ. 36) THEN 
         CENETA = 4.275             ! eta 4.1 => 4.45
         DELETA = 0.35
      ELSE IF ( IE .EQ. 37) THEN
         CENETA = 4.715             ! eta 4.45 => 4.98
         DELETA = 0.53
      ELSE
         CENETA = 999.0             ! error situation
         DELETA = 999.0
         IERR = 1
      END IF
C
      CENETA = SGN * CENETA
C----------------------------------------------------------------------
  999 RETURN
      END
