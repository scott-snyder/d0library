      SUBROUTINE INIT_SINTBL( IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INITIALIZE THE THE SIN/COS TABLES
C-        USED FOR CELL ROTATIONS
C-
C-   Inputs  :      NONE
C-   Outputs :      SINROT, COSROT in /SINTBL/
C-                  IERR       error flag
C-   Controls:      SINROT(1) is checked to see if initialation has been done
C-                     already
C-
C-   Created  15-NOV-1990   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SINTBL.INC'
      INTEGER  IERR, I
      REAL PHI, PSI
C
      IF( SINROT(1) .NE. 0.) GO TO 999
      CALL CALPHI( 1, 1, PHI, PSI, IERR)
      IF ( IERR .NE. 0) GO TO 999
C
      DO 100 I = 0, NMPHI
        COSROT(I) = COS(I * PSI)
        SINROT(I) = SIN(I * PSI)
  100 CONTINUE
C
C----------------------------------------------------------------------
  999 RETURN
      END
