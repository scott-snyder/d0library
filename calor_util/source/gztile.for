      FUNCTION GZTILE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return zebra pointer to TILE bank 
C-
C-   Returned value  : GZTILE - zebra pointer to TILE bank
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-JAN-1994   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER     GZTILE, GZPROC, LPROC
      EXTERNAL    GZPROC
      INCLUDE 'D0$LINKS:IZTILE.LINK'
C----------------------------------------------------------------------
      GZTILE = 0
      LPROC  = GZPROC()
      IF ( LPROC .GT. 0 ) GZTILE = LQ( LPROC - IZTILE )
  999 RETURN
      END
