      FUNCTION CD_ELECTRONICS_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization for CD_ELECTRONICS EXAMINE2   
C-                         Package
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-OCT-1990   Susan K. Blessing
C-   Updated   1-MAR-1993   Susan K. Blessing   Set CD_ELECTRONICS_INIT
C-    to true.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL CD_ELECTRONICS_INIT
C
      INTEGER IER
C
C----------------------------------------------------------------------
C
      CD_ELECTRONICS_INIT = .TRUE.
C
      CALL INRCP('CD_ELECTRONICS_RCP',IER)
C
      IF (IER.NE.0) CALL INTMSG(' CD_ELECTRONICS_RCP file not found.')
C
  999 RETURN
      END
