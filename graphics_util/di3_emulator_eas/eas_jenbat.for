      SUBROUTINE JENBAT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Ends a batch of update.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-JUL-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      EXTERNAL ERRHND
C
      BATCH = .FALSE.
      IF(NUDI3) THEN
      CALL KFUND
      ENDIF
C
  999 RETURN
      END
