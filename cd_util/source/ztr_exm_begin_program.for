      FUNCTION ZTR_EXM_BEGIN_PROGRAM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-OCT-1992   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL ZTR_EXM_BEGIN_PROGRAM
C
      LOGICAL OK
      LOGICAL ZTRINI, ZTR_EXM_VIEW_HISTOS
C
C----------------------------------------------------------------------
C
      ZTR_EXM_BEGIN_PROGRAM = .TRUE.
C
      OK = ZTRINI()
      OK = ZTR_EXM_VIEW_HISTOS()
C
  999 RETURN
      END
