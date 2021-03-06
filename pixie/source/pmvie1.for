C==============================================================================
      SUBROUTINE PMVIE1
C==============================================================================
C
C  Description:  Draws the YZ view of the D0 Muon Detector.
C  =============
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - September 23, 1988
C
C===============================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  =====================
C
      INTEGER IVIEW
      DATA IVIEW/1/
C
C  Executable Code:
C  =================
C
      CALL PMAXES(IVIEW)
      CALL PMDDET(IVIEW)
      CALL PMEVNT(IVIEW,3)
      RETURN
      END
