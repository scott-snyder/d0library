c DEC/CMS REPLACEMENT HISTORY, Element GZMHTT.FOR
c *1    10-MAR-1988 15:21:45 TAMI "Get pointer to bank MHTT"
c DEC/CMS REPLACEMENT HISTORY, Element GZMHTT.FOR
      INTEGER FUNCTION GZMHTT(ITRAK)
C==================================================================
C
C  Description:  Gets ZEBRA pointer to muon bank MHTT
C  ============
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - Jan 27,1988
C  DH 11/89 add GZMUOT call
C===================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMHTT.LINK'
C
C  Argument Declarations:
C  =======================
C
      INTEGER ITRAK                    ! Don't worry about this for now.
C
C  Local Declarations:
C  ===================
C
      INTEGER LMUOT,GZMUOT
C
C  Executable Code:
C  =================
C
      GZMHTT = 0
      LMUOT = GZMUOT(ITRAK)
      IF (LMUOT .NE. 0) THEN
        GZMHTT = LQ(LMUOT-IZMHTT)
      ENDIF
      RETURN
      END
