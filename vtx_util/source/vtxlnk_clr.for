      SUBROUTINE VTXLNK_CLR(LEVEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Zero links in VTXLNK link area
C-
C-   Inputs  : LEVEL (I) : How much to clear?
C-               -1: Clear CDD1 and all hit banks
C-                0: Clear all hit banks (vtxh on down)
C-                1: Clear VLAY and below
C-                2: Clear VSEC and below
C-                3: Clear VWDA only
C-                4: Clear CDD1 only
C-
C-        Note: strip banks are not used and are ignored here
C-
C-   Created   4-NOV-1993   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VTXLNK.INC'
C
      INTEGER LEVEL
C
      INTEGER LAY, SEC, NSEC(0:2)
      DATA NSEC / 16, 32, 32 /
C----------------------------------------------------------------------
C
      IF ( LEVEL .EQ. 4 .OR. LEVEL .EQ. -1 ) LCDD1 = 0
C
      IF ( LEVEL .LE. 0 ) LVTXH = 0
C
      DO LAY = 0, 2
        IF ( LEVEL .LE. 1 ) LVLAY(LAY) = 0
        DO SEC = 0, NSEC(LAY) - 1
          IF ( LEVEL .LE. 2 ) LVSEC(SEC,LAY) = 0
          IF ( LEVEL .LE. 3 ) LVWDA(SEC,LAY) = 0
        ENDDO
      ENDDO
C
  999 RETURN
      END
