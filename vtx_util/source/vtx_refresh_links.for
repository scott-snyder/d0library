      SUBROUTINE VTX_REFRESH_LINKS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Refresh the links in VTXLNK link area
C-
C-   Inputs  : None
C-   Outputs : All links in VTXLNK are refreshed, EXCEPT for strip banks,
C-             which are NOT used and are ignored here.
C-   Controls: 
C-
C-   Created   1-JUL-1994   Al Clark
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$INC:VTXLNK.INC'
C
      INTEGER GZVTXH, GZVLAY, GZVSEC, GZVWDA
      INTEGER LAY, SEC, NSEC(0:2)
      DATA NSEC / 16, 32, 32 /
C
C----------------------------------------------------------------------
      LCDD1 = LQ(LHEAD - IZCDD1)
C
      LVTXH = GZVTXH()
      IF ( LVTXH .GT. 0) THEN
        DO LAY = 0, 2
          LVLAY(LAY) = GZVLAY(LAY)
          IF ( LVLAY(LAY) .GT. 0) THEN
            DO SEC = 0, NSEC(LAY)-1
              LVSEC(SEC,LAY) = GZVSEC(LAY, SEC)
              IF ( LVSEC(SEC,LAY) .GT. 0) THEN
                LVWDA(SEC,LAY) = GZVWDA(LAY,SEC)
              ELSE
                LVWDA(SEC,LAY) = 0
              ENDIF
            ENDDO       ! SEC
          ELSE
            DO SEC = 0, NSEC(LAY)-1
              LVSEC(SEC,LAY) = 0
              LVWDA(SEC,LAY) = 0
            ENDDO           ! SEC
          ENDIF           ! IF LVLAY
        ENDDO           ! LAY
      ELSE
        CALL VTXLNK_CLR(1)    ! LVTXH=0; Clear VLAY and below
      ENDIF
  999 RETURN
      END
