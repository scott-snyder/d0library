      FUNCTION ZCLEAN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reset the number of hits in CD hit header 
C-   banks if lower banks banks have been dropped 
C-
C-   Created  27-FEB-1991   Daria Zieminska
C-   Updated   6-AUG-1991   Susan K. Blessing  Set ZCLEAN .TRUE. 
C-    and beautify.
C-   Updated   1-DEC-1991   Qizhong Li-Demarteau  fille ZTRAKS histograms
C-                                                at the end of the event 
C-   Updated  16-FEB-1994   Al Clark  Add call to VTX_MARK_HITS to associate
C-      hits in VCHT with tracks in VTXT and ZTRK 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      LOGICAL ZCLEAN,DROP,YES
C
      INTEGER LVTXH,LCDCH,LFDCH 
      INTEGER GZVTXH,GZCDCH,GZFDCH
C
C----------------------------------------------------------------------
C
      ZCLEAN = .TRUE.
C
      CALL VTX_MARK_HITS
C
      IF (IQ(LHEAD+9) .GT. 0) CALL ZTRHIS
C
      CALL EVBANK_CHECK('STA','VLAY',DROP)
      IF (DROP) THEN
        LVTXH=GZVTXH()
        IF (LVTXH.GT.0) IQ(LVTXH+2)=0
      END IF
C
      CALL EVBANK_CHECK('STA','VZLA',YES)
      IF (YES) THEN
        LVTXH=GZVTXH()
        IF (LVTXH.GT.0) THEN
          IQ(LVTXH+3)=0
          IF (DROP) IQ(LVTXH+1)=0
        END IF
      END IF
C
      CALL EVBANK_CHECK('STA','DLYR',DROP)
      IF (DROP) THEN
        LCDCH=GZCDCH()
        IF (LCDCH.GT.0) IQ(LCDCH+1)=0
      END IF
C
      CALL EVBANK_CHECK('STA','FTQD',DROP)
      CALL EVBANK_CHECK('STA','FPSC',YES)
      DROP=DROP.AND.YES
      IF (DROP) THEN
        LFDCH=GZFDCH()
        IF (LFDCH.GT.0) THEN
          IQ(LFDCH+1)=0
          IQ(LFDCH+10)=0
        END IF
      END IF
C
  999 RETURN
      END
