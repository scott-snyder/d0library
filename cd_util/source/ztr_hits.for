      SUBROUTINE ZTR_HITS( FULL_HITFINDING ) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call hit finding for each subdetector.
C-
C-   Inputs  : FULL_HITFINDING  If true, full hitfinding should be done.
C-   Outputs : 
C-
C-   Created  18-OCT-1993   Robert E. Avery
C-   Updated  13-APR-1994   NORMAN A. GRAF  Added call to TRD_HITS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FULL_HITFINDING
C
      INTEGER ERR
      LOGICAL VTXON, CDCON, FDCON, TRDON
      LOGICAL FIRST
      SAVE FIRST, VTXON, CDCON, FDCON, TRDON
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        CALL EZGET('VTXON',VTXON,ERR)
        CALL EZGET('CDCON',CDCON,ERR)
        CALL EZGET('FDCON',FDCON,ERR)
        CALL EZGET('TRDON',TRDON,ERR)
        CALL EZRSET
      ENDIF
C
      IF ( VTXON.AND.FULL_HITFINDING ) THEN
        CALL VTX_HITS
      ENDIF
C
      IF ( FDCON ) THEN
        CALL FDC_HITS( FULL_HITFINDING ) 
      ENDIF
C
      IF ( CDCON.AND.FULL_HITFINDING ) THEN
C        CALL CDC_HITS
      ENDIF
C
      IF ( TRDON.AND.FULL_HITFINDING ) THEN
        CALL TRD_HITS            ! create THIT bank for TRD
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
