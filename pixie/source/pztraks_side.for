      SUBROUTINE PZTRAKS_SIDE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display CD SIDE view
C-        (PHI between 45 and 135 deg on top 1/2 of screen
C-         PHI between 225 and 315 deg on bottom 1/2)
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   1-NOV-1991   Sharon Hagopian
C-   Updated  30-MAR-1992   Robert E. Avery  Add ZFIT tracks   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IER,CDCSECOLD,CDCSECNEW,CDCLABOLD,CDCLABNEW
      INTEGER CDCROADOLD,CDCROADNEW
      INTEGER VTXVEROLD,VTXBEAMOLD,VTXVERNEW,VTXBEAMNEW
      REAL PHICSAV,PHIWSAV,PHICEN,PHIWID
      LOGICAL EZERROR,ONLY,CDCSAV,VTXSAV,FDCSAV
C----------------------------------------------------------------------
      DATA PHICEN,PHIWID/87.1875,45./
      DATA CDCSECNEW,CDCLABNEW,CDCROADNEW/1,0,0/
      DATA VTXVERNEW,VTXBEAMNEW/0,0/
      DATA ONLY/.FALSE./
C----------------------------------------------------------------------
      CALL EZPICK('PX_SYSTEM_RCP')          ! Selecting SYSTEM bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PZTRAKS_SIDE',
     & 'Bank PX_SYSTEM_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PUGETV('PHI CENTER',PHICSAV)
      CALL PUGETV('PHI WIDTH',PHIWSAV)
      CALL PUSETV('PHI CENTER',PHICEN)
      CALL PUSETV('PHI WIDTH',PHIWID)
      CALL EZRSET
      CALL EZPICK('PX_CDCDIS_RCP')          ! Selecting CDCDIS bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PZTRAKS_SIDE',
     & 'Bank PX_CDCDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PUGETV('CDC ONLY',CDCSAV)
      CALL PUGETV('CDC DRAW SECTORS',CDCSECOLD)
      CALL PUGETV('CDC DRAW LABEL',CDCLABOLD)
      CALL PUGETV('CDC DRAW ROAD',CDCROADOLD)
      CALL PUSETV('CDC ONLY',ONLY)
      CALL PUSETV('CDC DRAW SECTORS',CDCSECNEW)
      CALL PUSETV('CDC DRAW LABEL',CDCLABNEW)
      CALL PUSETV('CDC DRAW ROAD',CDCROADNEW)
      CALL PDRZVW
      CALL PUSETV('CDC ONLY',CDCSAV)
      CALL PUSETV('CDC DRAW SECTORS',CDCSECOLD)
      CALL PUSETV('CDC DRAW LABEL',CDCLABOLD)
      CALL PUSETV('CDC DRAW ROAD',CDCROADOLD)
      CALL EZRSET
      CALL EZPICK('PX_VTXDIS_RCP')          ! Selecting VTXDIS bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PZTRAKS_SIDE',
     & 'Bank PX_VTXDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PUGETV('VTX ONLY',VTXSAV)
      CALL PUGETV('VTX DRAW VERTEX',VTXVEROLD)
      CALL PUGETV('VTX DRAW BEAMLINE',VTXBEAMOLD)
      CALL PUSETV('VTX ONLY',ONLY)
      CALL PUSETV('VTX DRAW VERTEX',VTXVERNEW)
      CALL PUSETV('VTX DRAW BEAMLINE',VTXBEAMNEW)
      CALL PVRZVW
      CALL PUSETV('VTX ONLY',VTXSAV)
      CALL PUSETV('VTX DRAW VERTEX',VTXVEROLD)
      CALL PUSETV('VTX DRAW BEAMLINE',VTXBEAMOLD)
      CALL EZRSET
      CALL EZPICK('PX_FDCDIS_RCP')          ! Selecting FDCDIS bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PZTRAKS_SIDE',
     & 'Bank PX_FDCDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PUGETV('FDC ONLY',FDCSAV)
      CALL PUSETV('FDC ONLY',ONLY)
      CALL PFDCRZ
      CALL PUSETV('FDC ONLY',FDCSAV)
      CALL EZRSET
      CALL EZPICK('PX_SYSTEM_RCP')          ! Selecting SYSTEM bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PZTRAKS_SIDE',
     & 'Bank PX_SYSTEM_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PZTRAKS_RZ
      CALL PUSETV('PHI CENTER',PHICSAV)
      CALL PUSETV('PHI WIDTH',PHIWSAV)
      CALL EZRSET
  999 RETURN
      END