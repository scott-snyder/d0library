      SUBROUTINE PZTRAKS_TOP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display CD TOP view
C-        (PHI between -45 and +45 deg on top 1/2 of screen
C-         PHI between 135 and 225 deg on bottom 1/2)
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
      DATA PHICEN,PHIWID/-2.8125,45./
      DATA CDCSECNEW,CDCLABNEW,CDCROADNEW/1,0,0/
      DATA VTXVERNEW,VTXBEAMNEW/0,0/
      DATA ONLY/.FALSE./
C----------------------------------------------------------------------
      CALL EZPICK('PX_SYSTEM_RCP')          ! Selecting SYSTEM bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PZTRAKS_TOP',
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
        CALL ERRMSG('PIXIE','PZTRAKS_TOP',
     & 'Bank PX_CDCDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PUGET_l('CDC ONLY',CDCSAV)
      CALL PUGET_i('CDC DRAW SECTORS',CDCSECOLD)
      CALL PUGET_i('CDC DRAW LABEL',CDCLABOLD)
      CALL PUGET_i('CDC DRAW ROAD',CDCROADOLD)
      CALL PUSET_l('CDC ONLY',ONLY)
      CALL PUSET_i('CDC DRAW SECTORS',CDCSECNEW)
      CALL PUSET_i('CDC DRAW LABEL',CDCLABNEW)
      CALL PUSET_i('CDC DRAW ROAD',CDCROADNEW)
      CALL PDRZVW
      CALL PUSET_l('CDC ONLY',CDCSAV)
      CALL PUSET_i('CDC DRAW SECTORS',CDCSECOLD)
      CALL PUSET_i('CDC DRAW LABEL',CDCLABOLD)
      CALL PUSET_i('CDC DRAW ROAD',CDCROADOLD)
      CALL EZRSET
      CALL EZPICK('PX_VTXDIS_RCP')          ! Selecting VTXDIS bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PZTRAKS_TOP',
     & 'Bank PX_VTXDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PUGET_l('VTX ONLY',VTXSAV)
      CALL PUGET_i('VTX DRAW VERTEX',VTXVEROLD)
      CALL PUGET_i('VTX DRAW BEAMLINE',VTXBEAMOLD)
      CALL PUSET_l('VTX ONLY',ONLY)
      CALL PUSET_i('VTX DRAW VERTEX',VTXVERNEW)
      CALL PUSET_i('VTX DRAW BEAMLINE',VTXBEAMNEW)
      CALL PVRZVW
      CALL PUSET_l('VTX ONLY',VTXSAV)
      CALL PUSET_i('VTX DRAW VERTEX',VTXVEROLD)
      CALL PUSET_i('VTX DRAW BEAMLINE',VTXBEAMOLD)
      CALL EZRSET
      CALL EZPICK('PX_FDCDIS_RCP')          ! Selecting FDCDIS bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PZTRAKS_TOP',
     & 'Bank PX_FDCDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PUGET_l('FDC ONLY',FDCSAV)
      CALL PUSET_l('FDC ONLY',ONLY)
      CALL PFDCRZ
      CALL PUSET_l('FDC ONLY',FDCSAV)
      CALL EZRSET
      CALL EZPICK('PX_SYSTEM_RCP')          ! Selecting SYSTEM bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PZTRAKS_TOP',
     & 'Bank PX_SYSTEM_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PZTRAKS_RZ
      CALL PUSETV('PHI CENTER',PHICSAV)
      CALL PUSETV('PHI WIDTH',PHIWSAV)
      CALL EZRSET
  999 RETURN
      END
