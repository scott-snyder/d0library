      SUBROUTINE PDCDC3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make 3D display for CDC
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  22-OCT-1992   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      INTEGER DRAW_DTRKS, DRAW_DHITS
      LOGICAL PU_PICK_ACTIVE, EZERROR
C----------------------------------------------------------------------
C
      IF ( PU_PICK_ACTIVE() ) THEN
        GO TO 999
      ENDIF
C
      CALL EZPICK('PX_CDCDIS_RCP')      ! Selecting CDCDIS_RCP bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PDCDC3D','Bank PX_CDCDIS_RCP NOT FOUND',
     &    'W')
        GOTO 999
      ENDIF
      CALL PUGETV('CDC DRAW 3D HITS',DRAW_DHITS)
      CALL PUGETV('CDC DRAW 3D TRKS',DRAW_DTRKS)
      CALL EZRSET
C
      CALL PDCDC3D_GEO(0,31)
C
      IF (DRAW_DHITS .GT. 0) THEN
        CALL PDHITS_3D
      ENDIF
      IF (DRAW_DTRKS .GT. 0) THEN
        CALL PDTRK_3D
      ENDIF
C
  999 RETURN
      END
