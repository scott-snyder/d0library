      SUBROUTINE PZCD3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make 3D display for whole Central Detector
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  15-FEB-1991   Qizhong Li-Demarteau
C-   Modified 18-MAY-1991   Nobuaki Oshima
C-   Updated  12-DEC-1991   Qizhong Li-Demarteau  added draw hits
C-   Updated  12-JUN-1992   Qizhong Li-Demarteau  added draw CDC tracks
C-                                                and primary vertex
C-   Updated   9-JUL-1992   Robert E. Avery
C-        Added FDC tracks, hits and geometry.
C-   Updated  13-AUG-1992   Nobuaki Oshima   Check picking mode.
C-   Updated  17-SEP-1992   Nobuaki Oshima
C-        Check drawing hits or do not module by module as same as tracks.
C-   Updated  15-OCT-1992   Tom Trippe  Activate, make DRAW_HITS integer.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER, DRAW_HITS
      LOGICAL DRAW_ZTRKS, DRAW_DTRKS
      LOGICAL DRAW_FTRKS, DRAW_VTRKS
      LOGICAL FDC_ONLY, PU_PICK_ACTIVE
      LOGICAL EZERROR
C----------------------------------------------------------------------
C-
C--- Check do picking...
C-
      IF ( PU_PICK_ACTIVE() ) THEN
        GO TO 999
      ENDIF
C-
C--- Get PX_ZTRAKSDIS params
C-
      CALL EZPICK('PX_ZTRAKSDIS_RCP')    
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PZCD3D','Bank PX_ZTRAKSDIS_RCP NOT FOUND',
     &    'W')
        GOTO 999
      ENDIF
      CALL PUGETV('ZTRAKS DRAW HITS  ',DRAW_HITS)
      CALL PUGETV('ZTRAKS DRAW ZTRKS ',DRAW_ZTRKS)
      CALL PUGETV('ZTRAKS DRAW DTRKS ',DRAW_DTRKS)
      CALL PUGETV('ZTRAKS DRAW FTRKS ',DRAW_FTRKS)
      CALL PUGETV('ZTRAKS DRAW VTRKS ',DRAW_VTRKS)
      CALL EZRSET
C-
C--- Draw Modules
C-
      IF (DRAW_DTRKS) THEN
        CALL PDCDC3D_GEO(0,31)
        CALL PTRD3D_GEO
      ENDIF
      IF (DRAW_FTRKS) THEN
        CALL PFDC3D_GEO
      ENDIF
      IF (DRAW_VTRKS) THEN
        CALL PVTX3D_GEO(0,31)
      ENDIF
C-
C--- Draw Vertex
C-
      CALL PZ_VERTEX_3D
C-
C--- Draw Hits
C-
      IF (DRAW_HITS.GT.0 .AND. DRAW_DTRKS) THEN
        CALL PDHITS_3D
      ENDIF
      IF (DRAW_HITS.GT.0 .AND. DRAW_FTRKS) THEN
        CALL PFHITS_3D
      ENDIF
      IF (DRAW_HITS.GT.0 .AND. DRAW_VTRKS) THEN
        CALL PVHITS_3D
      ENDIF
C-
C--- Draw Tracks
C-
      IF (DRAW_ZTRKS) THEN
        CALL PZTRK_3D
      ENDIF
      IF (DRAW_DTRKS) THEN
        CALL PDTRK_3D
      ENDIF
      IF (DRAW_FTRKS) THEN
        CALL EZPICK('PX_FDCDIS_RCP')
        CALL PUGETV('FDC ONLY',FDC_ONLY)
        CALL PUSETV('FDC ONLY', .FALSE. )
        CALL PFTRK_3D
        CALL PUSETV('FDC ONLY',FDC_ONLY)
        CALL EZRSET
      ENDIF
      IF (DRAW_VTRKS) THEN
        CALL PVTRK_3D
      ENDIF
C-
  999 RETURN
      END
