      SUBROUTINE PVTX3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make a #D display of the vertex
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  29-OCT-1992   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL EZERROR
      INTEGER IER,IFVSEC,IFVHITS,IFVTRKS
C----------------------------------------------------------------------
C
C ****  Getting drawing flags
C
      CALL EZPICK('PX_VTXDIS_RCP')      
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PVTX3D','Bank PX_VTXDIS_RCP NOT FOUND',
     &    'W')
        GOTO 999
      ENDIF
      CALL PUGETV('VTX DRAW 3D SEC ', IFVSEC )
      CALL PUGETV('VTX DRAW HITS', IFVHITS )
      CALL PUGETV('VTX DRAW TRACKS', IFVTRKS )
      CALL EZRSET
C
C ****  Draw the geometry of the vertex
C
      IF ( IFVSEC .GT. 0) THEN 
        CALL PVTX3D_GEO(0,31)
      ENDIF
C
C ****  Draw the Hits of the vertex
C
      IF ( IFVHITS .GT. 0 ) THEN
        CALL PVHITS_3D
      ENDIF
C
C ****  Draw Tracks
C
      IF ( IFVTRKS .GT. 0 ) THEN
        CALL PVTRK_3D
      ENDIF
  999 RETURN
      END
