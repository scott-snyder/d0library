      FUNCTION VERTEX_FDC_TRK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To find vertex  Z position from FDC tracks.
C-
C-   Returned value  : true: found vertex, false: vertex is not found
C-   Inputs  : none
C-   Outputs : VERT bank is filled
C-
C-   Created  22-MAY-1992   Robert E. Avery
C-   Updated  20-AUG-1992   Robert E. Avery   Call FIONIZ & FMARK_TRKHITS
C-                                                      for edge tracks.
C-   Updated  19-FEB-1993   Robert E. Avery   added "weight" and "nused"
C-   Updated  13-MAY-1993   Robert E. Avery  Change argument list of  
C-                              FEDGE_TRACKS to agree w. new version.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      REAL    PI, TWOPI, HALFPI, RAD
      INCLUDE 'D0$INC:PI.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
C
      INTEGER IER
      INTEGER HALF
      INTEGER WEIGHT, NUSED
      INTEGER NF, IDF(MAXTRK)
      REAL    ZVERTX, ZERROR, ZFERMX
C
      LOGICAL VERTEX_FDC_TRK
      LOGICAL FIRST
      LOGICAL EDGE_TRACKS
C
      SAVE FIRST,EDGE_TRACKS,ZFERMX
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      VERTEX_FDC_TRK = .FALSE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        CALL EZGET('ZFERMX',ZFERMX,IER)
        CALL EZRSET
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('EDGE_TRACKS',EDGE_TRACKS,IER)
        CALL EZRSET
      END IF
C
C  Do FDC full tracking
C
      CALL FTRAKS
C
C  Use FDC tracks to find vertex
C
      CALL ZVERT_FDC_TRK(ZVERTX,ZERROR,WEIGHT,NUSED)
C
C  check for acceptable vertex
C
      IF (ABS(ZERROR) .LE. ZFERMX) THEN
        VERTEX_FDC_TRK = .TRUE.
C
C  book and create VERT bank
C
        CALL ZFDCFL(ZVERTX,ZERROR,WEIGHT,NUSED)
C
C  Go back to find FDC edge tracks
C
        IF (EDGE_TRACKS) THEN
          DO HALF =  0, 1
            CALL FEDGE_TRACKS(HALF,ZVERTX,0.0,TWOPI,0.0,PI) 
          ENDDO
          CALL FIONIZ
          CALL FMARK_TRKHITS
        END IF
      ENDIF
C
C--------------------------------------------------------------------
  999 RETURN
      END
