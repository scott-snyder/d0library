      FUNCTION VERTEX_FDC() 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to find vertex's Z position from FDC hits
C-                         (temporarily only find ONE prime vertex,
C-                         later, will update it to find multivertices)
C-
C-   Returned value  : true: found vertex, false: vertex is not found
C-   Inputs  : none
C-   Outputs : VERT bank is filled
C-
C-   Created  13-SEP-1990   Jeffrey Bantly
C-   Updated  14-AUG-1991   Susan K. Blessing  Add EZRSETs. 
C-   Updated  19-FEB-1993   Robert E. Avery  Change call to  ZFDCFL
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ERR
      INTEGER IDX,LENGTH
      PARAMETER( IDX = 1 )
C
      REAL    ZVERTX, ZERROR, ZFERMX
C
      CHARACTER*4 FPATH
C
      LOGICAL VERTEX_FDC
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGETS('FPATH',IDX,FPATH,LENGTH,ERR)
        CALL EZRSET
        CALL EZPICK('VERTEX_RCP')
        CALL EZGET('ZFERMX',ZFERMX,ERR)
        CALL EZRSET
      END IF
C
      CALL PATHST(FPATH)
      VERTEX_FDC = .FALSE.
C
C  unpack part of FDC hits
C
      CALL ZFDCHT
C
C  histogramming FDC information to find vertex
C
      CALL ZFDCHS(ZVERTX,ZERROR)
C
C  check for acceptable vertex
C
      IF (ABS(ZERROR) .LE. ZFERMX) THEN
        VERTEX_FDC = .TRUE.
        CALL PATHRS
C
C  book and create VERT bank
C
        CALL ZFDCFL(ZVERTX,ZERROR,0.,0)
      ENDIF
C
C  clear FDC hit bank link area
C
      CALL FCLRLNK
C--------------------------------------------------------------------
  999 RETURN
      END
