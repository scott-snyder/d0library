      LOGICAL FUNCTION VERTEX_CDC() 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to find vertex's Z position from CDC hits
C-                         (temporarily only find ONE prime vertex,
C-                         later, will update it to find multivertices)
C-
C-   Returned value  : true: found vertex, false: vertex is not found
C-   Inputs  : none
C-   Outputs : VERT bank is filled
C-
C-   Created  27-FEB-1990   Qizhong Li-Demarteau
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    ZVERTX, ZERROR, ZCERMX
      CHARACTER*4 DPATH
      INTEGER ERR, IPATH
      EQUIVALENCE (IPATH,DPATH)
      INTEGER IER
      LOGICAL EZERROR
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','VERTEX_CDC',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('DPATH',IPATH,ERR)
        CALL EZRSET
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','VERTEX_CDC',
     &    'Unable to find bank VERTEX_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('ZCERMX',ZCERMX,ERR)
        CALL EZRSET
      END IF
      CALL PATHST(DPATH)
      VERTEX_CDC = .FALSE.
C
C  unpack part of CDC hits
C
      CALL ZCDCHT
C
C  histogramming CDC information to find vertex
C
      CALL ZCDCHS(ZVERTX,ZERROR)
      IF (ABS(ZERROR) .LE. ZCERMX) THEN
        VERTEX_CDC = .TRUE.
        CALL PATHRS
C
C  book and create VERT bank
C
        CALL ZCDCFL(ZVERTX,ZERROR,100)
      ENDIF
C
  999 RETURN
      END
