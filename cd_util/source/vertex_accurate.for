      LOGICAL FUNCTION VERTEX_ACCURATE() 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to find vertex's Z position from CDC tracks
C-                         (used when METHOD=2 in VERTEX.RCP)
C-
C-   Returned value  : true: found vertex, false: vertex is not found
C-   Inputs  : none
C-   Outputs : VERT bank is filled
C-
C-   Created  27-JAN-1991   Qizhong Li-Demarteau
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  20-MAR-1992   Qizhong Li-Demarteau  added an argument to ZCDCFL 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    ZVERTX, ZERROR, ZCERMX
      CHARACTER*4 DPATH
      INTEGER ERR, IPATH
      EQUIVALENCE (IPATH,DPATH)
      INTEGER RUN, ID, RUNSAV, IDSAV
      INTEGER IER
      LOGICAL EZERROR
      LOGICAL FIRST, DONE, DTREVT
      SAVE RUNSAV, IDSAV, FIRST
      DATA FIRST/.TRUE./
      DATA RUNSAV,IDSAV/-1,-1/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','VERTEX_ACCURATE',
     &    'Unable to find bank VERTEX_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('ZCERMX',ZCERMX,ERR)
        CALL EZRSET
      END IF
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
        DONE = DTREVT()
      ENDIF
      VERTEX_ACCURATE = .FALSE.
      IF (.NOT.DONE) GOTO 999
C
      CALL ZVERTX_ACCURATE(ZVERTX,ZERROR)
C
      IF (ABS(ZERROR) .LE. ZCERMX) THEN
        VERTEX_ACCURATE = .TRUE.
        CALL PATHRS
C
C  book and create VERT bank
C
        CALL ZCDCFL(ZVERTX,ZERROR,100)
      ENDIF
C
  999 RETURN
      END
