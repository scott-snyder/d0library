      FUNCTION VERTEX_MULTI() 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to find vertex's Z position from CDC tracks
C-                         (It handls multi-vertices)
C-
C-   Returned value  : true: found vertex, false: vertex is not found
C-   Inputs  : none
C-   Outputs : VERT banks are filled
C-
C-   Created  17-OCT-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUN, ID, RUNSAV, IDSAV
      INTEGER IER, ERR, I, WEIGHT(3), NTRK(3)
      LOGICAL VERTEX_MULTI, EZERROR
      LOGICAL FIRST, DONE, DTREVT
      REAL    ZVERTX(3), ZERROR(3), ZCERMX
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
      VERTEX_MULTI = .FALSE.
      IF (.NOT.DONE) GOTO 999
C
      CALL ZVERTX_MULTI(ZVERTX,ZERROR,WEIGHT,NTRK)
C
      CALL PATHRS
      DO 100 I = 1, 3
        IF (ABS(ZERROR(I)) .LE. ZCERMX) THEN
          VERTEX_MULTI = .TRUE.
C
C  book and create VERT bank
C
          CALL ZVERTFL_CDC(ZVERTX(I),ZERROR(I),WEIGHT(I),NTRK(I))
        ENDIF
  100 CONTINUE
C
  999 RETURN
      END
