      FUNCTION VERTEX_FIX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Recalculate and determine primary vertex.
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-JUL-1995   Srini Rajagopalan
C-   Updated   5-SEP-1995   Srini Rajagopalan  reorganization.
C-   Updated  25-SEP-1995   Srini Rajagopalan  Add bit set for vertex_change 
C-   Updated  26-SEP-1995   Srini Rajagopalan  Move num of track to word 12 
C-   Updated   3-OCT-1995   Srini Rajagopalan  Fix for LVERT=0 
C-   Updated  18-OCT-1995   Srini Rajagopalan  Fix bug: Bit setting for
C-                          vertex_Change flag.
C-   Updated  31-JAN-1996   Srini Rajagopalan  Fix argument to ERRMSG 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK'
C
      LOGICAL VERTEX_FIX
      LOGICAL RECOOK,EZERROR
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
      INTEGER   IER
      INTEGER   LVERH,LVERT,GZVERH,GZVERT
      INTEGER   TOTTRK,NTRACK
      INTEGER   VERSION,PASS
C
      REAL      ZVERT,ZCUR,ZCHANGE,ZDEFAULT
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN 
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_FIX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('BAD RCP','VERTEX_FIX',
     &                'Unable to find bank VERTEX_FIX_RCP','F')
        ENDIF
        CALL EZGET('ZCHANGE',ZCHANGE,IER)
        CALL EZGET('ZDEFAULT',ZDEFAULT,IER)
        CALL EZRSET
      ENDIF
C
      VERTEX_FIX = .TRUE.
C
C Check Reco version... ALL DTRK tracks are kept for Reco 12.11+
C
      CALL FULL_D0RECO_VERSION(VERSION,PASS)
      RECOOK = VERSION.GE.12 .AND. PASS.GE.11
      IF (.NOT. RECOOK) THEN
        IF (VERSION.EQ.0 .AND. PASS.EQ.0) THEN
          CALL ERRMSG('No HSTR bank','VERTEX_FIX',
     &      'RECO version unknown','W')
        ELSE
          CALL ERRMSG('Old D0RECO Version','VERTEX_FIX',
     &              'No VERTEX FIX applied','W')
          GO TO 999
        ENDIF
      ENDIF
C
C     LVERH 
C
      LVERT = GZVERT(1)
      IF (LVERT.GT.0) THEN
        ZCUR = Q(LVERT+5)
      ELSE
        CALL ERRMSG('No existing primary vertex','VERTEX_FIX',
     &              'Using default value','W')
        ZCUR = ZDEFAULT
      ENDIF
C
      CALL FLGSET('VERTEX_CHANGE',.FALSE.)
C
      CALL FIX_VERH               ! Fix problem with VERH bank
C      
C REDO Vertex finding
C
      CALL VERTEX_FIT
C
C Select vertex based on particle information
C
      CALL VERTEX_SELECT
C
C Compare new primary vertex with old, if change is large - set flag
C
      LVERH = GZVERH()
      LVERT = LQ(LVERH - IZVERT)
      IF (LVERT.GT.0) THEN
        ZVERT = Q(LVERT + 5)
      ELSE
        ZVERT = ZDEFAULT
      ENDIF
C
      IF (ABS(ZVERT-ZCUR).GT.ZCHANGE) THEN
        CALL FLGSET('VERTEX_CHANGE',.TRUE.)
        IQ(LVERH) = IBSET(IQ(LVERH),2)
      ENDIF
C
C Update VERH bank with number of tracks and old z position
C
      Q(LVERH + 11) = ZCUR
      CALL DTRK_IN_VERT(TOTTRK,NTRACK)
      IQ(LVERH + 12) = IQ(LVERH) + TOTTRK
      IQ(LVERH + 12) = IQ(LVERH) + NTRACK*256
C
  999 RETURN
      END
