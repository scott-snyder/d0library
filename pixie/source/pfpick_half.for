      SUBROUTINE PFPICK_HALF(HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Choose the FDC half to display in R-Z view.
C-      If RCP parameter 'FDC CHOOSE HALF'CHOOSE_HALF is false, 
C-      just use  'FDC HALF'. Otherwise, if both sides have tracks, 
C-      prompt user for half.
C-
C-   Outputs : HALF
C-
C-   Created  23-JAN-1992   Robert E. Avery
C-   Updated  17-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  17-FEB-1992   Robert E. Avery  If hardcopy, don't ask questions.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Output:
      INTEGER HALF
C
C  Local:
      INTEGER LFTRH, GZFTRH
      INTEGER NUMTRK(0:1),PREV_HALF 
      INTEGER II, JJ, LEN, IER
C
      LOGICAL CHOOSE_HALF
      LOGICAL EZERROR
      LOGICAL FLGVAL,HARDCOPY 
C
      CHARACTER*50 TEXT
      CHARACTER*60 PROM
      CHARACTER*80 STRING
C
      DATA PROM/' Enter FDC HALF - N(orth) or S(outh)>'/
C----------------------------------------------------------------------
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFTHTA','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('FDC CHOOSE HALF',CHOOSE_HALF)
      CALL PUGETV('FDC HALF',HALF)
C
      CALL PFFIX_HALF
C
      LFTRH=GZFTRH()
      IF(LFTRH.LE.5) THEN
        NUMTRK(0)=0
        NUMTRK(1)=0
      ELSE
        NUMTRK(0)=IQ(LFTRH+5)
        NUMTRK(1)=IQ(LFTRH+6)
      ENDIF
C
      IF ( CHOOSE_HALF ) THEN
        IF ( (NUMTRK(0).NE.0 ) .AND.  (NUMTRK(1).EQ.0) ) THEN 
          HALF = 0
        ELSEIF ( (NUMTRK(0).EQ.0 ) .AND.  (NUMTRK(1).NE.0) ) THEN 
          HALF = 1
        ELSEIF ( (NUMTRK(0).NE.0 ) .AND.  (NUMTRK(1).NE.0) ) THEN  
C
C  Prompt for half (If hard copy, use previous values)
C
          HARDCOPY = FLGVAL('HARDCOPY')
          IF ( .NOT.HARDCOPY ) THEN         
            CALL OUTMSG('1')
            WRITE(TEXT,201) NUMTRK
  201       FORMAT(' FDC Tracks in North FDC =',I4,',  South FDC =',I4)
            CALL OUTMSG(TEXT)
C
            CALL GETPAR(1,PROM,'U',STRING)
            CALL SWORDS(STRING,II,JJ,LEN)
            IF ( LEN .NE. 0 ) THEN
              IF (STRING(1:1).EQ.'N'.OR.STRING(1:1).EQ.'n') THEN
                HALF = 0
              ELSE IF (STRING(1:1).EQ.'S'.OR.STRING(1:1).EQ.'s') THEN
                HALF = 1
              ELSE
                READ ( STRING(1:LEN),*,ERR=999) HALF 
              END IF
            ELSE
              HALF = PREV_HALF
            END IF
          ELSE
            HALF = PREV_HALF
          END IF
        ENDIF
        PREV_HALF = HALF  
      ENDIF
C
      CALL PUSETV('FDC HALF',HALF)
      CALL EZRSET
  999 RETURN
      END
