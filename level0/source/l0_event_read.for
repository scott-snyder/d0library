      SUBROUTINE L0_EVENT_READ(CBUNCH,RAW_TIMES,RAW_CHARGE,NFDC_HITS,
     &                      CDCZ,FASTZ,NLC,LC_HIT_POSITIONS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in Level 0 event info from unformatted
C-                         file made using l0_event_write.for
C-
C-   Inputs  : none
C-   Outputs  : CBUNCH
C-              RAW_TIMES
C-              RAW_CHARGE
C-              NFDC_HITS
C-              CDCZ
C-              FASTZ
C-              NLC
C-              LC_HIT_POSITIONS
C-   Controls: none
C-
C-   Created  25-JUN-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RAW_TIMES(6,80)
      INTEGER RAW_CHARGE(6,80)
      INTEGER CBUNCH
      INTEGER NLC(8,2)
      INTEGER TIMES(72),CHARGE(72),NFDC_HITS(72)
      INTEGER NUM_HITS
      INTEGER END,ICH,LCH,IHIT,I
      INTEGER UNIT,IER
      INTEGER PAD_TO_CHAN(8)
C
      REAL CDCZ,FASTZ
      REAL LC_HIT_POSITIONS(10,8,2,3)
      REAL LONG_HIT_INFO(5,100)
C
      LOGICAL FIRST
C
      SAVE PAD_TO_CHAN,FIRST
      DATA PAD_TO_CHAN/21,22,23,24,29,30,31,32/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C  Fetch an output unit
C
      IF ( FIRST ) THEN
        CALL GTUNIT(652,UNIT,IER)
        FIRST= .FALSE.
      ENDIF
C
C  Read in correct bunch data to unformatted file
C
      READ (UNIT) TIMES,CHARGE,NFDC_HITS,CDCZ,FASTZ,NUM_HITS
      IF ( NUM_HITS.GT.100 ) NUM_HITS=100
      IF ( NUM_HITS.GT.0 ) THEN
        READ (UNIT) ((LONG_HIT_INFO(I,IHIT),I=1,5),IHIT=1,
     &    NUM_HITS)
      ENDIF
C
C ****  i=1 --> CH1
C ****  i=2 --> CH2
C ****  i=3 --> xfdc
C ****  i=4 --> yfdc
C ****  i=5 --> 'bin' along counter
C
C ****  ihit = ith FDC track hit through the counter
C
C
C  Load local variables
C
      DO ICH=1,72
        TIMES(ICH)  = RAW_TIMES(CBUNCH,ICH)
        CHARGE(ICH) = RAW_CHARGE(CBUNCH,ICH)
      ENDDO
C
      NUM_HITS = 0
      DO END=1,2
        DO LCH=1,8
          DO IHIT=1,NLC(LCH,END)
            NUM_HITS = NUM_HITS + 1
            LONG_HIT_INFO(1,NUM_HITS) = PAD_TO_CHAN(LCH)+((END-1)*36)
            LONG_HIT_INFO(2,NUM_HITS) = PAD_TO_CHAN(LCH)+4+((END-1)*36)
            LONG_HIT_INFO(3,NUM_HITS) = LC_HIT_POSITIONS(IHIT,LCH,END,1)
            LONG_HIT_INFO(4,NUM_HITS) = LC_HIT_POSITIONS(IHIT,LCH,END,2)
            LONG_HIT_INFO(5,NUM_HITS) = LC_HIT_POSITIONS(IHIT,LCH,END,3)
            IF ( NUM_HITS.GE.100 ) GOTO 100
          ENDDO
        ENDDO
      ENDDO
C
  100 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
