      SUBROUTINE L0_EVENT_WRITE(CBUNCH,RAW_TIMES,RAW_CHARGE,NFDC_HITS,
     &                      CDCZ,FASTZ,NLC,LC_HIT_POSITIONS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out Level 0 event info to ascii file
C-
C-   Inputs  : CBUNCH
C-             RAW_TIMES
C-             RAW_CHARGE
C-             NFDC_HITS
C-             CDCZ
C-             FASTZ
C-             NLC
C-             LC_HIT_POSITIONS
C-   Outputs : write out to file some of the data
C-   Controls: none
C-
C-   Created  25-JUN-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RAW_TIMES(80)
      INTEGER RAW_CHARGE(80)
      INTEGER CBUNCH,WBUNCH
      INTEGER NLC(8,2)
      INTEGER TIMES(72),CHARGE(72),NFDC_HITS(72)
      INTEGER NUM_HITS
      INTEGER END,ICH,LCH,IHIT,I
      INTEGER UNIT1,UNIT2,IER
      INTEGER PAD_TO_CHAN(8)
      INTEGER NUMEVT
C
      REAL CDCZ,FASTZ
      REAL LC_HIT_POSITIONS(10,8,2,3)
      REAL LONG_HIT_INFO(5,100)
C
      LOGICAL FIRST
C
      SAVE PAD_TO_CHAN,FIRST,NUMEVT
      DATA PAD_TO_CHAN/21,22,23,24,29,30,31,32/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C  Fetch an output unit
C
      IF ( FIRST ) THEN
        CALL GTUNIT(652,UNIT1,IER)
        CALL GTUNIT(694,UNIT2,IER)
        NUMEVT=0
        FIRST= .FALSE.
      ENDIF
C
C  Load local variables
C
      DO ICH=1,72
        TIMES(ICH)  = RAW_TIMES(ICH)
        CHARGE(ICH) = RAW_CHARGE(ICH)
      ENDDO
C
      NUM_HITS = 0
C      DO END=1,2
C        DO LCH=1,8
C          DO IHIT=1,NLC(LCH,END)
C            NUM_HITS = NUM_HITS + 1
C            LONG_HIT_INFO(1,NUM_HITS) = PAD_TO_CHAN(LCH)+((END-1)*36)
C            LONG_HIT_INFO(2,NUM_HITS) = PAD_TO_CHAN(LCH)+4+((END-1)*36)
C            LONG_HIT_INFO(3,NUM_HITS) = LC_HIT_POSITIONS(IHIT,LCH,END,1)
C            LONG_HIT_INFO(4,NUM_HITS) = LC_HIT_POSITIONS(IHIT,LCH,END,2)
C            LONG_HIT_INFO(5,NUM_HITS) = LC_HIT_POSITIONS(IHIT,LCH,END,3)
C            IF ( NUM_HITS.GE.100 ) GOTO 100
C          ENDDO
C        ENDDO
C      ENDDO
C
  100 CONTINUE
C
C  Write out correct bunch data to unformatted file
C
      WRITE (UNIT1) TIMES,CHARGE,NFDC_HITS,CDCZ,FASTZ,NUM_HITS
C      IF ( NUM_HITS.GT.100 ) NUM_HITS=100
C      IF ( NUM_HITS.GT.0 ) THEN
C        WRITE (UNIT1) ((LONG_HIT_INFO(I,IHIT),I=1,5),IHIT=1,
C     &    NUM_HITS)
C      ENDIF
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
C  Write out some off-bunch data for pedestals
C
C      IF ( NUMEVT.GT.1000 ) GOTO 999
C      NUMEVT=NUMEVT+1
C      WBUNCH = CBUNCH + 1
C      IF ( WBUNCH.GT.6 ) WBUNCH=1
C      DO ICH=1,72
C        TIMES(ICH)  = RAW_TIMES(WBUNCH,ICH)
C        CHARGE(ICH) = RAW_CHARGE(WBUNCH,ICH)
C      ENDDO
C      WRITE (UNIT2) TIMES,CHARGE
C
C----------------------------------------------------------------------
  999 RETURN
      END
