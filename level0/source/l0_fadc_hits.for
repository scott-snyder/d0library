      SUBROUTINE L0_FADC_HITS(N_TIME,N_PH,N_NHIT,N_BEST_HIT,
     &  S_TIME,S_PH,S_NHIT,S_BEST_HIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetchs information about the Level 0 FASTZ signals
C-                         as stored in Crate 71 TRGR FADCs.
C-
C-   Inputs  : none
C-   Outputs : N_TIME,S_TIME(10) - time of N,S L0 FASTZ pulse
C-             N_PH,  S_PH(10)   - pulse heights of N,S L0 FASTZ pulse
C-             N_NHIT,S_NHIT     - number of hits on N,S L0 FASTZ
C-             N_BEST_HIT,S_BEST_HIT - closest N,S L0 FASTZ hit to expected time
C-   Controls: none
C-
C-   Created  22-JUN-1993   Jeffrey Bantly
C-   Updated  26-JAN-1994   Jeffrey Bantly  make Run 1a AND Run 1b compatible
C-                                          and more RCP flexible.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NORTH_CHAN,SOUTH_CHAN
      INTEGER IHIT,NHIT
      INTEGER N_NHIT,S_NHIT
      INTEGER N_BEST_HIT,S_BEST_HIT
      INTEGER ERR
C
      REAL HIT_LOC(10),HIT_PH(10)
      REAL CHAN4_LOC,CHAN5_LOC,MIN_DIFF,IHIT_LOC
      REAL N_TIME(10),S_TIME(10)
      REAL N_PH(10),S_PH(10)
C
      LOGICAL EZERROR
      LOGICAL FIRST
C
      SAVE FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN    ! Book histograms
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('LEVEL0-NO-RCP','L0_FADC_HITS',
     &                                 'LEVEL0_RCP NOT FOUND.','W')
        ELSE
          CALL EZGET_i('NORTH_FADC_CHAN',NORTH_CHAN,ERR)
          IF (ERR.NE.0) NORTH_CHAN=4
          CALL EZGET_u('SOUTH_FADC_CHAN',SOUTH_CHAN,ERR)
          IF (ERR.NE.0) SOUTH_CHAN=5
          CALL EZGET('CHAN4_LOC',CHAN4_LOC,ERR)
          IF (ERR.NE.0) CHAN4_LOC=470.0
          CALL EZGET('CHAN5_LOC',CHAN5_LOC,ERR)
          IF (ERR.NE.0) CHAN5_LOC=500.0
          CALL EZRSET
        ENDIF
        FIRST=.FALSE.
      ENDIF
C
C ****  North End
C
      CALL TRGR_FADC_HITS(NORTH_CHAN,HIT_LOC,HIT_PH,NHIT)
      N_NHIT=NHIT
      N_BEST_HIT=0
      IF ( NHIT.GE.1 ) THEN
        N_BEST_HIT=1
        MIN_DIFF=ABS(HIT_LOC(1)*9.434-CHAN4_LOC)
        DO IHIT=1,NHIT
          N_TIME(IHIT)= HIT_LOC(IHIT)*9.434
          N_PH(IHIT)  = HIT_PH(IHIT)
          IHIT_LOC    = HIT_LOC(IHIT)*9.434
          IF ( ABS(IHIT_LOC-CHAN4_LOC).LT.MIN_DIFF ) THEN
            N_BEST_HIT= IHIT
            MIN_DIFF  = ABS(IHIT_LOC-CHAN4_LOC)
          ENDIF
        ENDDO
      ENDIF
C
C ****  South End
C
      CALL TRGR_FADC_HITS(SOUTH_CHAN,HIT_LOC,HIT_PH,NHIT)
      S_NHIT=NHIT
      S_BEST_HIT=0
      IF ( NHIT.GE.1 ) THEN
        S_BEST_HIT=1
        MIN_DIFF=ABS(HIT_LOC(1)*9.434-CHAN5_LOC)
        DO IHIT=1,NHIT
          S_TIME(IHIT)= HIT_LOC(IHIT)*9.434
          S_PH(IHIT)  = HIT_PH(IHIT)
          IHIT_LOC=HIT_LOC(IHIT)*9.434
          IF ( ABS(IHIT_LOC-CHAN5_LOC).LT.MIN_DIFF ) THEN
            S_BEST_HIT=IHIT
            MIN_DIFF  = ABS(IHIT_LOC-CHAN5_LOC)
          ENDIF
        ENDDO
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
