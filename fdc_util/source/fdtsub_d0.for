      SUBROUTINE FDTSUB_D0( B, TMPUBN1, TMPUBN2, FINDTP )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find Non-differentiated timing pulse
C-   in FADC channel, as used in D0 cosmic ray commissioning run.
C-     Begining of pulse: 2 bins with B > thr1
C-     End of pulse: 1 bins with B < thr1
C-     Require: Bmax > thr2
C-              Sum  > thr3
C-     Time = average bin (weighted w. B) from begin to end of pulse.
C-
C-   Inputs  : B(LFADC), Difference array of FADC channel.
C-   Outputs : TMPUBN1, found bin.
C-             TMPUBN2, found bin of second pulse (for study purposes).
C-             FINDTP, True if pulse found.
C-
C-   Alternate entry: FGET_TPS
C-   Purpose and Methods : Returns most recently computed TMPUBN1,TMPUBN2
C-
C-   Created  14-JAN-1991   Robert E. Avery
C-   Updated  26-APR-1991   Jeffrey Bantly  use updated PARAMS,RCP
C-   Updated  25-MAY-1992   Robert E. Avery   Include SHIFTT in TMPUBN
C-                                                      definition.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS/LIST'
C  Input
      INTEGER B(LFADC)
C  Output
      REAL    TMPUBN1
      REAL    TMPUBN2
      LOGICAL FINDTP
C  Local
      INTEGER I,J,K
      INTEGER SHIFTT
      INTEGER ILAST,IFIRST,INIT
      INTEGER IER
C
      REAL  SUM,SUMX
      REAL  THR1, THR2, THR3
      REAL  THR1TP, THR2TP, THR3TP
      REAL  BMAX
      REAL  TMPUBN1_SAVE 
      REAL  TMPUBN2_SAVE 
C
      LOGICAL IN_PULSE
C
      SAVE THR1TP,THR2TP,THR3TP,SHIFTT
      SAVE TMPUBN1_SAVE,TMPUBN2_SAVE 
C
      DATA INIT /0/
      DATA THR1TP /5./
      DATA THR2TP /30./
      DATA THR3TP /50./
C
C----------------------------------------------------------------------
C
      IF (INIT.EQ.0) THEN
        INIT = 1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('THR1TP',THR1TP,IER)
        CALL EZGET('THR2TP',THR2TP,IER)
        CALL EZGET('THR3TP',THR3TP,IER)
        CALL EZGET('SHIFTT',SHIFTT,IER)
        CALL EZRSET
C
      ENDIF
C
      FINDTP = .FALSE.
C
C 
C Special set of thresholds for tp:
C
      THR1 = THR1TP
      THR2 = THR2TP
      THR3 = THR3TP
C
      BMAX = 0
      SUM  = 0.
      SUMX = 0.
      IN_PULSE = .FALSE.
      DO I = 1,LFADC-2
        IF ( (.NOT. IN_PULSE)
     &      .AND. ( B(I) .GT. THR1 )
     &      .AND. ( B(I+1) .GT. THR1 ) ) THEN
          IFIRST=I
          IN_PULSE = .TRUE.
        ENDIF
        IF ( IN_PULSE ) THEN
          IF ( B(I) .GT. BMAX ) THEN
            BMAX = B(I)
          ENDIF
          IF (  B(I) .LT. THR1 ) THEN
            ILAST=I-1
            IF ( (BMAX .GT. THR2)
     &        .AND. ( SUM .GT. THR3 )  ) THEN
              FINDTP = .TRUE.
              GOTO 100
            ELSE
              IN_PULSE = .FALSE.
              BMAX = 0
              SUM  = 0.
              SUMX = 0.
            ENDIF
          ELSE
            SUM  = SUM  + B(I)
            SUMX = SUMX + B(I) * I
          ENDIF
        ENDIF
      ENDDO
C
  100 CONTINUE
C
      IF (FINDTP) THEN
        TMPUBN1 = (SUMX/SUM - .5) - SHIFTT
      ELSE
        TMPUBN1 = 0.
        TMPUBN2 = 0.
        GO TO 999
      END IF
C
C   For timing reslution study, also look at trailing edge:
C       (Same as above, but B(pulse) is inverted.)
C
      BMAX = 0
      SUM  = 0.
      SUMX = 0.
      FINDTP = .FALSE.
      IN_PULSE = .FALSE.
      DO I = ILAST,LFADC-2
        IF ( (.NOT. IN_PULSE)
     &      .AND. ( -B(I) .GT. THR1 )
     &      .AND. ( -B(I+1) .GT. THR1 ) ) THEN
          IN_PULSE = .TRUE.
        ENDIF
        IF ( IN_PULSE ) THEN
          IF ( -B(I) .GT. -BMAX ) THEN
            BMAX = B(I)
          ENDIF
          IF (  -B(I) .LT. THR1 ) THEN
            ILAST=I-1
            IF ( (-BMAX .GT. THR2)
     &        .AND. ( SUM .GT. THR3 )  ) THEN
              FINDTP = .TRUE.
              GOTO 200
            ELSE
              IN_PULSE = .FALSE.
              BMAX = 0
              SUM  = 0.
              SUMX = 0.
            ENDIF
          ELSE
            SUM  = SUM  - B(I)
            SUMX = SUMX - B(I) * I
          ENDIF
        ENDIF
      ENDDO
C
  200 CONTINUE
C
      IF (FINDTP) THEN
        TMPUBN2 = (SUMX/SUM - .5) - SHIFTT
      ELSE
        TMPUBN2 = 0.0
      END IF
      FINDTP = .TRUE.
C
  999 CONTINUE
      TMPUBN1_SAVE = TMPUBN1
      TMPUBN2_SAVE = TMPUBN2 
      RETURN
C
C----------------------------------------------------------------------
      ENTRY FGET_TPS(TMPUBN1, TMPUBN2)
C
      TMPUBN1 = TMPUBN1_SAVE
      TMPUBN2 = TMPUBN2_SAVE
C----------------------------------------------------------------------
      RETURN
      END
