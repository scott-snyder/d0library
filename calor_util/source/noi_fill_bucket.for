      SUBROUTINE NOI_FILL_BUCKET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate fractional contribution
C-                         of signals in past, present, and future
C-                         buckets
C-
C-
C-   Inputs  :
C-   Outputs :
C-              FRACTIONS(JJ) = FRACTIONAL AMPLITUDES LEFT
C-                    JJ=80 IS PRESENT BUCKET
C-                    JJ=1 IS BUCKET FURTHEST IN THE PAST
C-                    JJ=100 IS BUCKET FURTHEST IN THE FUTURE
C-   Controls:
C-
C-       Relevant RCP parameters:
C-             BASE_SUBTRACT = TRUE for do baseline subtraction
C-             T_CROSS=BEAM CROSSING INTERVAL (MUSEC)
C-             T_SAMP=BLS SAMPLING INTERVAL(MUSEC)
C-             VAL_MIN=SMALLEST(ABSOLUTE) FRACTION ENTERED
C-
C-   Created   5-SEP-1991   Peter Nemethy and Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      INTEGER JJ
      REAL AJJ,DUM,NOI_SHAPE
      REAL T_SIG_SAM,T_BASE_SAM,RENO,BASAM
C
C#####EXECUTION BEGINS HERE######
C
      DO JJ=1,100
        FRACTION(JJ)=0.0
        AJJ=FLOAT(JJ)
        T_SIG_SAM= T_CROSS*(80.0-AJJ)
        T_BASE_SAM= T_SIG_SAM-T_SAMP
        BASAM=0.0
C.........NOW DO BASELINE SUBTRACTION, IF WE TURN IT ON
        IF(BASE_SUBTRACT)BASAM=NOI_SHAPE(T_BASE_SAM)
        DUM=NOI_SHAPE(T_SIG_SAM)-BASAM
        IF(ABS(DUM).GE.VAL_MIN)FRACTION(JJ)=DUM
      ENDDO
      RENO=1.
      IF(FRACTION(80).NE.0.0)RENO=1./FRACTION(80)
      DO JJ=1,100
        FRACTION(JJ)=FRACTION(JJ)*RENO
      ENDDO
      RETURN
      END
