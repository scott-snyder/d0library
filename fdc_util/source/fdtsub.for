      SUBROUTINE FDTSUB( A, B, TMPUBN, FNDTP )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the timing pulses and use them
C-
C-   Inputs  : A, B     = Fadc data and first differences
C-   Outputs : TMPUBN   = Timing pulse bin
C-             FINDTP   = TRUE if timing pulse found
C-
C-   Created   1-MAR-1988   Jeffrey Bantly
C-   Modified  6-JUL-1988   Dan Claes
C-   Modified  7-SEP-1988   Dave Buchholz
C-   Updated  25-MAY-1992   Robert E. Avery   Include SHIFTT in TMPUBN
C-                                                      definition.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER IER
      INTEGER I, X1, X2, XM, J, ICALL
      INTEGER MAXB, MINB, MAXBBN, MINBBN
      INTEGER TMPMAX, TMPMXB, ENDBIN, SHIFTT, ITMPBN
      INTEGER NXBAD,NNBAD,BADMAXB(10),BADMINB(10),IX,IN
C
      INTEGER A(LFADC), B(LFADC)
      REAL    TMPUBN
C
      LOGICAL FNDTP, FNDTP1, FNDTP2, FNDTP3
C
      SAVE ICALL
      DATA ICALL / 0 /
C----------------------------------------------------------------------
      IF( ICALL .EQ. 0 ) THEN
        ICALL = 1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('SHIFTT',SHIFTT,IER)
        CALL EZRSET
      ENDIF
      FNDTP  = .FALSE.
      FNDTP1 = .FALSE.
      FNDTP2 = .FALSE.
      FNDTP3 = .FALSE.
      TMPUBN = 0.
      TMPMXB = 0
      TMPMAX = 0
C
      NXBAD = 0
      NNBAD = 0
      CALL VZERO(BADMAXB,10)
      CALL VZERO(BADMINB,10)
C
C Assuming TIMING PULSE displays max rise & fall times, search
C first difference,  2usec into event,  for max and min values
C
    5 CONTINUE
      MAXB = B(1)
      MINB = B(1)
      MAXBBN = 1
      MINBBN = 1
      DO 10 I=50,LFADC
        IF( B(I) .GT. MAXB ) THEN
          DO 11 IX=1,NXBAD
            IF(BADMAXB(IX).EQ.I) GOTO 8
   11     CONTINUE
          MAXB = B(I)
          MAXBBN = I
        ENDIF
    8   CONTINUE
        IF( B(I) .LT. MINB ) THEN
          DO 12 IN=1,NNBAD
            IF(BADMINB(IN).EQ.I) GOTO 10
   12     CONTINUE
          MINB = B(I)
          MINBBN = I
        ENDIF
   10 CONTINUE
C
C Require Max and Min be within 10 bins of one another for TIM PULS
C
      IF( ABS(MAXBBN - MINBBN) .LE. 10 ) THEN
        FNDTP1 = .TRUE.
      ELSE
        FNDTP1 = .FALSE.
        IF(MAXBBN.LT.MINBBN .AND. NXBAD.LT.10) THEN
          NXBAD=NXBAD+1
          BADMAXB(NXBAD)=MAXBBN
          GOTO 5
        ELSEIF(NNBAD.LT.10) THEN
          NNBAD=NNBAD+1
          BADMINB(NNBAD)=MINBBN
          GOTO 5
        ENDIF
      ENDIF
C
C Search same region in raw data for absolute maximum
C
  100 CONTINUE
      TMPMAX = A(190)
      TMPMXB = 190
      DO 30 I= 190, LFADC
        IF( A(I) .GT. TMPMAX ) THEN
          TMPMAX = A(I)
          TMPMXB = I
        ENDIF
   30 CONTINUE
      IF( TMPMAX .GE. 250 ) FNDTP2 = .TRUE.
C
      IF( ABS(MAXBBN - TMPMXB) .LE. 10 ) THEN
        FNDTP3 = .TRUE.
        GOTO 200
      ENDIF
C
  200 CONTINUE
  300 CONTINUE
C
      IF( FNDTP2 ) THEN
        TMPUBN = FLOAT(TMPMXB)     !peak in raw data
      ENDIF
      IF( FNDTP3 ) THEN
        TMPUBN = FLOAT(TMPMXB)     !peak in raw data
      ENDIF
      IF( FNDTP1 ) THEN
        TMPUBN = FLOAT(MAXBBN)        !max in 1st_diff - raw data
      ENDIF                             !never exceeded 75 counts
C
      I = TMPUBN
      IF ( I .GT. LFADC-4 ) THEN
        FNDTP = .TRUE.
        GOTO 400                        !can't perform check below
      ENDIF
      IF ( I .LT. 5 ) THEN
        FNDTP = .FALSE.
        GOTO 400                        !can't perform check below
      ENDIF
      IF ( A(I) .GT. (A(I-6)+A(I-5)+A(I+5)*0.5+A(I+6)*0.5) ) THEN
        FNDTP = .TRUE.
      ELSE
        FNDTP = .FALSE.
        TMPUBN = 0
      ENDIF
C
  400 CONTINUE
C
      ITMPBN = INT(TMPUBN)
      IF( ITMPBN .LT. 2 .OR.
     &    ITMPBN .GT. 511 ) GOTO 500
      X1 = A(ITMPBN) - A(ITMPBN-1)
      X2 = A(ITMPBN) - A(ITMPBN+1)
      IF( X1 .GT. X2 ) THEN
        XM = X1
      ELSE
        XM = X2
      ENDIF
      IF( ABS(XM) .GT. 0.01 )
     &    TMPUBN = TMPUBN + (X1-X2)/(2*XM)
C
  500 CONTINUE
C
      IF( TMPUBN .LT. 2. ) TMPUBN = 0.
      IF ( FNDTP ) THEN
        TMPUBN = TMPUBN - SHIFTT
      ELSE
        TMPUBN = 0.0
      ENDIF
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
