      FUNCTION HMATRIX_EVENT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main routine for Hmatrix accumulation
C-
C-   Returned value  : True if OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:USE_HMATRIX.INC'
      INCLUDE 'D0$LINKS:IZQUAN.LINK'
      LOGICAL HMATRIX_EVENT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL DO_HMATRIX_ANAL
      INTEGER IER,NEVENT,DUNIT,DMPUNI
      INTEGER HMATRIX_DUMP_EVENTS,IUSE
      REAL    CHISQ
      INTEGER I,IER1
      LOGICAL DIAGONALIZE
C----------------------------------------------------------------------
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        NEVENT = 0
        DUNIT = DMPUNI()
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET('DIAGONALIZE',DIAGONALIZE,IER)
        CALL EZRSET
      ENDIF
C
      IF ( IER.EQ.0 ) THEN
C
        IF ( ACCUMULATE ) THEN
          NEVENT = NEVENT + 1
          CALL HMTRFL(NEVENT)             ! UPDATE EVENT COUNTER
          CALL AVERFL                     ! ADD UP AVERAGES
          CALL EMATFL                     ! ADD UP CORRELATIONS
C
        ELSE
C
C ****  CALCULATE VISIBLE CHISQUARED AND PREDICT INVISIBLE QUANTITIES.
C
          IF ( IER.EQ.0 ) THEN
            CALL HMATRIX_CHISQUARED
            CALL HMATRIX_PREDICT
            IF ( DIAGONALIZE ) THEN
              CALL HMATRIX_DIAG_TRAN
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
      HMATRIX_EVENT = .TRUE.
  999 RETURN
      END
