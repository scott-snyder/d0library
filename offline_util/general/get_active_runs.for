      SUBROUTINE GET_ACTIVE_RUNS(NRUN,RUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scan the file TRIG_FILT_RUN and return the
C-   list of active runs. If a GLOBAL run is active then it will be
C-   the FIRST item in the returned list of runs and the run number
C-   will be NEGATIVE. Use the routine
C-
C-      CALL READ_TRIG_FILT_RUN_FILE
C-
C-   to return the triggers and filters for the specified run.
C-
C
C   Input:    None
C
C   Outputs:  NRUN    [I]   Number of active runs
C             RUN(*)  [I]   Active runs
C
C-   Created   3-JUN-1992   Pushpa C. Bhat, Harrison B. Prosper
C-     Based on Tami Kramer's READ_TRIG_FILT_RUN
C-   Updated  20-JUN-1992   Harrison B. Prosper
C-      Return each run once only
C-   Updated  27-JUN-1992   Harrison B. Prosper   
C-      Find global run
C-   Updated  15-JUL-1992   Harrison B. Prosper   
C-      Fix stream finding bug
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NRUN, RUN(*)
C----------------------------------------------------------------------
      INTEGER MAXRUN,MAXF
      PARAMETER( MAXRUN = 128 )
      PARAMETER( MAXF   = 128 )
      INTEGER IUSER
      PARAMETER( IUSER  = 12 )
C
      CHARACTER*(*) TRIG_FILT_RUN
      PARAMETER( TRIG_FILT_RUN = 'TRIG_FILT_RUN' )
C
      INTEGER IRUN(MAXRUN), IMAP(MAXRUN), JRUN, JFILT, JTRIG
      INTEGER NTRIG, NFILT,ITRIG(MAXF), RUN_TRIG(MAXF)
      INTEGER IUNIT,IERR,I,J,K,LAST_RUN
      INTEGER IFILT(MAXF),TRIG_FILT(MAXF)
C
      REAL    VALUE
C
      LOGICAL OK, ACTIVE
C
      CHARACTER*4  ASTRG
      CHARACTER*64 CTRIG, CFILT
      CHARACTER*80 MESSAGE,RECORD
C
C----------------------------------------------------------------------
C
      NRUN = 0
C
      CALL GTUNIT(IUSER,IUNIT,IERR)
      CALL D0OPEN(IUNIT,TRIG_FILT_RUN,'IF',OK)
      IF ( .NOT. OK ) GOTO 999
C
C ****  Get list of runs
C
      READ(IUNIT,101,ERR=900) NTRIG,ASTRG
  101 FORMAT(I4,1X,A4)
C
      J = 0
      DO I = 1,NTRIG
        READ(IUNIT,103,ERR=900) ITRIG(I),CTRIG,RUN_TRIG(I)
  103   FORMAT(I4,1X,A64,1X,I10)
        IF ( RUN_TRIG(I) .GT. 0 ) THEN
          IF ( J .LT. MAXRUN ) THEN
            J = J + 1
            IRUN(J) = RUN_TRIG(I)
          ENDIF
        ENDIF
      ENDDO
C
C ****  Sort runs and remove duplicates
C
      IF ( J .GT. 0 ) THEN
        CALL SRTINT(IRUN,J,IMAP)
C
        LAST_RUN = 0
        NRUN = 0
        DO I =  J, 1,-1
          IF ( IRUN(I) .NE. LAST_RUN ) THEN
            NRUN = NRUN + 1
            RUN(NRUN) = IRUN(I)
            LAST_RUN  = RUN(NRUN)
          ENDIF
        ENDDO
      ENDIF
C
C ****  Check for EXP or ALL stream; but first skip over filter bits
C
      READ(IUNIT,101,ERR=900) NFILT,ASTRG
      DO I = 1,NFILT
        READ(IUNIT,102,ERR=900) IFILT(I),CFILT,TRIG_FILT(I)
  102   FORMAT(I6,1X,A64,' ',I2)
      ENDDO
C
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
C
C ****  Look for record containing keyword ALL or EXPRESS
C
        READ(UNIT=IUNIT,FMT='(A)',ERR=900) RECORD
        IF ((INDEX(RECORD,'ALL') .GT. 0) .OR.
     &      (INDEX(RECORD,'EXPRESS') .GT. 0) ) THEN
          GOTO 800
        ENDIF
      ENDDO
      GOTO 900
C
C ****  Read first filter bit after the previous record
C
  800 CONTINUE
      READ(UNIT=IUNIT,FMT='(A)',ERR=900) RECORD
      JFILT = VALUE(RECORD,I,J,K)
C
C ****  Get associated trigger bit
C
      JTRIG = 0
      I     = 0
      DO WHILE ( I .LT. NFILT )
        I = I + 1
        IF ( IFILT(I) .EQ. JFILT ) THEN
          JTRIG = TRIG_FILT(I)
          I = NFILT
        ENDIF
      ENDDO
C
C ****  Get associated run
C
      JRUN = 0
      I    = 0
      DO WHILE ( I .LT. NTRIG )
        I = I + 1
        IF ( ITRIG(I) .EQ. JTRIG ) THEN
          JRUN = RUN_TRIG(I)
          I = NTRIG
        ENDIF
      ENDDO
C
C ****  Put global run first
C
      DO I =  1, NRUN
        IF ( JRUN .EQ. RUN(I) ) THEN
          J = RUN(1)
          RUN(1) =-JRUN       ! indicates a global run
          RUN(I) = J
          GOTO 900
        ENDIF
      ENDDO
C
  900 CONTINUE
      CLOSE(IUNIT)
C
  999 CONTINUE
      CALL RLUNIT(IUSER,IUNIT,IERR)
      RETURN
      END
