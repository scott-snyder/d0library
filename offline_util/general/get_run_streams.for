      SUBROUTINE GET_RUN_STREAMS
     &  (RUNNUM,MAXSTREAM,NSTREAM,STREAM,RUNTYPE,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Read appropriate TRIG_FILT_xxxx.INFO file and return the trigger/filter
C-    information for the specified run. If RUNNUM < 0 then read the
C-    file TRIG_FILT_RUN.
C-
C-  Input:    RUNNUM        [I]   Run_number
C-            MAXSTREAM     [I]   Maximum number of streams to return
C
C-  Outputs:  NSTREAM       [I]   Number of streams
C-            STREAM(*)     [I]   Streams
C-            RUNTYPE       [I]   0 - UNSPECIFIED
C-                                1 - GLOBAL      run (ALL, EXPRESS)
C-                                2 - SPECIAL     run
C-                                3 - CALIBRATION run (xxxx_CALIB)
C-
C-            STATUS        [I]     = 0 everything is OK
C-                                  =-1 Bad status, couldn't open file
C-                                  =-2 Read error
C-
C-   Created  25-AUG-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUNNUM, MAXSTREAM, NSTREAM
      CHARACTER*(*) STREAM(*)
      INTEGER RUNTYPE
      INTEGER STATUS
C----------------------------------------------------------------------
      LOGICAL OK, MATCHED
      INTEGER NFILT, NTRIG
      INTEGER ITRIG(128),IFILT(128),IMAP(128)
      INTEGER TRIG_FILT
      INTEGER RUN_TRIG, RUN
      INTEGER IUSER,IUNIT,IERR,I,J,K, NSTRM, II,JJ,KK, FBIT, NBITS
C
      CHARACTER*(*) TRIG_FILT_DIR
      CHARACTER*80 MESSAGE, TRIG_FILT_FILE
      CHARACTER*64 CTRIG,CFILT, STRM
C
      PARAMETER( TRIG_FILT_DIR = 'TRIG_FILT_DIR:' )
      PARAMETER( IUSER = 12)
C
      CHARACTER*4 ASTRG
      CHARACTER*7 CRUN
C----------------------------------------------------------------------
C
      RUNTYPE = 0
      NFILT   = 0
      NTRIG   = 0
      NSTREAM = 0
      STATUS  = 0
      RUN     = IABS(RUNNUM)
C
      CALL GTUNIT(IUSER,IUNIT,IERR)
C
C ****  Create name of file created by COOR if RUN > 0
C ****  otherwise read TRIG_FILT_RUN.INFO
C
      IF ( RUNNUM .GT. 0 ) THEN
        WRITE(CRUN,'(I7.7)')RUN
        TRIG_FILT_FILE=TRIG_FILT_DIR//'TRIG_FILT_'//CRUN//'.INFO'
      ELSE
        TRIG_FILT_FILE = 'TRIG_FILT_RUN'
      ENDIF
      CALL D0OPEN(IUNIT,TRIG_FILT_FILE,'IF',OK)
      IF ( .NOT. OK ) GOTO 888
C
C ****  Pick triggers bits for specified run number
C
      READ(IUNIT,101,ERR=900) NTRIG,ASTRG
C
      J = 0
      DO I = 1, NTRIG
        READ(IUNIT,103,ERR=900) ITRIG(J+1),CTRIG,RUN_TRIG
        IF ( RUN .EQ. RUN_TRIG ) THEN
          J = J + 1
        ENDIF
      ENDDO
      NTRIG = J
C
C ****  Sort trigger bits
C
      CALL SRTINT(ITRIG,NTRIG,IMAP)
C
C ****  Pick filters bits for specified trigger bits
C
      READ(IUNIT,101,ERR=900) NFILT,ASTRG
C
      J = 0
      DO I = 1, NFILT
        READ(IUNIT,102,ERR=900) IFILT(J+1),CFILT,TRIG_FILT
        CALL LOCNUM(TRIG_FILT,ITRIG,NTRIG,MATCHED,II)
        IF ( MATCHED ) THEN
          J = J + 1
        ENDIF
      ENDDO
      NFILT = J
C
C ****  Sort filter bits
C
      CALL SRTINT(IFILT,NFILT,IMAP)
C
C ****  Pick streams bits for specified filter bits
C
      READ(IUNIT,101,ERR=900) NSTRM,ASTRG
C
      DO I = 1, NSTRM
        READ(IUNIT,104,ERR=900) STRM, NBITS
        MATCHED = .FALSE.
C
        DO J =  1, NBITS
          READ(IUNIT,105,ERR=900) FBIT
          IF ( .NOT. MATCHED ) THEN
            CALL LOCNUM(FBIT,IFILT,NFILT,MATCHED,II)
          ENDIF
        ENDDO
C
        IF ( MATCHED ) THEN
          IF ( NSTREAM .LT. MAXSTREAM ) THEN
            NSTREAM = NSTREAM + 1
            CALL WORD(STRM,II,JJ,KK)
            STREAM(NSTREAM) = STRM(II:JJ)
          ENDIF
C
C ****  Check run type
C
          IF ( RUNTYPE .LE. 0 ) THEN
            IF     ( STRM(II:JJ) .EQ. 'ALL' ) THEN
              RUNTYPE = 1
            ELSEIF ( INDEX(STRM(II:JJ),'EXPRESS') .GT. 0 ) THEN
              RUNTYPE = 1
            ELSEIF ( INDEX(STRM(II:JJ),'CALIB') .GT. 0 ) THEN
              RUNTYPE = 3
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C ****  If run type not GLOBAL or CALIBRATION then it must be
C ****  a SPECIAL run unless NSTREAM is zero in which case set
C ****  the run type to UNSPECIFIED.
C
      IF ( NSTREAM .LE. 0 ) THEN
        RUNTYPE = 0
      ELSE
        IF ( RUNTYPE .LE. 0 ) THEN
          RUNTYPE = 2
        ENDIF
      ENDIF
      GO TO 999
C
C ****  ERROR HANDLING
C
  888 CONTINUE
      MESSAGE = TRIG_FILT_FILE
      CALL ERRMSG ('TRIG_FILT_FILE_NOT_FOUND','GET_RUN_STREAMS',
     &  MESSAGE,'W')
      STATUS=-1
      GOTO 999
C
  900 CONTINUE
      MESSAGE = TRIG_FILT_FILE
      CALL ERRMSG ('TRIG_FILT_FILE_READ_ERROR','GET_RUN_STREAMS',
     &  MESSAGE,'W')
      STATUS=-2
C
  999 CONTINUE
      CLOSE(IUNIT)
      CALL RLUNIT(IUSER,IUNIT,IERR)
      RETURN
C
C ****  FORMATS
C
  101 FORMAT(I4,1X,A4)
  102 FORMAT(I6,1X,A64,' ',I2)
  103 FORMAT(I4,1X,A64,1X,I10)
  104 FORMAT(1X,A16,5X,I4)
  105 FORMAT(1X,27X,I4)
      END
