      SUBROUTINE GET_RUN_FILTERS
     &  (RUNNUM,STREAM,MAXFILTER,NFILTER,FILTER,FILTBIT,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Read appropriate TRIG_FILT_xxxx.INFO file and return the filter
C-    information for the specified run and stream. If RUNNUM < 0 then
C-    read the file TRIG_FILT_RUN. The logical TRIG_FILT_DIR should point
C-    to the directory containing the files.
C-
C-  Input:    RUNNUM        [I]   Run_number
C-            STREAM        [C*]  Stream Name
C-            MAXFILTER     [I]   Maximum number of filters to return
C
C-  Outputs:  NFILTER       [I]   Number of filters
C-            FILTER(*)     [I]   Filters
C-            FILTBIT(*)    [I]   Filter bits
C-            STATUS        [I]     = 0 everything is OK
C-                                  =-1 Bad status, couldn't open file
C-                                  =-2 Read error
C-
C-   Created   2-Nov-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUNNUM
      CHARACTER*(*) STREAM
      INTEGER MAXFILTER, NFILTER
      CHARACTER*(*) FILTER(*)
      INTEGER FILTBIT(*)
      INTEGER STATUS
C----------------------------------------------------------------------
      LOGICAL OK, MATCHED
      INTEGER NFILT, NTRIG
      INTEGER ITRIG(128),IFILT(128),IMAP(128)
      INTEGER TRIG_FILT
      INTEGER RUN_TRIG, RUN
      INTEGER IUSER,IUNIT,IERR,I,J,K, NSTRM, II,JJ,KK, LL, FBIT, NBITS
C
      CHARACTER*(*) TRIG_FILT_DIR
      CHARACTER*80 MESSAGE, TRIG_FILT_FILE
      CHARACTER*64 CTRIG,CFILT(128), STRM
C
      PARAMETER( TRIG_FILT_DIR = 'TRIG_FILT_DIR:' )
      PARAMETER( IUSER = 12)
C
      CHARACTER*4 ASTRG
      CHARACTER*7 CRUN
C----------------------------------------------------------------------
C
      NFILT   = 0
      NTRIG   = 0
      NFILTER = 0
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
        READ(IUNIT,102,ERR=900) IFILT(J+1),CFILT(J+1),TRIG_FILT
        CALL LOCNUM(TRIG_FILT,ITRIG,NTRIG,MATCHED,II)
        IF ( MATCHED ) THEN
          J = J + 1
          IMAP(J) = J
        ENDIF
      ENDDO
      NFILT = J
C
C ****  Sort filter bits
C
      CALL SRTINT(IFILT,NFILT,IMAP)
C
C ****  Pick filter bits for specified stream
C
      READ(IUNIT,101,ERR=900) NSTRM,ASTRG
C
      CALL WORD(STREAM(1:LEN(STREAM)),II,JJ,LL)
C
      DO I = 1, NSTRM
        READ(IUNIT,104,ERR=900) STRM, NBITS
        CALL WORD(STRM,II,JJ,KK)
        IF ( INDEX(STRM(II:JJ),STREAM(1:LL)) .GT. 0 ) THEN
C
          DO J =  1, NBITS
            READ(IUNIT,105,ERR=900) FBIT
            CALL LOCNUM(FBIT,IFILT,NFILT,MATCHED,II)
            IF ( MATCHED ) THEN
              NFILTER = NFILTER + 1
              FILTER(NFILTER) = CFILT(IMAP(II))
              FILTBIT(NFILTER)= IFILT(II)
            ENDIF
          ENDDO
C
C ****  Bit counts should match
C
          IF ( NFILTER .NE. NBITS ) THEN
            STATUS=-3
            GOTO 999
          ENDIF
          GOTO 999
        ELSE
          DO J =  1, NBITS
            READ(IUNIT,105,ERR=900) FBIT
          ENDDO
        ENDIF
      ENDDO
      GOTO 999
C
C ****  ERROR HANDLING
C
  888 CONTINUE
C
C ****  UNABLE TO OPEN FILE
C
      STATUS=-1
      GOTO 999
C
  900 CONTINUE
C
C ****  READ ERROR
C
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
