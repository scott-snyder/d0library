      SUBROUTINE READ_TRIG_FILT_RUN_FILE(RUNNUM,NTRIG,ITRIG,CTRIG,
     &  NFILT,IFILT,CFILT,TRIG_FILT,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C     Read appropriate TRIG_FILT_xxxx.INFO file and return trigger/filter
C     information for the specified run. If RUNNUM < 0 then read the
C     file TRIG_FILT_RUN.
C
C   Input:    RUNNUM        [I]   Run_number
C
C   Outputs:  NTRIG         [I]   Number of triggers
C             NFILT         [I]   Number of filters
C             ITRIG(*)      [I]   Trigger bit #'s
C             IFILT(*)      [I]   Filter bit #'s
C             CTRIG(*)      [C*]  Trigger Names
C             CFILT(*)      [C*]  Filter Names
C             TRIG_FILT(*)  [I]   Trigger bit associated with filter bit
C             STATUS        [I]     = 0 everything is OK
C                                   =-1 Bad status, couldn't open file
C                                   =-2 Read error
C-
C-   Created  26-MAY-1992   Pushpa C. Bhat, Harrison B. Prosper
C-     Based on Tami Kramer's READ_TRIG_FILT_RUN
C-   Updated  21-AUG-1992   Harrison B. Prosper   
C-      Fix book-keeping
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUNNUM,NTRIG,NFILT
      INTEGER ITRIG(*),IFILT(*)
      INTEGER TRIG_FILT(*)
      CHARACTER*(*) CTRIG(*),CFILT(*)
      INTEGER STATUS
C----------------------------------------------------------------------
      LOGICAL OK
      INTEGER RUN_TRIG(32), RUN
      INTEGER IUSER,IUNIT,IERR,I,J,K
      CHARACTER*(*) TRIG_FILT_DIR
      CHARACTER*80 MESSAGE, TRIG_FILT_FILE
      PARAMETER( TRIG_FILT_DIR = 'TRIG_FILT_DIR:' )
      PARAMETER( IUSER = 12)
C
      CHARACTER*4 ASTRG
      CHARACTER*7 CRUN
C----------------------------------------------------------------------
C
      NFILT = 0
      NTRIG = 0
      STATUS= 0
      RUN   = IABS(RUNNUM)
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
        READ(IUNIT,103,ERR=900) ITRIG(J+1),CTRIG(J+1),RUN_TRIG(J+1)
        IF ( RUN .EQ. RUN_TRIG(J+1) ) THEN
          J = J + 1
        ENDIF
      ENDDO
      NTRIG = J
C
C ****  Pick filters bits for specified trigger bits
C
      READ(IUNIT,101,ERR=900) NFILT,ASTRG
C
      K = 0
      DO I = 1, NFILT
        READ(IUNIT,102,ERR=900) IFILT(K+1),CFILT(K+1),TRIG_FILT(K+1)
        J = 0
        DO WHILE ( J .LT. NTRIG )
          J = J + 1
          IF (ITRIG(J) .EQ. TRIG_FILT(K+1)) THEN
            K = K + 1   
            J = NTRIG     ! Match so exit this loop
          ENDIF
        ENDDO
      ENDDO
      NFILT = K
C
      GO TO 999
C
C ****  ERROR HANDLING
C
  888 CONTINUE
      MESSAGE = TRIG_FILT_FILE
      CALL ERRMSG ('TRIG_FILT_FILE_NOT_FOUND','READ_TRIG_FILT_RUN_FILE',
     &  MESSAGE,'W')
      STATUS=-1
      GOTO 999
C
  900 CONTINUE
      MESSAGE = TRIG_FILT_FILE
      CALL ERRMSG ('TRIG_FILT_FILE_READ_ERROR',
     &  'READ_TRIG_FILT_RUN_FILE',
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
  102   FORMAT(I6,1X,A64,' ',I2)
  103   FORMAT(I4,1X,A64,1X,I10)
      END
