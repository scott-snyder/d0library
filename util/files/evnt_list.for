      PROGRAM EVNT_LIST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read and printout contents of event files
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-OCT-1988   A.M.Jonckhere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INLUN,OUTLUN,IEVENT
C
C--     ZEBRA VARIABLES
      INCLUDE 'D0$INC:ZEBCOM.INC'
      COMMON/QUEST/IQUEST(20)
      INTEGER IQUEST
      CHARACTER*132 FILENAME,INFILE
      INTEGER TRULEN,STATUS,LEN,LLEN,I
      CHARACTER*132 evnt_list_log
c
      DATA FILENAME/' '/,INFILE/' '/
      DATA evnt_list_log/'EVNT_LIST'/
C----------------------------------------------------------------------
C--     INITIALIZE
C
C--     OPEN OUTPUT FILE
      OUTLUN=83
      CALL d0open(outlun,evnt_list_log,'FO',status)
C
      CALL MZEBRA(0)
      CALL INZCOM(0)
C
   10 CALL GET_STR(' Input data file:',INFILE)
      IF ( INFILE.EQ.'END' .OR. INFILE.EQ.'QUIT' ) GOTO 999
C
      FILENAME = INFILE
      LEN = TRULEN(FILENAME)
      DO I =  1, LEN
        IF ( FILENAME(I:I).EQ.':' ) THEN
          FILENAME = FILENAME(I+1:LEN)
          GOTO 15
        ENDIF
      ENDDO
C
   15 LEN = TRULEN(FILENAME)
      DO I =  1, LEN
        IF ( FILENAME(I:I).EQ.']' ) THEN
          FILENAME = FILENAME(I+1:LEN)
          GOTO 16
        ENDIF
      ENDDO
   16 CONTINUE
      LEN = TRULEN(FILENAME)
C
      WRITE(6,*) INFILE(1:TRULEN(INFILE))
      WRITE(OUTLUN,*) '[]'//FILENAME(1:LEN)
C
      INLUN=82
      CALL evopin(infile,'X',inlun,status)
C
C--     LOOP OVER EVENTS
      IEVENT = 0
  100 CALL EVTIN(INLUN,STATUS)
      IF ( STATUS.GT.3 ) THEN           ! 4 = System End-of-data
C                                       ! 5 = System End-of-data
C                                       ! 6 = attempt to read past end-of-data
        WRITE(OUTLUN,*) ' System End of file',IEVENT,status
        WRITE(6,*) ' System End of file',IEVENT,status
        GOTO 600
      ELSEIF ( STATUS.EQ.3 ) THEN       ! 3 = Zebra End-of-File
        WRITE(OUTLUN,*) ' ZEBRA  END of FILE',IEVENT
        WRITE(6,*) ' ZEBRA  END of FILE',IEVENT
        GOTO 500
      ELSEIF ( STATUS.EQ.2 ) THEN       ! 2 = Zebra end-run-record
        IF ( LHEAD.EQ.0 ) THEN
          WRITE(OUTLUN,*) ' ZEBRA  END of RUN',IEVENT
          WRITE(6,*) ' ZEBRA  END of RUN',IEVENT
          GOTO 500
        ELSE
          WRITE(OUTLUN,*) ' D0     END of RUN',IEVENT
          WRITE(6,*) ' D0     END of RUN',IEVENT
          GOTO 500
        ENDIF
      ELSEIF ( STATUS.EQ.1 ) THEN
        IF ( LHEAD.EQ.0 ) THEN
          WRITE(OUTLUN,*) ' ZEBRA  BEG of RUN',IEVENT
          WRITE(6,*) ' ZEBRA  BEG of RUN',IEVENT
          GOTO 500
        ELSE
          WRITE(OUTLUN,*) ' D0     BEG of RUN',IEVENT
          WRITE(6,*) ' D0     BEG of RUN',IEVENT
          GOTO 500
        ENDIF
      ELSEIF ( STATUS.LT.0 ) THEN
        WRITE(OUTLUN,*) ' READ ERROR ',IQUEST(1),IEVENT
        WRITE(6,*) ' READ ERROR ',IQUEST(1),IEVENT
        IF ( IQUEST(1).GT.-10 ) GOTO 100   ! Retry
        GOTO 600
      ENDIF
C
      IEVENT = IEVENT + 1
C
      llen = len
      IF ( llen.GT.27 ) llen = 27
      WRITE(6,12) IEVENT,IQ(LHEAD+9),IQ(LHEAD+6),
     &  IQ(LHEAD+7),IQ(LHEAD+8),FILENAME(1:LLEN)
C
      WRITE(OUTLUN,12) IEVENT,IQ(LHEAD+9),IQ(LHEAD+6),
     &  IQ(LHEAD+7),IQ(LHEAD+8),FILENAME(1:LLEN)
C
   12 FORMAT(' EVENT: ',I10,' OUTPUT EVENT/RUN:',2I10,
     &  ' INPUT EVENT/RUN:',2I10,3X,A)
C
C--     CLEAN ZEBRA BANK
  500 CALL MZWIPE(IXMAIN)
C
C--     GO TO BEGINNING OF LOOP
      GO TO 100
C
C--     FINISHED
  600 CALL MZWIPE(IXMAIN)
      CALL FZENDI(INLUN,'TU')
      CLOSE (INLUN)
      GOTO 10
C
  999 CLOSE (OUTLUN)
      END
