      SUBROUTINE D0DBL3_FILNAM( TODO_AREA, FILNAM, IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a (logical) directory name for server todo
C-                         area return the proper name for the output FZ file
C-                         to be used by server to update the database. The
C-                         file name will include sequence number.
C-
C-   Inputs  :  TODO_AREA   Directory in which file is to be placed.
C-                          This is normally the server TODO area.
C-
C-   Outputs :  FILNAM      FZ filename (should be dimensioned at least 19
C-                          plus the length of TODO_AREA)
C-
C-   Controls:  IOK         If .FALSE. then trouble
C-
C-   Created  15-JUN-1992   SHAHRIAR ABACHI
C-   Updated  17-JUN-1992   Stuart Fuess
C-                           From D0DB_GTFILNAM.  Use argument TODO_AREA
C-                           for directory.  Use SEQ for filename prefix.
C-                           Relocate CLOSE and RLUNIT.
C-   Updated  24-JUN-1992   SHAHRIAR ABACHI
C-                           If the file TODO_AREA:FZFILE_NUMBER.DAT does
C-                           not exist then it is created by first user in
C-                           the server todo area. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TODO_AREA, FILNAM
      INTEGER LFU, NUM
      CHARACTER*6 CNUM
      CHARACTER*80 FILEN,TFILEN
      INTEGER LIB$FIND_FILE_END,CONT,IER,NER
      LOGICAL IOK, STATUS, LIB$FIND_FILE
C
      IOK = .TRUE.
      NER = -1
      TFILEN = TODO_AREA // ':FZFILE_NUMBER.DAT'
C
   10 CONTINUE
      CONT = 0
      STATUS = LIB$FIND_FILE(TFILEN,FILEN,CONT)
      IF(.NOT. STATUS) THEN
        OPEN (UNIT=LFU,FILE=TFILEN,ACCESS='SEQUENTIAL'
     &           ,FORM='FORMATTED',STATUS='NEW',ERR=100)
        NUM = 0
        WRITE (CNUM,'(I6.6)') NUM
        WRITE(LFU,11) CNUM
        CLOSE(UNIT=LFU)
        GOTO 10
      ELSE
        CALL LIB$FIND_FILE_END(CONT)
        CALL GTUNIT(170,LFU,IER)
        OPEN (UNIT=LFU,FILE=FILEN,ACCESS='SEQUENTIAL'
     &           ,FORM='FORMATTED',STATUS='OLD',ERR=100)
        READ(LFU,*) NUM
        IF(NUM .EQ. 0) NUM = 1
        WRITE (CNUM,'(I6.6)') NUM
        FILNAM = TODO_AREA//':SEQ'//CNUM//'.D0DBL3FZ'
        NUM = NUM + 1
        REWIND(UNIT=LFU)
        WRITE (CNUM,'(I6.6)') NUM
        WRITE(LFU,11) CNUM
        CLOSE(UNIT=LFU)
        CALL RLUNIT(170,LFU,IER)
      ENDIF
   11 FORMAT(A)
C----------------------------------------------------------------------
      GOTO 999
C
  100 CONTINUE
      NER = NER + 1
      IF(NER .LT. 5) THEN
        CALL LIB$WAIT(10.0)
        GOTO 10
      ENDIF
      IOK = .FALSE.
      CALL INTMSG(
     &  ' D0DBL3_FILNAM: ERROR IN READING FZFILE_NUMBER FILE')
C
  999 RETURN
      END
