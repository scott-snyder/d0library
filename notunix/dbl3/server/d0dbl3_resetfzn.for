      SUBROUTINE D0DBL3_RESETFZN( DIR, IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Resets FZ file number to previous one in case
C-                         update did not succeeed.
C-
C-   Inputs  :  DIR       Directory in which file is located.
C-                        This is normally the logical DB$TODO
C-
C-   Outputs :
C-
C-   Controls:  IOK    If .FALSE. then trouble
C-
C-   Created  15-JUN-1992   SHAHRIAR ABACHI
C-   Updated  18-JUN-1992   Stuart Fuess  
C-                          From D0DB_RESETFZN.  Use argument DIR for 
C-                          directory.  Use SEQ for filename prefix.
C-                          Relocate CLOSE and RLUNIT.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DIR
      INTEGER LFU, NUM
      CHARACTER*6 CNUM
      CHARACTER*80 FILEN
      INTEGER LIB$FIND_FILE_END,CONT,IER,NER
      LOGICAL IOK, STATUS, LIB$FIND_FILE
C
      IOK = .TRUE.
      CONT = 0
      NER = -1
      FILEN = DIR//':FZFILE_NUMBER.DAT'
C
   10 CONTINUE
      STATUS = LIB$FIND_FILE(FILEN,FILEN,CONT)
      IF(STATUS) THEN
        CALL LIB$FIND_FILE_END(CONT)
        CALL GTUNIT(170,LFU,IER)
        OPEN (UNIT=LFU,FILE=FILEN,ACCESS='SEQUENTIAL'
     &           ,FORM='FORMATTED',STATUS='OLD',ERR=100)
        READ(LFU,*) NUM
        WRITE (CNUM,'(I6.6)') NUM
        NUM = NUM - 1
        IF(NUM .LT. 0) NUM = 0
        REWIND(UNIT=LFU)
        WRITE (CNUM,'(I6.6)') NUM
        WRITE(LFU,11) CNUM
   11   FORMAT(A)
        CLOSE(UNIT=LFU)
        CALL RLUNIT(170,LFU,IER)
      ELSE
        GOTO 100
      ENDIF
C----------------------------------------------------------------------
      GOTO 999
C
  100 CONTINUE
      NER = NER + 1
      IF(NER .LT. 3) THEN
        CALL LIB$WAIT(10.0)
        GOTO 10
      ENDIF
      IOK = .FALSE.
      CALL INTMSG(
     &  ' D0DBL3_RESETFZN: ERROR IN READING FZFILE_NUMBER FILE')
C
  999 RETURN
      END
