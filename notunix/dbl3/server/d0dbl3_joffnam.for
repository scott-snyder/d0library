      SUBROUTINE D0DBL3_JOFFNAM(FILNAM, IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return the proper name for the JOURNAL FZ file.
C-
C-   Inputs  :
C-
C-   Outputs :  FILNAM    FZ filename
C-
C-   Controls:  IOK       If .FALSE. then trouble
C-
C-   Created  22-JUN-1992   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILNAM
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INTEGER LFU, NUM
      CHARACTER*6 CNUM
      CHARACTER*80 FILEN
      INTEGER LIB$FIND_FILE_END,CONT,IER,NER
      LOGICAL IOK, STATUS, LIB$FIND_FILE
C
      IOK = .TRUE.
      CONT = 0
      NER = -1
      FILEN = 'JOFFSQNC'
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
        NUM = NUM + 1
        REWIND(UNIT=LFU)
        WRITE (CNUM,'(I6.6)') NUM
        FILNAM = 'SRVR_AREA'//':JOFF'//CNUM//'.DBFZCURR'
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
        CONT = 0
        GOTO 10
      ENDIF
      IOK = .FALSE.
      CALL INTMSG(
     &  ' D0DBL3_FILNAM: ERROR IN READING FZFILE_NUMBER FILE')
C
  999 RETURN
      END
