      INTEGER FUNCTION MUON_UNIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Manipulates access to the output unit for
C-                         muon monitoring.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-OCT-1991   s.repond
C-   DH 3/92 USE D0OPEN
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*24 DSTRING
      LOGICAL OK,OPENED,FIRST
      INTEGER DUMMY
      INTEGER OLD_RUN,IUNIT,IUSER
      INTEGER IER,IRUN,RUNNO
      CHARACTER*80 FILENAME,MESSAGE,PREFIX
      CHARACTER *(*) FILNAM
      SAVE FIRST,OPENED,OLD_RUN,IUNIT,FILENAME
      INTEGER MUON_UNIT_SET_FILE
      INTEGER MUON_UNIT_GET_FILE
      INTEGER MUON_UNIT_CLOSE
      INTEGER I,J,K
      DATA IUSER,IUNIT/111,0/
      DATA OPENED/.FALSE./,FIRST/.TRUE./
      DATA OLD_RUN/0/
      DATA FILENAME/'MUON_OUT_'/
C----------------------------------------------------------------------
      IF(FIRST) THEN
        CALL GTUNIT(IUSER,IUNIT,IER)
        IF(IER.NE.0)
     &    CALL INTMSG('GTUNIT','MUON_UNIT',
     &    'cannot open unit IUNIT')
        FIRST = .FALSE.
      ENDIF    !first
C
C  *** Check if new run
C
      IRUN = RUNNO()
      IF(IRUN.NE.OLD_RUN) THEN
C ***  rename file
        IF(OPENED) THEN
          CLOSE(IUNIT,ERR=1000)
          MESSAGE = ' MUON_UNIT, Closed file '//FILENAME
          CALL INTMSG(MESSAGE)
          OPENED = .FALSE.
        ENDIF
      ENDIF
C
C *** name the file with correct run number
C
      IF(.NOT.OPENED) THEN
        FILENAME = PREFIX
        CALL WORD(FILENAME,I,J,K)
        WRITE(FILENAME(J+1:J+6),'(I6.6)') IRUN
        CALL D0OPEN(IUNIT,FILENAME,'O',OK)
        IF(.NOT.OK) GO TO 1100
        MESSAGE = ' MUON_UNIT, Opened file '//FILENAME
        CALL INTMSG(MESSAGE)
        OPENED = .TRUE.
        OLD_RUN = IRUN
C
C **** Write header of file
C
        CALL SYS$ASCTIM(DUMMY,DSTRING,IQ(LHEAD+4),0)
        WRITE(IUNIT,100) IRUN
        WRITE(IUNIT,101) DSTRING
  100   FORMAT(1X,'=======','MUON CHAMBER and EFFICIENCY SUMMARY
     &    FOR RUN NUMBER ',I6.6,'  =======',//)
  101   FORMAT(1X, '       ','THIS DATA TAKEN ON ',A24,//)
      ENDIF
C
C *** Save argument
C
      MUON_UNIT = IUNIT
C
  998 RETURN
C_____________________________________________________________
      ENTRY MUON_UNIT_SET_FILE(FILNAM)
      PREFIX=FILNAM
      RETURN
      ENTRY MUON_UNIT_GET_FILE(FILNAM)
      FILNAM = FILENAME
  999 RETURN
      ENTRY MUON_UNIT_CLOSE
      IF(OPENED) THEN
        CLOSE(IUNIT,ERR=1000)
        MESSAGE = ' MUON_UNIT, Closed file '//FILENAME
        CALL INTMSG(MESSAGE)
        OPENED = .FALSE.
      ENDIF
      RETURN
C
C ****  CLOSE ERROR
C
 1000 CONTINUE
      MESSAGE = 'Cannot CLOSE file '//FILENAME
      CALL ERRMSG('BAD_CLOSE','MUON_UNIT_CLOSE',MESSAGE,'S')
      RETURN
C
C ****  OPEN ERROR
C
 1100 CONTINUE
      MESSAGE = 'Cannot OPEN file '//FILENAME
      CALL ERRMSG('BAD_OPEN','MUON_UNIT',MESSAGE,'S')
      RETURN
      END
