      PROGRAM EZFSIZ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scan an RCP text file and compute the number of
C-                         identifiers (including comments) and values
C-                         it contains, then update the \SIZE parameter.
C-                         Scans over multiple banks and checks for some types
C-                         of RCP errors. Errors are flagged int eh file and in
C-                         the interactive messages. RCPTEST should be run on
C-                         the file if there is any question about the nature
C-                         of the error. This verion replaces a previous
C-                         EZFSIZ but the code is almost complete re-done.
C-
C-   Created   7-MAY-1991   Chip Stewart
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX version.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*132 RCP_FILE
      INTEGER IER,LUN,LUNOUT,NIDS,NVAL,I,J,K,L,II,JJ,KK,LL,IP,IV
      INTEGER NLINE,OLD_NIDS,OLD_NVAL
      PARAMETER( LUN = 20 )
      PARAMETER( LUNOUT = 40 )
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INTEGER STATUS,ITYPE(MAXVAL),TOTAL,NERR,ARRAY_ID,LENGTH,LMODIFY
      REAL RVALUE(MAXVAL)
      CHARACTER*132 RECORD,STRING,NAME,RCP_BANK,BUFFER,MSG
      CHARACTER*132 START_LINE,STOP_LINE,SIZE_LINE
      CHARACTER DISPOSE*6
      INTEGER*2 BUFLEN
      INCLUDE 'D0$UTIL:CLIST.INC'
      LOGICAL START,OK
      INTEGER  FLAGS
C----------------------------------------------------------------------
C
C ****  Get command line if it exists
C
C&IF VAXVMS
      INTEGER  LIB$GET_FOREIGN
      FLAGS = 0
      STATUS = LIB$GET_FOREIGN(RCP_FILE,,LL,FLAGS)
      IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
C&ELSE
C&      INTEGER LENOCC
C&C--------------------------------------------------------------------
C&      PRINT '(A,$)',' RCP_FILE >'
C&      READ (5,'(A)') RCP_FILE
C&      LL=LENOCC(RCP_FILE)
C&ENDIF
      RCP_FILE = RCP_FILE(1:LL)
C
C ****  If no command line then get RCP_FILE interactively
C
      IF ( LL .EQ. 0) THEN
        WRITE(6,'(1X,/'' Enter name of RCP file : '',$)')
        READ (5,'(A)') RCP_FILE
        CALL SWORDS(RCP_FILE,II,JJ,LL)
        IF (RCP_FILE(II:II).EQ.' ') GOTO 999
      END IF
C
C ****  Open input/output RCP_FILE
C
      WRITE(6,60) RCP_FILE
  60  FORMAT('  RCP file:              ',A40,/)
      CALL D0OPEN (LUN,RCP_FILE(1:LL),'IF',OK)
      IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
      CALL D0OPEN (LUNOUT,RCP_FILE(1:LL),'OFL',OK)
      IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
      DISPOSE = 'DELETE'                ! Flag for new file disposal
C
C ****  Loop over RCP banks: Reset NIDS & NVAL counters
C
   70 NIDS = 1                          ! Identifier counter
      NLINE = 0                         ! Line counter
      NVAL = 1                          ! Value counter
      NERR = 0                          ! Error counter
      START_LINE = ' '
      SIZE_LINE  = ' '
      STOP_LINE  = ' '
C
C ****  Create array in CLIST of lines from file
C
      ARRAY_ID = LCREATE(STRING_TYPE,'ARRAY')
      IF ( ARRAY_ID .LT. 0 ) THEN
        CALL LERROR(ARRAY_ID)
        GOTO 950
      ENDIF
      STATUS = LRESET(ARRAY_ID)
      IF ( STATUS .LT. 0 ) THEN
        CALL LERROR(ARRAY_ID)
        GOTO 950                        !skip rest of file
      ENDIF
C
C **** Loop over input lines
C
   80 CONTINUE
      READ(LUN,FMT='(A)',END=90) RECORD   ! finish this RCP bank
      CALL SWORDS(RECORD,I,J,L)
      NLINE= NLINE + 1                  ! Bump line counter
C
C ****  CLIST - ADD TO LIST
C
      STATUS = LWRITE(ARRAY_ID,RECORD(1:CHRCRD))
      IF ( STATUS .LT. 0 ) THEN
        CALL LERROR(ARRAY_ID)
        GOTO 950                        !skip rest of file
      ENDIF
C
C ****  find \START - mark START_LINE
C
      CALL EZZDRC (RECORD,NAME,II,JJ,KK,LL)
      IF ( NAME(1:KK) .EQ. '\START') THEN
        LL = INDEX(RECORD,'\START ')
        CALL WORD(RECORD(LL+7:CHRCRD),I,J,L)
        RCP_BANK = RECORD(LL+6+I:LL+7+J)
        START_LINE = RECORD
      ELSE IF ( NAME(1:KK) .EQ. '\STOP') THEN
        STOP_LINE = RECORD
        GOTO 90                         ! finish this RCP bank
      ELSE IF ( NAME(1:KK) .EQ. '\SIZE') THEN
        SIZE_LINE = RECORD
        I = INDEX(RECORD,'\SIZE')
        STRING = RECORD(I+5:L)
        CALL WORD(STRING,II,JJ,KK)
        READ(STRING(II:JJ),'(I12)',ERR=80) OLD_NVAL
        STRING = STRING(JJ+1:L)
        CALL WORD(STRING,II,JJ,KK)
        READ(STRING(II:JJ),'(I12)',ERR=80) OLD_NIDS
      ELSE
        CALL EZZDEC (RECORD,RVALUE,ITYPE,TOTAL,IER)
        IF(IER.EQ.0)CALL EZERR_CHECK(STRING,RVALUE,ITYPE,TOTAL,IER)
        IF( IER.EQ.EZS_ENDOF_DATA) THEN
          STOP_LINE = '\STOP'
          GOTO 90                       ! FOUND \STOP LINE
        ELSE IF( IER.NE.0) THEN
          NERR = NERR + 1
          NIDS = NIDS + 1
          NVAL = NVAL + TOTAL
          DISPOSE = 'KEEP'
        ELSE IF( TOTAL .GT. 0 ) THEN
          NIDS = NIDS + 1
          NVAL = NVAL + TOTAL
        ENDIF
      ENDIF
      GOTO 80                           ! Loop back to read next line
C
   90 CONTINUE
C
C ****  Is this just end of file  ?
C
      IF (NLINE.EQ.0) GOTO 990          ! end of program
      IF (STOP_LINE.EQ. ' ') THEN
        NLINE = NLINE + 1
        RECORD = '\STOP'
        STATUS = LWRITE(ARRAY_ID,RECORD(1:CHRCRD))
        DISPOSE = 'KEEP'
      END IF
C
      IF (START_LINE.EQ.' ') THEN
C
C ****   no \START - write \START onto output file.
C
        CALL WORD (RCP_FILE,I,J,L)
        RCP_BANK = RCP_FILE(I:J-4)//'_RCP'
        WRITE(LUNOUT,'(''\START   '',A)') RCP_BANK(1:J-I+1)
        WRITE(LUNOUT,'(A6,2I10)') '\SIZE ',NVAL+10,NIDS+10
        DISPOSE = 'KEEP'
        WRITE(6,'(3X,'' ADD \START LINE FOR BANK '',A40)') RCP_BANK
      ELSE
        CALL SWORDS (START_LINE,I,J,L)
        WRITE(LUNOUT,'(A)')START_LINE(I:J)
        WRITE(LUNOUT,'(A6,2I10)') '\SIZE ',NVAL+10,NIDS+10
      END IF
      IF (SIZE_LINE.EQ.' ') THEN
        DISPOSE = 'KEEP'
      END IF

C
C ****  Type RCPSIZE summary for this bank
C
      WRITE(6,100) RCP_BANK,NVAL,NIDS,NERR
  100 FORMAT('  RCP bank:              ',A50,/,
     &       '  Number of values:      ',I10,/,
     &       '  Number of identifiers: ',I10,/,
     &       '  Number of errors       ',I10,/)
C
C ****  clist reset pointer
C
      STATUS = LRESET(ARRAY_ID)
      IF ( STATUS .LT. 0 ) THEN
        CALL LERROR(ARRAY_ID)
        GOTO 950                        ! end program
      ENDIF
C
C ****  Loop over output lines
C
      DO 110, I = 1, NLINE
C
C ****  clist - read record
C
        STATUS = LREAD(ARRAY_ID,RECORD,BUFLEN)
        LENGTH = BUFLEN
C
        IF(STATUS.LT.0) GOTO 110        ! next line
        IF ( record.EQ.START_LINE) GOTO 110
        IF ( record.EQ.SIZE_LINE) THEN
          IF ((NIDS+10).NE.OLD_NIDS .OR. (NVAL+10).NE.OLD_NVAL) THEN
            WRITE(6,'(''  OLD  '',A72)') SIZE_LINE
            WRITE(6,'(''  NEW  '',A6,2I10)') '\SIZE ',NVAL+10,NIDS+10
            DISPOSE = 'KEEP'
          END IF
          GOTO 110                      ! next line
        END IF
        STRING = RECORD
        CALL EZZDEC (STRING,RVALUE,ITYPE,TOTAL,IER)
        IF(IER.EQ.0) CALL EZERR_CHECK(STRING,RVALUE,ITYPE,TOTAL,IER)
        IF( IER.NE.0 .AND. IER.NE.EZS_ENDOF_DATA) THEN
C
C ****  Found error in RCP - write message to file
C
          STRING =  ' '
          CALL SWORDS(RECORD,II,JJ,KK)
          IF (JJ.LT.61) THEN
            WRITE(STRING,'(A,''!ERROR: USE RCPTEST'')') RECORD(1:61)
          ELSE
            WRITE(LUNOUT,
     &        '(''!'',60X, ''!ERROR: USE RCPTEST'')')
            STRING = RECORD
          END IF
          RECORD = STRING
          DISPOSE = 'KEEP'
        END IF
C
        CALL SWORDS(RECORD,II,JJ,KK)
        WRITE(LUNOUT,'(A)') RECORD(1:JJ)
  110 CONTINUE
C
C ****  clist delete array
C
        IF ( ARRAY_ID .GT. 0 ) THEN
          STATUS = LDELETE(ARRAY_ID)
        ENDIF
C
      GOTO 70                           ! Loop back for next RCP bank
C
  900 CONTINUE
      CALL WORD (RCP_FILE,I,J,L)
      WRITE(6,'(''  File: '',A,'' Not found''/)') RCP_FILE(I:J)
      GOTO 990
  950 CONTINUE
      WRITE(6,'(''  ERROR in CLIST ''/)')
  990 CONTINUE
      CLOSE(UNIT=LUN)
      CALL WORD (RCP_FILE,I,J,L)
      IF (DISPOSE.EQ.'DELETE') THEN
        WRITE(6,'(1X,A,'' unchanged by RCPSIZE''/)') RCP_FILE(I:J)
      ELSE
        WRITE(6,'(1X,A,'' modified by RCPSIZE ''/)') RCP_FILE(I:J)
      END IF
      CLOSE(UNIT=LUNOUT,STATUS=DISPOSE)
  999 CONTINUE
      END



