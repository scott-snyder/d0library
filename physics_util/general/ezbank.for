      SUBROUTINE EZBANK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simple routine to help debug RCP banks
C-   interactively.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   6-JUN-1990   Harrison B. Prosper
C-   Updated  28-JUN-1991   Harrison B. Prosper
C-      Add EZPRINT and EZSTYLE
C-   Updated   3-JUL-1991   Harrison B. Prosper
C-      Bug fix
C-   Updated   3-APR-1992   James T. Linnemann  input from screen if in .INP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER MXCOMM,ID
      PARAMETER( MXCOMM = 11 , ID = 888 )
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
C----------------------------------------------------------------------
      CHARACTER*(CHRCRD) STRING,COMMAND,QUALIFIER,RECORD,FILENAME
      CHARACTER*72 HELP(MXCOMM)
C
      INTEGER I,J,L,II,JJ,LL,LUNOUT,IER,OPTION,STATUS,IUNIT
C
      LOGICAL ACTIVE,DISK,PERFORM_ACTION,RESET,EZERROR,FIRST
C
      DATA HELP/
     &' HELP                           Display this page',
     &' DIRE         [out-file-name]   Display list of RCP banks',
     &' STYLE        [n]               Number of values/line',
     &' DUMP[/NAMES] [bank-name]       Dump specified bank',
     &' PRINT        parameter-name    Print specified parameter',
     &' PICK         bank-name         Call EZPICK with specified bank',
     &' RESET                          Call EZRSET',
     &' SHOW                           Give name of current RCP bank',
     &' DISK                           Output to disk',
     &' SCREEN                         Output to screen (Default)',
     &' EXIT                           Exit EZBANK'
     &  /
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MENDEF
      ENDIF
C
      LUNOUT = 6                        ! Default to screen
      DISK   = .FALSE.
C
      CALL GTUNIT(ID,IUNIT,IER)
      IF ( IER .NE. 0 ) THEN
        CALL INTMSG(
     &        '  EZBANK: *** Unable to obtain unit number ***')
        GOTO 999
      ENDIF
C
      CALL OUTMSG('1')
C
      ACTIVE = .TRUE.
      DISK   = .FALSE.
      DO WHILE ( ACTIVE )
C
C ****  Prompt
C
        STRING = ' '
        IF (TRMFLG) THEN  !reading commands from screen
          CALL GETPAR(1,'EZBANK> ','U',STRING)
        ELSE              !reading commands from file
          CALL INPTRM
          CALL GETPAR(1,'EZBANK> ','U',STRING)
          CALL INPCMD
        ENDIF
C
C ****  Get command
C
        CALL WORD(STRING,I,J,L)
        COMMAND = STRING(I:J)
C
C ****  Check for qualifier
C
        I = INDEX(COMMAND,'/')
        IF ( I .GT. 0 ) THEN
          QUALIFIER = COMMAND(I:)
          COMMAND   = COMMAND(1:I-1)
          L = I - 1
        ELSE
          QUALIFIER = ' '
        ENDIF
C
C ****  Get value
C
        STRING = STRING(J+1:)          ! Get value
        CALL WORD(STRING,II,JJ,LL)
        STRING = STRING(II:JJ)
C
C ****  A C T I O N S
C

COMMAND EXIT

        IF     ( COMMAND(1:1) .EQ. 'E' ) THEN
          ACTIVE = .FALSE.

COMMAND HELP

        ELSEIF ( COMMAND(1:1) .EQ. 'H'  ) THEN
          DO I = 1, MXCOMM
            CALL INTMSG(HELP(I))
          ENDDO
          CALL INTMSG('  ')

COMMAND DIRE

        ELSEIF ( COMMAND(1:3) .EQ. 'DIR' ) THEN
          IF ( DISK ) THEN
            IF ( LL .LE. 0 ) THEN
              STRING = 'DIRECTORY'
              II = 1
              JJ = 9
              LL = 9
            ENDIF
C
            FILENAME = 'EZBANK_'//STRING(1:LL)
            RECORD = ' EZBANK: Output to File '//FILENAME
            CALL INTMSG(RECORD)
C
            OPEN(UNIT=LUNOUT,FILE=FILENAME,
C&IF VAXVMS,ULTRIX,SIUNIX,ALFOSF
     &          STATUS='NEW',CARRIAGECONTROL='LIST')
C&ENDIF
C&IF IBMAIX
C&     &          STATUS='NEW')
C&ENDIF
          ENDIF
C
          CALL EZDBUG(LUNOUT)
C
          IF ( DISK ) THEN
            CLOSE(UNIT=LUNOUT)
          ENDIF

COMMAND EZSTYLE

        ELSEIF ( COMMAND(1:2) .EQ. 'ST'  ) THEN
          IF ( LL .LE. 0 ) THEN
            CALL EZSTYLE(0,' ')
          ELSE
            CALL EZSTYLE(1,STRING(1:LL))
          ENDIF

COMMAND DUMP

        ELSEIF ( COMMAND(1:2) .EQ. 'DU'  ) THEN
C
          RESET = .FALSE.
          PERFORM_ACTION  = .TRUE.
C
          IF ( LL .GT. 0 ) THEN
            CALL EZPICK(STRING(1:LL))
            IF ( EZERROR(IER) ) THEN
              RECORD = ' EZBANK: Non-existent bank '//STRING(1:LL)
              CALL INTMSG(RECORD)
C
              PERFORM_ACTION  = .FALSE.
            ELSE
              RESET = .TRUE.
            ENDIF
          ELSE
            CALL EZTELL(STRING,LL)
            PERFORM_ACTION = .TRUE.
          ENDIF
C
          IF ( PERFORM_ACTION ) THEN
C
            RECORD = ' EZBANK: Dumping bank: '//STRING(1:LL)
            CALL INTMSG(RECORD)
C
            IF ( DISK ) THEN
C
              FILENAME = 'EZBANK_'//STRING(1:LL)
              RECORD = ' EZBANK: Output to File '//FILENAME
              CALL INTMSG(RECORD)
C
              OPEN(UNIT=LUNOUT,FILE=FILENAME,
C&IF VAXVMS,ULTRIX,SIUNIX,ALFOSF
     &          STATUS='NEW',CARRIAGECONTROL='LIST')
C&ENDIF
C&IF IBMAIX
C&     &          STATUS='NEW')
C&ENDIF
            ENDIF
C
            IF ( QUALIFIER(1:2) .EQ. '/N' ) THEN
              OPTION = 2
            ELSE
              OPTION = 0
            ENDIF
C
            CALL EZDUMP(LUNOUT,0,OPTION)
C
            IF ( DISK ) THEN
              CLOSE(UNIT=LUNOUT)
            ENDIF
C
            IF ( RESET ) THEN
              CALL EZRSET
            ENDIF
          ENDIF

COMMAND PRINT

        ELSEIF ( COMMAND(1:2) .EQ. 'PR'  ) THEN
C
          IF ( LL .GT. 0 ) THEN
C
            RECORD = ' EZBANK: Printing parameter: '//STRING(1:LL)
            CALL INTMSG(RECORD)
C
            IF ( DISK ) THEN
C
              FILENAME = 'EZBANK_'//STRING(1:LL)
              RECORD = ' EZBANK: Output to File '//FILENAME
              CALL INTMSG(RECORD)
C
              OPEN(UNIT=LUNOUT,FILE=FILENAME,
C&IF VAXVMS,ULTRIX,SIUNIX,ALFOSF
     &          STATUS='NEW',CARRIAGECONTROL='LIST')
C&ENDIF
C&IF IBMAIX
C&     &          STATUS='NEW')
C&ENDIF
            ENDIF
C
            CALL EZPRINT(LUNOUT,STRING(1:LL),IER)
            IF ( EZERROR(IER) ) THEN
              RECORD = ' EZBANK: Non-existent parameter: '//
     &          STRING(1:LL)
              CALL INTMSG(RECORD)
            ENDIF
C
            IF ( DISK ) THEN
              CLOSE(UNIT=LUNOUT)
            ENDIF
          ELSE
            CALL INTMSG(
     &        ' EZBANK: *** You must give a parameter name!')
          ENDIF

COMMAND PICK

        ELSEIF ( COMMAND(1:2) .EQ. 'PI'  ) THEN
C
          IF ( LL .GT. 0 ) THEN
            CALL EZPICK(STRING(1:LL))
            IF ( EZERROR(IER) ) THEN
              RECORD = ' EZBANK: Non-existent bank: '//
     &          STRING(1:LL)
              CALL INTMSG(RECORD)
            ELSE
              CALL EZTELL (STRING,LL)
              RECORD = ' EZBANK: Current RCP-bank: '//STRING(1:LL)
              CALL INTMSG(RECORD)
            ENDIF
          ENDIF

COMMAND RESET

        ELSEIF ( COMMAND(1:1) .EQ. 'R'  ) THEN
          CALL EZRSET
          CALL EZTELL (STRING,LL)
          RECORD = ' EZBANK: Current RCP-bank: '//STRING(1:LL)
          CALL INTMSG(RECORD)

COMMAND SHOW

        ELSEIF ( COMMAND(1:2) .EQ. 'SH'  ) THEN
          CALL EZTELL (STRING,LL)
          RECORD = ' EZBANK: Current RCP-bank: '//STRING(1:LL)
          CALL INTMSG(RECORD)

COMMAND DISK

        ELSEIF ( COMMAND(1:3) .EQ. 'DIS'  ) THEN
          LUNOUT = IUNIT
          DISK   = .TRUE.
          CALL INTMSG(' EZBANK: Write to DISK')

COMMAND SCREEN

        ELSEIF ( COMMAND(1:2) .EQ. 'SC'   ) THEN
          LUNOUT = 6
          DISK   = .FALSE.
          CALL INTMSG(' EZBANK: Write to SCREEN')
C
        ELSE
          RECORD = ' EZBANK: *** Error *** Bad command: '//
     &    STRING(1:LL)
          CALL INTMSG(RECORD)
        ENDIF
      ENDDO
C
      CALL RLUNIT(ID,IUNIT,IER)
  999 RETURN
      END
