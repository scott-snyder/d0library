      LOGICAL FUNCTION EDIT_GET_COMMAND(COMMAND,SELECT,ACT)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Parse an editting command string into the
C-     'selection' and 'action' strings.  The selection string is used
C-     to find the entry of interest and the action string describes
C-     what to do on that entry
C-
C-   Inputs  : COMMAND  - the initial unparsed editting command
C-   Outputs : SELECT   - the selection command
C-             ACT      - the action command
C-   Controls:
C-
C-   Created  10-OCT-1996   John Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*) COMMAND,SELECT,ACT
C- KEY WORDS
      CHARACTER*(*) SELECT_KEY,ACTION_KEY
      PARAMETER (SELECT_KEY='MATCH=',ACTION_KEY=',ACTION=')
C- External functions
      INTEGER LENOCC,ICLOCU
C- Temporary local variables
      INTEGER I,J,K,IACT,ISEL,IERR,IKEY,FLEN
C- Long term local variables
      LOGICAL FILE
      INTEGER INFILE
      CHARACTER*256 OLD_COMMAND,LOCAL_COMMAND
      DATA OLD_COMMAND/' '/,INFILE/0/,FILE/.FALSE./
      SAVE OLD_COMMAND,INFILE,FILE
C-----------------------------------------------------------------------

      EDIT_GET_COMMAND = .TRUE.
      LOCAL_COMMAND=COMMAND(1:LENOCC(COMMAND))

C- Have I already done this command?

      IF( LOCAL_COMMAND.EQ.OLD_COMMAND .AND. .NOT.FILE ) THEN
        EDIT_GET_COMMAND = .FALSE.
        GOTO 999
      ENDIF

C- If this is a new command, check if it's a file, and open if necessary

      IF( LOCAL_COMMAND.NE.OLD_COMMAND ) THEN
        OLD_COMMAND=LOCAL_COMMAND
        IF( FILE .AND. INFILE.NE.0 ) CLOSE(INFILE)
        FLEN=LENOCC(LOCAL_COMMAND)
        INQUIRE(file=LOCAL_COMMAND(1:FLEN),EXIST=FILE,ERR=903)
 903    CONTINUE
        IF( FILE ) THEN
          IF( INFILE.EQ.0 ) CALL D0DAD_GTUNIT(INFILE,IKEY,IERR)
C&IF IBMAIX,LINUX
C&          OPEN(INFILE,FILE=LOCAL_COMMAND(1:FLEN),STATUS='OLD')
C&ELSE
          OPEN(INFILE,FILE=LOCAL_COMMAND(1:FLEN),STATUS='OLD',READONLY)
C&ENDIF
        ENDIF
      ENDIF

C- If I'm reading a file of commands, get the next entry.

      IF( FILE ) READ(INFILE,'(A)',END=901) LOCAL_COMMAND

C- A command needing parsing is in LOCAL_COMMAND.  Parse it...

C-   Does it begin with SELECT_KEY
      J=LENOCC(SELECT_KEY)
      K=LENOCC(LOCAL_COMMAND)
      ISEL=ICLOCU(SELECT_KEY,J,LOCAL_COMMAND,1,K)
      IF( ISEL.EQ.0 ) GOTO 902

C-   Does it have the string introducing the action field
      J=LENOCC(ACTION_KEY)
      IACT=ICLOCU(ACTION_KEY,J,LOCAL_COMMAND,1,K)
      IF( IACT.EQ.0 ) GOTO 902

C-   Extract the selection command allowing for the possibility of 
C-   parenthetical grouping ala VMS
      I=ISEL+LENOCC(SELECT_KEY)
      J=IACT-1
      IF( LOCAL_COMMAND(I:I).EQ.'(' ) THEN
        I=I+1
        IF( LOCAL_COMMAND(J:J).NE.')') goto 902
        J=J-1
      ENDIF
      SELECT = LOCAL_COMMAND(I:J)

C-   Extract the action command, allowing for the possibility of 
C-   parenthetical grouping ala VMS
      I=IACT+LENOCC(ACTION_KEY)
      J=LENOCC(LOCAL_COMMAND)
      IF( LOCAL_COMMAND(I:I).EQ.'(' ) THEN
        I=I+1
        IF( LOCAL_COMMAND(J:J).NE.')') goto 902
        J=J-1
      ENDIF
      ACT = LOCAL_COMMAND(I:J)

 999  CONTINUE
      RETURN

C- Read the last entry from a command file (or bailing out because of error)

 901  CONTINUE
      EDIT_GET_COMMAND=.FALSE.
      CLOSE(INFILE)
      FILE=.FALSE.
      OLD_COMMAND = ' '
      RETURN

C- Unparsable command.  Bail out

 902  CONTINUE
      I=LENOCC(LOCAL_COMMAND)
      IF( LDDBG.GT.0 ) WRITE(*,9001) LOCAL_COMMAND(1:I)
 9001 FORMAT(' Cannot parse string: ',A)
      EDIT_GET_COMMAND = .FALSE.
      IF(FILE) GOTO 901
      GOTO 999

      END
