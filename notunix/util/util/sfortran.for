      PROGRAM SFORTRAN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pre-compile .SFO routines and invoke the
C-   FORTRAN compiler.
C-
C-   Inputs  : Command line
C-   Outputs :
C-   Controls:
C-
C-   Created   7-OCT-1990   Harrison B. Prosper
C-   Updated   8-JUL-1992   Harrison B. Prosper  
C-    Increase size of command line buffer 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,J,K,L,II,JJ,KK,IER,III,JJJ
      INTEGER LENGTH, LFILE, LQUAL, LREC
C
      INTEGER LUN
      PARAMETER( LUN = 40 )
      INTEGER LUNINP
      PARAMETER( LUNINP = 50 )
      INTEGER LUNOUT
      PARAMETER( LUNOUT = 60 )
C
C ****  RTL routines
C
      INTEGER STATUS
      INTEGER LIB$GET_FOREIGN
      INTEGER LIB$DO_COMMAND
C
      LOGICAL ACTIVE, SQL_MACRO, OUTPUT, LOG
C
      CHARACTER*80  RECORD,STRING,SQLCOMMAND
      CHARACTER*255 FILENAME, OUT_FILENAME, QUALIFIERS
      CHARACTER*511 COMMAND_LINE
C
C ****  Status codes for SFORTRAN; defined in
C ****  SFORTRAN_MESSAGES.MSG
C
      EXTERNAL SFO_SUCCESS, SFO_OPENFAILED, SFO_INVCLINE,
     &         SFO_INVMACRO, SFO_ERRPROC, SFO_NOTFOUND, SFO_NODECLAR
C----------------------------------------------------------------------
      CALL TRNLNM('SFO$LOG',STRING,LENGTH)
      LOG = STRING(1:LENGTH) .NE. 'SFO$LOG'
C
C ****  Get command line
C
      STATUS = LIB$GET_FOREIGN(COMMAND_LINE,,LENGTH)
      IF( .NOT. STATUS ) THEN
        CALL LIB$SIGNAL(SFO_INVCLINE)
      ENDIF
C
C ****  Extract file-name and qualifiers
C
      I = INDEX(COMMAND_LINE(1:LENGTH),'/')
      IF ( I .GT. 0 ) THEN
        QUALIFIERS = COMMAND_LINE(I:LENGTH)
        CALL WORD(QUALIFIERS,II,JJ,KK)
        FILENAME   = QUALIFIERS(JJ+1:)
        QUALIFIERS = QUALIFIERS(II:JJ)
      ELSE
        FILENAME   = COMMAND_LINE(1:LENGTH)
        QUALIFIERS = ' '
      ENDIF

      CALL WORD(FILENAME,II,JJ,LFILE)
      FILENAME = FILENAME(II:JJ)
C
C ****  Check for file extension; default is .SFO
C ****  Create name of file to be pre-compiled and compiled
C
      I = INDEX(FILENAME,']')           ! Test for directory spec.
      IF ( I .LE. 0 ) THEN
        I = INDEX(FILENAME,':')
      ENDIF
      J = INDEX(FILENAME(I+1:),'.')
      IF ( J .LE. 0 ) THEN
        FILENAME = FILENAME(1:LFILE)//'.SFO'
        LFILE = LFILE + 4
      ENDIF
      OUT_FILENAME = FILENAME(1:LFILE)//'$'

      CALL WORD(QUALIFIERS,II,JJ,LQUAL)
      QUALIFIERS = QUALIFIERS(II:JJ)
C
      IF ( LOG ) THEN
        WRITE(6,'('' %SFORTRAN-I-InpFile, '',A)') FILENAME(1:LFILE)
        WRITE(6,'('' %SFORTRAN-I-IntFile, '',A)') 
     &    OUT_FILENAME(1:LFILE+1)
        WRITE(6,'('' %SFORTRAN-I-Qual, '',A)') QUALIFIERS(1:LQUAL)
      ENDIF
C
C ****  Open input and output files
C
      OPEN(UNIT=LUNINP,FILE=FILENAME,STATUS='OLD',READONLY,ERR=900)
      OPEN(UNIT=LUNOUT,FILE=OUT_FILENAME,STATUS='NEW',
     &  CARRIAGECONTROL='LIST',ERR=910)
C
C ****  Loop over input file
C
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        READ(UNIT=LUNINP,FMT='(A)',END=200) RECORD
        CALL SWORDS(RECORD,II,LREC,KK)
C
C ****  Check for a !SQL or !CONT macros
C
        STRING = RECORD
        CALL STR$UPCASE(STRING,STRING)
        I = INDEX(STRING,'!SQL')
        IF ( I .LE. 0 ) THEN
          I = INDEX(STRING,'!CONT')
        ENDIF

        IF ( I .GT. 0 ) THEN
          SQLCOMMAND = STRING(I:)
          CALL WORD(SQLCOMMAND,III,JJJ,L)
          OUTPUT = .FALSE.
          SQL_MACRO = .TRUE.
        ELSE
          OUTPUT = .TRUE.
          SQL_MACRO = .FALSE.
        ENDIF
C
        IF ( OUTPUT ) THEN
          WRITE(UNIT=LUNOUT,FMT='(A)') RECORD(1:LREC)
        ENDIF
C
        IF ( SQL_MACRO ) THEN
C
C ****  Expand !SQL or !CONT macros
C
          CALL SFO_PROCESS_COMMAND(SQLCOMMAND(III:JJJ),
     &                             STRING,LUN,LUNOUT,IER)
          IF     ( IER .EQ. -1 ) THEN
            GOTO 920
          ELSEIF ( IER .EQ. -2 ) THEN
            GOTO 930
          ELSEIF ( IER .EQ. -3 ) THEN
            GOTO 940
          ELSEIF ( IER .EQ. -4 ) THEN
            GOTO 950
          ENDIF
        ENDIF
      ENDDO
C
C ****  Close input and output files
C
  200 CONTINUE
      CLOSE(UNIT=LUNINP)
      CLOSE(UNIT=LUNOUT)
C
C ****  Execute SQL pre-compiler/compiler
C
      STATUS = LIB$DO_COMMAND('$ SFO '//OUT_FILENAME(1:LFILE+1)//' '//
     &  '"'//QUALIFIERS(1:LQUAL)//'"')
      GOTO 999
C
C ****  Error handling
C
  900 CONTINUE
      CALL LIB$SIGNAL(SFO_OPENFAILED, %VAL(1), FILENAME(1:LFILE))
      GOTO 999

  910 CONTINUE
      CALL LIB$SIGNAL(SFO_OPENFAILED, %VAL(1), OUT_FILENAME(1:LFILE+1))
      GOTO 999

  920 CONTINUE
      CALL LIB$SIGNAL(SFO_INVMACRO, %VAL(1), RECORD(1:LREC))
      GOTO 999

  930 CONTINUE
      CALL LIB$SIGNAL(SFO_ERRPROC, %VAL(1), RECORD(1:LREC))
      GOTO 999

  940 CONTINUE
      CALL LIB$SIGNAL(SFO_NOTFOUND, %VAL(1), RECORD(1:LREC))
      GOTO 999

  950 CONTINUE
      CALL LIB$SIGNAL(SFO_NODECLAR, %VAL(1), RECORD(1:LREC))
C
  999 CONTINUE
      END
