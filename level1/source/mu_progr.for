      SUBROUTINE MU_PROGR(LUN,PROGR_STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :  Unit numbert of input file
C-   Outputs :  PROGR_STATUS Error Status. 0 if no error.
C-   Controls:  None
C-
C-   Created  30-NOV-1991   Kamel A. Bazizi
C-   Updated   5-NOV-1992   Guilherme Lima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INTEGER LUN, PROGR_STATUS
      INTEGER MARRAY(8)
C<<
      INTEGER STRLEN,IOFF,I,J,N,OUTBIT,MUMIN,IDUMMY
      LOGICAL CCTFLG,OTCFLG
      DATA CCTFLG/.FALSE./,  OTCFLG/.TRUE./
C<<
      INTEGER COM_DATUM, COM_DATA(8)
      INTEGER L1_DATUM
      CHARACTER*10 COMMAND, COMMANDS(8)
      CHARACTER*8  DEVICE, DEVICES(8)
      CHARACTER*6  ATTR, ATTRS(8)
C<<
      CHARACTER*72 MSTRING
      CHARACTER*32 STRING
      CHARACTER*1 CHAR
C<<
      DATA COMMANDS / 'SET_MUCT', 'SET_MUPT', 'SET_MURS', 5*' ' /
      DATA ATTRS    /     'MUCT',     'MUPT',     'MURS', 5*' ' /
C<<
      DATA DEVICES / 'MU_TCC', 7*' ' /
C<<
C----------------------------------------------------------------------
      PROGR_STATUS = -1
C<<
C.. Skips the first two lines
      READ(LUN,FMT=100,END=90) MSTRING
      READ(LUN,FMT=100,END=90) MSTRING
  100 FORMAT(A)
C<<
C.. Reads the programming info and interprets it
    5 READ(LUN,FMT=100,END=90) MSTRING
C<<
C.. Decompose command line in the expected parameters
      STRLEN = LEN(MSTRING)
      IOFF=0
      CALL WORD(MSTRING(IOFF+1:STRLEN), I, J, N)
      COMMAND=MSTRING(IOFF+I:IOFF+J)
C<<
      IOFF=IOFF+J
      CALL WORD(MSTRING(IOFF+1:STRLEN), I, J, N)
      DEVICE=MSTRING(IOFF+I:IOFF+J)
      IF (DEVICE .NE. 'MU_TCC') THEN
        PROGR_STATUS=PARSE_BAD_PARAM
        RETURN
      ENDIF
C<<
      IOFF=IOFF+J
      CALL WORD(MSTRING(IOFF+1:STRLEN), I, J, N)
      ATTR=MSTRING(IOFF+I:IOFF+J)
C<<
C<<
      IF (COMMAND(1:8) .EQ. 'SET_MUCT') THEN
C<<
C.. Here for Level 1.0 commands
        IOFF=IOFF+J
        CALL WORD(MSTRING(IOFF+1:STRLEN), I, J, N)
        READ(MSTRING(IOFF+I:IOFF+J),102,ERR=91) OUTBIT
C<<
        IOFF=IOFF+J
        CALL WORD(MSTRING(IOFF+1:STRLEN), I, J, N)
        READ(MSTRING(IOFF+I:IOFF+J),102,ERR=91) MUMIN
C<<
        IOFF=IOFF+J
        CALL WORD(MSTRING(IOFF+1:STRLEN), I, J, N)
        STRING=MSTRING(IOFF+I:IOFF+J)
C<<
C.. Interprets this line in terms of trigger regions requested
        CALL MU_COOR_PARSE( STRING, L1_DATUM )
C<<
C.. Sets this bit in the TRGMON table, and go get the next bit
        CALL MU_TRIG_MON_INIT_L1( OUTBIT, MUMIN, L1_DATUM )
        GOTO 5
C<<
      ELSEIF (COMMAND(1:8) .EQ. 'SET_MUPT') THEN
C<<
C.. Here for Level 1.5 commands
        IOFF=IOFF+J
        CALL WORD(MSTRING(IOFF+1:STRLEN), I, J, N)
        READ(MSTRING(IOFF+I:IOFF+J),102,ERR=91) OUTBIT      !read in OUTBIT
        IF (OUTBIT.LT.0 .OR. OUTBIT.GE.15) THEN
          PROGR_STATUS=PARSE_BAD_RANGE
          RETURN
        ENDIF
C<<
Czzz        IOFF=IOFF+J
Czzz        CALL WORD(MSTRING(IOFF+1:STRLEN), I, J, N)
Czzz        READ(MSTRING(IOFF+I:IOFF+J),102,ERR=91) IDUMMY
C        IF (IDUMMY.LT.  .OR. IDUMMY.GE.  ) THEN
C          PROGR_STATUS=PARSE_BAD_RANGE
C          RETURN
C        ENDIF
C<<
        IOFF=IOFF+J
        CALL WORD(MSTRING(IOFF+1:STRLEN), I, J, N)
        READ(MSTRING(IOFF+I:IOFF+J),102,ERR=91) MUMIN       !read in MUMIN
C        IF (MUMIN.LT.1.OR.MUMIN.GT.2) THEN
        IF (MUMIN.NE.1) THEN
          PROGR_STATUS=PARSE_BAD_RANGE
          RETURN
        ENDIF
C<<
        IOFF=IOFF+J
        CALL WORD(MSTRING(IOFF+1:STRLEN), I, J, N)
        STRING=MSTRING(IOFF+I:IOFF+J)
C<<
C.. Interprets this line in terms of trigger regions requested
        CALL MU_COOR_PARSE( STRING, L1_DATUM )
C<<
C.. Sets this bit in the TRGMON table, and go get the next bit
        CALL MU_TRIG_MON_INIT_L15( OUTBIT, MUMIN, L1_DATUM )
        GOTO 5
C<<
C<<
      ELSEIF (COMMAND(1:8) .EQ. 'SET_MURS') THEN
C<<
C.. Here for Parsing Reference Sets
C..  (This feature is not currently implemented in hardware - Dec. 3, 92, GL)
        GOTO 5
C<<
      ELSE
        PROGR_STATUS=PARSE_BAD_UNKNOWN
        RETURN
      ENDIF
C<<
C<<
   90 CONTINUE
C<<
C.. No problems found
      PROGR_STATUS=0
      CALL INTMSG(' Succesful MUON Configuration Request.')
      RETURN
C<<
C.. Here for errors
   91 CONTINUE
      PROGR_STATUS=-1
      CALL ERRMSG('MU-ERRDECPAR','MU_PROGR',' Error decoding integer '
     &    //'parameters from MUON PROGRAMMING FILE','F')
      RETURN
C<<
  102 FORMAT(I2)
      END
