      SUBROUTINE EZZWRT (PARAM,REMARK,NUMBER,TYPE,TOTAL,MAXBUF,
     &  NBUF,BUFFER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write a parameter in RCP format to the
C-   character buffer BUFFER(*).
C-
C-   Inputs  : PARAM            [C*]    Identifier (32-char. max)
C-             REMARK           [C*]    Remark (character)
C-             NUMBER(*)        [R]     Value(s) (Given as a REAL array)
C-             TYPE(*)          [I]     Value types (INTEGERS)
C-             TOTAL            [I]     Number of values
C-             MAXBUF           [I]     Maximum size of buffer
C-
C-   Outputs : NBUF             [I]     Number of strings in buffer;
C-                                      If negative this signifies that
C-                                      the buffer is too small.
C-             BUFFER(*)        [C*]    Character buffer
C-
C-   Controls: None
C-
C-   Created  13-JUN-1991   Harrison B. Prosper
C-      taken from ezzdmp
C-   Updated  15-JUL-1992   Harrison B. Prosper  
C-      Handle integer exactly 
C-   Updated  11-JAN-1995   sss
C-      Don't choke on VTDBL.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PARAM
      CHARACTER*(*) REMARK
      REAL          NUMBER(*)
      INTEGER       TYPE(*)
      INTEGER       TOTAL
      INTEGER       MAXBUF, NBUF
      CHARACTER*(*) BUFFER(*)
C----------------------------------------------------------------------
      CHARACTER*(*) LINE_FORMAT
      INTEGER STYLE_TYP,VAL_PER_LINE,STYLE_TYPE,VALUES_PER_LINE
      INTEGER II,JJ,NN,IPOS,JPOS,LPOS,LENVAL
      INTEGER I,J,K,L,M,N,LREMAR,LIDENT,LLL,NNN
      INTEGER IVAL
      REAL    RVAL
      EQUIVALENCE(IVAL,RVAL)
      REAL    RBIG,RSMALL,VALUE
      LOGICAL ARRAY,WRAP_AROUND
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      CHARACTER*6   STRG6
      CHARACTER*16  FORM(10)
      CHARACTER*40  IDENTF
      CHARACTER*(CHRCRD)  OUTLIN,COMENT,BLANK,RECORD,STRG,STRING
C----------------------------------------------------------------------
      LOGICAL FIRST,DUMP
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA VALUES_PER_LINE/5/           ! INIT
      DATA STYLE_TYPE/0/                ! can be overidden by EZSTYLE
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST   = .FALSE.
        FORM(1) = '(I12)'
        FORM(2) = '(F12.4)'
        FORM(3) = '(1PE12.4)'
        FORM(4) = '(A)'
        FORM(5) = '(A)'
        FORM(7) = '(Z)'
        LENVAL  = 12                    ! Number of chars/value
        RBIG    = 10000.
        RSMALL  = 0.1
        BLANK   =
     &  '                                                    '
      ENDIF
C
      NBUF = 0
C
C ***********************************
C ****  Get length of identifier
C ***********************************
C
      L = LEN(PARAM)
      CALL WORD (PARAM(1:L),I,J,LIDENT)
      IDENTF = PARAM(I:J)
C
C ***********************************
C ****  Get length of remark
C ***********************************
C
      L = LEN(REMARK)
      CALL SWORDS (REMARK(1:L),I,J,LREMAR)
      IF ( LREMAR .GT. 0 ) THEN
        IF (REMARK(I:I) .EQ. '!' ) THEN
          COMENT = REMARK(I:J)
        ELSE
          COMENT = '!'//REMARK(I:J)
          LREMAR = LREMAR + 1
        ENDIF
      ELSE
        COMENT = ' '
        LREMAR = 1
      ENDIF
C
C ***********************************
C ****  DETERMINE PARAMETER TYPE
C ***********************************
C
      ARRAY = .FALSE.
C
      IF ( (TOTAL .LE. 0) .OR. (TYPE(1) .EQ. 0) ) THEN
        NBUF = NBUF + 1
        BUFFER(NBUF) = COMENT(1:LREMAR) ! Write out remark only
        GOTO 999
      ENDIF
C
      IF ( TOTAL .EQ. 1 ) THEN
C
C ****  SIMPLE PARAMETER
C
        OUTLIN = IDENTF(1:LIDENT)
        M = LIDENT/JTAB
        IPOS = JTAB*(M+1) + 1               ! Start of value field
      ELSE
C
C ****  Check if this is an array parameter or a single long string
C
        IF ( TYPE(1) .GT. VTCHR )THEN ! Is this a string type
          LLL   = TYPE(1)-VTCHR       ! Get length of string
          NNN   = 1 + (LLL-1)/4       ! Get length in 32-bit words
          IF ( NNN .EQ. TOTAL ) THEN  ! Is this a single string
            IF ( STYLE_TYPE .LE. 0 ) THEN
              ARRAY = .FALSE.         ! Yes; this is a long string
            ELSE
              ARRAY = .TRUE.
            ENDIF
          ELSE
            ARRAY = .TRUE.            ! This is an array
          ENDIF
        ELSE
          ARRAY = .TRUE.              ! This is an array
        ENDIF
C
        IF ( ARRAY ) THEN
C
C ****  ARRAY PARAMETER; write array name
C
          OUTLIN = '\ARRAY '//IDENTF(1:LIDENT)
          JPOS = 7 + LIDENT
          M = JPOS/JTAB
          JPOS = JTAB*(M+1)
          WRITE(UNIT=STRG6,FMT='(I6)') TOTAL
          OUTLIN = OUTLIN(1:JPOS)//STRG6
C
          JPOS = JPOS + 6
          M = JPOS/JTAB
          JPOS = JTAB*(M+1)
          OUTLIN = OUTLIN(1:JPOS)//COMENT(1:LREMAR)
C
          IF ( NBUF .LT. MAXBUF ) THEN
            NBUF = NBUF + 1
            BUFFER(NBUF) = OUTLIN
          ELSE
            NBUF = -NBUF                ! Buffer too small
            GOTO 999
          ENDIF
C
          IPOS = JTAB                   ! Start of value field
          OUTLIN = ' '
        ELSE
C
C ****  SINGLE long string
C
          OUTLIN = IDENTF(1:LIDENT)
          M = LIDENT/JTAB
          IPOS = JTAB*(M+1) + 1         ! Start of value field
        ENDIF
      ENDIF
C
C ***********************************
C ****  WRITE OUT VALUES
C ***********************************
C
      JJ = 1                          ! Value/line counter
      II = 0                          ! Loop counter
C
  100 CONTINUE
      II = II + 1
      JPOS = IPOS + LENVAL - 1        ! End position of value field
      LPOS = LENVAL                     ! Field length
C
      IF     ( TYPE(II) .EQ. VTINT ) THEN
C
C ****  INTEGER TYPE
C
        RVAL = NUMBER(II)
        WRITE(UNIT=STRING,FMT=FORM(1)) IVAL

      ELSEIF ( TYPE(II) .EQ. VTHEX ) THEN
C
C ****  HEX TYPE
C
        RVAL = NUMBER(II)
        WRITE(UNIT=STRING,FMT=FORM(7)) IVAL
        CALL WORD(STRING,I,J,K)
        STRING = '$'//STRING(I:J)
C
      ELSEIF ( (TYPE(II) .EQ. VTREAL) .OR.
     &         (TYPE(II) .EQ. VTREFM) .OR.
     &         (TYPE(II) .EQ. VTDBL)) THEN
C
C ****  REAL TYPE
C
        IF ( (ABS(NUMBER(II)) .GE. RSMALL) .AND.
     &       (ABS(NUMBER(II)) .LE. RBIG) ) THEN
          J = 2
        ELSE
          J = 3
        ENDIF
        WRITE(UNIT=STRING,FMT=FORM(J)) NUMBER(II)
C
      ELSEIF ( TYPE(II) .EQ. VTLOG ) THEN
C
C ****  LOGICAL TYPE
C
        IF ( NUMBER(II) .NE. 0. ) THEN
          STRING = BLANK(1:LENVAL-5)//' TRUE'
        ELSE
          STRING = BLANK(1:LENVAL-5)//'FALSE'
        ENDIF
C
      ELSEIF ( TYPE(II) .GT. VTCHR ) THEN
C
C ****  CHARACTER TYPE
C
        LLL = TYPE(II)-VTCHR            ! Get string length
        NNN = 1 + (LLL-1)/4             ! Get string length in 32-bit words
        CALL UHTOC (NUMBER(II),4,STRG,LLL)      ! Convert to characters
C
        IF ( ARRAY ) THEN
          STRING = ''''//STRG(1:LLL)//''''
        ELSE
C
C ****  LONG STRING
C
          IF ( LLL .LE. (LENVAL-2) ) THEN
            STRING = BLANK(1:LENVAL-2-LLL)//''''//STRG(1:LLL)//''''
            LLL    = LENVAL - 2
          ELSE
            STRING = ''''//STRG(1:LLL)//''''
          ENDIF
        ENDIF
C
        LPOS = LLL + 2               ! Field length
        JPOS = IPOS + LLL + 1        ! final position of value
        M    = JPOS/JTAB
        JPOS = JTAB*(M+1)            ! End of field
C
        IF ( STYLE_TYPE .LE. 0 ) THEN
          JJ = VALUES_PER_LINE                   ! Default style
        ENDIF
        II = II + NNN - 1               ! Update loop counter
      ENDIF
C
C ****  Check length of record
C
      IF ( JPOS .LT. CHRCRD ) THEN
        OUTLIN = OUTLIN(1:IPOS-1)//STRING(1:LPOS)
        WRAP_AROUND = .FALSE.
      ELSE
        JJ  = VALUES_PER_LINE           ! Force dump
        WRAP_AROUND = .TRUE.
      ENDIF
C
C ***********************************
C ****  WRITE out RCP record
C ***********************************
C
C ****  Add remarrk
C
      IF ( .NOT. ARRAY ) THEN
        M = JPOS/JTAB
        JPOS = JTAB*(M+1)
        OUTLIN = OUTLIN(1:JPOS)//COMENT(1:LREMAR)       ! Add remark
      ENDIF
C
      IF ( TOTAL .EQ. 1 ) THEN
        IF ( NBUF .LT. MAXBUF ) THEN
          NBUF = NBUF + 1
          BUFFER(NBUF) = OUTLIN
        ELSE
          NBUF = -NBUF                ! Buffer too small
          GOTO 999
        ENDIF
      ELSE
C
        DUMP = (JJ .GE. VALUES_PER_LINE ) .OR.
     &           (II .GE. TOTAL  )              .OR.
     &             ((TYPE(II+1) .GT. VTCHR) .AND. (STYLE_TYPE .LE. 0))
C
        IF ( DUMP ) THEN
          IF ( NBUF .LT. MAXBUF ) THEN
            NBUF = NBUF + 1
            BUFFER(NBUF) = OUTLIN
          ELSE
            NBUF = -NBUF                ! Buffer too small
            GOTO 999
          ENDIF
          JJ = 1
          IPOS = JTAB
          OUTLIN = ' '
          IF ( WRAP_AROUND ) THEN
            OUTLIN = OUTLIN(1:IPOS-1)//STRING(1:LPOS)
            IPOS = IPOS + LPOS + 1      ! Start of next field
          ENDIF
        ELSE
          IPOS = JPOS + 2               ! Start of next field
          JJ = JJ + 1                   ! Increment value counter
        ENDIF
      ENDIF
C
C ***********************************
C ****  END-OF-LOOP
C ***********************************
      IF ( II .LT. TOTAL ) GOTO 100
C
C ***********************************
C ****  Append \END if this is an array
C ***********************************
      IF ( ARRAY ) THEN
        IF ( WRAP_AROUND ) THEN
          IF ( NBUF .LT. MAXBUF ) THEN
            NBUF = NBUF + 1
            BUFFER(NBUF) = OUTLIN
          ELSE
            NBUF = -NBUF                ! Buffer too small
            GOTO 999
          ENDIF
        ENDIF
C
        IF ( NBUF .LT. MAXBUF ) THEN
          NBUF = NBUF + 1
          BUFFER(NBUF) = '\END'
        ELSE
          NBUF = -NBUF                ! Buffer too small
          GOTO 999
        ENDIF
      ENDIF
      RETURN
C
C ****  Set the dump style
C
      ENTRY EZSTYLE(STYLE_TYP,LINE_FORMAT)
      STYLE_TYPE      = STYLE_TYP
      VALUES_PER_LINE = VALUE(LINE_FORMAT,I,J,K)
      IF ( VALUES_PER_LINE .LE. 0 ) THEN
        VALUES_PER_LINE = 5
      ENDIF
  999 RETURN
      END
