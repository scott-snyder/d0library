      FUNCTION VALUE (STRING,II,JJ,TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the REAL*4 representation of 1st number
C-                         (or Boolean) embedded in a character string.
C-                         Valid types are:
C-
C-                         INTEGER, REAL, REAL in E-Format, BOOLEAN
C-                         (i.e., TRUE,FALSE), LOGICAL (.TRUE., .FALSE.)
C-                         and Hollerith string of 4 characters maximum.
C-
C-   Inputs  : STRING   String containing number or boolean value or
C-                      Character string of 4 characters. Character string
C-                      is delimited by single or double quotes.
C-
C-   Outputs : VALUE    Value of first number or boolean encountered.
C-             II       Start of first number or boolean in string
C-             JJ       End of first number or boolean in string
C-             TYPE      1:     INTEGER
C-                       2:     REAL
C-                       3:     REAL in E-format
C-                       4:     BOOLEAN (Note: usually FALSE is represented
C-                                       by INTEGER 0 and TRUE by the
C-                                       complement, namely INTEGER -1 )
C-                              or LOGICAL
C-                       5:     Hollerith of maximum 4 characters
C-
C-                      -1:     Invalid type.
C-                       0:     No numbers or booleans present in string.
C-
C-   Created  19-FEB-1988   Harrison B. Prosper
C-   Modified 16-JUN-1988   Harrison B. Prosper
C-                          Can now return truth value of a boolean.
C-   Updated  18-SEP-1988   Rajendran Raja   Added Hollerith ability
C-   Modified 16-OCT-1988   Harrison B. Prosper Simplified some code.
C-   Modified 11-NOV-1988   Harrison B. Prosper Simplified some more code.
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL         TRUTH
      REAL            RTRUTH
      EQUIVALENCE (TRUTH,RTRUTH)
C
      REAL            X,VALUE,XHOLL
      INTEGER         II,JJ,KK,NN,TYPE,I,J,K,M,N,I1,I2,OFFSET
      INTEGER         HUGE,IMIN,QUOTES,BOOLE
      INTEGER         MAXLEN,ELOWER,EUPPER,PLUS,MINUS,NINE,PERIOD,ZERO
C
      CHARACTER*(*)   STRING
      LOGICAL         TVALUE(4)
      INTEGER         LCHARS(19)
      CHARACTER*5     CHARS(19)
      CHARACTER*16    NUMBER
      CHARACTER*15    DIGIT
C
      PARAMETER( PERIOD =  1 )
      PARAMETER( ZERO   =  2 )
      PARAMETER( NINE   = 11 )
      PARAMETER( PLUS   = 12)
      PARAMETER( MINUS  = 13 )
      PARAMETER( EUPPER = 14 )
      PARAMETER( QUOTES = 14 )
      PARAMETER( MAXLEN = 15 )
      PARAMETER( ELOWER = 15 )
      PARAMETER( BOOLE  = 16 )
      PARAMETER( NN     = 16 )
      PARAMETER( HUGE   = 99999 )
C
      DATA CHARS/
     &   '.','0','1','2','3','4','5','6','7','8','9','+','-',
     &   '''','"','TRUE','FALSE','true','false'/
      DATA DIGIT/'.0123456789+-Ee'/
      DATA LCHARS/15*1,4,5,4,5/
      DATA TVALUE/.TRUE.,.FALSE.,.TRUE.,.FALSE./
C----------------------------------------------------------------------
C ****  II      Start position of number or logical in string
C ****  JJ      End position of number or logical in string
C
C ****  SCAN FOR NUMBERS, BOOLEANS or HOLLERITHS
C
C ****  Determine start position of FIRST embedded number, boolean etc.
C
      N = LEN (STRING)
      M = N     ! M is length of string to be scanned
      II = HUGE
      DO 10 J = ZERO,BOOLE+3
        I = INDEX (STRING(1:M),CHARS(J)(1:LCHARS(J)))
        IF ( I.GT.0 ) THEN
          IF ( I.LT.II ) THEN
            II = I
            KK = J
          ENDIF
          M = I-1       ! Shorten length of string to be scanned
        ENDIF
   10 CONTINUE
C
      IF ( II .GE. HUGE ) THEN
C
C ****  NO DIGIT, BOOLEAN OR HOLLERITH
C
        II = 1
        JJ = 0
        TYPE = 0
        VALUE  = 0.0
C
      ELSEIF ( KK. LE. MINUS ) THEN
C
C ****  NUMBER
C
C ****  Determine end of number
C ****  Check each character for VALID DIGIT symbol.
C
C ****  Define initial membership of set DIGIT
C
        IF ( STRING(II-1:II-1) .EQ. '.' ) THEN
          II = II - 1
          I1 = ZERO
          I2 = NINE
          TYPE = 2
        ELSE
          I1 = PERIOD
          I2 = NINE
          TYPE = 1
        ENDIF
C
        OFFSET = II - 1
C
        DO 50 K = 2,NN
          KK = OFFSET + K
          I = INDEX (DIGIT(I1:I2),STRING(KK:KK)) ! Get next character
          IF ( I .EQ. 0 ) GOTO 60 ! Character not in set DIGIT
C
C ****  Restrict set of digits according to whether a
C       `.',`E',`e' `+' or `-' has been found.
C
          IF     ( STRING(KK:KK) .EQ. '.' ) THEN
            I1 = ZERO
            I2 = ELOWER
            TYPE = 2
          ELSEIF ( STRING(KK:KK) .EQ. '-' ) THEN
            I2 = NINE
          ELSEIF ( STRING(KK:KK) .EQ. '+' ) THEN
            I2 = NINE
          ELSEIF ( STRING(KK:KK) .EQ. 'E' ) THEN
            I2 = MINUS
            TYPE = 3
          ELSEIF ( STRING(KK:KK) .EQ. 'e' ) THEN
            I2 = MINUS
            TYPE = 3
          ENDIF
   50   CONTINUE
   60   CONTINUE
C
C ****  Compute end position of number
C
        JJ = OFFSET + K - 1
C
C ****  Read value into variable
C
        READ (UNIT=STRING(II:JJ),FMT=*,ERR=1000) X
        VALUE = X
C
      ELSEIF ( KK .GE. BOOLE ) THEN
C
C ****  BOOLEAN or LOGICAL
C
        JJ = II + LCHARS(KK) - 1
C
C ****  Check if this is a logical
C
        IF ( II .GT. 0 ) THEN
          IF ( STRING(II-1:II-1) .EQ. '.' ) II = II - 1
          IF ( STRING(JJ+1:JJ+1) .EQ. '.' ) JJ = JJ + 1
        ENDIF
        TYPE = 4
        TRUTH = TVALUE(KK-BOOLE+1)
        VALUE = RTRUTH
C
      ELSE
C
C ****  HOLLERITH
C
        I = II
        IF ( I.GT.0 ) THEN
          II = I+1
          J = INDEX(STRING(II:N),CHARS(QUOTES)(1:1))+I
          IF ( J.EQ.I ) THEN
            J = INDEX(STRING(II:N),CHARS(QUOTES+1)(1:1))+I
          ENDIF
          IF ( J.EQ.I ) THEN  !Only left delimiter. Allow it!
            JJ = II + 3
          ELSE
            JJ = MIN(J-1,II+3) !MAXIMUM 4 CHARACTERS
          ENDIF
          CALL UCTOH(STRING(II:II),XHOLL,4,JJ-II+1) !HOLLERITH
          II = I
          JJ = J   !Include quotes in definition of character field.
        ENDIF
        TYPE = 5
        VALUE = XHOLL
      ENDIF
C
  999 RETURN
C
 1000 CONTINUE          ! **** ERROR HANDLING
      TYPE =-1
      VALUE  = 0.0
      RETURN
      END
