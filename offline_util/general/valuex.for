      FUNCTION VALUEX (STRING,II,JJ,ITYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the value of the 1st number
C-                         (or Boolean) embedded in a character string.
C-                         Unlike VALUE this function operates only on the
C-                         first sub-string within the string.
C-
C-                         Valid types are:
C-
C-                         INTEGER, REAL, REAL in E-Format, BOOLEAN
C-                         (i.e., TRUE,FALSE), LOGICAL (.TRUE., .FALSE.),
C-                         Hollerith string of 4 characters maximum,
C-                         DOUBLE PRECISION and HEXADECIMAL.
C-
C-   Inputs  : STRING   String containing number or boolean value or
C-                      Character string of 4 characters. Character string
C-                      is delimited by single or double quotes.
C-
C-   Outputs : II       Start of first number or boolean in string
C-             JJ       End of first number or boolean in string
C-             ITYPE     1:     INTEGER
C-                       2:     REAL
C-                       3:     REAL in E-format
C-                       4:     BOOLEAN (Note: usually FALSE is represented
C-                                       by INTEGER 0 and TRUE by the
C-                                       complement, namely INTEGER -1 )
C-                              or LOGICAL
C-                       5:     Hollerith of maximum 4 characters
C-                       6:     DOUBLE PRECISION
C-                       7:     HEXADECIMAL
C-
C-                      -1:     Invalid type.
C-                       0:     No numbers or booleans present in string.
C-
C-   Entry points: DVALUEX - Returns most recent double precision value in
C-                           double precision format.
C-                 IVALUEX - Returns most recent integer or hex value in
C-                           integer format.
C-                 LVALUEX - Returns most recent logical or boolean value
C-                           in logical format.
C-
C-   Created   1-DEC-1988   Harrison B. Prosper
C-   Modified  4-JAN-1991   Herbert Greenlee
C-     Added DOUBLE PRECISION and HEXADECIMAL types and alternate entry
C-     points.
C-   Modified 14-JAN-1992   Herbert Greenlee
C-     Removed machine block around hexadecimal.
C-     Changed formats from list-directed to typed so that error branches
C-       will be taken on UNIX
C-   Modified 11-May-1992   Herbert Greenlee
C-     VALUEX broke again in IRIX 4.0 (took error branch when it didn't
C-     used to).  Fixed.
C-   Modified 31-DEC-1992   Hyon-Joo Kehayias
C-     Corrected calculation of the end position of a word when '*' is found
C-   Modified 15-Dec-1993
C-     Allow values to end in comma.
C-   Modified 17-DEC-1993   sss
C-     Speed up integer conversions.
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TRUTH,LVALUEX
      REAL    RTRUTH
      EQUIVALENCE ( TRUTH, RTRUTH  )
      CHARACTER*(*)   STRING
      CHARACTER*12    ZFORM
      CHARACTER*3     CW
      CHARACTER       C
      REAL            X,VALUEX,XHOLL
      INTEGER         IX,IVALUEX
      DOUBLE PRECISION DX,DVALUEX
      SAVE IX,DX,TRUTH                  ! For alternate entry points.
      INTEGER         II,JJ,KK,LL,NN,ITYPE,I,J,K,M,N,W,IOS
C
C *** Smallest number which can be represented in an INTEGER.
C *** It is assumed that maxint = -minint-1 (ie, two's complement).
C
      INTEGER  MAXINT
      PARAMETER (MAXINT = 2147483647)
C----------------------------------------------------------------------
C ****  II      Start position of number or logical in string
C ****  JJ      End position of number or logical in string
C
      N = LEN (STRING)
      CALL WORD (STRING(1:N),II,JJ,N)
C
      IF ( N .LE. 0 ) THEN
        II = 1
        JJ = 0
        ITYPE = 0
        VALUEX  = 0.0
        GOTO 999
      ENDIF
C
C ****  CHECK FOR ASTERISK
C
      LL = INDEX(STRING(II:JJ),'*')
      IF ( LL .GT. 0 ) THEN
        JJ = II + LL - 2
C
C ****  CHECK IF THE RESULT STRING IS EMPTY
C
        IF (JJ .LT. II ) THEN
          II = 1
          JJ = 0
          ITYPE = 0
          VALUEX  = 0.0
          GOTO 999
        ENDIF
      END IF
C
C ****  Read value into single precision real variable.  Force error branch
C       for character, hexadecimal, logical and boolean.
C
      IF(STRING(II:II).EQ.'''' .OR. STRING(II:II).EQ.'"' .OR.
     &  STRING(II:II).EQ.'$' .OR.
     &  STRING(II:II).EQ.'T' .OR. STRING(II:II).EQ.'t' .OR.
     &  STRING(II:II).EQ.'F' .OR. STRING(II:II).EQ.'f' .OR.
     &  STRING(II:II+1).EQ.'.T' .OR. STRING(II:II+1).EQ.'.t' .OR.
     &  STRING(II:II+1).EQ.'.F' .OR. STRING(II:II+1).EQ.'.f')
     &  GO TO 100

C
      IF ( INDEX(STRING(II:JJ),'.') .GT. 0 ) THEN
C
C ****  CONSTRUCT CHARACTER FIELD WIDTH
C
        W = JJ - II + 1
        IF(STRING(JJ:JJ).EQ.',' .AND. W.GT.1)W = W - 1
        WRITE(CW,'(I3.3)')W
        READ (UNIT=STRING(II:JJ),FMT='(F'//CW//'.0)',
     &        ERR=100,IOSTAT=IOS)X
C
C ****  Floating point formats handled below.
C
C ****  Check for E-format
C
        IF ( (INDEX(STRING(II:JJ),'E') .GT. 0 ) .OR.
     &       (INDEX(STRING(II:JJ),'e') .GT. 0 ) ) THEN
          ITYPE = 3
C
C ****  Check for D-format (remember exact d. p. value)
C
        ELSEIF ( (INDEX(STRING(II:JJ),'D') .GT. 0 ) .OR.
     &           (INDEX(STRING(II:JJ),'d') .GT. 0 ) ) THEN
          READ (UNIT=STRING(II:JJ),FMT='(D'//CW//'.0)',ERR=100,
     &      IOSTAT=IOS) DX
          X=DX
          ITYPE = 6
        ELSE
C
C ****  Otherwise F-format
C
          ITYPE = 2
        ENDIF
      ELSE
C
C ****  INTEGER (remember exact 32-bit value).
C ****  Build the number as a negative integer so that we can correctly
C ****  represent -MAXINT-1.
C
        IX = 0
        J = -1
        DO I=II, JJ
          C = STRING(I:I)
          IF (C .EQ. '-') THEN  ! Flip sign if we see an initial `-'
            IF (IX .NE. 0) GOTO 1000
            J = -J
          ELSE IF (C .EQ. '+') THEN  ! Do nothing if we see an initial `+'
            IF (IX .NE. 0) GOTO 1000
          ELSE IF (C .GE. '0' .AND. C .LE. '9') THEN
            K = ICHAR (C) - ICHAR ('0')
            IF (IX .LT. (K-MAXINT-1)/10) GOTO 1000 ! overflow check
            IX = IX*10 - K
          ELSE IF (C.EQ.',') THEN
            CONTINUE           ! ignore commas
          ELSE
            GOTO 1000           ! any other chars are illegal
          ENDIF
        ENDDO
        IF (IX .EQ. -MAXINT-1 .AND. J .EQ. -1) GOTO 1000 ! overflow check
        IX = IX * J             ! set sign
        X = IX
        ITYPE = 1
      ENDIF
      VALUEX = X
      GOTO 999
C
C **** All types other than real and decimal integer are handled here.
C
  100 CONTINUE
C
C ****  CONSTRUCT CHARACTER FIELD WIDTH
C
      W = JJ - II + 1
      IF(STRING(JJ:JJ).EQ.',' .AND. W.GT.1)W = W - 1
      WRITE(CW,'(I3.3)')W
C
C ****  Hexadecimal handled here (remember exact 32-bit value)
C
      IF(STRING(II:II).EQ.'$')THEN
        W = JJ - II
        IF(STRING(JJ:JJ).EQ.',' .AND. W.GT.1)W = W - 1
        IF(W.GT.8)GO TO 1000
        WRITE(ZFORM,'(''(Z'',I1,'')'')')W
        READ(UNIT=STRING(II+1:JJ),FMT=ZFORM,ERR=1000,IOSTAT=IOS)IX
        VALUEX = IX
        ITYPE = 7
C
C ****  This might be a boolean
C
      ELSEIF ((STRING(II:II) .EQ. 'T') .OR.
     &        (STRING(II:II) .EQ. 'F') .OR.
     &        (STRING(II:II) .EQ. 't') .OR.
     &        (STRING(II:II) .EQ. 'f')) THEN
        READ (UNIT=STRING(II:JJ),FMT='(L'//CW//')',ERR=1000, IOSTAT=IOS)
     &    TRUTH
        VALUEX = RTRUTH
        ITYPE = 4
C
C ****  This might be a logical
C
      ELSEIF ((STRING(II:II+1) .EQ. '.T') .OR.
     &        (STRING(II:II+1) .EQ. '.t') .OR.
     &        (STRING(II:II+1) .EQ. '.F') .OR.
     &        (STRING(II:II+1) .EQ. '.f')) THEN
        READ (UNIT=STRING(II:JJ),FMT='(L'//CW//')',ERR=1000,IOSTAT=IOS)
     &    TRUTH
        VALUEX = RTRUTH
        ITYPE = 4
C
C ****  Could be a hollerith
C
      ELSEIF ((STRING(II:II) .EQ. '''') .OR.
     &        (STRING(II:II) .EQ. '"' )) THEN
C
C ****  HOLLERITH
C
        JJ = II + 5
        CALL UCTOH(STRING(II+1:JJ-1),XHOLL,4,4)
        ITYPE = 5
        VALUEX = XHOLL
      ELSE
        GOTO 1000
      ENDIF
C
  999 RETURN
C
 1000 CONTINUE          ! **** ERROR HANDLING
      ITYPE =-1
      VALUEX  = 0.0
      RETURN
C
C ****  The following entry points can be used to get unconverted values.
C
      ENTRY DVALUEX()
      DVALUEX = DX
      GO TO 999
C
      ENTRY IVALUEX()
      IVALUEX = IX
      GO TO 999
C
      ENTRY LVALUEX()
      LVALUEX = TRUTH
      GO TO 999
      END
