      SUBROUTINE VALUSY (STRING,NUMBER,TYPE,NUMS,EXPAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract all valid numbers from a string and
C-                         return values in an array together with
C-                         number type for each element of the array.
C-                         This routine is like VALUEX, except that it
C-                         can operate on character strings of variable
C-                         length. If EXPAND is greater than zero then
C-                         any character string with more than 4 characters
C-                         will be padded out to a string of length EXPAND
C-                         characters.
C-
C-   Inputs:   STRING      String from which numbers are to be extracted
C-
C-   Outputs : NUMBER(*)   Array of values (given as REAL); use
C-                         equivalences to convert to INTEGER and LOGICAL,
C-                         and DHTOC to convert to CHARACTER.
C-             TYPE(*)     Array giving value type (INTEGER):
C-                         VTINT        (1)    INTEGER
C-                         VTREAL       (2)    REAL
C-                         VTREFM       (3)    REAL in E-format
C-                         VTLOG        (4)    LOGICAL
C-                         VTCHAR       (5)    HOLLERITH
C-                         VTDBL        (6)    DOUBLE
C-                         VTHEX        (7)    HEXADECIMAL
C-                         VTCHR+n      (10+n) CHARACTER
C-                                       CHARACTER string of length n.
C-                            -1        INVALID number representation
C-
C-                         NOTE: The symbols VTxxx are defined in
C-                         D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
C-
C-             NUMS        Number of values returned
C-
C-   Controls: EXPAND      If > 0 then any string with more than
C-                         4 characters is to padded out with
C-                         blanks up to a string of EXPAND characters.
C-
C-   Note : Uses the function VALUEX.
C-
C-      Valid numbers are of type:
C-
C-              INTEGER, REAL, REAL in E-format, BOOLEAN
C-              (or LOGICAL) and CHARACTER STRINGS.
C-
C-      Declarations of the form N*xx are allowed (except for strings
C-      with more than 4 characters).
C-
C-                         For example 3*10.45
C-
C-   Created  15-JUN-1989   Harrison B. Prosper
C-   Updated   8-AUG-1989   Harrison B. Prosper
C-   Updated  10-NOV-1989   Harrison B. Prosper
C-   Now treats all strings as variable length
C-   Updated  19-Apr-1991   Herbert B. Greenlee
C-     Modified to handle hex types without truncation to 24-bits
C-   Updated   9-DEC-1991   Krzysztof L. Genser   
C-      80-->132 blanks     
C-   Updated  15-JUL-1992   Harrison B. Prosper   
C-      Handle integers exactly
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) STRING
      REAL          NUMBER(*)
      INTEGER       TYPE(*)
      INTEGER       NUMS
      INTEGER       EXPAND
C
      LOGICAL       ACTIVE
      CHARACTER*16  SIZE
      CHARACTER*132  BLANKS
      CHARACTER*132 CTEMP
      INTEGER       CODE,N,I,J,NS,K,L,IX,IY
      INTEGER       II,JJ,III,JJJ,LLL,IV,JV,LV,LENGTH,IVALUEX
      REAL          VALUEX,X,Y
      EQUIVALENCE (Y,IY), (X,IX)
C
C ****  Value type definitions
C
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
      DATA BLANKS /
     &  '                                                            '/
C----------------------------------------------------------------------
C
      NS = LEN (STRING)
      II = 1
      N  = 0  ! ZERO Item counter
  100 CONTINUE
C
C ****  NOTE: If CODE equals zero then string contains non-numeric characters
C
      X = VALUEX (STRING(II:NS),I,J,CODE)     ! Do NOT modify I and J
      ACTIVE = CODE .NE. VTNULL
C
      IF ( ACTIVE ) THEN
C
C ****  Check for string
C
        IF ( CODE .EQ. VTCHAR ) THEN
C
          III = II + I                  ! START of string
          JJJ = INDEX(STRING(III:NS),'''')
          IF ( JJJ .LE. 0 ) GOTO 190
C
          JJJ = III + JJJ - 2           ! END of string
          LLL = JJJ - III + 1           ! String length
          IF ( LLL .LE. 0 ) GOTO 190    ! Exclude null strings
C
          II = JJJ + 2                  ! Update string pointer
C
C ****  Check whether to expand string; look for '\' after quote
C ****  within string, then overide length value if EXPAND > 4
C
          IF ( STRING(II:II) .EQ. '\' ) THEN
            II = II + 1                 ! Update string pointer
            LENGTH = VALUEX (STRING(II:NS),I,J,K)
            IF ( I .EQ. 1 ) THEN
              II = II + J               ! Update string pointer
              IF ( LENGTH .GT. 4 ) THEN
                LLL = LENGTH            ! Expanded length
              ENDIF
            ENDIF
          ENDIF
C
          IF ( EXPAND .GT. 4 ) THEN
            LLL = EXPAND                ! Expanded length
          ENDIF
C
          CODE = VTCHR + LLL           ! Add string length to type
C
C ****  Pack characters into 32-bit words
C
          K   = 1 + (LLL-1)/4           ! Round up to a full-word
          LLL = K*4
          CTEMP = STRING(III:JJJ)//BLANKS
          CALL DCTOH (LLL,CTEMP,NUMBER(N+1))
          DO 150 L = 1,K
            N = N + 1
            TYPE(N) = CODE
  150     CONTINUE
C
          GOTO 100                    ! Move to next value
        ENDIF
C
  190   CONTINUE
C
C ****  SQUEEZE BLANKS OUT BEFORE CHECKING FOR '*' DUE TO ODD
C ****  BEHAVIOUR OF READING NUMBER IN FORM N*X 
C
        II = II + J   
        CALL WORD (STRING(II:NS),IV,JV,LV)
        IF ( IV .GT. 1 ) II = II + IV - 1   
        IF ( STRING(II:II) .EQ. '*' ) THEN
          II = II + 1
          Y = VALUEX (STRING(II:NS),I,J,CODE)
C-  Check for hex -- HBG
          IF((CODE.EQ.VTHEX) .OR. (CODE.EQ.VTINT))IY = IVALUEX()
          II = II + J
          K = X
          DO 200 I = 1,K
            N = N + 1
            NUMBER(N) = Y
            TYPE(N) = CODE
  200     CONTINUE
          CALL WORD (STRING(II:NS),IV,JV,LV)
          IF ( IV .GT. 1 ) II = II + IV - 1   
        ELSE
          N = N + 1
C-  Check for hex -- HBG
          IF((CODE.EQ.VTHEX) .OR. (CODE.EQ.VTINT))IX = IVALUEX()
          NUMBER(N) = X
          TYPE(N) = CODE
        ENDIF
      ELSE
        ACTIVE = .FALSE.
      ENDIF
      IF ( ACTIVE ) GOTO 100
C
      NUMS = N
  999 RETURN
      END
