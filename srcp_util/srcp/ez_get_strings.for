      SUBROUTINE EZ_GET_STRINGS
     &  (NAME,ISTRING,MAXSTR,NUMSTR,LENSTR,STRING,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Starting with the ISTRING'th character string
C-   return up to MAXSTR character strings from the array parameter NAME in
C-   the currently selected RCP bank. The strings do NOT have to be
C-   contiguous within the array.
C-
C-   Inputs  : NAME     [C*]    Name of parameter array in RCP bank.
C-             ISTRING  [I]     Ordinal position of first string to return.
C-
C-   Outputs : MAXSTR   [I]     Maximum number of strings to return.
C-                              0 - return all strings from istring on.
C-             NUMSTR   [I]     Number of strings returned.
C-             LENSTR(*)[I]     Array of string lengths.
C-             STRING(*)[C*]    Array of strings
C-             IER      [I]     0 --- OK
C-
C-   Controls: None
C-
C-   Created   3-JUN-1991   Harrison B. Prosper
C-   Updated   9-DEC-1991   Krzysztof L. Genser  
C-   8000-->13200 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER ISTRING,MAXSTR,NUMSTR,LENSTR(*)
      CHARACTER*(*) STRING(*)
      INTEGER IER
C----------------------------------------------------------------------
      INTEGER MAXVAL
      PARAMETER( MAXVAL = 13200 )
      INTEGER RCPVAL(MAXVAL),RCPTYP(MAXVAL)
C
      INTEGER I,LENGTH,II,NWORD,TOTAL
      LOGICAL EZERROR, CHECK_COUNT
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      NUMSTR = 0
C
C ****  Load local arrays RCPVAL(*) and RCPTYP(*) with contents
C ****  of array parameter NAME.
C
      CALL EZGET_VALUE_TYPE (NAME(1:LEN(NAME)),RCPVAL,RCPTYP,TOTAL,IER)
      IF ( EZERROR(IER) ) GOTO 999
C
      CHECK_COUNT = MAXSTR .GT. 0
      II = 0
      I  = 0
      DO WHILE ( I .LT. TOTAL )
        I = I + 1
        IF ( RCPTYP(I) .GT. VTCHR ) THEN        ! Check for character type
          LENGTH = RCPTYP(I) - VTCHR            ! Get string length
C
C ****  Convert to character string
C
          II = II + 1                           ! String counter
          IF ( II .GE. ISTRING ) THEN
            NUMSTR = NUMSTR + 1
            LENSTR(NUMSTR) = LENGTH
            STRING(NUMSTR) = ' '                ! Clear string
            CALL DHTOC(LENGTH,RCPVAL(I),STRING(NUMSTR))
            IF ( CHECK_COUNT .AND. (NUMSTR .GE. MAXSTR) ) GOTO 999
          ENDIF
C
C ****  Move to next value in array
C
          NWORD = (LENGTH+3)/4                  ! Number of words/string
          I = I + NWORD - 1
        ENDIF
      ENDDO
  999 RETURN
      END
