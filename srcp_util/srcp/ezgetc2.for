      SUBROUTINE EZGETC2 (IARRAY,ITYPE,IPOINT,STRING,LENGTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Assuming a string begins at IARRAY(IPOINT)
C-   return it, and its length, in the variables STRING and LENGTH,
C-   respectively. Use EZGET_VALUE_TYPE to return the arrays IARRAY
C-   and ITYPE for the specified RCP parameter. IMPORTANT: Upon exit
C-   the pointer IPOINT is updated so that it points to the first word
C-   after the string. This routine is useful for decoding an RCP
C-   parameter, of mixed type, which contains character strings.
C-
C-   Inputs  : IARRAY(*)        [I]     Array of values
C-             ITYPE(*)         [I]     Array of types (in upper word)
C-             IPOINT           [I]     Pointer to start of string in
C-                                      array IARRAY.
C-   Outputs : IPOINT           [I]     Pointer to word after string
C-             STRING           [C*]    Character string
C-             LENGTH           [I]     Length of character string
C-
C-   Controls: None
C-
C-   Created  12-DEC-1989   Harrison B. Prosper
C-   Updated   8-APR-1991   LUPE HOWELL  , Harrison B. Prosper 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IARRAY(*)
      INTEGER ITYPE(*)
      INTEGER IPOINT
      CHARACTER*(*) STRING
      INTEGER LENGTH
C
      INTEGER NWORDS,ITYP
      INTEGER EZZSHFT
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      ITYP = EZZSHFT(ITYPE(IPOINT),-NBITS)
      IF ( ITYP .GT. VTCHR ) THEN
        LENGTH = ITYP-VTCHR        ! String length
        CALL DHTOC(LENGTH,IARRAY(IPOINT),STRING)
        NWORDS = 1 + (LENGTH-1)/4
        IPOINT = IPOINT + NWORDS
      ELSE
        STRING = ' '
        LENGTH = 0
      ENDIF
  999 RETURN
      END
