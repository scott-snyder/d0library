      SUBROUTINE EZGETC1 (IARRAY,ITYPE,IPOINT,STRING,LENGTH)
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
C-             ITYPE(*)         [I]     Array of types
C-             IPOINT           [I]     Pointer to start of string in
C-                                      array IARRAY.
C-   Outputs : IPOINT           [I]     Pointer to word after string
C-             STRING           [C*]    Character string
C-             LENGTH           [I]     Length of character string
C-
C-   Controls: None
C-
C-   Created  12-DEC-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IARRAY(*)
      INTEGER ITYPE(*)
      INTEGER IPOINT
      CHARACTER*(*) STRING
      INTEGER LENGTH
C
      INTEGER VTCHR,NWORDS
      PARAMETER( VTCHR = 10 )
C
C----------------------------------------------------------------------
      IF ( ITYPE(IPOINT) .GT. VTCHR ) THEN
        LENGTH = ITYPE(IPOINT)-VTCHR        ! String length
        CALL DHTOC(LENGTH,IARRAY(IPOINT),STRING)
        NWORDS = 1 + (LENGTH-1)/4
        IPOINT = IPOINT + NWORDS
      ELSE
        STRING = ' '
        LENGTH = 0
      ENDIF
  999 RETURN
      END
