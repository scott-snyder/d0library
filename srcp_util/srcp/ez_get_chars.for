      SUBROUTINE EZ_GET_CHARS(NAME,NCHR,CHR,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the first NCHR contiguous set of 
C-                         character strings from the parameter NAME in
C-                         the currently selected RCP bank. EZ_GET_CHARS
C-                         works off both pure string array parameters and
C-                         arrays of mixed type. The maximum array size is
C-                         8000 32-bit words.
C-
C-   Inputs  : NAME     Name of array of strings in RCP bank.
C-
C-   Outputs : NCHR    Number of character strings
C-             CHR(*)  Character strings
C-             IER      0 --- OK
C-
C-   Controls: None
C-
C-   Created   2-NOV-1989   Rajendran Raja
C-   Updated   5-DEC-1989   Harrison B. Prosper
C-      Adapted to new string format
C-   Updated   9-DEC-1991   Krzysztof L. Genser   
C-   8000-->13200 
C-   Updated  17-Feb-1992   Herbert Greenlee
C-      Changed dummy argument CHAR to CHR (reserved name).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME,CHR(*)
      INTEGER NCHR,IER
      INTEGER I,L,NWORD,TOTAL
C
      INTEGER MAXVAL
      PARAMETER( MAXVAL = 13200 )
      INTEGER RCPAR(MAXVAL),RCPTYP(MAXVAL)
      LOGICAL FOUND, EZERR
C
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      NCHR = 0
C
      CALL EZGET_VALUE_TYPE (NAME,RCPAR,RCPTYP,TOTAL,IER)
      IF ( EZERR (IER) ) GOTO 999
C
      FOUND = .FALSE.                           ! Found string = FALSE 
      I = 0
      DO WHILE ( I .LT. TOTAL )
        I = I + 1
        IF ( RCPTYP(I) .GT. VTCHR ) THEN        ! Check for character type
          L = RCPTYP(I) - VTCHR                 ! Get string length
          NWORD = (L+3)/4                       ! NUMBER OF WORDS
          NCHR = NCHR + 1
          CHR(NCHR) = ' '                     ! Initializing it.
          CALL UHTOC(RCPAR(I),4,CHR(NCHR),L)
          I = I + NWORD - 1
          FOUND = .TRUE.                        ! Found string = TRUE 
        ELSE
          IF ( FOUND ) GOTO 999
        ENDIF
      ENDDO
  999 RETURN
      END
