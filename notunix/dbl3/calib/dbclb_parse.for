      SUBROUTINE DBCLB_PARSE ( PATH, PARSE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Break the path name into its elements
C-
C-   Inputs  : PATH     DBL path name
C-   Outputs : PARSE    array of character strings
C-   Controls: 
C-
C-   Created  28-FEB-1990   J.Green
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      CHARACTER*(*) PATH
      CHARACTER*(*) PARSE(*)
      INTEGER       LPATH, SLASH1, SLASH2
      INTEGER       I
      INTEGER       TRULEN              ! function to get true length
C----------------------------------------------------------------------
      LPATH = TRULEN ( PATH )           ! length without trailing blanks
      IF ( PATH(1:2) .NE. '//' ) THEN
        CALL INTMSG ( ' DBCLB_PARSE: Illegal path name ' )
        GO TO 999
      ENDIF
C      
      SLASH1 = 2
      I = 1
      DO WHILE ( SLASH1 .NE. 0 .AND. I .LE. 4 )
        SLASH2 = SLASH1 + INDEX ( PATH(SLASH1+1:LPATH), '/' )
        IF ( SLASH2 .EQ. SLASH1 ) SLASH2 = LPATH+1
        PARSE(I) = PATH(SLASH1+1:SLASH2-1)
        SLASH1 = SLASH2
        I = I + 1
      ENDDO
C
  999 RETURN
      END
