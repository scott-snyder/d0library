C----------------------------------------------------------------------
      SUBROUTINE ALLWORDS(LINEIN,MAXWORDS,WORDCOUNT,WORDS,LENWORD)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Split a line into separate words (ala C/Unix
C-      main(argc,argv).  Segments delimited by " ... " are parsed into
C-      a single arguement.
C-
C-   Inputs  : LINEIN     - Input character string
C-             MAXWORDS   - Maximum number of words to be parsed.
C-   Outputs : WORDCOUNT  - The number of words found
C-             WORDS(I)   - The i-th word found
C-             LENWORD(I) - The length of the i-th word.
C-   Controls:
C-
C-   Created   9-Jan-1995   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) LINEIN,WORDS(*)
      INTEGER MAXWORDS,WORDCOUNT,LENWORD(*),LASTLEN
      INTEGER LENOCC
C-----------------------------------------------------------------------
C
      LASTLEN=1
      WORDCOUNT=0
      DO WHILE( WORDCOUNT.LT.MAXWORDS .AND. LASTLEN.GT.0 )
        WORDCOUNT=WORDCOUNT+1
        CALL GETWORD(LINEIN,WORDCOUNT,WORDS(WORDCOUNT))
        LENWORD(WORDCOUNT)=LENOCC(WORDS(WORDCOUNT))
        LASTLEN=LENWORD(WORDCOUNT)
      ENDDO
C
      IF(LASTLEN.LE.0 ) WORDCOUNT=WORDCOUNT-1
C
 999  CONTINUE
      RETURN
      END
