      INTEGER FUNCTION GET_INDEX ( N, ARRAY, STRING )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the position in the ARRAY that matches the
C-   STRING.
C-
C-   Inputs  : N       [I]        - number of elements in the array
C-             ARRAY   [C*(*)](N) - n-dimensional array of character strings
C-             STRING  [C*(*)]    -    character string we look for
C-   Outputs : none
C-
C-   Returned value:   the index of the entry of ARRAY that matches STRING; 0 if
C-                     no match is found.
C-   Controls: none
C-
C-   Created  27-FEB-1992   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  N
      CHARACTER*(*)  ARRAY(N), STRING
C----------------------------------------------------------------------
      INTEGER  I
C----------------------------------------------------------------------
      GET_INDEX = 0
      DO I = 1, N
        IF ( STRING .EQ. ARRAY(I) ) THEN
          GET_INDEX = I
          RETURN
        ENDIF
      ENDDO
      RETURN
      END
