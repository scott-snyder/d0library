      SUBROUTINE EZSETC (PARAM,ISTART,NCHAR,STRING,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store Character string in array PARAM in
C-                         the currently selected SRCP bank. The
C-                         string is assumed to be begin at position
C-                         ISTART in the array and to be NCHAR
C-                         characters long. If NCHAR is ZERO then the
C-                         length of the string is assumed to be 32
C-                         characters.
C-
C-   Inputs  : PARAM       Name of array parameter containing string
C-             ISTART      Starting position of string in array
C-             NCHAR       Number of characters in string
C-             STRING      String to store (256 characters maximum).
C-
C-   Outputs : ERROR       0 --- OK. See error codes for EZSET.
C-
C-
C-   Controls: None
C-
C-   Created  11-MAR-1989   Harrison B. Prosper
C-   Updated   5-DEC-1989   Harrison B. Prosper  
C-      Adapted to new string format 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM
      INTEGER       ISTART
      INTEGER       NCHAR
      CHARACTER*(*) STRING
      INTEGER       ERROR
C
      INTEGER I,J,K,L,N,LSTR,IEND,LPAR
      INTEGER IARRAY(64)                ! Maximum of 256 bytes.
      CHARACTER*64 PAR
C----------------------------------------------------------------------
C
C ****  Check number of characters
C
      IF ( NCHAR .LE. 0 ) THEN
        N = 32
      ELSE
        N = NCHAR
      ENDIF
      IF ( N .GT. 256 ) N = 256
C
C ****  Check length of input string
C
      LSTR = LEN(STRING)
      IF ( LSTR .LT. N ) N = LSTR
      K = 1 + (N-1)/4           ! Convert to number of full-words
      IEND = ISTART + K - 1
C
C ****  Get data
C
      L = LEN(PARAM)
      CALL EZGETA (PARAM(1:L),ISTART,IEND,1,IARRAY,ERROR)
      IF ( ERROR .NE. 0 ) GOTO 999
C
C ****  Convert from character string to integer array
C
      CALL UCTOH (STRING(1:N),IARRAY,4,N)
C
C ****  Store data
C
      CALL EZSETA (PARAM(1:L),ISTART,IEND,1,IARRAY,ERROR)
C
  999 RETURN
      END
