      SUBROUTINE EZGETC (PARAM,ISTART,NCHAR,STRING,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get Character string from array PARAM in
C-                         the currently selected SRCP bank. Each
C-                         string is assumed to be begin at position
C-                         ISTART in the array and to be NCHAR
C-                         characters long. If NCHAR is ZERO then the
C-                         length of the string is assumed to be 32
C-                         characters.
C-
C-   Inputs  : PARAM       Name of array parameter containing string
C-             ISTART      Starting position of string in array
C-             NCHAR       Number of characters to return
C-
C-   Outputs : STRING      Returned string (256 characters maximum)
C-             ERROR       0 --- OK. See error codes for EZGET.
C-
C-   Controls: None
C-
C-   Created  11-MAR-1989   Harrison B. Prosper
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
C----------------------------------------------------------------------
C
C ****  Check number of characters required
C
      CALL VZERO(IARRAY(1),NCHAR/4)
      STRING = ' '
      IF ( NCHAR .LE. 0 ) THEN
        N = 32
      ELSE
        N = NCHAR
      ENDIF
      IF ( N .GT. 256 ) N = 256
C
C ****  Check length of output string
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
C ****  Convert from integer array to character string
C
      CALL UHTOC (IARRAY,4,STRING(1:N),N)
C
  999 RETURN
      END
