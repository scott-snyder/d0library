      SUBROUTINE DCTOH (NCHAR,STRING,HOLLER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert from character string to holleriths.
C-                         This just provides a tidy interface to
C-                         the CERNLIB routine UCTOH.
C-
C-   Inputs  : NCHAR       Number of characters to convert.
C-             STRING      String to convert.
C-
C-   Outputs : HOLLER(*)   Integer or integer array to be converted.
C-
C-   Controls: None
C-
C-   Created  12-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       NCHAR
      INTEGER       HOLLER(*)
      CHARACTER*(*) STRING
      INTEGER N
C----------------------------------------------------------------------
      N = LEN (STRING)
      N = MIN (N,NCHAR)
      CALL UCTOH (STRING(1:N),HOLLER,4,N)
  999 RETURN
      END
