      SUBROUTINE DHTOC (NCHAR,HOLLER,STRING)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert from holleriths to character string.
C-                         This just provides a tidy interface to
C-                         the CERNLIB routine UHTOC.
C-
C-   Inputs  : NCHAR       Number of bytes to convert to characters.
C-             HOLLER(*)   Integer or integer array to be converted.
C-   Outputs : STRING      Output string.
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
      CALL UHTOC (HOLLER,4,STRING(1:N),N)
  999 RETURN
      END
