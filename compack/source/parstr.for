      CHARACTER*40 FUNCTION PARSTR(IN,INLEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a left justified string found at integer
C-                         address (somewhat VAX-specific)
C-
C-   Inputs  : IN: Integer address of string (passed by %VAL)
C-             INLEN: Length of string
C-   Outputs : None
C-   Controls: None
C-
C-   Original April 1987 Jan S. Hoftun
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE 
      INTEGER INLEN
      CHARACTER*1 IN(INLEN)
      CHARACTER*40 TEMP
      INTEGER I,USELEN
C---------------------------------------------------------------------
      IF(INLEN.LE.40) THEN
         USELEN=INLEN
      ELSE
         USELEN=40
      ENDIF
      WRITE(TEMP,FMT='(40(A1:))') (IN(I),I=1,USELEN)
      PARSTR=TEMP
      RETURN
      END
