      SUBROUTINE CDTIME (TIMEST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a Creation Date and Time stamp IN a
C-                         20-character string.
C-
C-   Inputs  : None
C-   Outputs : TIMEST      Time stamp in format:
C-                         dd-mmm-yy   hh:mm:ss
C-   Controls: 
C-
C-   Created   9-JAN-1989   Harrison B. Prosper
C-   Updated  11-Mar-1992   Herbert Greenlee
C-      Added ELSE block for UNIX.  UNIX version uses 4-character year,
C-        but still 20 characters: dd-mmm-yyyy hh:mm:ss
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TIMEST
      CHARACTER*10   TIMBUF,DATBUF
C&IF VAXVMS
C&ELSE
C&      CHARACTER*26 CTEMP
C&      INTEGER OFFTIM, D0TLOCT
C&ENDIF
C----------------------------------------------------------------------
C&IF VAXVMS,SIUNIX
      DATBUF = ' '
      TIMBUF = ' '
C
      CALL DATE (DATBUF(1:9))
      CALL TIME (TIMBUF(3:10))
      TIMEST = DATBUF//TIMBUF
C&ELSE
C&      CALL OFTSTR(D0TLOCT(OFFTIM()),CTEMP)
C&      TIMEST = CTEMP(1:20)
C&ENDIF
C
  999 RETURN
      END
