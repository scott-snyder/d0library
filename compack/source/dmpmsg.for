      SUBROUTINE DMPMSG(IUNI,LINOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Send a line to dump file.
C-
C-   Inputs  : IUNI :   Unit number used for dump
C-             LINOUT : Line to be output
C-   Outputs : None
C-
C-   Created  29-JUN-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IUNI
      CHARACTER*(*) LINOUT
C----------------------------------------------------------------------
      WRITE(IUNI,100) LINOUT
  100 FORMAT(' ',A)
  999 RETURN
      END
