      SUBROUTINE CONDBL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-DEC-1993   SHAHRIAR ABACHI   Similar to constp
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IXWIPE
      INCLUDE 'D0$INC:ZEBDBL.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INTEGER JO,JDBLO,JDBLC,JDBLN,JDCPH
      INTEGER JLINK
      DATA JLINK /20/
C----------------------------------------------------------------------
C
C **** NOTE: The banks DBLx contain no data, flag is set to integer
C
      JO=2     
      JDBLH=0
C
      IXWIPE = IDVDBL
      CALL MZWIPE(IXWIPE)
      CALL MZBOOK(IDVDBL,JDBLH,JDBLH,1,'DBLH',10,10,10,JO,0)
      CALL MZBOOK(IDVDBL,JDBLC,JDBLH,-IZSTPC,'DBLC',JLINK,JLINK,10,JO,0)
C
  999 RETURN
      END
