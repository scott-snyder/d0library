      SUBROUTINE PUFTDA(HALF,QUAD,SECTOR)
C-----------------------------------------------------------------------
C-   Purpose: Eliminates the unused portion of
C-            bank 'FTDA' after it has been filled.
C- 
C-   Created   7-JAN-1987   Tom Trippe 
C-   Updated   7-OCT-1988   Jeffrey Bantly  modified for new hit format 
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up 
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INTEGER HALF,QUAD,SECTOR,NHITS,NWFTDA
      INTEGER NPTWHT,NWDSHT,NHMXV,MXFTDA
C----------------------------------------------------------------------
      IF(LFTDA(SECTOR,QUAD,HALF).NE.0) THEN
        MXFTDA = IQ(LFTDA(SECTOR,QUAD,HALF)-1)
        NHITS  = IQ(LFTDA(SECTOR,QUAD,HALF)+1)
        NPTWHT = IQ(LFTDA(SECTOR,QUAD,HALF)+2)
        NWDSHT = IQ(LFTDA(SECTOR,QUAD,HALF)+3)
        NWFTDA=3+NPTWHT*2+NWDSHT*NHITS
        IF(NWFTDA.LT.MXFTDA) CALL 
     &    MZPUSH(IXCOM,LFTDA(SECTOR,QUAD,HALF),0,NWFTDA-MXFTDA,'R')
        IF(NWFTDA.GT.MXFTDA) CALL 
     &    MZPUSH(IXCOM,LFTDA(SECTOR,QUAD,HALF),0,NWFTDA-MXFTDA,' ')
      ENDIF
C---------------------------------------------------------------------
      RETURN
      END
