      SUBROUTINE PUFTSC(HALF,QUAD,SECTOR)
C-----------------------------------------------------------------------
C-   Purpose: Eliminates the unused portion of
C-            bank 'FTSC' after it has been filled.
C- 
C-   Created   7-JAN-1987   Tom Trippe 
C-   Updated   7-OCT-1988   Jeffrey Bantly  modified for new hit format 
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up 
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INTEGER HALF,QUAD,SECTOR,NHITS,NWFTSC
      INTEGER NPTWHT,NWDSHT,NHMXV,MXFTSC
C-----------------------------------------------------------------------
      IF(LFTSC(SECTOR,QUAD,HALF).NE.0) THEN
        MXFTSC = IQ(LFTSC(SECTOR,QUAD,HALF)-1)
        NHITS  = IQ(LFTSC(SECTOR,QUAD,HALF)+1)
        NPTWHT = IQ(LFTSC(SECTOR,QUAD,HALF)+2)
        NWDSHT = IQ(LFTSC(SECTOR,QUAD,HALF)+3)
        NWFTSC=3+NPTWHT*2+NWDSHT*NHITS
        IF(NWFTSC.LT.MXFTSC) CALL 
     &    MZPUSH(IXCOM,LFTSC(SECTOR,QUAD,HALF),0,NWFTSC-MXFTSC,'R')
        IF(NWFTSC.GT.MXFTSC) CALL 
     &    MZPUSH(IXCOM,LFTSC(SECTOR,QUAD,HALF),0,NWFTSC-MXFTSC,' ')
      ENDIF
C----------------------------------------------------------------------
      RETURN
      END
