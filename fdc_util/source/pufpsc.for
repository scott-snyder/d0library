      SUBROUTINE PUFPSC(HALF,SECTOR)
C-----------------------------------------------------------------------
C-   Purpose: Eliminates the unused portion of
C-            bank 'FPSC' after it has been filled.
C- 
C-   Created   7-JAN-1987   Tom Trippe 
C-   Updated   7-OCT-1988   Jeffrey Bantly  modified for new hit format 
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up 
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INTEGER HALF,SECTOR,NHITS,NWFPSC
      INTEGER NPTWHT,NWDSHT,NHMXV,MXFPSC
C----------------------------------------------------------------------
      IF(LFPSC(SECTOR,HALF).NE.0) THEN
        MXFPSC = IQ(LFPSC(SECTOR,HALF)-1)
        NHITS  = IQ(LFPSC(SECTOR,HALF)+1)
        NPTWHT = IQ(LFPSC(SECTOR,HALF)+2)
        NWDSHT = IQ(LFPSC(SECTOR,HALF)+3)
        NWFPSC=3+NPTWHT*2+NWDSHT*NHITS
        IF(NWFPSC.LT.MXFPSC) CALL 
     &    MZPUSH(IXCOM,LFPSC(SECTOR,HALF),0,NWFPSC-MXFPSC,'R')
        IF(NWFPSC.GT.MXFPSC) CALL 
     &    MZPUSH(IXCOM,LFPSC(SECTOR,HALF),0,NWFPSC-MXFPSC,' ')
      ENDIF
C----------------------------------------------------------------------
      RETURN
      END
