      SUBROUTINE PUFPDA(HALF,SECTOR)
C-----------------------------------------------------------------------
C-   Purpose: Eliminates the unused portion of
C-            bank 'FPDA' after it has been filled.
C- 
C-   Created   7-JAN-1987   Tom Trippe 
C-   Updated   7-OCT-1988   Jeffrey Bantly   modified for new hit format
C-   Updated  16-MAR-1990   Jeffrey Bantly  general cleanup 
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INTEGER HALF,SECTOR,NHITS,NWFPDA
      INTEGER NPTWHT,NWDSHT,NHMXV,MXFPDA
C-----------------------------------------------------------------------
      IF(LFPDA(SECTOR,HALF).NE.0) THEN
        MXFPDA = IQ(LFPDA(SECTOR,HALF)-1)
        NHITS  = IQ(LFPDA(SECTOR,HALF)+1)
        NPTWHT = IQ(LFPDA(SECTOR,HALF)+2)
        NWDSHT = IQ(LFPDA(SECTOR,HALF)+3)
        NWFPDA=3+NPTWHT*2+NWDSHT*NHITS
        IF(NWFPDA.LT.MXFPDA) CALL 
     &    MZPUSH(IXCOM,LFPDA(SECTOR,HALF),0,NWFPDA-MXFPDA,'R')
        IF(NWFPDA.GT.MXFPDA) CALL 
     &    MZPUSH(IXCOM,LFPDA(SECTOR,HALF),0,NWFPDA-MXFPDA,' ')
      ENDIF
C----------------------------------------------------------------------
      RETURN
      END
