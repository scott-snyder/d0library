      FUNCTION GZFPDA(HALF,SECTOR)
C-----------------------------------------------------------------------
C-
C  Purpose and Methods : Returns pointer to Zebra bank 'FPDA' for a 
C-                       specified forward detector half and phi cell. 
C-                       Returns 0 if bank not booked.
C
C  Input: HALF,SECTOR
C
C-   Created  xx-MAR-1987   Daria Zieminska   
C-   Updated  18-OCT-1989   Jeffrey Bantly  use FDCLNK.INC 
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated   8-NOV-1990   Jeffrey Bantly  check if bank dropped 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:FDCLNK.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFPDA.LINK/LIST'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
C
      INTEGER GZFPDA
      INTEGER HALF,SECTOR,GZFPSC,LKFPSC
C
      CHARACTER*4 PATH_LNK(0:1,0:35)   ! path for which link has been set
      CHARACTER*4 PATH       ! path for which link is wanted 
C
C--------------------------------------------------------------

      IF(LFPDA(SECTOR,HALF).EQ.0) THEN               ! link not set
        LKFPSC=GZFPSC(HALF,SECTOR)
        IF(LKFPSC.NE.0) LFPDA(SECTOR,HALF)=LQ(LKFPSC-IZFPDA)
        GZFPDA=LFPDA(SECTOR,HALF)
        CALL PATHGT(PATH)
        PATH_LNK(HALF,SECTOR)=PATH
      ELSE                              ! link set
        CALL PATHGT(PATH)
        IF(PATH.NE.PATH_LNK(HALF,SECTOR).OR.
     &          IAND(IQ(LFPDA(SECTOR,HALF)),ISTAT_DROP).NE.0) THEN
C                                          ! link set for wrong path
C                                          ! or bank dropped
          GZFPDA=0
          LKFPSC=GZFPSC(HALF,SECTOR)
          IF (LKFPSC.NE.0) LFPDA(SECTOR,HALF)=LQ(LKFPSC-IZFPDA)
          GZFPDA=LFPDA(SECTOR,HALF)
          PATH_LNK(HALF,SECTOR)=PATH
        ELSE
          GZFPDA=LFPDA(SECTOR,HALF)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
