      FUNCTION GZFPSC(HALF,SECTOR)
C-----------------------------------------------------------------------
C
C  Purpose and Methods : Returns pointer to Zebra bank 'FPSC' for a 
C                        specified forward detector half and phi cell. 
C                        Returns 0 if bank not booked.
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
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZFPSC.LINK/LIST'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
C
      INTEGER GZFPSC
      INTEGER HALF,SECTOR,GZFDUN,LKFDUN
C
      CHARACTER*4 PATH_LNK(0:1,0:35)   ! path for which link has been set
      CHARACTER*4 PATH       ! path for which link is wanted 
C
C--------------------------------------------------------------
C
      IF(LFPSC(SECTOR,HALF).EQ.0) THEN               ! link not set
        LKFDUN=GZFDUN(HALF,1)
        IF(LKFDUN.NE.0) LFPSC(SECTOR,HALF)=LQ(LKFDUN-IZFPSC-SECTOR)
        GZFPSC=LFPSC(SECTOR,HALF)
        CALL PATHGT(PATH)
        PATH_LNK(HALF,SECTOR)=PATH
      ELSE                              ! link set
        CALL PATHGT(PATH)
        IF(PATH.NE.PATH_LNK(HALF,SECTOR).OR.
     &            IAND(IQ(LFPSC(SECTOR,HALF)),ISTAT_DROP).NE.0) THEN
C                                          ! link set for wrong path
C                                          ! or bank dropped
          GZFPSC=0
          LKFDUN=GZFDUN(HALF,1)
          IF (LKFDUN.NE.0) LFPSC(SECTOR,HALF)=LQ(LKFDUN-IZFPSC-SECTOR)
          GZFPSC=LFPSC(SECTOR,HALF)
          PATH_LNK(HALF,SECTOR)=PATH
        ELSE
          GZFPSC=LFPSC(SECTOR,HALF)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
