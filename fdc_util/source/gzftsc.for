      FUNCTION GZFTSC(HALF,QUAD,SECTOR)
C-----------------------------------------------------------------------
C
C  Purpose and Methods : Returns pointer to Zebra bank 'FTSC' for a 
C                        specified forward detector half,theta quadrant 
C                        and cell. Returns 0 if bank not booked.
C
C  Input: HALF,QUAD,SECTOR
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
      INCLUDE 'D0$LINKS:IZFTSC.LINK/LIST'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
C
      INTEGER GZFTSC
      INTEGER HALF,QUAD,SECTOR,GZFTQD,LKFTQD
C
      CHARACTER*4 PATH_LNK(0:1,0:7,0:5) ! path for which link has been set
      CHARACTER*4 PATH       ! path for which link is wanted 
C
C----------------------------------------------------------------------------
C
      IF(LFTSC(SECTOR,QUAD,HALF).EQ.0) THEN               ! link not set
        LKFTQD=GZFTQD(HALF,QUAD)
        IF(LKFTQD.NE.0) LFTSC(SECTOR,QUAD,HALF)=
     &                                 LQ(LKFTQD-IZFTSC-SECTOR)
        GZFTSC=LFTSC(SECTOR,QUAD,HALF)
        CALL PATHGT(PATH)
        PATH_LNK(HALF,QUAD,SECTOR)=PATH
      ELSE                              ! link set
        CALL PATHGT(PATH)
        IF(PATH.NE.PATH_LNK(HALF,QUAD,SECTOR).OR.
     &          IAND(IQ(LFTSC(SECTOR,QUAD,HALF)),ISTAT_DROP).NE.0) THEN 
C                                         ! link set for wrong path
C                                         ! or bank dropped
          GZFTSC=0
          LKFTQD=GZFTQD(HALF,QUAD)
          IF (LKFTQD.NE.0) LFTSC(SECTOR,QUAD,HALF)=
     &                                   LQ(LKFTQD-IZFTSC-SECTOR)
          GZFTSC=LFTSC(SECTOR,QUAD,HALF)
          PATH_LNK(HALF,QUAD,SECTOR)=PATH
        ELSE
          GZFTSC=LFTSC(SECTOR,QUAD,HALF)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
