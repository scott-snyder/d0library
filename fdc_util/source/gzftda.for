      FUNCTION GZFTDA(HALF,QUAD,SECTOR)
C-----------------------------------------------------------------------
C  Returns pointer to Zebra bank 'FTDA' for a specified forward detector
C  half,theta unit,quadrant and cell. Returns 0 if bank not booked.
C
C  Input:
C        HALF,QUAD,SECTOR
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
      INCLUDE 'D0$LINKS:IZFTDA.LINK/LIST'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
C
      INTEGER GZFTDA
      INTEGER HALF,QUAD,SECTOR,GZFTSC,LKFTSC
C
      CHARACTER*4 PATH_LNK(0:1,0:7,0:5) ! path for which link has been set
      CHARACTER*4 PATH       ! path for which link is wanted
C
C----------------------------------------------------------------------------
C
      IF(LFTDA(SECTOR,QUAD,HALF).EQ.0) THEN               ! link not set
        LKFTSC=GZFTSC(HALF,QUAD,SECTOR)
        IF(LKFTSC.NE.0) LFTDA(SECTOR,QUAD,HALF)=
     &                                 LQ(LKFTSC-IZFTDA)
        GZFTDA=LFTDA(SECTOR,QUAD,HALF)
        CALL PATHGT(PATH)
        PATH_LNK(HALF,QUAD,SECTOR)=PATH
      ELSE                              ! link set
        CALL PATHGT(PATH)
        IF(PATH.NE.PATH_LNK(HALF,QUAD,SECTOR).OR.
     &         IAND(IQ(LFTDA(SECTOR,QUAD,HALF)),ISTAT_DROP).NE.0) THEN
C                                              ! link set for wrong path
C                                              ! or bank dropped
          GZFTDA=0
          LKFTSC=GZFTSC(HALF,QUAD,SECTOR)
          IF (LKFTSC.NE.0) LFTDA(SECTOR,QUAD,HALF)=
     &                                   LQ(LKFTSC-IZFTDA)
          GZFTDA=LFTDA(SECTOR,QUAD,HALF)
          PATH_LNK(HALF,QUAD,SECTOR)=PATH
        ELSE
          GZFTDA=LFTDA(SECTOR,QUAD,HALF)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
