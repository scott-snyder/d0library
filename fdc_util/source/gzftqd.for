      FUNCTION GZFTQD(HALF,QUAD)
C-----------------------------------------------------------------------
C
C  Purpose and Methods : Returns pointer to Zebra bank 'FTQD' for a 
C                        specified forward detector half,theta unit 
C                        and quadrant . Returns 0 if bank not booked.
C
C  Input: HALF,QUAD
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
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$LINKS:IZFTQD.LINK/LIST'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
C
      INTEGER GZFTQD
      INTEGER HALF,UNIT,QUAD,GZFDUN,LKFTHE
C
      CHARACTER*4 PATH_LNK(0:1,0:7)   ! path for which link has been set
      CHARACTER*4 PATH       ! path for which link is wanted 
C
      DATA UNIT /0/
C
C--------------------------------------------------------------
      GZFTQD=0
      IF(LFTQD(QUAD,HALF).EQ.0) THEN               ! link not set
        LKFTHE=GZFDUN(HALF,UNIT)
        IF(LKFTHE.NE.0) LFTQD(QUAD,HALF)=LQ(LKFTHE-IZFTQD-QUAD)
        GZFTQD=LFTQD(QUAD,HALF)
        CALL PATHGT(PATH)
        PATH_LNK(HALF,QUAD)=PATH
      ELSE                                         ! link set
        CALL PATHGT(PATH)
        IF(PATH.NE.PATH_LNK(HALF,QUAD).OR.
     &     IAND(IQ(LFTQD(QUAD,HALF)),ISTAT_DROP).NE.0) THEN 
C                                                  ! link set for wrong path
C                                                  ! or bank dropped
          GZFTQD=0
          LKFTHE=GZFDUN(HALF,UNIT)
          IF (LKFTHE.NE.0) LFTQD(QUAD,HALF)=LQ(LKFTHE-IZFTQD-QUAD)
          GZFTQD=LFTQD(QUAD,HALF)
          PATH_LNK(HALF,QUAD)=PATH
        ELSE
          GZFTQD=LFTQD(QUAD,HALF)
        ENDIF
      ENDIF
C----------------------------------------------------------------------
      RETURN
      END
