      FUNCTION GZFDUN(HALF,UNIT)
C-----------------------------------------------------------------------
C-
C  Purpose and Methods : Returns pointer to Zebra bank 'FDUN' for a 
C-                       specified forward detector half and theta or 
C-                       phi unit. Returns 0 if bank not booked.
C
C  Input: HALF,UNIT
C
C-   Created  xx-MAR-1987   Daria Zieminska 
C-   Updated  18-OCT-1989   Jeffrey Bantly   use FDCLNK.INC
C-   Updated  26-FEB-1990   Jeffrey Bantly   use logical format
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$LINKS:IZFDUN.LINK/LIST'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
C
      INTEGER GZFDUN
      INTEGER HALF,UNIT,GZFHLF,LKFHLF
C
      CHARACTER*4 PATH_LNK(0:1,0:1) ! path for which link has been set
      CHARACTER*4 PATH       ! path for which link is wanted 
C
C--------------------------------------------------------------
C
      IF(LFDUN(UNIT,HALF).EQ.0) THEN               ! link not set
        LKFHLF=GZFHLF(HALF)
        IF(LKFHLF.NE.0) LFDUN(UNIT,HALF)=LQ(LKFHLF-IZFDUN-UNIT)
        GZFDUN=LFDUN(UNIT,HALF)
        CALL PATHGT(PATH)
        PATH_LNK(HALF,UNIT)=PATH
      ELSE                              ! link set
        CALL PATHGT(PATH)
        IF(PATH.NE.PATH_LNK(HALF,UNIT).OR.
     &            IAND(IQ(LFDUN(UNIT,HALF)),ISTAT_DROP).NE.0) THEN
C                                         ! link set for wrong path
C                                         ! or bank dropped
          GZFDUN=0
          LKFHLF=GZFHLF(HALF)
          IF (LKFHLF.NE.0) LFDUN(UNIT,HALF)=LQ(LKFHLF-IZFDUN-UNIT)
          GZFDUN=LFDUN(UNIT,HALF)
          PATH_LNK(HALF,UNIT)=PATH
        ELSE
          GZFDUN=LFDUN(UNIT,HALF)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
