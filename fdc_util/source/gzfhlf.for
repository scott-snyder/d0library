      FUNCTION GZFHLF(HALF)
C-----------------------------------------------------------------------
C-
C-  Purpose and Methods : Returns pointer to Zebra bank 'FHLF' for a 
C-                        specified forward detector half . Returns 0 
C-                        if bank not booked.
C
C-  Input: HALF
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
      INCLUDE 'D0$LINKS:IZFHLF.LINK'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
C
      INTEGER GZFHLF
      INTEGER HALF,GZFDCH,LKFDCH
C
      CHARACTER*4 PATH_LNK(0:1)   ! path for which link has been set
      CHARACTER*4 PATH            ! path for which link is wanted 
C
C--------------------------------------------------------------
C
      IF(LFHLF(HALF).EQ.0) THEN               ! link not set
        LKFDCH=GZFDCH()
        IF(LKFDCH.NE.0) LFHLF(HALF)=LQ(LKFDCH-IZFHLF-HALF)
        GZFHLF=LFHLF(HALF)
        CALL PATHGT(PATH)
        PATH_LNK(HALF)=PATH
      ELSE                              ! link set
        CALL PATHGT(PATH)
        IF(PATH.NE.PATH_LNK(HALF).OR.
     &    IAND(IQ(LFHLF(HALF)),ISTAT_DROP).NE.0) THEN
C                                         ! link set for wrong path
C                                         ! or bank dropped
          GZFHLF=0
          LKFDCH=GZFDCH()
          IF (LKFDCH.NE.0) LFHLF(HALF)=LQ(LKFDCH-IZFHLF-HALF)
          GZFHLF=LFHLF(HALF)
          PATH_LNK(HALF)=PATH
        ELSE
          GZFHLF=LFHLF(HALF)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
