      FUNCTION GZFDCH()
C----------------------------------------------------------
C
C  Purpose and Methods : Returns pointer to zebra bank FDCH
C
C-   Created  xx-MAR-1987   Daria Zieminska
C-   Updated  18-OCT-1989   Jeffrey Bantly  use FDCLNK.INC
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   8-NOV-1990   Jeffrey Bantly  check if bank dropped
C-   Updated  10-DEC-1991   Susan K. Blessing  Add call to FDPLNK
C-   Updated   7-JAN-1992   Susan K. Blessing  Add call to FGEAN_CHK
C-    to check if running D0Geant
C-   Updated  27-JAN-1992   Susan K. Blessing  Get rid of machine block for 
C-    IAND.
C-
C----------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$LINKS:IZFDCH.LINK/LIST'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
C
      INTEGER GZFDCH
      INTEGER DUMMY,LKHITS,GZHITS
C
      CHARACTER*4 PATH_LNK   ! path for which link has been set
      CHARACTER*4 PATH       ! path for which link is wanted
C
      LOGICAL FIRST,FGEAN_CHK
C
      DATA FIRST/.TRUE./
C
C--------------------------------------------------------------
C
      IF (FIRST) THEN
        IF (.NOT.FGEAN_CHK()) THEN
          CALL FDPLNK        
        END IF
        FIRST = .FALSE.
      END IF
C
      IF(LFDCH.EQ.0) THEN               ! link not set
        LKHITS=GZHITS()
        IF(LKHITS.NE.0) LFDCH=LQ(LKHITS-IZFDCH)
        GZFDCH=LFDCH
        CALL PATHGT(PATH)
        PATH_LNK=PATH
      ELSE                              ! link set
        CALL PATHGT(PATH)
        IF(PATH.NE.PATH_LNK.OR.IAND(IQ(LFDCH),ISTAT_DROP).NE.0) THEN
C                                         ! link set for wrong path
C                                         ! or bank dropped
          GZFDCH=0
          LKHITS=GZHITS()
          IF(LKHITS.NE.0) LFDCH=LQ(LKHITS-IZFDCH)
          GZFDCH=LFDCH
          PATH_LNK=PATH
        ELSE
          GZFDCH=LFDCH
        ENDIF
      ENDIF
C----------------------------------------------------------
      RETURN
      END
