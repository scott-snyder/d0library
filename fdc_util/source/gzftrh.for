      FUNCTION GZFTRH()
C----------------------------------------------------------
C
C  Returns pointer to Zebra bank FTRH
C
C-   Created   xx-DEC-1988   Daria Zieminska
C-   Updated  17-OCT-1989   Jeffrey Bantly  cleanup
C-   Updated  26-FEB-1990   Jeffrey Bantly  use FDLTRK.INC
C-   Updated  10-DEC-1991   Susan K. Blessing  Add call to FPLTRK 
C-   Updated   7-JAN-1992   Susan K. Blessing  Add call to FGEANCHK
C-    to check if running D0Geant
C-   Updated  27-JAN-1992   Susan K. Blessing   Get rid of machine block for 
C-    IAND.
C-    
C----------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FDLTRK.INC'
      INCLUDE 'D0$LINKS:IZFTRH.LINK/LIST'
C
      INTEGER GZFTRH
      INTEGER LKZTRH,GZZTRH
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
          CALL FPLTRK
        END IF
        FIRST = .FALSE.
      END IF
C
      IF(LFTRH.EQ.0) THEN               ! link not set
        LKZTRH=GZZTRH()
        IF(LKZTRH.NE.0) LFTRH=LQ(LKZTRH-IZFTRH)
        GZFTRH=LFTRH
        CALL PATHGT(PATH)
        PATH_LNK=PATH
      ELSE                              ! link set
        CALL PATHGT(PATH)
        IF(PATH.NE.PATH_LNK) THEN       ! link set for wrong path
          GZFTRH=0
          LKZTRH=GZZTRH()
          IF(LKZTRH.NE.0) LFTRH=LQ(LKZTRH-IZFTRH)
          GZFTRH=LFTRH
          PATH_LNK=PATH
        ELSE
          GZFTRH=LFTRH
        ENDIF
      ENDIF
C----------------------------------------------------------
      RETURN
      END
