      SUBROUTINE MTC_DUMP_TTYPE(IUNIT)
C----------------------------------------------------------------------
C- MTC_DUMP_TTYPE: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Write out the tower
C-      calorimeter section, layer number and ieta for the
C-      3x3 projective tower about the current eta, phi
C-
C-   Inputs  : contents of /MTC_ETOWERS/:
C-             Atower   - char*8 indicating tower location
C-   Outputs : dump of calorimeter cell locations in the current tower
C-
C-   Created  8-sept-1993       Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- Input
      INTEGER IUNIT
      INCLUDE 'D0$INC:MTC_ETOWERS.INC'
      INCLUDE 'D0$INC:MTC_AETOWERS.INC'
C- local
      INTEGER ILYR
C----------------------------------------------------------------------
      WRITE(IUNIT,85)
   85 FORMAT(/,' MTC_DUMP_TTYPE:  Tower type dump ')
      WRITE(IUNIT,86)
   86 FORMAT(1X,'ilyr',' cal module - layer - ieta ')
      DO 10 ILYR=1,17
        IF(ATOWER(0,0,ILYR).NE.'--------')
     &      WRITE(IUNIT,84) ILYR,
     &      ATOWER(-1,-1,ILYR),ATOWER(0,-1,ILYR),ATOWER(1,-1,ILYR),
     &      ATOWER(-1,0,ILYR),ATOWER(0,0,ILYR),ATOWER(1,0,ILYR),
     &      ATOWER(-1,1,ILYR),ATOWER(0,1,ILYR),ATOWER(1,1,ILYR)
   10 CONTINUE
   84 FORMAT(1X,I2,1X,3A9, 2X, 3A9, 2X, 3A9)
C----------------------------------------------------------------------
  999 RETURN
      END
