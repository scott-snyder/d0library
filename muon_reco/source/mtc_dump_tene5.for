      SUBROUTINE MTC_DUMP_TENE5(IUNIT)
C----------------------------------------------------------------------
C- MTC_DUMP_TENE5: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Write out the energies found in the
C-      projective towers ABOUT the
C-      3x3 tower about the current eta, phi
C-
C-   Inputs  : contents of /MTC_ETOWERS/:
C-             ENtower(+-2,*,1:18),(*,+-2,1:18)  - tower energies,
C-   Outputs : dump of calorimeter cell energies
C-
C-   Created  16-AUG-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- Input
      INTEGER IUNIT
      INCLUDE 'D0$INC:MTC_ETOWERS.INC'
      INCLUDE 'D0$INC:MTC_AETOWERS.INC'
      INCLUDE 'D0$INC:MTC_E5TOWERS.INC'
C- local
      INTEGER ILYR,IE2,IP2
C----------------------------------------------------------------------
      WRITE(IUNIT,85)
   85 FORMAT(/,' MTC_DUMP_TENE5:  Energy dump in outer 16 towers ',
     &  'indexed by ieta,iphi relative to central tower')

      WRITE(IUNIT,86)
   86 FORMAT(1X,'ly',
     & '  -2,2 -1,2  0,2  1,2',2x,'  2,2  2,1  2,0 2,-1',2x,
     &  ' 2,-2 1,-2 0,-2 -1-2',2x,' -2-2 -2-1 -2,0 -2,1',2x,' E5x5')

      DO 10 ILYR=1,18
        IF(ATOWER(0,0,ILYR).NE.'--------')
     &      WRITE(IUNIT,84) ILYR,
     &      (ENTOWER(IE2,+2,ILYR), IE2=-2,1),
     &      (ENTOWER(+2,IP2,ILYR), IP2=+2,-1,-1),
     &      (ENTOWER(IE2,-2,ILYR), IE2=+2,-1,-1),
     &      (ENTOWER(-2,IP2,ILYR), IP2=-2,1),     ESUM5(ILYR)
   10 CONTINUE
   84 FORMAT(1X,I2,1X,4F5.1, 2X, 4F5.1, 2X, 4F5.1, 2X, 4F5.1, 2X, F5.1)
C----------------------------------------------------------------------
      WRITE(IUNIT,*) ' '
      WRITE(IUNIT,87)
     &      (FCNTCHI(IE2,+2), IE2=-2,1),
     &      (FCNTCHI(+2,IP2), IP2=+2,-1,-1),
     &      (FCNTCHI(IE2,-2), IE2=+2,-1,-1),
     &      (FCNTCHI(-2,IP2), IP2=-2,1),     FCNTCHI5
   87 FORMAT(1X,2X,1X,4F5.2, 2X, 4F5.2, 2X, 4F5.2, 2X, 4F5.2, 2X, F5.2)
C----------------------------------------------------------------------
  999 RETURN
      END
