      SUBROUTINE MTC_DUMP_TENE(IUNIT)
C----------------------------------------------------------------------
C- MTC_DUMP_TENE: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Write out the energies found in the
C-      3x3 projective tower about the current eta, phi
C-
C-   Inputs  : contents of /MTC_ETOWERS/:
C-             ENtower  - tower energies,
C-             ESUM3     - energy sum in each layer
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
C- local
      INTEGER ILYR
C----------------------------------------------------------------------
      WRITE(IUNIT,85) 
   85 FORMAT(/,' MTC_DUMP_TENE:  9 central tower energies indexed ',
     &  'by ieta,iphi rel to central eta,phi')
      WRITE(IUNIT,86)
   86 FORMAT(1X,'lyr ','cal#-ie',2X,
     &  ' -1,-1  0,-1  1,-1    -1,0   0,0   1,0    -1,1   0,1   1,1',
     &  2X,'   sum')

      DO 10 ILYR=1,18
        IF(ATOWER(0,0,ILYR).NE.'--------')
     &      WRITE(IUNIT,84) ILYR,ATOWER(0,0,ILYR),
     &      ENTOWER(-1,-1,ILYR),ENTOWER(0,-1,ILYR),ENTOWER(1,-1,ILYR),
     &      ENTOWER(-1,0,ILYR),ENTOWER(0,0,ILYR),ENTOWER(1,0,ILYR),
     &      ENTOWER(-1,1,ILYR),ENTOWER(0,1,ILYR),ENTOWER(1,1,ILYR),
     &      ESUM3(ILYR)
   10 CONTINUE
   84 FORMAT(1X,I2,1X,A8, 2X, 3F6.2, 2X, 3F6.2, 2X, 3F6.2, 2X, F6.2)

      WRITE(IUNIT,*) ' '
      WRITE(IUNIT,87) FCNTCHI(-1,-1),FCNTCHI(0,-1),FCNTCHI(1,-1),
     &          FCNTCHI(-1,0), FCNTCHI(0,0), FCNTCHI(1,0),
     &          FCNTCHI(-1,1), FCNTCHI(0,1), FCNTCHI(1,1),
     &          FCNTCHI3
   87 FORMAT(1X,2X,1X,8X, 2X, 3F6.2, 2X, 3F6.2, 2X, 3F6.2, 2X, F6.2)
C----------------------------------------------------------------------
  999 RETURN
      END
