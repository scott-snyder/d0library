      SUBROUTINE MTC_DUMP_TCHI(IUNIT)
C----------------------------------------------------------------------
C- MTC_DUMP_TCHI: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Write out the chi squareds in the
C-      3x3 projective tower about the current eta, phi
C-
C-   Inputs  : contents of /MTC_ETOWERS/:
C-             CHItower - tower chisquareds,
C-             ATOWER   - the id of cal cells in the current tower
C-             fcntchi(,) - the fraction of layers hit in each of 9 towers
C-             fcntchi3 - the fraction of 3x3 layers hit

C-   Outputs : dump of calorimeter cell chi squareds
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
      REAL    SUM
      INTEGER ILYR,IE2,IP2
C----------------------------------------------------------------------
      WRITE(IUNIT,85)
   85 FORMAT(/,' MTC_DUMP_TCHI:  Tower chi2 dump ')
      WRITE(IUNIT,86)
   86 FORMAT(1X,'ilyr','   cal#',2X,18X,2X,18X,2X,18X,2X,'   sum')

      DO 10 ILYR=1,18
        IF(ATOWER(0,0,ILYR).NE.'--------') THEN

          SUM = 0.
          DO 20 IE2=ITLO,ITHI
            DO 30 IP2=ITLO,ITHI
              SUM = SUM + CHITOWER(IE2,IP2,ILYR)
   30       CONTINUE
   20     CONTINUE

          IF(CHIT3(ILYR).LT.5555..AND.SUM.LT.5555.) THEN
            WRITE(IUNIT,84) ILYR,ATOWER(0,0,ILYR),
     &          CHITOWER(-1,-1,ILYR),CHITOWER(0,-1,ILYR),
     &          CHITOWER(1,-1,ILYR),CHITOWER(-1,0,ILYR),
     &          CHITOWER(0,0,ILYR),CHITOWER(1,0,ILYR),
     &          CHITOWER(-1,1,ILYR),CHITOWER(0,1,ILYR),
     &          CHITOWER(1,1,ILYR),CHIT3(ILYR)
          ELSE IF(CHIT3(ILYR).LT.55555..AND.SUM.LT.55555.) THEN
            WRITE(IUNIT,83) ILYR,ATOWER(0,0,ILYR),
     &          CHITOWER(-1,-1,ILYR),CHITOWER(0,-1,ILYR),
     &          CHITOWER(1,-1,ILYR),CHITOWER(-1,0,ILYR),
     &          CHITOWER(0,0,ILYR),CHITOWER(1,0,ILYR),
     &          CHITOWER(-1,1,ILYR),CHITOWER(0,1,ILYR),
     &          CHITOWER(1,1,ILYR),CHIT3(ILYR)
          ELSE IF(CHIT3(ILYR).LT.555555..AND.SUM.LT.555555.) THEN
            WRITE(IUNIT,82) ILYR,
     &          CHITOWER(-1,-1,ILYR),CHITOWER(0,-1,ILYR),
     &          CHITOWER(1,-1,ILYR),CHITOWER(-1,0,ILYR),
     &          CHITOWER(0,0,ILYR),CHITOWER(1,0,ILYR),
     &          CHITOWER(-1,1,ILYR),CHITOWER(0,1,ILYR),
     &          CHITOWER(1,1,ILYR),CHIT3(ILYR)
          ELSE
            WRITE(IUNIT,81) ILYR,
     &          CHITOWER(-1,-1,ILYR),CHITOWER(0,-1,ILYR),
     &          CHITOWER(1,-1,ILYR),CHITOWER(-1,0,ILYR),
     &          CHITOWER(0,0,ILYR),CHITOWER(1,0,ILYR),
     &          CHITOWER(-1,1,ILYR),CHITOWER(0,1,ILYR),
     &          CHITOWER(1,1,ILYR),CHIT3(ILYR)
          END IF
        END IF
   10 CONTINUE                                          ! loop over layers

   81 FORMAT(1X,I2,1X,3E9.0, 2X, 3E9.0, 2X, 3E9.0, 2X, E9.0)
   82 FORMAT(1X,I2,1X,3F7.0, 2X, 3F7.0, 2X, 3F7.0, 2X, F7.0)
   83 FORMAT(1X,I2,1X,A8, 2X, 3F6.0, 2X, 3F6.0, 2X, 3F6.0, 2X, F6.0)
   84 FORMAT(1X,I2,1X,A8, 2X, 3F6.1, 2X, 3F6.1, 2X, 3F6.1, 2X, F6.1)
C----------------------------------------------------------------------
  999 RETURN
      END
