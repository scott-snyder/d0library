      SUBROUTINE MTC_TEST_MPV()
C----------------------------------------------------------------------
C- MTC_TEST_MPV: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : print the MPV results produced by
C-      MTC_FILL_MPV to see if it is working properly
C-
C-   Inputs  : none
C-   Outputs : print statements to sys$output
C-
C-   Created   8-SEPT-1993      Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MTC_MPVCOM.INC'
C- needed for calling MTC_ICD_TUBE
      INTEGER ICOUNT
      CHARACTER*4 ATUBE
C- functns ...
      INTEGER MTC_IWHERE,MTC_IWHLAYER,MTC_ISUBETA
C- local
      INTEGER IETA,IPHI,ILYR,IHERE,ISUB,ISETA
      CHARACTER*6 A(37,17), A_W(37,17)
      DATA A/629*'------'/, A_W/629*'------'/
C----------------------------------------------------------------------
      IPHI = 1
      DO 20 IETA=1,37
        DO 30 ILYR=1,17
          IHERE = MTC_IWHERE(IETA,IPHI,ILYR)
          ISUB  = MTC_IWHLAYER(IHERE,ILYR)
          IF(IHERE.NE.0) ISETA = MTC_ISUBETA(IHERE,ISUB,IETA)
          IF(IHERE.EQ.0) THEN
            GO TO 30
          ELSE IF(IHERE.NE.4) THEN
            WRITE(A(IETA,ILYR),88) 
     &        1000.*(GEVMPV(IHERE,ISUB,ISETA))
            WRITE(A_W(IETA,ILYR),88) 
     &        1000.*sqrt(GEVMPV_W(IHERE,ISUB,ISETA))
          ELSE
            CALL MTC_ICD_TUBE(IETA,IPHI,ICOUNT,ATUBE)
            WRITE(A(IETA,ILYR),88) 
     &        1000.*(GEV_MPV_ICD(ICOUNT))
            WRITE(A_W(IETA,ILYR),88) 
     &        1000.*sqrt(GEV_MPV_ICD_W(ICOUNT))
          END IF
   30   CONTINUE
   20 CONTINUE
   88 FORMAT(1x,F5.0)
C- print out results ...
      WRITE(6,*) ' 1x1 MPV in each calorimeter cell, layer, eta '
      DO 40 IETA=37,1,-1
        WRITE(6,80) (A(IETA,ILYR),ILYR=1,17)
   40 CONTINUE
      WRITE(6,*) ' 1x1 uncertainties '
      DO 41 IETA=37,1,-1
        WRITE(6,80) (A_W(IETA,ILYR),ILYR=1,17)
   41 CONTINUE
   80 FORMAT(1X,17A6)
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      IPHI = 1
      DO 320 IETA=1,37
        DO 330 ILYR=1,17
          IHERE = MTC_IWHERE(IETA,IPHI,ILYR)
          ISUB  = MTC_IWHLAYER(IHERE,ILYR)
          IF(IHERE.NE.0) ISETA = MTC_ISUBETA(IHERE,ISUB,IETA)
          IF(IHERE.EQ.0) THEN
            GO TO 330
          ELSE IF(IHERE.NE.4) THEN
            WRITE(A(IETA,ILYR),89) 
     &        1000.*(GEVMPV3(IHERE,ISUB,ISETA))
            WRITE(A_W(IETA,ILYR),89) 
     &        1000.*sqrt(GEVMPV_W3(IHERE,ISUB,ISETA))
          ELSE
            CALL MTC_ICD_TUBE(IETA,IPHI,ICOUNT,ATUBE)
            WRITE(A(IETA,ILYR),89) 
     &        1000.*(GEV_MPV_ICD3(ICOUNT))
            WRITE(A_W(IETA,ILYR),89) 
     &        1000.*sqrt(GEV_MPV_ICD_W3(ICOUNT))
          END IF
  330   CONTINUE
  320 CONTINUE
   89 FORMAT(1x,F5.0)
C- print out results ...
      WRITE(6,*) ' 3x3 MPV in each calorimeter cell, layer, eta '
      DO 340 IETA=37,1,-1
        WRITE(6,81) (A(IETA,ILYR),ILYR=1,17)
  340 CONTINUE
      WRITE(6,*) ' 3x3 uncertainties '
      DO 341 IETA=37,1,-1
        WRITE(6,81) (A_W(IETA,ILYR),ILYR=1,17)
  341 CONTINUE
   81 FORMAT(1X,17A6)
C----------------------------------------------------------------------
  999 RETURN
      END
