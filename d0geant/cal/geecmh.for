      SUBROUTINE GEECMH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Sets up End Cap MH geometry
C-                        Replaces GEECFH, GEECLK
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  16-OCT-1988   Rajendran Raja
C-   Updated  20-JUN-1989   Rajendran Raja  
C-   Put in GSORD, different names for MFA and MFB sub volumes 
C-   Updated  25-JUN-1989   N.A. Graf
C-                          Put in mother volumes MHM+/-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INTEGER ISEG,NSGEMH,NLN,IZ,IAB
      CHARACTER*32 CSTRNG
      CHARACTER*7 ECMFH(2,2)
      CHARACTER*18 MUTRVL(2)
      DATA MUTRVL/'MH_MOTHER_VOLUME+Z',  
     &            'MH_MOTHER_VOLUME-Z'/  
      DATA ECMFH/'EC_MFA+',
     &           'EC_MFB+',
     &           'EC_MFA-',
     &           'EC_MFB-'/  !EC middle fine hadronic segment
C
      CHARACTER*11 ECMFHE(2,2)
      DATA ECMFHE/'EC_MFA+EPA+',
     &            'EC_MFB+EPB+',
     &            'EC_MFA-EPA-',
     &            'EC_MFB-EPB-'/  !EC middle fine hadronic segment endplates
C
      CHARACTER*7 ECMCH(2,2)
      DATA ECMCH/'EC_MCA+',
     &           'EC_MCB+',
     &           'EC_MCA-',
     &           'EC_MCB-'/  !EC middle coarse hadronic segment
C
      CHARACTER*11 ECMCHE(2,2)
      DATA ECMCHE/'EC_MCA+EPC+',
     &            'EC_MCB+EPD+',
     &            'EC_MCA-EPC-',
     &            'EC_MCB-EPD-'/  !EC middle coarse hadronic segment endplates
C
      CHARACTER*7 ECMHG(2)
      DATA ECMHG/'EC_MHG+',
     &           'EC_MHG-'/       !EC MH massless gaps
C
      CHARACTER*15 ECFHZD(2)
      CHARACTER*15 ECCHZD(2)
      DATA ECFHZD/'MFH_DIVISIONS+Z',
     &            'MFH_DIVISIONS-Z'/
      DATA ECCHZD/'MCH_DIVISIONS+Z',
     &            'MCH_DIVISIONS-Z'/
C
C----------------------------------------------------------------------
C
      CALL GTSRCP('EC_MH_SEGMENTS',NSGEMH,1)
C
      DO 400 IZ = 1,2    !+/- z
        CALL VOLPOS(MUTRVL(IZ))
        DO 300 IAB = 1,2   !A and B sections
          DO 200 ISEG=1,NSGEMH
            CALL STRINT(ECMFH(IAB,IZ),ISEG,CSTRNG,NLN)
            CALL VOLPOS(CSTRNG(1:NLN))      !Fine hadronic A and B
            CALL STRINT(ECMCH(IAB,IZ),ISEG,CSTRNG,NLN)
            CALL VOLPOS(CSTRNG(1:NLN))      !Coarse hadronic A and B
  200     CONTINUE
C
          CALL VOLPOS(ECMFHE(IAB,IZ)) !Endplates
          CALL VOLPOS(ECMCHE(IAB,IZ))
C
          CALL STZDV1(ECFHZD(IZ),ECMFH(IAB,IZ),IAB)  !Z DIVISIONS. One needs
C                                        ! to only hang from 1st copy.
          CALL STZDV1(ECCHZD(IZ),ECMCH(IAB,IZ),IAB)
C
C
C ****  TO ORDER THE VOLUMES
C
C Order MFH volumes in local Y co-ordinate system
C
          CALL VOLORD(ECMFH(IAB,IZ)//'1',2)     
C
C Order MCH volumes in local Y co-ordinate system
C
          CALL VOLORD(ECMCH(IAB,IZ)//'1',2)     
  300   CONTINUE
C
        DO 100 ISEG=1,NSGEMH
        CALL STRINT(ECMHG(IZ),ISEG,CSTRNG,NLN)
        CALL VOLPOS(CSTRNG(1:NLN))      !POSITION MH MASSLESS GAPS
  100   CONTINUE
C
C Order MHM volumes in Phi
C
          CALL VOLORD(MUTRVL(IZ),6)
C
  400 CONTINUE
C
  999 RETURN
      END
