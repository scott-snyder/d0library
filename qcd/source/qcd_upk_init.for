      LOGICAL FUNCTION QCD_UPK_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INITIALIZE NTUIPLE MAKER
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created 28-DEC-1995   Bob Hirosky
C-   Updated 06-MAR-1996   Andrew G. Brandt  Add package switches
C-   Updated 11-MAR-1996   Andrew G. Brandt  move GAP_GET_RCP from NTUP_FILL
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GAP_CATD_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      LOGICAL FIRST,SAM,CAH,L0,FTK
      INTEGER I,LENG,TRULEN
      CHARACTER*32 PNAME
      DATA FIRST/.TRUE./
C
C include program builder common
C
      LOGICAL PBD_FLAG_VALUE( 9)
      CHARACTER*32 PBD_FLAG_NAME( 9)
      INTEGER PBD_FLAG_MAX
C-
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX

C----------------------------------------------------------------------
C
C
      QCD_UPK_INIT=.TRUE.
C
C Intitialization
C
      IF(FIRST)THEN
C
C Get all RCP values for job and fill them into common block
C
        CALL QCD_GET_RCP
C
C Get Gap ntuple RCP
C
        IF(GAP_JET) CALL GAP_GET_RCP
C
C Set path as chosen by RCP switch
C
        CALL PATHST(UPATH)
C
C First set default switches
C
        IF(CATDE.EQ.0) THEN
          CAH = .FALSE.
        ELSE
          CAH = .TRUE.
        END IF
        L0 = .FALSE.
        FTK = .FALSE.
        SAM = .FALSE.
C
C Check for override of default switches
C
        DO I = 1, NPACK
          PNAME = PACK(I)
          LENG = MIN(TRULEN(PNAME),32)
          IF(PNAME(1:LENG).EQ.'SAMRECO') THEN
            SAM = .TRUE.
          ELSE IF(PNAME(1:LENG).EQ.'LEVEL0') THEN
            L0 = .TRUE.
          ELSE IF(PNAME(1:LENG).EQ.'CAHITS') THEN
            CAH = .TRUE.
          ELSE IF(PNAME(1:LENG).EQ.'FTRAKS') THEN
            FTK = .TRUE.
          END IF
        END DO
C
C Turn off undesired packages
C
        DO I = 1, PBD_FLAG_MAX
          PNAME = PBD_FLAG_NAME(I)
          LENG = MIN(TRULEN(PNAME),32)
          IF((PNAME(1:LENG).EQ.'SAMRECO'.AND.SAM.EQ..FALSE.).OR.
     +       (PNAME(1:LENG).EQ.'LEVEL0'.AND.L0.EQ..FALSE.).OR.
     +       (PNAME(1:LENG).EQ.'CAHITS'.AND.CAH.EQ..FALSE.).OR.
     +       (PNAME(1:LENG).EQ.'FTRAKS'.AND.FTK.EQ..FALSE.))
     +       PBD_FLAG_VALUE(I)=.FALSE.
        END DO
C
        FIRST = .FALSE.
      ENDIF
  999 RETURN
      END
