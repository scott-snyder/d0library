      PROGRAM FILT_SHADOW_FILTER_STP                                          
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make up STP file for a Level-2 TYPE
C-
C-   Created    7-MAR-94   by L2STATE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PFNUM,NBANK,LSL2H,GZSL2H
      LOGICAL ANS,OP_OK
      CHARACTER RCP_BANK(100)
C----------------------------------------------------------------------
      CALL SETCOM
      CALL MZEBRA(0)
      CALL INZCOM(2)
      CALL INZSTP
      CALL BKSL2H(LSL2H)
      CALL ERRINI(0,.TRUE.) !one copy only
      CALL FILTER_STP_HISTORY
      CALL L2JETS_INIT
      CALL L2_EM_INIT
      CALL MUON_L2_INIT
      CALL TOOL1_INIT
      CALL L2ETSUM_INIT
      CALL L2ETMISS_INIT
      CALL L2_PASS_FAIL_INIT
      CALL L2_TEST_INIT
      CALL L2SETUP_INIT
      CALL L2_MIN_BIAS_INIT
      CALL L2_PRESCALE_INIT
      CALL L2_ETACUT_INIT
      CALL L2TAU_INIT
      CALL L2_CONFIRM_L15_INIT
      CALL L2_MASSCUT_INIT
      CALL L2_KEEP_CD_RAW_INIT
      CALL L2_ACOL_JETS_INIT
      CALL D0OPEN(12,'FILT_SHADOW_L2_MAIN.STP','OU',OP_OK)
      IF(OP_OK) THEN
        CALL EZDIR(RCP_BANK,NBANK)
        CALL EZCHAIN(RCP_BANK,NBANK)
        CALL FZFILE(12,0,'O')
        LSL2H = GZSL2H()
        CALL FZOUT(12,IXSTP,LSL2H,1,' ',1,0,0)
        CALL FZENDO(12,'T')
        CLOSE(12)
        CALL DZSURV('LSTPH DOWNLOAD SURVEY',IXSTP,LSTPH)
      ENDIF
      END
