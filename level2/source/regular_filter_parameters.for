      SUBROUTINE FILTER_PARAMETERS(RUN_NUMBER,PARFLG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize Level-2 filter TOOLs at BEGIN RUN
C-
C-   Inputs  : RUN_NUMBER : Current run number
C-             PARFLG : BYTE array of flags
C-                      for new parameter sets to read
C-   Outputs : None
C-   Controls: None
C-
C-   Created   22-NOV-93   by L2STATE
C-   Updated   22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUN_NUMBER
      BYTE PARFLG(40)
C----------------------------------------------------------------------
      CALL TOOL1_PARAMETERS(PARFLG(1))
      CALL L2JETS_PARAMETERS(PARFLG(10))
      CALL MUON_L2_PARAMETERS(PARFLG(12))
      CALL L2_EM_PARAMETERS(PARFLG(17))
      CALL L2ETMISS_PARAMETERS(PARFLG(18))
      CALL L2ETSUM_PARAMETERS(PARFLG(19))
      CALL L2_PASS_FAIL_PARAMETERS(PARFLG(20))
      CALL L2_TEST_PARAMETERS(PARFLG(21))
      CALL L2SETUP_PARAMETERS(PARFLG(22))
      CALL L2_MIN_BIAS_PARAMETERS(PARFLG(25))
      CALL L2_PRESCALE_PARAMETERS(PARFLG(26))
      CALL L2_ETACUT_PARAMETERS(PARFLG(27))
      CALL L2_CONFIRM_L15_PARAMETERS(PARFLG(2))
      CALL L2TAU_PARAMETERS(PARFLG(28))
  999 RETURN
      END
