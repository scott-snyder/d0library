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
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUN_NUMBER
      BYTE PARFLG(1)
C----------------------------------------------------------------------
      CALL L2JETS_PARAMETERS(PARFLG(10))
      CALL CDC_L2TRK_PARAMETERS(PARFLG(13))
      CALL MUON_L2_PARAMETERS(PARFLG(12))
      CALL COSMIC_VTX_PARAMETERS(PARFLG(16))
      CALL COSMIC_L2_FDC_PARAMETERS(PARFLG(15))
      CALL L2_EM_PARAMETERS(PARFLG(17))
      CALL TOOL1_PARAMETERS(PARFLG(1))
      CALL L2ETMISS_PARAMETERS(PARFLG(18))
      CALL L2ETSUM_PARAMETERS(PARFLG(19))
      CALL L2_MU_COSMICS_PARAMETERS(PARFLG(30))
      CALL L2SETUP_PARAMETERS(PARFLG(22))
      CALL COSMIC_TRD_PARAMETERS(PARFLG(31))
  999 RETURN
      END
