      SUBROUTINE PRTRGR_L15_SECTION_START(LTRGR_LEVEL15,L15_ARR,
     &   LFCS_START,LFPS_START,LTPS_START,LLDSP_START,LGDSP_START,
     &   LDEBS_START)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to extract the beginning array of sections of
C-                         TRGR_L15 bank.
C-
C-   Inputs  : LTRGR_LEVEL15 The offset into IQ of the L15 Datablock crate
C-             L15_ARR     Total arrays in TRGR_l15
C-
C-   Outputs : LFCS_START  Beginning of Frame Code Section arrays
C-             LFPS_START  Beginning of Frame Param Section arrays
C-             LTPS_START  Beginning of Tool Param Section arrays
C-             LLDSP_START Beginning of Local DSP Section arrays
C-             LGDSP_START Beginning of Blobal DSP Section arrays
C-             LDEBS_START Beginning of Debug Section arrays
C-
C-   Controls: none
C-
C-   Created  23-NOV-1993   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
C
      INTEGER TOT_ARR
      PARAMETER (TOT_ARR = L15CAL_BANK_LENGTH - L15CAL_TRAILER_LENGTH)
      INTEGER LTRGR_LEVEL15, L15_ARR(TOT_ARR)
      INTEGER LFCS_START, LFPS_START, LTPS_START
      INTEGER LLDSP_START, LGDSP_START, LDEBS_START
      INTEGER CH_NLWF, FC_NLWF, FP_NLWF, TP_NLWF
      INTEGER LDSP_NLWF, GDSP_NLWF
C
C-----------------------------------------------------------------------
C
      CH_NLWF = 0
      FC_NLWF = 0
      FP_NLWF = 0
      TP_NLWF = 0
      LDSP_NLWF = 0

      LFCS_START = 0
      LFPS_START = 0
      LTPS_START = 0
      LLDSP_START = 0
      LGDSP_START = 0
      LDEBS_START = 0
      CH_NLWF = L15_ARR(1)
      LFCS_START = LTRGR_LEVEL15 + CH_NLWF + 1
      FC_NLWF = L15_ARR(LFCS_START-LTRGR_LEVEL15+1)
      LFPS_START = LFCS_START + FC_NLWF + 1
      FP_NLWF = L15_ARR(LFPS_START-LTRGR_LEVEL15+1)
      LTPS_START = LFPS_START + FP_NLWF + 1
      TP_NLWF = L15_ARR(LTPS_START-LTRGR_LEVEL15+1)
      LLDSP_START = LTPS_START + TP_NLWF + 1
      LDSP_NLWF = L15_ARR(LLDSP_START-LTRGR_LEVEL15+1)
      LGDSP_START = LLDSP_START + LDSP_NLWF +1
      GDSP_NLWF = L15_ARR(LGDSP_START-LTRGR_LEVEL15+1)
      LDEBS_START = LGDSP_START + GDSP_NLWF + 1
C----------------------------------------------------------------------
  999 RETURN
      END
