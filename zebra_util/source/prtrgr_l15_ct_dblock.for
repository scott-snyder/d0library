      SUBROUTINE PRTRGR_L15_CT_DBLOCK(LUN, LTRGR_LEVEL15)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the summary of the Level 1.5 Datablock
C-                         from the selected TRGR bank.
C-
C-   Inputs  : LUN       The unit number to write to.
C-             LTRGR_LEVEL15 The offset into IQ of the L15 Datablock crate
C-
C-   Outputs : file output
C-   Controls: none
C-
C-   Created  23-NOV-1993   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
C
      INTEGER LUN, LTRGR_LEVEL15, LFCS_START, LFPS_START, LTPS_START
      INTEGER LLDSP_START, LGDSP_START, LDEBS_START
      INTEGER CH_NLWF, FC_NLWF, FP_NLWF, TP_NLWF
      INTEGER LDSP_NLWF, GDSP_NLWF
C
C-----------------------------------------------------------------------
C

      WRITE (LUN,*)
      WRITE (LUN,*) 'Level 1.5 Datablock Contents (PRTRGR)'
      WRITE (LUN,*) '====================================='
      WRITE (LUN,*)
C
      IF (LTRGR_LEVEL15 .LE. 0) THEN
        WRITE (LUN,*) 'Couldn''t find LEVEL 1.5 crate'
        CALL ERRMSG('NO LEVEL 1.5 CRATE', 'PRTRGR_L15_FW_AND_CT_DBLOCK',
     &    ' Couldn''t find LEVEL 1.5 crate ', 'W')
        GOTO 999
      ENDIF
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Level 1.5 Crate Header'
      WRITE (LUN,*) '----------------------'
      WRITE (LUN,*)
      CALL PRTRGR_L15_CRATE_HEADER(LUN, IQ(LTRGR_LEVEL15), CH_NLWF)
      LFCS_START = LTRGR_LEVEL15 + CH_NLWF + 1
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Frame Code Section'
      WRITE (LUN,*) '------------------'
      WRITE (LUN,*)
      CALL PRTRGR_L15_FRAME_CODE_SECTION( LUN, IQ(LFCS_START), FC_NLWF)
      LFPS_START = LFCS_START + FC_NLWF + 1
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Frame Parameter Section'
      WRITE (LUN,*) '-----------------------'
      WRITE (LUN,*)
      CALL PRTRGR_L15_FRAME_PARAM_SECTION(LUN, IQ(LFPS_START), FP_NLWF)
      LTPS_START = LFPS_START + FP_NLWF + 1
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Tool Parameter Section'
      WRITE (LUN,*) '----------------------'
      WRITE (LUN,*)
      CALL PRTRGR_L15_TOOL_PARAM_SECTION(LUN, IQ(LTPS_START), TP_NLWF)
      LLDSP_START = LTPS_START + TP_NLWF + 1
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Local DSP Section'
      WRITE (LUN,*) '-----------------'
      WRITE (LUN,*)
      CALL PRTRGR_L15_LOCAL_DSP(LUN, IQ(LLDSP_START), LDSP_NLWF)
      LGDSP_START = LLDSP_START + LDSP_NLWF + 1
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Global DSP Section'
      WRITE (LUN,*) '------------------'
      WRITE (LUN,*)
      CALL PRTRGR_L15_GLOBAL_DSP(LUN, IQ(LGDSP_START), GDSP_NLWF)
      LDEBS_START = LGDSP_START + GDSP_NLWF + 1
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'DeBug Section'
      WRITE (LUN,*) '-------------'
      WRITE (LUN,*)
      CALL PRTRGR_L15_DEBUG_SECTION(LUN, LDEBS_START, IQ(LDEBS_START))
C----------------------------------------------------------------------
  999 RETURN
      END
