C-
C-                      LEVEL15_CAL_TRIGGER_DATA_BLOCK
C-                      ==============================
C-
C-  created 15nov93 S. Eno U. Maryland
C-
C-  added Frame_param section, and renamed original Param
C-  section to Tool_Param  6-march-1994 G. Snow
C-  also re-ordered common block entries to mimic order in TRGR bank
C----------------------------------------------------------------------
C
C     Level 1 Trigger Data Block
C     ==========================
C
      INTEGER L15CAL_FRAME_BLOCK (L15CAL_FRAME_LENGTH)
      INTEGER L15CAL_TOOL_PARAM_BLOCK (L15CAL_TOOL_PARAM_LENGTH)
      INTEGER L15CAL_FRAME_PARAM_BLOCK (L15CAL_FRAME_PARAM_LENGTH)
      INTEGER L15CAL_LOCAL_DSP_BLOCK (L15CAL_LOCAL_DSP_LENGTH)
      INTEGER L15CAL_GLOBAL_DSP_BLOCK (L15CAL_GLOBAL_DSP_LENGTH)
      INTEGER L15CAL_DEBUG_BLOCK (L15CAL_DEBUG_LENGTH)
C
C----------------------------------------------------------------------
C
C     Crate header and trailer
C     ========================
C
      INTEGER L15CAL_CRATE_HEADER(L15CAL_HEADER_LENGTH)
      INTEGER L15CAL_CRATE_TRAILER(L15CAL_TRAILER_LENGTH)
c
c
c
      common/l15cal_data_block/ l15cal_crate_header,
     &                          l15cal_frame_block,
     &                          l15cal_frame_param_block,
     &                          l15cal_tool_param_block,
     &                          l15cal_local_dsp_block,
     &                          l15cal_global_dsp_block,
     &                          l15cal_debug_block,
     &                          l15cal_crate_trailer
