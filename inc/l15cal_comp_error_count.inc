C----------------------------------------------------------------------         
C- L15CAL_COMP_ERROR_COUNT.INC
C-
C-   Created   4-MAY-1994   Gregory L. Griffin
C-
C----------------------------------------------------------------------


      INTEGER NUM_HEADER_DIFFS, NUM_FRAME_CODE_DIFFS,
     &        NUM_FRAME_PARAM_DIFFS, NUM_TOOL_PARAM_DIFFS,
     &        NUM_LDSP_DIFFS, NUM_GDSP_DIFFS, NUM_DEBUG_DIFFS(0:4)
      INTEGER Ecount_num_no_2_crates 
      INTEGER num_no_real_crate, num_no_simu_crate
      INTEGER num_no_real_bank,  num_no_simu_bank
      INTEGER Ecount_LWTF_Header, Ecount_Crate_ID(2)
      INTEGER Ecount_data_valid(2), Ecount_data_invalid
      INTEGER Ecount_data_valid_nomatch, Ecount_Header 
      INTEGER Ecount_Frame_Code, Ecount_Frame_Param
      INTEGER Ecount_Tool_Param
      INTEGER Ecount_LDSP, Ecount_GDSP
      INTEGER Ecount_Debug
      INTEGER count_normal(2), count_MFP(2), count_not_norm_MFP(2)
      INTEGER count_normal_head(2), count_MFP_head(2)
      INTEGER diff_LWTF_debug(2)
      INTEGER diff_header(L15CAL_HEADER_LENGTH)
      INTEGER diff_LDSP_numvalid(11), diff_LDSP_LW(11,3)
      INTEGER diff_LDSP_header(11)
      INTEGER diff_GDSP_header    , diff_GDSP_LW(4)
      INTEGER diff_frame_code(6)
      INTEGER diff_Frame_Param_list
      INTEGER diff_frame_univ_param(FRAME_UNI_NLW)
      INTEGER 
     & diff_frame_term_param(L15CAL_NUM_TERMS_HARDWARE,FRAME_TERM_NLW)
      INTEGER diff_Tool_Param_list
      INTEGER 
     & diff_tool_local_param(L15CAL_NUM_TERMS_HARDWARE,LOC_NLW)
      INTEGER 
     & diff_tool_global_param(L15CAL_NUM_TERMS_HARDWARE,GLB_NLW)
      INTEGER diff_debug_type(0:4), diff_debug_num_type(0:4),
     &        diff_debug_entry_header(0:4)
      INTEGER diff_debug(0:4,11,4), diff_debug_at_least_one(0:4,11,4)

      COMMON /L15CAL_COMP_ERROR_COUNT/
     &  NUM_HEADER_DIFFS, NUM_FRAME_CODE_DIFFS,
     &  NUM_FRAME_PARAM_DIFFS, NUM_TOOL_PARAM_DIFFS,
     &  NUM_LDSP_DIFFS, NUM_GDSP_DIFFS, NUM_DEBUG_DIFFS,
     &  Ecount_num_no_2_crates,
     &  num_no_real_crate, num_no_simu_crate,
     &  num_no_real_bank,  num_no_simu_bank,
     &  Ecount_LWTF_Header, Ecount_Crate_ID,                            
     &  Ecount_data_valid, Ecount_data_invalid,
     &  Ecount_data_valid_nomatch, Ecount_Header ,
     &  Ecount_Frame_Code, Ecount_Frame_Param,
     &  Ecount_Tool_Param,
     &  Ecount_LDSP, Ecount_GDSP,
     &  Ecount_Debug,
     &  count_normal, count_MFP, count_not_norm_MFP,
     &  count_normal_head, count_MFP_head,
     &  diff_LWTF_debug,
     &  diff_header,
     &  diff_LDSP_numvalid, diff_LDSP_LW,
     &  diff_LDSP_header,
     &  diff_GDSP_header, diff_GDSP_LW,
     &  diff_frame_code,
     &  diff_Frame_Param_list, 
     &  diff_frame_univ_param, diff_frame_term_param,
     &  diff_Tool_Param_list,
     &  diff_tool_local_param, diff_tool_global_param,
     &  diff_debug_type, diff_debug_num_type, diff_debug_entry_header,
     &  diff_debug, diff_debug_at_least_one
