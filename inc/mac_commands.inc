C----------------------------------------------------------------------
C-
C-   Created  15-APR-1993   R. J. Genik II
C-
C-   Contains the n first non-ambiguous characters of each command
C----------------------------------------------------------------------
      CHARACTER*32 MAC_ARRAY_Name
      Parameter (MAC_ARRAY_Name  = 'MAC_INSTRUCTIONS')
      CHARACTER*10 MAC_Begin, MAC_End, Hist_Begin, Hist_End,
     +  Bin_code_Begin, Bin_code_End, Set_Ref_File, Set_Data_File,
     +  show_bin_contents,no_show_bin_contents, read_bin_code_file,
     +  Set_Output_File, Write_Description, Use_Bin_Codes_CMD,
     +  NO_Use_Bin_Codes,hist_comment, summarize_all_CMD, 
     +  confirm_ok_CMD, output_all_failures_CMD, Reset_Bin_Names,
     +  GTUnit_ID_CMD, REF_ID_Offset_CMD, tmp_id_offset_cmd,
     +  del_after_compare_all_CMD, test_option_pct_CMD
      Parameter (MAC_Begin       = '\BEGIN_IN')
      Parameter (MAC_End         = '\END_INSTR')
      Parameter (Hist_Begin      = '\HIST')
      Parameter (Hist_End        = '\END_H')
      Parameter (Bin_code_Begin  = '\BIN_C')
      Parameter (Bin_code_End    = '\END_B')
      Parameter (Set_Ref_File    = '\REFER')
      Parameter (Set_Data_File   = '\DATA')
      Parameter (show_bin_contents = '\SHOW_BIN')
      Parameter (no_show_bin_contents = '\NO_SHOW')
      Parameter (read_bin_code_file = '\READ_BIN')
      Parameter ( Set_Output_File = '\SET_OUT')
      Parameter ( Write_Description = '\WRITE')
      Parameter ( Use_Bin_Codes_CMD = '\USE_BIN_C')
      Parameter ( NO_Use_Bin_Codes = '\NO_USE_')
      Parameter ( hist_comment = '\COMMENT')
      Parameter ( summarize_all_CMD = '\SUMMAR')
      Parameter ( confirm_ok_CMD = '\CONFIR')
      Parameter ( output_all_failures_CMD = '\OUTPUT')
      Parameter ( Reset_Bin_Names = '\RESET_B')
      Parameter (GTUnit_ID_CMD   = '\SET_GT')
      Parameter (REF_ID_Offset_CMD = '\SET_REF')
      Parameter (tmp_id_offset_cmd = '\SET_TMP')
      Parameter (del_after_compare_all_CMD = '\DEL_AFT')
      Parameter (test_option_pct_CMD = '\PCT_OPT')
