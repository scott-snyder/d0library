C----------------------------------------------------------------------
C-
C-   Created  15-APR-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
C
C ****  Global parameters, some set via RCP.
C
      Integer nbins_max, no_comps_done
      Parameter(nbins_max = 40000) 
      LOGICAL Underflow,Overflow,Summarize_all,del_after_compare_all, 
     +  last_command,use_gt
      INTEGER Reference_id_offset, Igtunit_userid, Current_IDX,
     +  tmp_hist_offset,Ref_unit,Out_Unit,Data_Unit,Tmp_Unit
      INTEGER nbad_runs
C Output switches
      LOGICAL Show_Bins, Confirm_OK, output_all_failures, use_bin_codes,
     +  reset_names_of_bins
      Common/Mac_global/Underflow,Overflow,Summarize_all,
     +  Reference_id_offset, Igtunit_userid ,Current_IDX,
     +  tmp_hist_offset,use_gt
     +  ,Ref_unit,Out_Unit,Data_Unit,Tmp_Unit,no_comps_done,
     +  nbad_runs,del_after_compare_all, last_command, Show_Bins,
     +  Confirm_OK, output_all_failures, use_bin_codes, 
     +  reset_names_of_bins
C
C ****  Character Strings
C
      CHARACTER*10 Test_option_pct
      Character*80 Data_file,Reference_file,Outfile_Loc,Bin_code_file,
     +  Description_file
      CHARACTER*32 Bin_code_name(-1:nbins_max)
      Common/Mac_global_chrs/Test_option_pct, Data_file,Reference_file,
     +  Outfile_Loc,Bin_code_file,Description_file,Bin_code_name
C
C ****  HBOOK Access
C
      Integer NWPAWC
      Real H
      Parameter (NWPAWC = 50000)
      Common/PAWC/H(NWPAWC)
C
C ****  HBOOK parameters
C
      Character*6 topdir_ref,topdir_dat
      Parameter (topdir_ref = 'LUNREF')
      Parameter (topdir_dat = 'LUNDAT')
