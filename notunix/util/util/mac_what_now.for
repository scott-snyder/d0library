      SUBROUTINE MAC_WHAT_NOW(Opt_Command)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Link to other commands besides standard 6
C-   parameter read in for each histogram. New commands should be entered
C-   here. Right now we chaeck for optional commands between
C-   \begin_Instructions, and \End_instructions, either within the
C-   histogram begin-end loop, or between histograms.
C-   Significance of the two placements not yet determined.
C-
C-   Inputs  : Opt_Command
C-   Outputs : Whatever the command is set up to do
C-   Controls:
C-
C-   Created  18-APR-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MAC_Commands.inc'
      INCLUDE 'D0$INC:MAC_Global.inc'
      INCLUDE 'D0$INC:MAC_Current.inc'
      Character*(*) Opt_Command
      Character*80 Dummy_Command_for_switches, 
     +  Dummy_Command_for_integers
      LOGICAL MAC_Loc_Com_Str, This_Command
      INTEGER CMD_Beg,CMD_End,CMD_Len,I_ERR, Int_Beg, Int_End, I_Type
      Real Value
C----------------------------------------------------------------------
      This_Command = MAC_Loc_Com_Str(Opt_Command,Bin_Code_Begin)
      If (This_Command) then
        Call MAC_Read_Bin_Codes
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,Set_Ref_File)
      If (This_Command) then
        Call MAC_Set_Reference_File
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,Set_Data_File)
      If (This_Command) then
        Call MAC_Set_Data_File
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,Show_Bin_Contents)
      If (This_Command) then
        Show_Bins = .true.
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,No_Show_Bin_Contents)
      If (This_Command)  then
        Show_Bins = .false.
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,Read_Bin_Code_File)
      If (This_Command) then
        Call MAC_Read_Bin_Code_File
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,Set_Output_file)
      If (This_Command) then
        Call MAC_Close_Outfile_loc
        Call MAC_Open_Outfile_Loc
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,Write_Description)
      If (This_Command) then
        Call MAC_Write_Description
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,Use_Bin_Codes_CMD)
      If (This_Command) then
        Use_Bin_Codes = .true.
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,NO_Use_Bin_Codes)
      If (This_Command) then
        Use_Bin_Codes = .false.
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,Reset_Bin_Names)
      If (This_Command) then
        Call MAC_Get_Next_Command(Dummy_Command_for_Switches,i_err)
        IF (i_err.ne.0) Call Errmsg('Read Failed',
     +  'MAC_What_Now',
     +  'Cannot Read Reset_Bin_Names setting from Instruction file','W')
        Call MAC_Set_Switch(reset_names_of_bins,
     +    Dummy_Command_for_Switches)
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,hist_Comment)
      If (This_Command) then
        Call MAC_Get_Next_Command(Current_Comment,i_err)
        IF (i_err.ne.0) Call Errmsg('Read Failed',
     +  'MAC_What_Now',
     +  'Cannot Read Comment from Instruction file','W')
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,test_option_pct_CMD)
      If (This_Command) then
        Call MAC_Get_Next_Command(test_option_pct,i_err)
        IF (i_err.ne.0) Call Errmsg('Read Failed',
     +  'MAC_What_Now',
     +  'Cannot Read test_option_pct from Instruction file','W')
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,summarize_all_CMD)
      If (This_Command) then
        Call MAC_Get_Next_Command(Dummy_Command_for_Switches,i_err)
        IF (i_err.ne.0) Call Errmsg('Read Failed',
     +  'MAC_What_Now',
     +  'Cannot Read Summarize_All setting from Instruction file','W')
        Call MAC_Set_Switch(Summarize_All,Dummy_Command_for_Switches)
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,GTUnit_ID_CMD)
      If (This_Command) then
        Call MAC_Get_Next_Command(Dummy_Command_for_Integers,i_err)
        IF (i_err.ne.0) Call Errmsg('Read Failed',
     +  'MAC_What_Now',
     +  'Cannot Read GTUNIT ID setting from Instruction file','W')
        IGTUNIT_USERID = INT(Value(Dummy_Command_for_Integers
     +    ,int_beg,int_end,I_type))
        IF (I_Type.NE.1)  ! it wasn't an integer
     +    CALL Errmsg('Decode FAILED','MAC_What_Now'
     +      ,'I was expecting an integer for the GTUNIT ID','W')
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,REF_ID_Offset_CMD)
      If (This_Command) then
        Call MAC_Get_Next_Command(Dummy_Command_for_Integers,i_err)
        IF (i_err.ne.0) Call Errmsg('Read Failed',
     +  'MAC_What_Now',
     +  'Cannot Read Ref_ID_Offset setting from Instruction file','W')
        REFERENCE_ID_OFFSET = INT(Value(Dummy_Command_for_Integers
     +    ,int_beg,int_end,I_type))
        IF (I_Type.NE.1)  ! it wasn't an integer
     +    CALL Errmsg('Decode FAILED','MAC_What_Now'
     +      ,'I was expecting an integer for the GTUNIT ID','W')
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,tmp_id_offset_CMD)
      If (This_Command) then
        Call MAC_Get_Next_Command(Dummy_Command_for_Integers,i_err)
        IF (i_err.ne.0) Call Errmsg('Read Failed',
     +  'MAC_What_Now',
     +  'Cannot Read TMP_ID_OFFSET setting from Instruction file','W')
        TMP_HIST_OFFSET = INT(Value(Dummy_Command_for_Integers
     +    ,int_beg,int_end,I_type))
        IF (I_Type.NE.1)  ! it wasn't an integer
     +    CALL Errmsg('Decode FAILED','MAC_What_Now'
     +      ,'I was expecting an integer for the TMP_ID_OFFSET','W')
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,Confirm_ok_CMD)
      If (This_Command) then
        Call MAC_Get_Next_Command(Dummy_Command_for_Switches,i_err)
        IF (i_err.ne.0) Call Errmsg('Read Failed',
     +  'MAC_What_Now',
     +  'Cannot Read Comment from Instruction file','W')
        Call MAC_Set_Switch(confirm_ok,Dummy_Command_for_Switches)
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,
     +  del_after_compare_all_CMD)
      If (This_Command) then
        Call MAC_Get_Next_Command(Dummy_Command_for_Switches,i_err)
        IF (i_err.ne.0) Call Errmsg('Read Failed',
     +  'MAC_What_Now',
     +  'Cannot Read del_after_compare from Instruction file','W')
        Call MAC_Set_Switch(del_after_compare_all, 
     +    Dummy_Command_for_Switches)
        Return
      Endif
      This_Command = MAC_Loc_Com_Str(Opt_Command,
     +  output_all_failures_CMD)
      If (This_Command) then
        Call MAC_Get_Next_Command(Dummy_Command_for_Switches,i_err)
        IF (i_err.ne.0) Call Errmsg('Read Failed',
     +  'MAC_What_Now',
     +  'Cannot Read Comment from Instruction file','W')
        Call MAC_Set_Switch(output_all_failures,
     +    Dummy_Command_for_Switches)
        Return
      Endif
C
C ****  if we got to here, we didn't find anything
C ****  Error handling: Warn user about inability to decode string, and
C ****  dump string to output file
C
          CALL Errmsg('Command Decode Failure','MAC_What_Now?'
     +      ,'Ignoring last command','W')
          Call Swords(Opt_Command,CMD_Beg,CMD_End,CMD_Len)
          Write (out_unit,100) Opt_Command(CMD_Beg:CMD_End)
  100     Format(X,' Don''t Understand :','"',A,'"')
        Return
  999 RETURN
      END
