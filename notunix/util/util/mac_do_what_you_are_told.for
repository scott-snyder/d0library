      SUBROUTINE MAC_DO_WHAT_YOU_ARE_TOLD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decodes instructions and calls appropriate
C-    subroutines. 'Guts' of the MAC (mega autocompare histograms) package.
C-
C-   Inputs  : None
C-   Outputs : Comparison outputs, errors if detected
C-   Controls: 'Instruction file' from currently picked RCP bank
C-
C-   Created  18-APR-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MAC_Global.inc'
      INCLUDE 'D0$INC:MAC_Current.inc'
      INCLUDE 'D0$INC:MAC_Commands.inc'
      INTEGER IErrorr
      Character*80 Current_command
      LOGICAL MAC_Loc_Com_Str,this_command
C----------------------------------------------------------------------
C
C ****  Set current IDX to 1, get next command, make sure
C ****  we can read it, and it says to Begin Instructions
C
      No_comps_done = 0
      Current_IDX=1
C
C ****  get first command, if not begin instructions, crash.
C
      Call MAC_Get_Next_Command(Current_Command,Ierrorr)
C                              !Increments Current_IDX
      If ((Ierrorr.NE.0).and.(Ierrorr.NE.-99))
     +   Call Errmsg('MAC Cannot read RCP file','Mega_Autocompare'
     +   ,'Aborting Execution','F')
      this_command = MAC_Loc_Com_Str(Current_Command,MAC_Begin)
      If (this_command) THEN
C
C ****  Get Next Command (GNC) If hist info, fill Mac_Current
C ****  with and start comparisons after End_Histogram
C ****  Else check what the command is, and execute it, if 
C ****  \End_instructions goto 800 and quietly return.
C ****  Continue looping until \Begin_Histogram
C
   10   Do 600 While (.not.Last_Command)
        Call MAC_Get_Next_Command(Current_Command,Ierrorr)
C                                !Increments Current_IDX
        If ((Ierrorr.NE.0).and.(Ierrorr.NE.-99))
     +     Call Errmsg('MAC Cannot read RCP file','Mega_Autocompare'
     +     ,'Aborting Execution','F')
   15   this_command = MAC_Loc_Com_Str(Current_Command,Hist_Begin)
        If (this_command) THEN
C
C ****  histogram loop begins, fill current common block
C
          Call MAC_Read_6_Hist_Params(MAC_ARRAY_Name,Current_IDX,
     +      IErrorr)
          If ((Ierrorr.NE.0).and.(Ierrorr.NE.-99))
     +       Call Errmsg('MAC Cannot read RCP file','Mega_Autocompare'
     +       ,'Aborting Execution','F')
C
C ****  Loop over remaining commands until \End_Histogram,
C ****  then do comparison.
C
   20     Call MAC_Get_Next_Command(Current_Command,Ierrorr)
C                                  !Increments Current_IDX
          If ((Ierrorr.NE.0).and.(Ierrorr.NE.-99))
     +      Call Errmsg('MAC Cannot read RCP file','Mega_Autocompare'
     +      ,'Aborting Execution','F')
          this_command = MAC_Loc_Com_Str(Current_Command,Hist_End)
          If (this_command) THEN
            Call MAC_Compare
          Else
            Call MAC_What_Now(Current_Command) ! Here we issue
                                               ! warnings if not understood,
                                               ! then skip to next command
            GOTO 20! next command w/i histogram
          Endif
C
C ****  histogram loop ends
C
C
C ****  this is the Else from not finding a '\Histogram_begin' statement.
C
        Else
          this_command = MAC_Loc_Com_Str(Current_Command,MAC_end)
          If (this_command) Goto 800! End of instructions
          Call MAC_What_Now(Current_Command)
          If (Last_Command) Goto 700! abnormal termination
        Endif
C
C ****  go get next histogram
C
  600 End do ! End of Do While not last command
C
C ****  No begin instructions command, crash now.
C
      Else 
        Call Errmsg('MAC Cannot read RCP file',
     +             'Mega_Autocompare' ,
     +'First command must \begin_instructions, Aborting Execution','F')
      Endif
C
C ****  700 is reached when last command is read before proper end of
C ****  instructions command is reached.
C
  700 Call Errmsg('Syntax Error ','Mega_Autocompare',
     +  'Logical <eof> reached before \End_Instructions','W')
      Write (out_unit,710)
  710 Format (X,'*** Abnormal Termination on <eof> error'
     +  ,/,/)
  800 Write (out_unit,900) no_comps_done,nbad_runs
  900 format (x,/,3x,'Done. I did ',I,' comparisons and found ',I,
     +  ' bad histograms')
      Call MAC_Close_Reference
      Call MAC_Close_Outfile_loc
      Call MAC_Close_Data
  999 Return
      End
