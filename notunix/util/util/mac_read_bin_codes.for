      SUBROUTINE mac_read_bin_codes
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads bin codes from Instruction file.
C-
C-  Note: Not Easy to debug or change....
C-
C-   Inputs  : Instruction file text.
C-   Outputs : Fills BIN_CODE_NAME array with info
C-   Controls:
C-
C-   Created  21-APR-1993   R. J. Genik II
C-   Updated  23-MAY-1993   R. J. Genik II  for beta release 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MAC_Global.inc'
      INCLUDE 'D0$INC:MAC_Commands.inc'
      INTEGER ierr,current_code,eye,jay,kay
      REAL    Value
      LOGICAL tmp_logical,mac_loc_com_str
      CHARACTER*80 tmp_string,tmp_code
C----------------------------------------------------------------------
    5 CALL mac_get_next_command(tmp_code,ierr)! increments Current_IDX
      If (Ierr.eq.-99) Goto 700 ! eof detected
      IF (ierr.NE.0) Goto 800! we are having problems
      tmp_logical = mac_loc_com_str(tmp_code,'\END')
      IF (tmp_logical) Goto 599 ! Found the end, return
      current_code = int(value(tmp_code,eye,jay,kay))
      IF (kay.NE.1) THEN ! it wasn't an integer
        CALL errmsg('EZGETS FAILED','MAC_READ_Bin_Codes'
     +      ,'I was expecting an integer for the Bin Code #','W')
        Goto 800! be very unforgiving
      ENDIF
      Call Mac_Get_Next_Command(Tmp_string,ierr)
      If (Ierr.eq.-99) Goto 700 ! eof detected
      IF (ierr.NE.0) Goto 800! we are having problems
C
C ****  All is ok now, set the name
C
      IF (Current_Code.GT.nbins_max) then
        Call Errmsg('READ FAILED', 'MAC_Read_Bin_Codes',
     +'integer from MAC Instructions GT NBins_Max' ,'W')
      ELSEIF (Current_Code.LT.-1) then
      Call Errmsg('READ FAILED', 'MAC_Read_Bin_Codes',
     +'integer from MAC Instructions LT -1' ,'W')
      ELSE 
        Call Swords(Tmp_string,eye,jay,kay)
        If (kay.gt.32) then
          BIN_CODE_NAME(Current_Code) = Tmp_string(eye:(eye+31))
        Else
          BIN_CODE_NAME(Current_Code) = Tmp_string
        Endif
      ENDIF
      Goto 5 ! there were not enough Goto's
  599 RETURN
C
C ****  error handling <eof> detected
C
  700 CALL errmsg('READ FAILED','MAC_READ_BIN_CODES'
     +      ,'Logical <eof> detected, Aborting execution','F')
      Goto 999 ! even though we issued a fatal error, someone might setup
               ! things later to change that.
C
C ****  severe error handling
C
  800 current_idx = current_idx - 1 !jump back one to avoid special cases
      CALL errmsg('EZGETS FAILED','MAC_READ_BIN_CODES'
     +      ,'I''ll skip down to next \End_xxx','W')
  810 CALL mac_get_next_command(tmp_string,ierr)
      If (ierr.eq.-99) goto 700
      IF (ierr.NE.0) ! we are REALLY having problems, crash here
     +   CALL errmsg('EZGETS FAILED','MAC_READ_BIN_CODES'
     +      ,'Can''t read instructions, Aborting execution','F')
      tmp_logical = mac_loc_com_str(tmp_string,'\END')
      IF (.NOT.tmp_logical) Goto 810
  999 Return
      END
