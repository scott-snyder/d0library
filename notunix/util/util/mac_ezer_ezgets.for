      SUBROUTINE MAC_EZER_EZGETS( ezParameter, ezindex, 
     +    ezstring, ez_routine, ezIERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Just calls ezgets and checks for errors
C-                         and calls ezerr with a W if found
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-APR-1993   R. J. Genik II
C-   Updated  23-MAY-1993   R. J. Genik II  for beta release of MAC, calls
C-   mac_get_next_command instead of ezgets. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ezParameter, ezstring, ez_routine
      Character*32 up_ezParameter, tmp_string
      INTEGER ezindex,str_beg,str_end,str_len,ezierr,ier
      CHARACTER*132 error_mess
C----------------------------------------------------------------------
      Call Upcase (ezParameter, up_ezParameter)
      Call Swords (up_ezParameter, str_beg, str_end, str_len)
      Call Mac_Get_Next_Command(ezstring,ier)
      IF ((IER.NE.0).and.(ier.ne.-99)) then
         Write( error_mess,FMT=1) ezParameter(str_beg:str_end),ezindex
    1    Format(' CANNOT READ',A,' IDX = ',I)
         Call Swords (error_mess, str_beg, str_end, str_len)
         Call Errmsg('READ FAILED',ez_routine
     +    ,TMP_STRING(str_beg:str_end),'W')
         ezIERR = -1
      ElseIf (Ier.eq.-99) then ! eof detected next
         Write( error_mess,FMT=2) ezParameter(str_beg:str_end),ezindex
         Call Swords (error_mess, str_beg, str_end, str_len)
         Call Errmsg('READ Warning',ez_routine
     +    ,TMP_STRING(str_beg:str_end),'W')
         ezIERR = -1
    2    Format(' Last Command in file detected : ',A,' IDX = ',I)
      Else
        ezierr = 0
      Endif
  999 RETURN
      END
