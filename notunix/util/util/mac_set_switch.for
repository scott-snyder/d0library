      SUBROUTINE MAC_SET_SWITCH(Switch,Position)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decodes Character position and sets switch true
C-   or false.
C-
C-   Inputs  :  Switch :Logical
C-              Position CHR = True or False 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-DEC-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL Switch
      CHARACTER*80 Position
      Integer begin_pos,end_pos,leng
C----------------------------------------------------------------------
      Call Swords (Position,begin_pos,end_pos,leng)
      If (Position(Begin_pos:End_Pos).eq.'.TRUE.') Then
        Switch = .true.
      Elseif (Position(Begin_pos:End_Pos).eq.'.FALSE.') then
        Switch = .false.
      Else
        Call Errmsg('Decode Failed', 'MAC_Set_Switch',
     +  'Cannot decode switch setting, ignoring change.', 'W')
      Endif
  999 RETURN
      END
