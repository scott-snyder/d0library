      SUBROUTINE MAC_GET_NEXT_COMMAND(New_com,I_ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets next command from 'Instructions'
C-
C-   Inputs  : None
C-   Outputs : New_Com the next command string
C-             Also increments Current_IDX via ez_get_next_value_type
C-   Controls: 
C-
C-   Created  18-APR-1993   R. J. Genik II
C-   Updated  18-MAY-1993   R. J. Genik II   
C-      Changed to EZGET_NEXT_VALUE_TYPE for beta release
C-      previous working version is ;5
C----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'D0$INC:MAC_Global.inc'
      Include 'D0$INC:MAC_Current.inc'
      Include 'D0$INC:MAC_Commands.inc'
      Integer I_ERR,eye_err, I_type, I_lval
      CHARACTER*80 Temp_New_Com,New_com, last_com, tmp_chr
      real val_r
      logical val_l
      integer val_i, eye, jay, kay
      Equivalence (val_r,val_i,val_l)
      LOGICAL first
      SAVE first, last_com
      DATA first / .true. /
C---------------------------------------------------------------------- 
      IF( first ) THEN
        first = .false.
        last_command = .false. ! initialize this variable here
        Last_Com = 'No Previous Command, First Call to routine'
      ENDIF
C
      Call CFill(' ',New_Com,1,80) ! Initializes New_Com to blanks
      Call CFill(' ',temp_New_Com,1,80) ! Initializes to blanks
      Call EZGET_NEXT_VALUE_TYPE (MAC_Array_name,val_i,temp_new_com,
     +  I_Type, I_Lval, eye_err, Current_IDX)
        IF (eye_ERR.NE.0) then
          If (eye_err.eq.1) then ! it is the last instruction, set i_err to
                                 ! -99 for for this, and proceed with
                                 ! decoding. set Last command flag
            I_err = -99
            Last_Command = .true.
          Else
            Call Errmsg('READ FAILED','MAC_GET_NEXT_COMMAND'
     +       ,'Cannot Read Next Command','W')
            Call Swords(Last_Com, eye, jay, kay)
            Write (Tmp_chr,10) Last_Com(eye:jay)
   10       Format (X,'Last Command Read :',A)
            Call Errmsg('Read Failure Info','MAC_GET_NEXT_COMMAND', 
     +        tmp_chr, 'W')
            I_err = eye_err
            Return ! severe error in reading command
          Endif
        Else
           I_ERR = 0 ! Command read ok, go on to decoding
        Endif
C
C ****  decoding of returned values, return a character data type of what
C ****  was in the storage location
C
        If (I_type.ge.10) then ! it was a character
          New_com = temp_new_com
        Elseif (I_Type.eq.1) then ! it was an integer
          Call VnumI(1,val_i,' ',' ',' ',New_com,I_lval)
        Elseif ((I_Type.eq.2).or.(I_Type.eq.3)) then ! it was real
          Call VnumR(1,val_r,' ',' ',' ',New_com,I_lval)
        Else ! it was a logical, set by force
          If (val_l) then
            New_com = '.TRUE.'
          else
            New_com = '.FALSE.'
          endif
        Endif
      Last_Com = New_Com ! used in dumping errors
  999 RETURN
      END
