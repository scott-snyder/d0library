      SUBROUTINE MAC_SET_DATA_FILE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Resets the data file to the next parameter
C-   in the instruction file.
C-
C-   Inputs  : Next parameter
C-   Outputs : Filename set in MAC_Global
C-   Controls: 
C-
C-   Created  29-APR-1993   R. J. Genik II
C-   Updated  23-MAY-1993   R. J. Genik II  Altered error handling to
C-   ignore ierr = -99, the last command in instruction file
C-   Updated  26-OCT-1993   R. J. Genik II  I don't remember why I did the
C-   above, but I must have run into a special case and besides, this error
C-   condition will be taken care of by a higher level routine. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MAC_Global.inc'
      CHARACTER*80 Temp_File_Name
      INTEGER IERR
C----------------------------------------------------------------------
      Call MAC_Get_Next_Command(Temp_File_Name,Ierr)
      IF (Ierr.ne.0) Then
        If (Ierr.eq.-99) then
          Ierr = 0 ! This error code is ok here. uh why? This means it is
                   ! the last command in the instruction file.
        Else
          Goto 800
        Endif
      Endif
      Call Mac_Close_Data ! Close Old file
      Data_file = Temp_File_Name !Set name in common block
      Call MAC_Open_Data ! Open New File
      Write (Out_Unit,100)
  100 Format (X,' Data file now set to : ')
      Call MAC_Echo_Filename(Data_file,Out_unit)
  599 RETURN
C
C ****  error handling
C
  800 CALL Errmsg('Read Error','MAC_Set_Data_File'
     +   ,'Can''t read filename, ignoring command','W')
      Return
      END
