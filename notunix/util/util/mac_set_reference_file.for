      SUBROUTINE MAC_SET_REFERENCE_FILE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Resets the reference file to the next parameter
C-   in the instruction file.
C-
C-   Inputs  : Next parameter
C-   Outputs : Filename set in MAC_Global
C-   Controls: 
C-
C-   Created  29-APR-1993   R. J. Genik II
C-   Updated  23-MAY-1993   R. J. Genik II  Altered error handling to
C-   ignore ierr = -99, the last command in instruction file 
C-   Updated  26-OCT-1993   R. J. Genik II  see note in MAC_SET_DATA_FILE 
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
          Ierr = 0 ! This error code is ok here
        Else
          Goto 800
        Endif
      Endif
      Call Mac_Close_Reference ! Close Old file
      Reference_file = Temp_File_Name !Set name in common block
      Call MAC_Open_Reference ! Open New File
      Write (Out_Unit,100)
  100 Format (X,' Reference file now set to : ')
      Call MAC_Echo_Filename(Reference_file,Out_unit)
  599 RETURN
C
C ****  error handling
C
  800 CALL Errmsg('Read Error','MAC_Set_Reference_File'
     +   ,'Can''t read filename, ignoring command','W')
      Return
      END
