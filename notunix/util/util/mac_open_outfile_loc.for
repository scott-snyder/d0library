      SUBROUTINE MAC_OPEN_OUTFILE_LOC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Opend output file for MAC using DZero standards.
C-
C-            Entry MAC_CLOSE_OUTFILE_LOC - releases the unit number
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-APR-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      integer ierr
      INCLUDE 'D0$INC:MAC_Global.inc'
      LOGICAL first,opened_ok
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
C... open output file
C
      IF( first ) THEN
        first = .false.
      Call gtunit(Igtunit_userid, OUT_UNIT,IERR)
      if (ierr.ne.0) Call Errmsg('GTUNIT failed',
     +  'MAC_Compare',
     +  'CANNOT get unit for output file','F')
      ELSE ! We are Changing the output file from the instruction file;
           ! therefore, we have to get the filename.
        Call MAC_Get_Next_Command(OUTFILE_LOC,ierr)
        IF (ierr.ne.0) Call Errmsg('Read Failed',
     +  'MAC_Compare',
     +  'Cannot Read output file name from Instruction file','F')
      ENDIF
C
      Call d0open (OUT_unit,OUTFILE_LOC,'OF',opened_ok)
      IF (.NOT.opened_ok) Call Errmsg('D0OPEN FAILED',
     +  'MAC_Compare',
     +  'Cannot open OUTFILE_LOC from MAC RCP','F')
C
C ****  Set the default HBOOK output device to the outfile unit number
C
      Call Houtpu(Out_Unit)
      Return
C
      Entry MAC_CLOSE_OUTFILE_LOC
      Close (UNIT=OUT_UNIT)
  999 RETURN
      END
