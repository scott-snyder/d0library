      SUBROUTINE MAC_WRITE_DESCRIPTION
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Echoes the contents of the file specified by the
C-   next parameter in the instruction file to the output file
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-APR-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'D0$INC:MAC_Global.inc'
      LOGICAL opened_ok
      integer ierr
C----------------------------------------------------------------------
C... put description file into output
C
      Call MAC_Get_Next_Command(description_file,IERR)
      IF (IERR.ne.0) then
        Call Errmsg('READ FAILED',
     +  'MAC_Compare',
     +  'Cannot READ description filename from MAC Instructions','W')
        goto 999
      Endif
      Call d0open (Tmp_Unit,description_file,'IF',opened_ok)
      IF (.NOT.opened_ok) Call Errmsg('D0OPEN FAILED',
     +  'MAC_Compare',
     +  'Cannot open descr_file from MAC_Compare_RCP','F')
      Call DESCR_HEADER(Tmp_Unit,OUT_UNIT)
      close (unit=Tmp_Unit)
  999 RETURN
      END
