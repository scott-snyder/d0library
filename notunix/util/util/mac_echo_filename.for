      SUBROUTINE MAC_ECHO_FILENAME(file_name,unit_number)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Echoes the passed filename to the unit number
C-   passed. If the name is a DCL logical, it translates the logical name
C-   and writes out the translation. It is intended that this routine
C-   access none of the MAC common blocks. There is a limit of 255
C-   characters on the logical name, which is the current limit on VMS.
C-   Also a limit of 255 characters is imposed by VMS and observed here.
C-
C-   Inputs  : file_name - name of file or logical name
C-             unit_number - unit number of output file.
C-   Outputs : See above
C-   Controls: none
C-
C-   Created   6-DEC-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER beg_chr,end_chr,name_length,log_or_not,TRNLNM,unit_number
      CHARACTER*(*) file_name
      CHARACTER*255 logtrans
C----------------------------------------------------------------------
      Call Swords(file_name,beg_chr,end_chr,name_length)
      log_or_not = TRNLNM(file_name(beg_chr:end_chr),logtrans,
     +  name_length)
      if (log_or_not) then
        Call Swords(logtrans,beg_chr,end_chr,name_length)
        Write(unit_number,100) logtrans(beg_chr:end_chr)
      Else
        Write(unit_number,100) file_name(beg_chr:end_chr)
  100   format (4X,A,/)      
      Endif
  999 RETURN
      END
