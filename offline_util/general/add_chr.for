      SUBROUTINE ADD_CHR(string,chr)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Searches string for the indicated character, if
C-   not found, adds it to the end of the string.
C-
C-   Inputs  : string chr*(*), chr chr*1
C-   Outputs : string with chr definitely in it.
C-   Controls: none
C-
C-   Created   1-APR-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) string
      CHARACTER*1 chr
      INTEGER eye,jay,kay,chr_there
        Call huoptc(String,chr,chr_there)
        if (chr_there.ne.1) then
          Call swords(string,eye,jay,kay)
          String = String(eye:jay)//chr
        endif
  999 RETURN
      END
