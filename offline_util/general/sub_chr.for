      SUBROUTINE SUB_CHR(string,chr)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Searches string for the indicated character, if
C-   found, sutracts it from the string.
C-
C-   Inputs  : string chr*(*), chr chr*1
C-   Outputs : string with chr definitely not in it.
C-   Controls: none
C-
C-   Created   1-APR-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) string
      CHARACTER*1 chr
      INTEGER chr_there
      chr_there = Index(String,chr)
        if (chr_there.gt.1) then
          String = String(1:(chr_there-1))//String(chr_there+1:)
        elseif (chr_there.eq.1) then
          String = String(2:)
        endif
  999 RETURN
      END
