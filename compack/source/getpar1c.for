      subroutine getpar1c (prompt, typarr, par1)
      IMPLICIT NONE
      CHARACTER*(*) PROMPT(*)
      CHARACTER*1 TYPARR(*)
      character*(*) PAR1
      call getpar(1, prompt, typarr, par1)
      return
      end
