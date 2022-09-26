      subroutine getpar1 (prompt, typarr, par1)
      IMPLICIT NONE
      CHARACTER*(*) PROMPT(*)
      CHARACTER*1 TYPARR(*)
      INTEGER PAR1
      call getpar(1, prompt, typarr, par1)
      return
      end
