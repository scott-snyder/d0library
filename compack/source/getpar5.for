      subroutine getpar5 (prompt, typarr, par1, par2, par3, par4, par5)
      IMPLICIT NONE
      CHARACTER*(*) PROMPT(*)
      CHARACTER*1 TYPARR(*)
      INTEGER PAR1
      INTEGER PAR2
      INTEGER PAR3
      INTEGER PAR4
      INTEGER PAR5
      call getpar(5, prompt, typarr, par1, par2, par3, par4, par5)
      return
      end
