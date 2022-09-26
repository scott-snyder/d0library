      subroutine getpar9 (prompt, typarr, par1, par2, par3, par4, par5,
     &     par6, par7, par8, par9)
      IMPLICIT NONE
      CHARACTER*(*) PROMPT(*)
      CHARACTER*1 TYPARR(*)
      INTEGER PAR1
      INTEGER PAR2
      INTEGER PAR3
      INTEGER PAR4
      INTEGER PAR5
      INTEGER PAR6
      INTEGER PAR7
      INTEGER PAR8
      INTEGER PAR9
      call getpar(5, prompt, typarr, par1, par2, par3, par4, par5, par6,
     &     par7, par8, par9)
      return
      end
