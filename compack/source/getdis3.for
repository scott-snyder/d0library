C&IF VAXVMS
      SUBROUTINE GETDIS3(LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3)
C&ELSE
C&      SUBROUTINE GETDIS3(LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3,
C&     *           PAR4,PAR5,PAR6,par7,par8)
C&ENDIF
      IMPLICIT NONE
      CHARACTER*(*) LABELS(3)
      CHARACTER*1 TYPARR(3)
      INTEGER LIMITS(2,3)
      INTEGER PAR1,PAR2,PAR3
C&IF VAXVMS
      call getdis (3, labels, typarr, limits, par1, par2, par3)
C&ELSE
C&      INTEGER par4, par5, par6, par7, par8
C&      call getdis (3, labels, typarr, limits, par1, par2, par3, par4,
C&     &     par5, par6, par7, par8)
C&ENDIF
      return
      end
