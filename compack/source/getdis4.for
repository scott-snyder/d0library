C&IF VAXVMS
      SUBROUTINE GETDIS4(LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3,par4)
C&ELSE
C&      SUBROUTINE GETDIS4(LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3,
C&     *           PAR4,PAR5,PAR6,par7,par8,par9,par10)
C&ENDIF
      IMPLICIT NONE
      CHARACTER*(*) LABELS(4)
      CHARACTER*1 TYPARR(4)
      INTEGER LIMITS(2,4)
      INTEGER PAR1,PAR2,PAR3,par4
C&IF VAXVMS
      call getdis (4, labels, typarr, limits, par1, par2, par3, par4)
C&ELSE
C&      INTEGER par5, par6, par7, par8, par9, par10
C&      call getdis (4, labels, typarr, limits, par1, par2, par3, par4,
C&     &     par5, par6, par7, par8, par9, par10)
C&ENDIF
      return
      end
