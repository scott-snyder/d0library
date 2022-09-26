C&IF VAXVMS
      SUBROUTINE GETDIS5(LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3,par4,par5)
C&ELSE
C&      SUBROUTINE GETDIS5(LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3,
C&     *           PAR4,PAR5,PAR6,par7,par8,par9,par10,par11,par12)
C&ENDIF
      IMPLICIT NONE
      CHARACTER*(*) LABELS(5)
      CHARACTER*1 TYPARR(5)
      INTEGER LIMITS(2,5)
      INTEGER PAR1,PAR2,PAR3,par4,par5
C&IF VAXVMS
      call getdis (5, labels, typarr, limits, par1, par2, par3, par4,
     &     par5)
C&ELSE
C&      INTEGER par6, par7, par8, par9, par10, par11, par12
C&      call getdis (5, labels, typarr, limits, par1, par2, par3, par4,
C&     &     par5, par6, par7, par8, par9, par10, par11, par12)
C&ENDIF
      return
      end
