C&IF VAXVMS
      SUBROUTINE GETDIS6(LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3,
     *           PAR4,PAR5,PAR6)
C&ELSE
C&      SUBROUTINE GETDIS6(LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3,
C&     *           PAR4,PAR5,PAR6,par7,par8,par9,par10,par11,par12,
C&     *           par13,par14)
C&ENDIF
      IMPLICIT NONE
      CHARACTER*(*) LABELS(6)
      CHARACTER*1 TYPARR(6)
      INTEGER LIMITS(2,6)
      INTEGER PAR1,PAR2,PAR3,par4,par5,par6
C&IF VAXVMS
      call getdis (6, labels, typarr, limits, par1, par2, par3, par4,
     &     par5, par6)
C&ELSE
C&      INTEGER par7, par8, par9, par10, par11, par12, par13, par14
C&      call getdis (6, labels, typarr, limits, par1, par2, par3, par4,
C&     &     par5, par6, par7, par8, par9, par10, par11, par12, 
C&     &     par13, par14)
C&ENDIF
      return
      end
