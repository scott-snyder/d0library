C&IF VAXVMS
      SUBROUTINE GETDIS2(LABELS,TYPARR,LIMITS,PAR1,PAR2)
C&ELSE
C&      SUBROUTINE GETDIS2(LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3,
C&     *           PAR4,PAR5,PAR6)
C&ENDIF
      IMPLICIT NONE
      CHARACTER*(*) LABELS(2)
      CHARACTER*1 TYPARR(2)
      INTEGER LIMITS(2,2)
      INTEGER PAR1,PAR2
C&IF VAXVMS
      call getdis (2, labels, typarr, limits, par1, par2)
C&ELSE
C&      INTEGER par3, par4, par5, par6
C&      call getdis (2, labels, typarr, limits, par1, par2, par3, par4,
C&     &     par5, par6)
C&ENDIF
      return
      end
