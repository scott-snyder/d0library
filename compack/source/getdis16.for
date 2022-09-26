C&IF VAXVMS
      SUBROUTINE GETDIS16(LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3,
     *           PAR4,PAR5,PAR6,PAR7,PAR8,PAR9,PAR10,
     *           PAR11,PAR12,PAR13,PAR14,PAR15,PAR16)
C&ELSE
C&      SUBROUTINE GETDIS6(LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3,
C&     *           PAR4,PAR5,PAR6,par7,par8,par9,par10,par11,par12,
C&     *           par13,par14,par15,par16,par17,par18,par19,par20,
C&     *           par21,par22,par23,par24,par25,par26,par27,par28,
C&     *           par29,par30,par31,par32,par33,par34)
C&ENDIF
      IMPLICIT NONE
      CHARACTER*(*) LABELS(16)
      CHARACTER*1 TYPARR(16)
      INTEGER LIMITS(2,16)
      INTEGER PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,PAR7
      INTEGER PAR8,PAR9,PAR10,PAR11,PAR12,PAR13
      INTEGER PAR14,PAR15,PAR16
C&IF VAXVMS
      call getdis (16, labels, typarr, limits, par1, par2, par3, par4,
     &     par5, par6, par7, par8, par9, par10, par11, par12, par13,
     &     par14, par15, par16)
C&ELSE
C&      INTEGER par17, par18, par19, par20, par21, par22, par23, par24,
C&     *           par25,par26,par27,par28,par29,par30,par31,par32,
C&     *           par33,par34
C&      call getdis (16, labels, typarr, limits, par1, par2, par3, par4,
C&     *           PAR5,PAR6,par7,par8,par9,par10,par11,par12,
C&     *           par13,par14,par15,par16,par17,par18,par19,
C&     *           par20,par21,par22,par23,par24,par25,par26,
C&     *           par27,par28,par29,par30,par31,par32,par33,par34)
C&ENDIF
      return
      end
