      SUBROUTINE UDST_FIX_PNUT(LPNUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : complete PNUT bank
C-
C-   Inputs  : LPNUT
C-
C-   Created   7-JAN-1994   Sailesh Chopra
C-   Updated  23-JAN-1994   Ulrich Heintz  include z-component
C-   Updated   8-AUG-1994   Ian Adam  add check on Et>0 before atan2
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    ET,EZ,PHI
      INTEGER LPNUT,IPTR
      IF(LPNUT.EQ.0)THEN
        CALL ERRMSG('LPNUT=0','UDST_FIX_PNUT',' ','W')
        GOTO 999
      ENDIF
      ET  = Q(LPNUT+7)
      EZ  = Q(LPNUT+5)

      IF (IQ(LPNUT+10).eq.32769) THEN
        CALL ERRMSG('RESERVED OPERAND FAULT','UDST_FIX_BANK',' ','W')
        IQ(LPNUT+10)=0
        GOTO 999
      ENDIF

      PHI = Q(LPNUT+10)

      IF(Q(LPNUT+3).EQ.0) Q(LPNUT+3) = ET*COS(PHI)  ! x-component
      IF(Q(LPNUT+4).EQ.0) Q(LPNUT+4) = ET*SIN(PHI)  ! y-component

      IF (ET.GT.0) THEN
        IF(Q(LPNUT+9).EQ.0.)Q(LPNUT+9) = -LOG(TAN(ATAN2(ET,EZ)/2)) ! eta
        IF(Q(LPNUT+6).EQ.0) Q(LPNUT+6) = SQRT(ET**2+EZ**2)  ! energy
        IF(Q(LPNUT+8).EQ.0) Q(LPNUT+8) = ATAN2(ET,EZ)       ! theta
      ELSE
        CALL ERRMSG('PNUT ET ZERO','UDST_FIX_PNUT',' ','W')
        Q(LPNUT+9) = 0.0
        Q(LPNUT+6) = 0.0
        Q(LPNUT+8) = 0.0
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
