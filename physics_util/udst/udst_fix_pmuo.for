      SUBROUTINE UDST_FIX_PMUO(LPMUO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill computed quantities for muons
C-
C-   Inputs  : LPMUO - pointer to bank
C-
C-   Created  18-NOV-1995   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    ET,ETA,PHI
      INTEGER LPMUO

      IF(LPMUO.EQ.0)THEN
        CALL ERRMSG('LPMUO=0','UDST_FIX_PMUO','called with LPMUO=0','W')
        GOTO 999
      ENDIF

      ET  = Q(LPMUO+14)
      ETA = Q(LPMUO+16)
      PHI = Q(LPMUO+17)
C... compute kinematic quantities
      Q(LPMUO+10) = ET*COS(PHI)
      Q(LPMUO+11) = ET*SIN(PHI)
      Q(LPMUO+12) = ET*SINH(ETA)
C... recover sign of charge
      IF(Q(LPMUO+13).NE.0)
     &  IQ(LPMUO+2)=INT(Q(LPMUO+13)/ABS(Q(LPMUO+13)))*14
      Q(LPMUO+13) = ABS(Q(LPMUO+13))
C----------------------------------------------------------------------
  999 RETURN
      END
