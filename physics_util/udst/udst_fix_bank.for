      SUBROUTINE UDST_FIX_BANK(LBANK,IPTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reconstruct full set of kinematic quantities
C-                         from Et, eta, phi.
C-
C-   Inputs  : LBANK - pointer to bank
C-             IPTR  - offset of Ex
C-
C-   Created   5-JAN-1994   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    ET,ETA,PHI
      INTEGER LBANK,IPTR

      IF(LBANK.EQ.0)THEN
        CALL ERRMSG('LINK=0','UDST_FIX_BANK','called with LBANK=0','W')
        GOTO 999
      ENDIF

      ET  = Q(LBANK+IPTR+4)
      ETA = Q(LBANK+IPTR+6)

      IF (IQ(LBANK+IPTR+7).eq.32769) THEN
        CALL ERRMSG('RESERVED OPERAND FAULT','UDST_FIX_BANK',' ','W')
        IQ(LBANK+IPTR+7)=0
        GOTO 999
      ENDIF

      PHI = Q(LBANK+IPTR+7)

      IF(Q(LBANK+IPTR)  .EQ.0) Q(LBANK+IPTR)   = ET*COS(PHI)
      IF(Q(LBANK+IPTR+1).EQ.0) Q(LBANK+IPTR+1) = ET*SIN(PHI)
      IF(Q(LBANK+IPTR+2).EQ.0) Q(LBANK+IPTR+2) = ET*SINH(ETA)
      IF(Q(LBANK+IPTR+3).EQ.0) Q(LBANK+IPTR+3) = ET*COSH(ETA)
      IF(Q(LBANK+IPTR+5).EQ.0) Q(LBANK+IPTR+5) = 2.*ATAN(EXP(-ETA))
C----------------------------------------------------------------------
  999 RETURN
      END
