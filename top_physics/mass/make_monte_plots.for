      SUBROUTINE MAKE_MONTE_PLOTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make Monte Carlo plots
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNS.INC'
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:EVENT_QUAN1.INC'
      INTEGER LISAE,LISAQ,LISAL,LISAJ
      INTEGER GZISAL,GZISAQ,GZISAJ
      EQUIVALENCE (LISAE,CSTLNK(LNKMX)),(LISAQ,CSTLNK(LNKMX-1))
      EQUIVALENCE (LISAL,CSTLNK(LNKMX-2)),(LISAJ,CSTLNK(LNKMX-3))
C
      INTEGER IER
      LOGICAL MONTE_CARLO_DATA
      CHARACTER*8 LABEL,NAME
      REAL    TOP(8),TBAR(8),TSUM(8)
      REAL    KT,KT2
      INTEGER I
C
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------

      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL DO_HBOOK('MONTE_HISTS')
        CALL EZRSET
      ENDIF
C
      IF ( COMBNUM.GT.1.0 ) THEN
        RETURN
      ENDIF
      IF ( .NOT.MONTE_CARLO_DATA() ) RETURN
C
C ****  HERE ONCE PER EVENT
C
      CALL UZERO(TOP,1,4)
      CALL UZERO(TBAR,1,4)
C
      LISAJ = GZISAJ()
      DO WHILE (LISAJ.NE.0)
        NAME=LABEL(IQ(LISAJ+1))
        IF ( NAME(1:2).EQ.'TP' ) THEN
C TOP QUARK 4 VECTOR
          CALL UCOPY(Q(LISAJ+2),TOP,8)
        ELSEIF ( NAME(1:2).EQ.'TB' ) THEN
C ANTI-TOP QUARK 4 VECTOR
          CALL UCOPY(Q(LISAJ+2),TBAR,8)
        ENDIF
        LISAJ = LQ(LISAJ)
      ENDDO
C
      IF ( TOP(4).EQ.0.0.OR.TBAR(4).EQ.0.0 ) THEN
        CALL ERRMSG('TOP_MASS','MAKE_MONTE_PLOTS',
     &    'TOP AND TOPBAR NOT PRESENT IN MC DATA','W')
        RETURN
      ENDIF
C
      DO I = 1 , 4
        TSUM(I) = TOP(I)+TBAR(I)
      ENDDO
C
      KT2 = TSUM(1)**2+TSUM(2)**2
      KT=SQRT(KT2)
      CALL DO_HF1(601,KT,1.0)
      CALL DO_HF1(602,KT2,1.0)
C
  999 RETURN
      END
