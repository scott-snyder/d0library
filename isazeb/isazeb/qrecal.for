      SUBROUTINE QRECAL
C---------------------------------------------------------
C-
C-  Recalculate jet momenta starting from particles
C-
C-     SDP Jan.,1986, rev. Sept. 86
C-
C---------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:JETSET.INC'
      INCLUDE 'D0$INC:KEYS.INC'
      INCLUDE 'D0$INC:PARTCL.INC'
      INCLUDE 'D0$INC:PJETS.INC'
      INCLUDE 'D0$INC:PRIMAR.INC'
      INTEGER IOR,I,K,J,JET
      REAL MASQ
C
      IF(NJSET.NE.0) THEN
C
      CALL VZERO(PJSET,5*NJSET)   ! Zero parton vectors
      CALL VZERO(PJETS,5*NJET)    ! Zero primary jet vectors
C
      DO 100 I=1,NPTCL
      DO 90 J=1,4            !  protect against exact zeros
   90 IF(ABS(PPTCL(J,I)).LT..00001) PPTCL(J,I)=.00001
      IF(IORIG(I).LT.0) THEN
        IOR=IABS(IORIG(I))        
        K=MOD(IOR,IPACK)
        DO 91 J=1,4
   91   PJSET(J,K)=PJSET(J,K)+PPTCL(J,I)
        JET=IOR/IPACK
        IF(JET.LT.11.AND.JET.GT.0) THEN
          DO 92 J=1,4
   92     PJETS(J,JET)=PJETS(J,JET)+PPTCL(J,I)
        ENDIF
      ENDIF
  100 CONTINUE
C
C  Recalculate primary parton jet masses
      DO 200 I=1,NJET
      MASQ=PJETS(4,I)**2-PJETS(1,I)**2-PJETS(2,I)**2-PJETS(3,I)**2
      IF(MASQ.GT.0) PJETS(5,I)=SQRT(MASQ)
      IF(KEYS(3))  THEN    ! recalculate W vector
          DO 201 J=1,4
  201     QWJET(J)=PJETS(J,1)+PJETS(J,2)
          QWJET(5)=SQRT(QWJET(4)**2-QWJET(1)**2-QWJET(2)**2
     1    -QWJET(3)**2)
      ENDIF
  200 CONTINUE
C
C  Recalculate parton jet masses
      DO 210 I=1,NJSET
      IF(JDCAY(I).EQ.0) THEN
        MASQ=PJSET(4,I)**2-PJSET(1,I)**2-PJSET(2,I)**2-PJSET(3,I)**2
        IF(MASQ.GT.0) PJSET(5,I)=SQRT(MASQ)
      ENDIF
  210 CONTINUE
C
      ENDIF
      RETURN
      END
