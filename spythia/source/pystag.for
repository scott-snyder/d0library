C*********************************************************************
 
      SUBROUTINE PYSTAG(X,Q2,XPGA)
 
C...Gives anomalous part of photon structure function
C...via call to Schuler routine.
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      SAVE /LUDAT1/,/LUDAT2/
      SAVE /PYPARS/,/PYINT1/
      DIMENSION XPGA(-6:6)
      DOUBLE PRECISION XX,QQ,PP,ALL,PDFF(-2:6),PDFFP(-2:6)
 
C...Check that Q2 big enough for non-vanishing structure function.
      FAC=1.
      IF(MSTP(15).EQ.1.OR.MSTP(15).EQ.3) THEN
        FAC=PARP(17)**2
      ELSEIF(MSTP(15).EQ.2.OR.MSTP(15).EQ.4) THEN
        IF(MSTP(82).LE.1) FAC=(PARP(81)/PARP(15))**2
        IF(MSTP(82).GE.2) FAC=(PARP(82)/PARP(15))**2
      ENDIF
      IF(Q2.LE.FAC*PARP(15)**2) THEN
        DO 100 KFL=-6,6
        XPGA(KFL)=0.
  100   CONTINUE
        RETURN
      ENDIF
 
C...Normal evaluation; at nominal or reduced scale.
      XX=X
      QQ=Q2
      IF(MSTP(15).EQ.1.OR.MSTP(15).EQ.2) QQ=Q2/FAC
      PP=PARP(15)**2
      ALL=PARP(1)
      NF=3
      IF(Q2.GT.PARP(16)*PMAS(4,1)**2) NF=4
      IF(Q2.GT.PARP(16)*PMAS(5,1)**2) NF=5
      CALL PYSTGS(XX,QQ,PP,ALL,NF,PDFF)
 
C...Extra evaluation in some alternatives; subtracted from above.
      IF(MSTP(15).EQ.3.OR.MSTP(15).EQ.4) THEN
        PP=QQ/FAC
        CALL PYSTGS(XX,QQ,PP,ALL,NF,PDFFP)
        DO 110 J=-2,6
        PDFF(J)=PDFF(J)-PDFFP(J)
  110   CONTINUE
      ENDIF
 
 
C...Output: u and d in right order; threshold suppression.
      XPGA(0)=X*PDFF(0)
      XPGA(1)=X*PDFF(2)
      XPGA(2)=X*PDFF(1)
      XPGA(3)=X*PDFF(3)
      XPGA(4)=X*PDFF(4)*MAX(0.,1.-PARP(16)*PMAS(4,1)**2/Q2)
      XPGA(5)=X*PDFF(5)*MAX(0.,1.-PARP(16)*PMAS(5,1)**2/Q2)
      XPGA(6)=0.
      DO 120 KFL=1,6
      XPGA(-KFL)=XPGA(KFL)
  120 CONTINUE
 
      RETURN
      END
