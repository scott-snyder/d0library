      FUNCTION ULALPS_SUSY(Q2,SUSY_THRESH, SUSY_OFFSET)

C...Purpose: to give the value of alpha_strong.
CMRENNA
C  Modified 9 Jan 96 to include SUSY contributions to
C   the running.
C
CMRENNA
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      SAVE /LUDAT1/,/LUDAT2/

C...Constant alpha_strong trivial.
      IF(MSTU(111).LE.0) THEN
        ULALPS_SUSY=PARU(111)
        MSTU(118)=MSTU(112)
        PARU(117)=0.
        PARU(118)=PARU(111)
        RETURN
      ENDIF
CMRENNA
C.....This is the SUSY threshold in GeV**2.
      SSYTHR2=susy_thresh*susy_thresh
CMRENNA
      MSTU(114)=6

C...Find effective Q2, number of flavours and Lambda.
      Q2EFF=Q2
      IF(MSTU(115).GE.2) Q2EFF=MAX(Q2,PARU(114))
      NF=MSTU(112)
      ALAM2=PARU(112)**2
  100 IF(NF.GT.MAX(2,MSTU(113))) THEN
        Q2THR=PARU(113)*PMAS(NF,1)**2
        IF(Q2EFF.LT.Q2THR) THEN
          NF=NF-1
          ALAM2=ALAM2*(Q2THR/ALAM2)**(2./(33.-2.*NF))
          GOTO 100
        ENDIF
      ENDIF
  110 IF(NF.LT.MIN(8,MSTU(114))) THEN
        Q2THR=PARU(113)*PMAS(NF+1,1)**2
        IF(Q2EFF.GT.Q2THR) THEN
          NF=NF+1
          ALAM2=ALAM2*(ALAM2/Q2THR)**(2./(33.-2.*NF))
          GOTO 110
        ENDIF
      ENDIF
      B0=(33.-2.*NF)/6.
CMRENNA
C...Check if we exceed the SUSY threshold
      IF(SSYTHR2.LE.Q2EFF) THEN
          NFS=6.
          B0P=(27.-2.*NF-NFS)/6.
          ALAM2=ALAM2*(ALAM2/SSYTHR2)**(B0/B0P-1.0)         
          B0=B0P
      ENDIF
CMRENNA
      IF(MSTU(115).EQ.1) Q2EFF=Q2EFF+ALAM2
      PARU(117)=SQRT(ALAM2)
C...Evaluate first or second order alpha_strong.
      ALGQ=LOG(MAX(1.0001,Q2EFF/ALAM2))
      IF(MSTU(111).EQ.1) THEN
        ULALPS_SUSY=MIN(PARU(115),PARU(2)/(B0*ALGQ))
      ELSE
        B1=(153.-19.*NF)/6.
        ULALPS_SUSY=MIN(PARU(115),PARU(2)/(B0*ALGQ)*(1.-B1*LOG(ALGQ)/
     &  (B0**2*ALGQ)))
      ENDIF
      MSTU(118)=NF
      PARU(118)=ULALPS_SUSY - susy_offset
      RETURN
      END


