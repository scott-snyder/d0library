C*********************************************************************
 
      SUBROUTINE PYSTHG(KF,X,Q2,P2,ALAM,XPGA)
C...Purpose: to evaluate the structure function of a photon which
C...branched at a scale P2 and then evolved homogeneously to Q2.
C...KF=0 gives a mixture of allowed flavours at photon branching,
C...else the specific flavour.
C...ALAM is the 4-flavour Lambda, which is automatically converted
C...to 3- and 5-flavour equivalents as needed.
      COMMON/PYINT1/MINT(400),VINT(400)
      SAVE /PYINT1/
      DIMENSION XPGA(-6:6)
      DATA PMC/1.3/, PMB/4.6/
 
C...Reset output.
      DO 100 KFL=-6,6
      XPGA(KFL)=0.
  100 CONTINUE
      KFA=IABS(KF)
 
C...Read out or pick original quark flavour of photon branching.
      IF(KFA.EQ.0.OR.KFA.GT.5) THEN
  110   RKF=11.*RLU(0)
        KFA=1
        IF(RKF.GT.1.) KFA=2
        IF(RKF.GT.5.) KFA=3
        IF(RKF.GT.6.) KFA=4
        IF(RKF.GT.10.) KFA=5
        IF(KFA.EQ.4.AND.Q2.LT.PMC**2) GOTO 110
        IF(KFA.EQ.5.AND.Q2.LT.PMB**2) GOTO 110
      ENDIF
 
C...Calculate Lambda; protect against unphysical Q2 and P2 input.
      ALAM3=ALAM*(PMC/ALAM)**(2./27.)
      ALAM5=ALAM*(ALAM/PMB)**(2./23.)
      P2EFF=MAX(P2,1.2*ALAM3**2)
      IF(KFA.EQ.4) P2EFF=MAX(P2EFF,PMC**2)
      IF(KFA.EQ.5) P2EFF=MAX(P2EFF,PMB**2)
      Q2EFF=MAX(Q2,P2EFF)
      VINT(231)=P2EFF
 
C...Find number of flavours at lower and upper scale.
      NFP=4
      IF(P2EFF.LT.PMC**2) NFP=3
      IF(P2EFF.GT.PMB**2) NFP=5
      NFQ=4
      IF(Q2EFF.LT.PMC**2) NFQ=3
      IF(Q2EFF.GT.PMB**2) NFQ=5
 
C...Find s as sum of 3-, 4- and 5-flavour parts.
      S=0.
      IF(NFP.EQ.3) THEN
        Q2DIV=PMC**2
        IF(NFQ.EQ.3) Q2DIV=Q2EFF
        S=S+(6./27.)*LOG(LOG(Q2DIV/ALAM3**2)/LOG(P2EFF/ALAM3**2))
      ENDIF
      IF(NFP.LE.4.AND.NFQ.GE.4) THEN
        P2DIV=P2EFF
        IF(NFP.EQ.3) P2DIV=PMC**2
        Q2DIV=Q2EFF
        IF(NFQ.EQ.5) Q2DIV=PMB**2
        S=S+(6./25.)*LOG(LOG(Q2DIV/ALAM**2)/LOG(P2DIV/ALAM**2))
      ENDIF
      IF(NFQ.EQ.5) THEN
        P2DIV=PMB**2
        IF(NFP.EQ.5) P2DIV=P2EFF
        S=S+(6./23.)*LOG(LOG(Q2EFF/ALAM5**2)/LOG(P2DIV/ALAM5**2))
      ENDIF
 
C...Evaluate structure functions below or above threshold.
      IF((KFA.EQ.4.AND.Q2.LT.PMC**2).OR.(KFA.EQ.5.AND.Q2.LT.PMB**2).OR.
     &Q2.LE.P2) THEN
        XVAL=X*1.5*(X**2+(1.-X)**2)
        XGLU=0.
        XSEA=0.
      ELSE
        XL=-LOG(X)
        XVAL= (1.5/(1.-0.197*S+4.33*S**2)*X**2 + (1.5+2.10*S)/
     &  (1.+3.29*S)*(1.-X)**2 + 5.23*S/(1.+1.17*S+19.9*S**3)*X*(1.-X))*
     &  X**(1./(1.+1.5*S)) * (1.-X**2)**(8.*S/3.)
        XGLU= 4.*S/(1.+4.76*S+15.2*S**2+29.3*S**4) *
     &  X**(-2.03*S/(1.+2.44*S)) * ((1.-X)*XL)**(4.*S/3.) *
     &  ((4.*X**2+7.*X+4.)*(1.-X)/3. - 2.*X*(1.+X)*XL)
        XSEA= S**2/(1.+4.54*S+8.19*S**2+8.05*S**3) *
     &  X**(-1.54*S/(1.+1.29*S)) * (1.-X)**(8.*S/3.) *
     &  ((8.-73.*X+62.*X**2)*(1.-X)/9. + (3.-8.*X**2/3.)*X*XL +
     &  (2.*X-1.)*X*XL**2)
      ENDIF
 
C...Fill structure functions; threshold factors for c and b sea.
      SLL=LOG(LOG(Q2EFF/ALAM**2)/LOG(P2EFF/ALAM**2))
      XPGA(0)=XGLU
      XPGA(1)=XSEA
      XPGA(2)=XSEA
      XPGA(3)=XSEA
      IF(Q2.GT.PMC**2) THEN
        SCH=MAX(0.,LOG(LOG(PMC**2/ALAM**2)/LOG(P2EFF/ALAM**2)))
        XPGA(4)=XSEA*(1.-(SCH/SLL)**2)
      ENDIF
      IF(Q2.GT.PMB**2) THEN
        SBT=MAX(0.,LOG(LOG(PMB**2/ALAM**2)/LOG(P2EFF/ALAM**2)))
        XPGA(5)=XSEA*(1.-(SBT/SLL)**2)
      ENDIF
      XPGA(KFA)=XPGA(KFA)+XVAL
      DO 120 KFL=1,5
      XPGA(-KFL)=XPGA(KFL)
  120 CONTINUE
 
      RETURN
      END
