      SUBROUTINE L2_COMP_MUOT(LUN,LMUOT,IB,OKOK,
     &  MUOT_XIN,MUOT_YIN,MUOT_ZIN,MUOT_XOUT,MUOT_YOUT,MUOT_ZOUT,
     &  MUOT_XCIN,MUOT_YCIN,MUOT_ZCIN,MUOT_CHB,MUOT_CHNB,
     &  MUOT_XCOUT,MUOT_YCOUT,MUOT_ZCOUT,MUOT_BDL,MUOT_P,MUOT_DP,
     &  MUOT_ECAL,MUOT_EFE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-        compare MUOT banks 
C
C     grabs info on the MUOT bank 
C
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-APR-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:HEADER.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER LMUOT(*),IB,LUN
      LOGICAL OKOK
C
      INTEGER NCAND1,NCAND2,NREP1,NREP2,IP1,IP2,I,J,NRUN,NEV
      INTEGER LMUOT1,LMUOT2
      INTEGER NWID1,NWID2,NS1,NS2,NQ1,NQ2
      INTEGER FW41,FW42,FW51,FW52,FW61,FW62,FW71,FW72
      LOGICAL LNWID,LNS,LNQ,LFW4,LFW5,LFW6,LFW7
      LOGICAL LXIN,LYIN,LZIN,LXOUT,LYOUT,LZOUT
      LOGICAL LXCIN,LYCIN,LZCIN,LCHB,LCHNB
      LOGICAL LXCOUT,LYCOUT,LZCOUT,LBDL,LP,LDP
      LOGICAL LECAL,LEFE
      LOGICAL LDUMP,L2_NXT_REAL_DIFF,L2_NXT_INT_DIFF
      REAL MUOT_XIN,MUOT_YIN,MUOT_ZIN,MUOT_XOUT,MUOT_YOUT,MUOT_ZOUT
      REAL MUOT_XCIN,MUOT_YCIN,MUOT_ZCIN,MUOT_CHB,MUOT_CHNB
      REAL MUOT_XCOUT,MUOT_YCOUT,MUOT_ZCOUT,MUOT_BDL,MUOT_P,MUOT_DP
      REAL MUOT_ECAL,MUOT_EFE
      REAL XIN1,XIN2,YIN1,YIN2,ZIN1,ZIN2,XOUT1,XOUT2,YOUT1,YOUT2
      REAL ZOUT1,ZOUT2,XCIN1,XCIN2,YCIN1,YCIN2,ZCIN1,ZCIN2
      REAL CHB1,CHB2,CHNB1,CHNB2,XCOUT1,XCOUT2,YCOUT1,YCOUT2
      REAL ZCOUT1,ZCOUT2,BDL1,BDL2,P1,P2,DP1,DP2,ECAL1,ECAL2,EFE1,EFE2
      CHARACTER*1 CNWID,CNS,CNQ,CFW4,CFW5,CFW6,CFW7
      CHARACTER*1 CXIN,CYIN,CZIN,CXOUT,CYOUT,CZOUT
      CHARACTER*1 CXCIN,CYCIN,CZCIN,CCHB,CCHNB
      CHARACTER*1 CXCOUT,CYCOUT,CZCOUT,CBDL,CP,CDP
      CHARACTER*1 CECAL,CEFE
C
C----------------------------------------------------------------------
C
      OKOK = .TRUE.
      NRUN = IQ(LHEAD+6)
      NEV = IQ(LHEAD+9)
C
      IF (IB.NE.2) THEN
        CALL ERRMSG('L2_COMPARE','L2_COMP_MUOT',
     &    'L2_COMP_MUOT called for IB.NE.2 ! Not allowed!!!','F')
        RETURN
      ENDIF
C
C     make sure there is MUOT - no mistakes
C
      IF (LMUOT(1).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_MUOT: Run/Event '',2I7,
     &      '' has NO MUOT bank for SIMULATION'')') NRUN,NEV
        GOTO 999
      ENDIF
      IF (LMUOT(2).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_MUOT: Run/Event '',2I7,
     &      '' has NO MUOT bank for DATA'')') NRUN,NEV
        GOTO 999
      ENDIF
C
C     loop over banks in the chain
C
      NCAND1 = 0
      LMUOT1 = LMUOT(1)
      DO WHILE (LMUOT1.GT.0)
        NCAND1 = NCAND1 + 1
        LMUOT1 = LQ(LMUOT1)
      ENDDO
      NCAND2 = 0
      LMUOT2 = LMUOT(2)
      DO WHILE (LMUOT2.GT.0)
        NCAND2 = NCAND2 + 1
        LMUOT2 = LQ(LMUOT2)
      ENDDO
      IF (NCAND1.NE.NCAND2) THEN
        WRITE(LUN,'('' L2_COMP_MUOT: Run/Event '',2I7,
     &      '' MUOT discrepancy in #Candidates (S,D): '',2I6)')
     &      NRUN,NEV,NCAND1,NCAND2
        GOTO 999
      ENDIF
C
C     go back and check bank by bank - same number of candidates
C     in both streams so we can just use one of the pointers as 
C     the DO-loop index
C
      LMUOT1 = LMUOT(1)
      LMUOT2 = LMUOT(2)
      DO WHILE (LMUOT1.GT.0)
        IP1 = LMUOT1    !note LINEAR CHAIN - reset IP1/2
        IP2 = LMUOT2
        LNWID     = L2_NXT_INT_DIFF(IP1,IP2,NWID1,NWID2,CNWID)
        LNS       = L2_NXT_INT_DIFF(IP1,IP2,NS1,NS2,CNS)
        LNQ       = L2_NXT_INT_DIFF(IP1,IP2,NQ1,NQ2,CNQ)
        LFW4      = L2_NXT_INT_DIFF(IP1,IP2,FW41,FW42,CFW4)
        LFW5      = L2_NXT_INT_DIFF(IP1,IP2,FW51,FW52,CFW5)
        LFW6      = L2_NXT_INT_DIFF(IP1,IP2,FW61,FW62,CFW6)
        LFW7      = L2_NXT_INT_DIFF(IP1,IP2,FW71,FW72,CFW7)
        LXIN      = L2_NXT_REAL_DIFF(IP1,IP2,XIN1,XIN2,MUOT_XIN,CXIN)
        LYIN      = L2_NXT_REAL_DIFF(IP1,IP2,YIN1,YIN2,MUOT_YIN,CYIN)
        LZIN      = L2_NXT_REAL_DIFF(IP1,IP2,ZIN1,ZIN2,MUOT_ZIN,CZIN)
        LXOUT     = L2_NXT_REAL_DIFF(IP1,IP2,XOUT1,XOUT2,MUOT_XOUT,
     &              CXOUT)
        LYOUT     = L2_NXT_REAL_DIFF(IP1,IP2,YOUT1,YOUT2,MUOT_YOUT,
     &              CYOUT)
        LZOUT     = L2_NXT_REAL_DIFF(IP1,IP2,ZOUT1,ZOUT2,MUOT_ZOUT,
     &              CZOUT)
        LXCIN     = L2_NXT_REAL_DIFF(IP1,IP2,XCIN1,XCIN2,MUOT_XCIN,
     &              CXCIN)
        LYCIN     = L2_NXT_REAL_DIFF(IP1,IP2,YCIN1,YCIN2,MUOT_YCIN,
     &              CYCIN)
        LZCIN     = L2_NXT_REAL_DIFF(IP1,IP2,ZCIN1,ZCIN2,MUOT_ZCIN,
     &              CZCIN)
        LXCOUT    = L2_NXT_REAL_DIFF(IP1,IP2,XCOUT1,XCOUT2,MUOT_XCOUT,
     &              CXCOUT)
        LYCOUT    = L2_NXT_REAL_DIFF(IP1,IP2,YCOUT1,YCOUT2,MUOT_YCOUT,
     &              CYCOUT)
        LZCOUT    = L2_NXT_REAL_DIFF(IP1,IP2,ZCOUT1,ZCOUT2,MUOT_ZCOUT,
     &              CZCOUT)
        LCHB      = L2_NXT_REAL_DIFF(IP1,IP2,CHB1,CHB2,MUOT_CHB,CCHB)
        LCHNB     = L2_NXT_REAL_DIFF(IP1,IP2,CHNB1,CHNB2,MUOT_CHNB,
     &              CCHNB)
        LBDL      = L2_NXT_REAL_DIFF(IP1,IP2,BDL1,BDL2,MUOT_BDL,CBDL)
        LP        = L2_NXT_REAL_DIFF(IP1,IP2,P1,P2,MUOT_P,CP)
        LDP       = L2_NXT_REAL_DIFF(IP1,IP2,DP1,DP2,MUOT_DP,CDP)
        LECAL     = L2_NXT_REAL_DIFF(IP1,IP2,ECAL1,ECAL2,MUOT_ECAL,
     &              CECAL)
        LEFE      = L2_NXT_REAL_DIFF(IP1,IP2,EFE1,EFE2,MUOT_EFE,CEFE)
        IP1 = IP1 + 1     !skip spare word 27
        IP2 = IP2 + 1     !skip spare word 27
C
        LDUMP = LNWID.OR.LNS.OR.LNQ.OR.LFW4.OR.LFW5.OR.LFW6.OR.LFW7.OR.
     &    LXIN.OR.LYIN.OR.LZIN.OR.LXOUT.OR.LYOUT.OR.LZOUT.OR.LXCIN.OR.
     &    LYCIN.OR.LZCIN.OR.LCHB.OR.LCHNB.OR.LXCOUT.OR.
     &    LYCOUT.OR.LZCOUT.OR.LBDL.OR.LP.OR.LDP.OR.LECAL.OR.LEFE
        IF (LDUMP) THEN
          OKOK = .FALSE.
          WRITE(LUN,'(/,'' L2_COMP_MUOT:  RUN/EVENT '',2I7,
     &        '' MUOT entry discrepancy: Bank # '',I4,/,
     &        '' "*" denote variables whose differences is '',
     &        ''outside of tolerances'')') NRUN,NEV,IQ(LMUOT1-5)
          WRITE(LUN,'(
     &      ''      HITS     -------FLAGS------    ''
     &      '' ------POSITION IN---------'',
     &      '' --------POSITION OUT------'',/,
     &      ''      W   S  Q    #1   #2   #3   #4        X        Y''
     &      ''        Z,         X        Y        Z'')')
          WRITE(LUN,'(''SIM '',2(I3,A1),I2,A1,1X,4(I4,A1),1X,
     &      6(F8.2,A1))')
     &      NWID1,CNWID,NS1,CNS,NQ1,CNQ,FW41,CFW4,FW51,CFW5,FW61,CFW6,
     &      FW71,CFW7,XIN1,CXIN,YIN1,CYIN,ZIN1,CZIN,
     &      XOUT1,CXOUT,YOUT1,CYOUT,ZOUT1,CZOUT
          WRITE(LUN,'(''DAT '',2(I3,A1),I2,A1,1X,4(I4,A1),1X,
     &      6(F8.2,A1))')
     &      NWID2,CNWID,NS2,CNS,NQ2,CNQ,FW42,CFW4,FW52,CFW5,FW62,CFW6,
     &      FW72,CFW7,XIN2,CXIN,YIN2,CYIN,ZIN2,CZIN,
     &      XOUT2,CXOUT,YOUT2,CYOUT,ZOUT2,CZOUT
          WRITE(LUN,'(
     &      ''     --COSINES (IN)--  --COSINES(OUT)--  --CHI**2---'',
     &      ''                   --ELOSS-'',/,
     &      ''       X     Y     Z     X     Y     Z   BEND NOBEND'',
     &      ''   BDL      P    DP  CAL   FE'')')
          WRITE(LUN,'(''SIM '',6(F5.2,A1),F5.1,A1,F6.1,A1,F5.1,A1,
     &      F7.1,A1,F4.1,A1,2(F4.1,A1))')
     &      XCIN1,CXCIN,YCIN1,CYCIN,ZCIN1,CZCIN,
     &      XCOUT1,CXCOUT,YCOUT1,CYCOUT,ZCOUT1,CZCOUT,CHB1,CCHB,
     &      CHNB1,CCHNB,BDL1,CBDL,P1,CP,DP1,CDP,ECAL1,CECAL,EFE1,CEFE
          WRITE(LUN,'(''DAT '',6(F5.2,A1),F5.1,A1,F6.1,A1,F5.1,A1,
     &      F7.1,A1,F4.1,A1,2(F4.1,A1))')
     &      XCIN2,CXCIN,YCIN2,CYCIN,ZCIN2,CZCIN,
     &      XCOUT2,CXCOUT,YCOUT2,CYCOUT,ZCOUT2,CZCOUT,CHB2,CCHB,
     &      CHNB2,CCHNB,BDL2,CBDL,P2,CP,DP2,CDP,ECAL2,CECAL,EFE2,CEFE
        ENDIF
C
C       and on to the next one
C
        LMUOT1 = LQ(LMUOT1)
        LMUOT2 = LQ(LMUOT2)
      ENDDO
C
      RETURN
C
  999 CONTINUE
C
C     things are amiss
C
      OKOK = .FALSE.
      RETURN
C
      END
