      FUNCTION QCD_UPK_MUONS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get muon information for QCD ntuple maker
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   29-FEB-1996   Andrew G. Brandt  based on Dylan/Jamal's MUONS.FOR
C-   Updated   05-MAR-1996   Andrew G. Brandt  include MDST path
C-   Updated   11-MAR-1996   Andrew G. Brandt  fix IOFF bug
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPMUO.LINK'
      INCLUDE 'D0$INC:QCD_MUON.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INTEGER LPARH,GZPARH,LPMUO
      INTEGER NS,LMUOT,SORTBANKS
      INTEGER NUM_MUON_CUT
      PARAMETER (NUM_MUON_CUT=5)
      INTEGER LMDST,GZMDST,NREP,IOFF,NMLOOP,I
      LOGICAL QCD_UPK_MUONS
C----------------------------------------------------------------------
      QCD_UPK_MUONS = .TRUE.
C
C Intialize
C
      NMUTOT = 0
      NMUON  = 0
      DO I=1,NUM_MUON_CUT
        MU_EX(I) = -999.
        MU_EY(I) = -999.
        MU_EZ(I) = -999.
        MU_ETOT(I) = -999.
        MU_ET(I) = -999.
        MU_THETA(I) = -999.
        MU_ETA(I) = -999.
        MU_PHI(I) = -999.
        MU_SIGN(I) = -999
        MU_NCDTR(I) = -999
        MU_QUAD(I) = -999
        MU_FLGWRD4(I) = -999
        MU_CHISQ(I) = -999.
        MU_T0(I) = -999.
        MU_ISO1NN(I) = -999.
        MU_ISO2NN(I) = -999.
        MU_ELOSS(I) = -999.
        MU_EOBS_2N(I) = -999.
        MU_EOBS_C4(I) = -999.
        MU_EOBS_C6(I) = -999.
        MU_DPHI(I) = -999.
        MU_DTHETA(I) = -999.
        MU_CLEAN_STAT(I) = -999
        MU_CLEAN_STAT(I) = -999
        MU_TRKHITS(I) = -999
        MU_TRKFITHITS(I) = -999
        MU_SCNT_TOF(I) = -999.
        MU_SCNT_TOF_EXP(I) = -999.
        MU_IVERT(I) = -999
        MU_IMP_BV(I) = -999.
        MU_IMP_NBV(I) = -999.
        MU_EM1NN(I) = -999.
        MU_EM2NN(I) = -999.
        MU_ETOT1NN(I) = -999.
        MU_FRACT(I) = -999.
        MU_HFRACT(I) = -999.
        MU_EFRACT_H1(I) = -999.
        MU_FLGWRD1(I) = -999
        MU_FLGWRD2(I) = -999
        MU_FLGWRD3(I) = -999
        MU_XCOS(I) = -999.
        MU_YCOS(I) = -999.
        MU_BDL(I) = -999.
      END DO
C
C MDST path
C
      IF(UPATH.EQ.'MDST') THEN
        LMDST = GZMDST()
        IF (LMDST .LE. 0) THEN
          CALL ERRMSG('QCD_UPK_MUONS','QCD_UPK_MUONS',
     &    'NO MDST BANK FOUND','W')
          GO TO 999
        ENDIF
        NMUTOT=IQ(LMDST+6)
        NMLOOP=MIN(NMUTOT,NUM_MUON_CUT)
        NREP=IQ(LMDST+5)
        DO NMUON=1,NMLOOP
          IOFF=IQ(LMDST+7)+(NMUON-1)*NREP-1
          LPMUO = LMDST + IOFF
C
C Fill info as in Jamal's routine (except IQ becomes NINT(Q())
C
          MU_EX(NMUON)      = Q(LPMUO+10)
          MU_EY(NMUON)      = Q(LPMUO+11)
          MU_EZ(NMUON)      = Q(LPMUO+12)
          MU_ETOT(NMUON)    = Q(LPMUO+13)
          MU_ET(NMUON)      = Q(LPMUO+14)
          MU_THETA(NMUON)   = Q(LPMUO+15)
          MU_ETA(NMUON)     = Q(LPMUO+16)
          MU_PHI(NMUON)     = Q(LPMUO+17)
          MU_SIGN(NMUON)    = NINT(Q(LPMUO+2)) !sign (14=mu-,-14=mu+)
          MU_NCDTR(NMUON)   = NINT(Q(LPMUO+6)) !number of cd trks in cone
          MU_QUAD(NMUON)    = NINT(Q(LPMUO+7)) !quad
          MU_FLGWRD4(NMUON) = NINT(Q(LPMUO+9)) !quality flag
          MU_CHISQ(NMUON)   = Q(LPMUO+23)   !chi^2/degree of freedom
          MU_T0(NMUON)      = Q(LPMUO+24)   !floated t0 offset (ns)
          MU_ISO1NN(NMUON)  = Q(LPMUO+29)   !iso based on cells hit+1 nbr
          MU_ISO2NN(NMUON)  = Q(LPMUO+30)   !iso based on cells hit+2 nbrs
          MU_ELOSS(NMUON)   = Q(LPMUO+33)   !E loss expected in CAL
          MU_EOBS_2N(NMUON) = Q(LPMUO+34)   !E obs in cal (cells+2 nbrs)
          MU_EOBS_C4(NMUON) = Q(LPMUO+35)   !E obs in cal (in cone 0.4)
          MU_EOBS_C6(NMUON) = Q(LPMUO+36)   !E obs in cal (in cone 0.6)
          MU_DPHI(NMUON)    = Q(LPMUO+38)   !D_phi (deg)
          MU_DTHETA(NMUON)  = Q(LPMUO+39)   !D_theta (deg)
          MU_SCNT_TOF(NMUON)= Q(LPMUO+52)   !scint tof
          MU_SCNT_TOF_EXP(NMUON)= Q(LPMUO+53)   !expected scint tof
          MU_IMP_BV(NMUON)  = Q(LPMUO+56)   !impact param, bend view, mu only
          MU_IMP_NBV(NMUON) = Q(LPMUO+57)   !imp param, nonbend view, mu only
          MU_EM1NN(NMUON)   = Q(LPMUO+79)   !EM nrg; hit cells+1 nbr
          MU_EM2NN(NMUON)   = Q(LPMUO+80)   !EM nrg; hit cells+2 nbrs
          MU_ETOT1NN(NMUON) = Q(LPMUO+84)   !E tot in CAL; hit cells+1 nbr
          MU_FRACT(NMUON)   = Q(LPMUO+93)   !frac of layers in MTC
          MU_HFRACT(NMUON)  = Q(LPMUO+94)   !frac of had layers hit in 3x3
          MU_EFRACT_H1(NMUON)=Q(LPMUO+98)   !frac of tot cal E in last layer
C
C Several IQ words not filled properly in current MDST
C
C          MU_CLEAN_STAT(NMUON)  = IQ(LPMUO+45)  !CLEANMU status word
C          MU_TRKHITS(NMUON) = IQ(LPMUO+46)  !hits on track, A,B,C
C          MU_TRKFITHITS(NMUON) = IQ(LPMUO+47)  !hits in track fit, A,B,C
C          MU_IVERT(NMUON)   = IQ(LPMUO+54)   !Vertex used (index)
C also MUOT words are not filled  FLAGWD1-3,X+YCOS,BDL
        END DO
        NMUON=NMLOOP
        GO TO 999
      ENDIF
C
C Standrd PMUO code for non-MDST path
C
C... Get link to PMUO bank and sort by Et using Marc Paterno's routine
      LPARH=GZPARH()
      LPMUO = LQ(LPARH-IZPMUO)        !Get location of PMUO bank
      LPMUO = SORTBANKS(LPMUO,14,'D','F') !D for descending; F for fltn pt
C
C... Loop through muon banks and pick up mu's
      IF(LPMUO.NE.0) THEN
        DO WHILE (LPMUO.GT.0)
          NMUTOT = NMUTOT+1
C
C... Fill only the number of muons requested
          IF (NMUTOT.LE.NUM_MUON_CUT) THEN
            NMUON = NMUTOT
C
C... Get info from PMUO
            MU_EX(NMUON)      = Q(LPMUO+10)
            MU_EY(NMUON)      = Q(LPMUO+11)
            MU_EZ(NMUON)      = Q(LPMUO+12)
            MU_ETOT(NMUON)    = Q(LPMUO+13)
            MU_ET(NMUON)      = Q(LPMUO+14)
            MU_THETA(NMUON)   = Q(LPMUO+15)
            MU_ETA(NMUON)     = Q(LPMUO+16)
            MU_PHI(NMUON)     = Q(LPMUO+17)
C
            MU_SIGN(NMUON)    = IQ(LPMUO+2)   !sign (14=mu-,-14=mu+)
            MU_NCDTR(NMUON)   = IQ(LPMUO+6)   !number of cd trks in cone
            MU_QUAD(NMUON)    = IQ(LPMUO+7)   !quad
            MU_FLGWRD4(NMUON) = IQ(LPMUO+9)   !quality flag
            MU_CHISQ(NMUON)   = Q(LPMUO+23)   !chi^2/degree of freedom
            MU_T0(NMUON)      = Q(LPMUO+24)   !floated t0 offset (ns)
            MU_ISO1NN(NMUON)  = Q(LPMUO+29)   !iso based on cells hit+1 nbr
            MU_ISO2NN(NMUON)  = Q(LPMUO+30)   !iso based on cells hit+2 nbrs
            MU_ELOSS(NMUON)   = Q(LPMUO+33)   !E loss expected in CAL
            MU_EOBS_2N(NMUON) = Q(LPMUO+34)   !E obs in cal (cells+2 nbrs)
            MU_EOBS_C4(NMUON) = Q(LPMUO+35)   !E obs in cal (in cone 0.4)
            MU_EOBS_C6(NMUON) = Q(LPMUO+36)   !E obs in cal (in cone 0.6)
            MU_DPHI(NMUON)    = Q(LPMUO+38)   !D_phi (deg)
            MU_DTHETA(NMUON)  = Q(LPMUO+39)   !D_theta (deg)
            MU_CLEAN_STAT(NMUON)  = IQ(LPMUO+45)  !CLEANMU status word
            MU_TRKHITS(NMUON) = IQ(LPMUO+46)  !hits on track, A,B,C
            MU_TRKFITHITS(NMUON) = IQ(LPMUO+47)  !hits in track fit, A,B,C
            MU_SCNT_TOF(NMUON)= Q(LPMUO+52)   !scint tof
            MU_SCNT_TOF_EXP(NMUON)= Q(LPMUO+53)   !expected scint tof
            MU_IVERT(NMUON)   = IQ(LPMUO+54)   !Vertex used (index)
            MU_IMP_BV(NMUON)  = Q(LPMUO+56)   !impact param, bend view, mu only
            MU_IMP_NBV(NMUON) = Q(LPMUO+57)   !imp param, nonbend view, mu only
            MU_EM1NN(NMUON)   = Q(LPMUO+79)   !EM nrg; hit cells+1 nbr
            MU_EM2NN(NMUON)   = Q(LPMUO+80)   !EM nrg; hit cells+2 nbrs
            MU_ETOT1NN(NMUON) = Q(LPMUO+84)   !E tot in CAL; hit cells+1 nbr
            MU_FRACT(NMUON)   = Q(LPMUO+93)   !frac of layers in MTC
            MU_HFRACT(NMUON)  = Q(LPMUO+94)   !frac of had layers hit in 3x3
            MU_EFRACT_H1(NMUON)=Q(LPMUO+98)   !frac of tot cal E in last layer
C
C... Get link from MUOT; write out some of the words
            NS = IQ(LPMUO-2)            !Get NS value for muon bank
            LMUOT = LQ(LPMUO-NS-1)      !Get location of MUOT bank
C
            MU_FLGWRD1(NMUON) = IQ(LMUOT+4)    !Flag word 1
            MU_FLGWRD2(NMUON) = IQ(LMUOT+5)    !Flag word 2
            MU_FLGWRD3(NMUON) = IQ(LMUOT+6)    !Flag word 3
            MU_XCOS(NMUON)    = Q(LMUOT+14)    !x dir. cosine
            MU_YCOS(NMUON)    = Q(LMUOT+15)    !y dir. cosine
            MU_BDL(NMUON)     = Q(LMUOT+22)    !BDL used in MUPQCK for |P| calc
C
          ENDIF   !end selection of desired number of muons
C
C... Goto next PMUO
          LPMUO = LQ(LPMUO)
        ENDDO
C
      ENDIF
C
  999 RETURN
      END


