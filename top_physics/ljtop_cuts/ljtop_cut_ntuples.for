      SUBROUTINE LJTOP_CUT_NTUPLES(IDH,PJETS,PWLEP,PTOP1,PTOP2,MIS_ET,
     &      LEP_ET,LEP_ISO,DIST_EL,PWHAD,MWTR,INDX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       fill an NTUPLE for top analysis
C-
C-   Inputs  : 
C-   IDH= id of Ntuple to be filled
C-   PWLEP= 4-momenta W->leptons
C-   PJETS= 4-momenta of jets + PT
C-   PTOP1(5) = 4-vector of top with W decaying leptonically + mass
C-   PTOP2(5) =   "          "           "      hadronically + mass
C-   PWHAD(5) =   "      of W decaying hadronically + mass
C-   LEP_ET(3)= ET's of elec (1), muon (2), photon (3)
C-   LEP_ISO(2)= isolation parameter (1) (photon or el), (2) muon
C-   DIST_EL = distance of closest approach
C-   MWTR= W transverse mass
C-       INDX= ordered list for PJETS, 1=B assoc. with W->leptons
C-                                     2,3= jets from W-> hadrons
C-                                     4= B assoc. with W->hadrons
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-MAY-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDH,INDX(*),I,K
      REAL    PWLEP(5),PWHAD(7),PTOP1(5),PTOP2(5),PJETS(5,*)
      REAL    MIS_ET,LEP_ET(3),PSUM(5),ETSUM
      REAL    DIST_EL,LEP_ISO(2),MWTR
      REAL    ETB1,ETB2,ETJ1,ETJ2,ETWL,ETWH
      REAL    MWH,MTP1,MTP2,LET,EISO,MISO,DIST
      REAL    MET,ETP1,ETP2,ELET,PHET,MUET,MWT
      INTEGER NUM_NTPL
      PARAMETER (NUM_NTPL=20)
      REAL    NTPL(NUM_NTPL)
      EQUIVALENCE (ETB1,NTPL(1)),(ETB2,NTPL(2)),(ETJ1,NTPL(3))
      EQUIVALENCE (ETJ2,NTPL(4)),(ETWL,NTPL(5))
      EQUIVALENCE (ETWH,NTPL(6)),(EISO,NTPL(7)),(MISO,NTPL(8))
      EQUIVALENCE (MTP1,NTPL(9)),(MTP2,NTPL(10)),(PHET,NTPL(11))
      EQUIVALENCE (ELET,NTPL(12)),(MUET,NTPL(13)),(DIST,NTPL(14))
      EQUIVALENCE (MWT,NTPL(15)),(MWH,NTPL(16))
      EQUIVALENCE (MET,NTPL(17)),(LET,NTPL(18))
      EQUIVALENCE (ETP1,NTPL(19)),(ETP2,NTPL(20))
      CHARACTER*4 TAGS(NUM_NTPL)
      DATA TAGS/'ETB1','ETB2','ETJ1','ETJ2','ETWL','ETWH','EISO',
     &          'MISO','MTP1','MTP2','PHET','ELET','MUET',
     &          'DIST','MWT','MWH','MET','LET','ETP1','ETP2'/
C
C              NTUPLES DEFINITIONS
C  ETB1= ET OF b-jet for top1, ETB2= ET OF b-jet for top1 
C  ETJ1,ETJ2 = jets for W -> jets
C  ETWL= ET OF W -> l + nu, ETWH= ET of W-> jets
C  EISO= electron isolation, MISO= muon isolation
C  MTP1= mass of top 1, MTP2= mass of top2
C  PHET= ET of photon, ELET= ET of electron, MUET= ET of muon
C  DIST= distance closest app. of electron track
C  MWT= transverse mass of W-> l + nu, MWH= mass of W -> jets
C  MET= missing ET, LET= lepton ET
C  ETP1= ET of top1, ETP2= ET of top2
C  events with only 3 jets have have ETB2=ETP2=MTP2=-10.
C  events with no muons have MUET=-10.
C  events with no elec. have ELET=-10.
C----------------------------------------------------------------------
C
C          calculate masses and Et's
      ETB1=PJETS(5,INDX(1))
      ETWL=SQRT(PWLEP(1)**2+PWLEP(2)**2)
      MTP1=PTOP1(5)
      ETWH=SQRT(PWHAD(1)**2+PWHAD(2)**2)
      ETJ1=PJETS(5,INDX(2))
      ETJ2=PJETS(5,INDX(3))
      ETP1=SQRT(PTOP1(1)**2+PTOP1(2)**2)
      MET=MIS_ET
      ELET=LEP_ET(1)
      MUET=LEP_ET(2)
      PHET=LEP_ET(3)
      LET=AMAX1(MUET,ELET,PHET)
      MWT=MWTR
      MWH=PWHAD(5)
      DIST=DIST_EL
      EISO=LEP_ISO(1)
      MISO=LEP_ISO(2)
      ETB2=-10.
      MTP2=-10.
      ETP2=-10.
      IF(PTOP2(5).GT.0) THEN
        MTP2=PTOP2(5)
        ETB2=PJETS(5,INDX(4))
        ETP2=SQRT(PTOP2(1)**2+PTOP2(2)**2)
      ENDIF
C
      CALL HFN(IDH,NTPL)
      GOTO 999
C
C
      ENTRY LJTOP_CUT_BOOK(IDH)
C
      CALL HBOOKN(IDH,'Parton Top analysis',NUM_NTPL,' ',50000,TAGS)
C
  999 RETURN
      END
