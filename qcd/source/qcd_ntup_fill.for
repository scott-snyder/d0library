      FUNCTION QCD_NTUP_FILL()
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills QCD jet ntuples (designed for MDST but
C-                         of general use)
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   18-DEC-1992   Andrew G. Brandt base on J Yu's QCD_JETS
C-   Updated   25-SEP-1993   Andrew G. Brandt add extra algorithms
C-                           and update GLOB and tags
C-   Updated   14-OCT-1993   Remove SIGET
C-   Updated   16-NOV-1993   Sqrt(CAJET_MASS)!
C-   Updated   25-FEB-1994   Add extra jet energy words INCJ_CUT ALLJ_CUT
C-                           Change PNUT+Z switches to add extra words
C-   Updated   28-FEB-1994   Modify to allow GAP ntuple: GAPJ_CUT etc..
C-   Updated   09-MAR-1994   add METphi MASK2   IQQ QQQ
C-   Updated   17-MAR-1994   add  MASK2 for NNOQCD>32
C-   Updated   02-NOV-1994   convert to CW
C-   Updated   23-JAN-1995   add USER.INC
C-   Updated   31-JUL-1995   Bob Hirosky   replace qcd_mask w/ logical array
C-   Updated   02-SEP-1995   Andrew Brandt add GAP_NTUP_BOOK Remove USER.INC
C-   Updated   07-SEP-1995   Andrew Brandt remove GAP_CATD_INFO.INC
C-   Updated   13-NOV-1995   Andrew Brandt fill J's ELC words and ESCALE words
C-   Updated   17-NOV-1995   Andrew Brandt Add HCDIR call to properly fill Ntup
C-   Updated   06-DEC-1995   Andrew Brandt Add AIDA words, LUM
C-   Updated   02-MAR-1996   Andrew Brandt Add MUON words, new ELC words
C-   Updated   08-MAR-1996   Andrew Brandt Change extra vert words
C-   Updated   11-MAR-1996   Andrew Brandt Move GAP_GET_RCP to UPK_INIT
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INCLUDE 'D0$INC:QCD_JET.INC/LIST'
      INCLUDE 'D0$INC:QCD_NUT.INC/LIST'
      INCLUDE 'D0$INC:QCD_ELC.INC/LIST'
      INCLUDE 'D0$INC:QCD_PHO.INC/LIST'
      INCLUDE 'D0$INC:QCD_GLOB.INC/LIST'
      INCLUDE 'D0$INC:QCD_MUON.INC/LIST'
      INCLUDE 'D0$INC:QCD_AIDA.INC/LIST'
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
      INCLUDE 'D0$INC:QCD_JUTL_JETS.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC/LIST'
C
C
C ****  JET related variables
C
      INTEGER LISAE,GZISAE
      LOGICAL FIRST,QCD_NTUP_FILL
      INTEGER NJBAD,NALG
C
C ****  NTUPLE VARIABLES
C
      INTEGER ISTAT,I,J,IJ
      INTEGER L1TYPE
C
      LOGICAL BADCHK,ALLJ_CUT,GAP_NTUP_FILL,USER_NTUP_FILL,OK
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      QCD_NTUP_FILL=.TRUE.
C
      IF(FIRST)THEN
        FIRST = .FALSE.
C
C Set flag to kill bad jets
C
        BADCHK=.FALSE.
        IF(BADTST.EQ.2) BADCHK=.TRUE.
C
C ****  Booking ntuple for all jet or gap study
C
C
C
        CALL HROPEN(50,'NTUPLE','JET$FILE','N',8191,ISTAT)
C
        CALL HBNT(1,'QCD_NTUPLE',' ')
        CALL HBNAME(1, 'HEAD',RUNNUM,QCD_EVT_INFO_TAG)
        IF(DO_LUM) CALL HBNAME(1, 'HEAD',LUM,QCD_EVT_INFO2_TAG)
        IF(MC_DATA) CALL HBNAME(1, 'MC',WEIGHT,QCD_MC_INFO_TAG)
        CALL HBNAME(1, 'JUTL',QCDFILT,QCD_JUTL_HEAD1_TAG)
        IF(VERTWD.EQ.2)
     &    CALL HBNAME(1, 'JUTL2',NZ,QCD_JUTL_HEAD2_TAG)
        IF(GLOBWD.GE.1) CALL HBNAME(1, 'GLOB1',ETOT,QCD_GLOB1_TAG)
        IF(GLOBWD.GE.2) CALL HBNAME(1, 'GLOB2',NCDCTK,QCD_GLOB2_TAG)
        CALL HBNAME(1, 'JETS',NJETS,QCD_JETS1_TAG)
        IF(DO_FULL_JET) THEN
          CALL HBNAME(1, 'JETS',EXJ,QCD_JETS2_TAG)
          IF(ESCALE) THEN
            CALL HBNAME(1, 'JETS',ETUN,QCD_JETS3_TAG)
            IF(UPATH.EQ.'RECO') THEN
              CALL HBNAME(1, 'JETS',DEMFR,QCD_JETS5_TAG)
            END IF
          END IF
          IF(DO_ELC.AND.UPATH.EQ.'RECO') THEN
            CALL HBNAME(1, 'JETS',DJNEP,QCD_JETS6_TAG)
          END IF
        END IF
        IF(JTCSON) CALL HBNAME(1, 'JETS',ETR1,QCD_JETS4_TAG)
        IF(PNUTWD.GE.1) CALL HBNAME(1, 'NUT1',NUT_ET,QCD_NUT1_TAG)
        IF(PNUTWD.GE.2) CALL HBNAME(1, 'NUT2',NUT_E,QCD_NUT2_TAG)
        IF(MUONWD.GE.1) CALL HBNAME(1, 'MUON',NMUTOT,QCD_MUON1_TAG)
        IF(MUONWD.GE.2) CALL HBNAME(1, 'MUON',MU_EX,QCD_MUON2_TAG)
        IF(DO_ELC) THEN
          CALL HBNAME(1, 'PELC',NELEC,QCD_ELC1_TAG)
          CALL HBNAME(1, 'PELC',ELC_EM_FRAC,QCD_ELC2_TAG)
          CALL HBNAME(1, 'PELC',ELC_NCELLS,QCD_ELC3_TAG)
          CALL HBNAME(1, 'PELC',ELC_EMV_PRBXY,QCD_ELC4_TAG)
        END IF
        IF(DO_PHO) CALL HBNAME(1, 'PPHO',NPHOT,QCD_PHO_TAG)
        IF(DO_L1L2) THEN
          IF(L1WD.GT.10) THEN
            L1TYPE=L1WD-10
          ELSE
            L1TYPE=L1WD
          END IF
          IF(L1TYPE.EQ.1.OR.L1TYPE.EQ.3) THEN
            CALL HBNAME(1, 'L1TT',NTT,QCD_JUTL_JETS1_TAG)
          END IF
          IF(L1TYPE.EQ.2.OR.L1TYPE.EQ.3) THEN
            CALL HBNAME(1, 'L1LJ',NLJ,QCD_JUTL_JETS2_TAG)
          END IF
          IF(L1WD.LT.10) THEN
            CALL HBNAME(1, 'L2',NL2,QCD_JUTL_JETS3_TAG)
          END IF
        END IF
        IF(DO_AIDA) CALL HBNAME(1, 'AIDA',NAIDA,QCD_AIDA_TAG)
        IF(GAP_JET) CALL GAP_NTUP_BOOK
        IF(DO_USER_BOOK) CALL USER_BOOK
        CALL HPRNT(1)
C
C Find number of algorithms to be used
C
        NALG=NCONES
        IF(NOMERG) NALG=NALG+1
        IF(NNEIGH) NALG=NALG+1
C
C End of IF(FIRST)
C
      ENDIF
C
C ****  ISAJET information for MC
C
      IF(MC_DATA) THEN
        LISAE=GZISAE()
        WEIGHT=Q(LISAE+12)
        CROSS_SECTION=Q(LISAE+11)
      ELSE
        WEIGHT=1.
        CROSS_SECTION=0.
      ENDIF

C
C Loop over jet algorithms
C
      DO 997 J=1,NALG
C
C Set cone size to load into algorithm
C For normal cones RALG=RCONE
C
        IF(J.LE.NCONES) THEN
          RALG=RCONES(J)
        ELSE
C
C For Merge R=-.7 for NN R=-1.
C
          IF(J.EQ.NCONES+1) THEN
            IF(NOMERG) THEN
              RALG=-.7
            ELSE
              RALG=-1.
            END IF
C
C IF J=NCONES+2 then both MERGE and NN are on and NN is last
C
          ELSE
            RALG=-1.
          END IF
        END IF
C
C Call jet unpacking passing algorithm choice
C
        CALL QCD_UPK_JETS(J,BADCHK,NJBAD)
C
C If no jets for this algorithm type do not make ntuple
C
        IF(.NOT.MINB.AND.NJETS.LE.0.) GO TO 997
C
C Only fill requested number of jets above ET threshold
        NJETS=MIN(NJETS,NJREQ_ALL)
        IJ=0
        DO I=1,NJETS
          IF(ETJ(I).GT.ETMIN) IJ=IJ+1
        END DO
        NJETS=IJ
C
C Unpack Missing ET information--must be after cafix call
C
        IF (PNUTWD.GT.0)  CALL QCD_UPK_NUT
C
C If event does not pass ALL ntup user jet cuts do not make ntuple
C
        IF(ALL_JET) THEN
          OK=ALLJ_CUT()
          IF(.NOT.OK) GO TO 997
        END IF
C
C Get GAP info if requested and passes GAPJ_CUT
C
        IF(GAP_JET) THEN
          OK=GAP_NTUP_FILL()
          IF(.NOT.OK) GO TO 997
        END IF
C
C Allow for user words
C
        IF(DO_USER_FILL) THEN
          OK=USER_NTUP_FILL()
          IF(.NOT.OK) GO TO 997
        END IF
C
C Reset directory and fill Ntuple
C
        CALL HCDIR('//NTUPLE',' ')
        CALL HFNT(1)
C
C End of algorithm loop
C
  997 CONTINUE
C
  999 RETURN
      END
