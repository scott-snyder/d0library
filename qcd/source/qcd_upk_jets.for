      SUBROUTINE QCD_UPK_JETS(ALGNUM,BADCHK,NJBAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack jets from MDST
C-
C-   Returned value  :
C-   Inputs  : ALGNUM= choice of algorithm 1-NCONES correspond to RCONES
C-                     next MERGE then NN
C-             BADCHK  if true remove bad jets from list
C-   Outputs : NJBAD= number of bad jets for event
C-             Filled QCD_JETS bank
C-   Controls:
C-
C-   Created   20-DEC-1992   Andrew G. Brandt
C-   Updated   06-MAY-1993   Andrew G. Brandt add ESCALE correction
C-   Updated   07-JUL-1993   Andrew G. Brandt fix BADTST=2 bug
C-   Updated   29-JUL-1993   Andrew G. Brandt switch from JET_CAL to
C-                           JET_CORRECTION
C-   Updated   25-SEP-1993   Andrew G. Brandt add extra algorithms
C-   Updated   28-SEP-1993   Andrew G. Brandt fix index bug
C-   Updated   21-OCT-1993   Andrew G. Brandt use standard GTJETS and JTCS
C-   Updated   12-NOV-1993   Andrew G. Brandt protect for NJETS=0
C-   Updated   16-NOV-1993   Andrew G. Brandt Use ZEV instead of TRK_Z
C-   Updated   03-DEC-1993   Bob Hirosky Add RCP file for user TEMPLATE defntn
C-   Updated   27-JAN-1994   Andrew G. Brandt add correct JTCS code
C-   Updated   23-FEB-1994   Andrew G. Brandt add CAFIX and new ESCALE jet wds
C-   Updated   09-MAR-1994   Andrew G. Brandt replace OLDEMF w/EMF force LJETS=0
C-   Updated   02-NOV-1994   Andrew G. Brandt CW can set ZOFFA+B in QJC RCP
C-   Updated   13-FEB-1995   Andrew G. Brandt Undo CAFIX for multiple algos
C_                                            generalize NN template, fill N1
C-   Updated   30-AUG-1995   Bob Hirosky   Add seed and precluster values
C-   Updated   07-NOV-1995   Andrew G. Brandt Update Q_J_C_2 for CAFIX V5
C-             save ETLO,HI,UN JNEP wd and DEMF,DICF,DCHF
C-   Updated   06-DEC-1995   Andrew G. Brandt Move CAFIX_BEGIN to QCD_UPK_MAIN
C-                                            CALL EZRSET
C-   Updated  21-DEC-1995   Bob Hirosky   Add Jet Quality Flag
C-   Updated  21-JAN-1996   Bob Hirosky   modify Jet Quality Flag
C-   Updated   4-FEB-1996   Bob Hirosky   use L0 VTX for qual flag if QNT_DATA
C-   Updated   5-MAR-1996   Andrew Brandt QNT_DATA switch to QCD_UPK_JUTL_HEAD
C-   Updated  13-MAR-1996   Andrew Brandt add GTJETS_2E to get vtx/jet assoc
C-             use L0 for num interaction correction of Flag=0 or 1 +GAP trigger
C-   Updated  29-MAR-1996   Andrew Brandt allow D0 angles (-.7) low seed (.73)
C-                                        intialize
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QCD_JET.INC/LIST'
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
C
      INTEGER NJBAD,BAD_FLAG
      LOGICAL BADCHK
C
      INTEGER NUM_JETS,IER
      INTEGER IJET,JJET,INDJ(MAX_JET)
C
      INTEGER I
C
      INTEGER IJ,IVERS,ISPL
      REAL    E(7),ICDFR,CHFR,HOTR
      REAL    THETA,PHI,ETA
      REAL    ZOFFA,ZOFFB
C
      REAL    UN_EXJ(MAX_JET),UN_EYJ(MAX_JET),UN_EZJ(MAX_JET),
     +        UN_ENJ(MAX_JET),UN_ETJ(MAX_JET),
     +        UN_PHIJ(MAX_JET),UN_ETAJ(MAX_JET),
     +        UN_WIDJ(MAX_JET),UN_EMFRJ(MAX_JET),UN_IFLAGJ(MAX_JET),
     +        UN_NCELLJ(MAX_JET),UN_ICDFRJ(MAX_JET),UN_CHFRJ(MAX_JET),
     +        UN_CELFRJ(MAX_JET),UN_ETANNU(MAX_JET,10),
     +        UN_JN90(MAX_JET),UN_JNTRK(MAX_JET),
     +        UN_SEEDJ(MAX_JET),UN_PRECLUJ(MAX_JET),
     +        UN_ETUN(MAX_JET),UN_ETLO(MAX_JET),UN_ETHI(MAX_JET),
     +        UN_DEMFR(MAX_JET),UN_DCHFR(MAX_JET),UN_DICFR(MAX_JET),
     +        UN_DJNEP(MAX_JET),UN_NTKV1(MAX_JET),UN_NTKV2(MAX_JET)
C
      REAL SEED,PRECLU,JUL1,JUL2,JUL3
      REAL DEMF,DCHF,DICF,ERLO,ERHI
      REAL ET_JNEP
      INTEGER NV1,NV2
      LOGICAL QCD_CHK_JET_QUAL
C
      REAL J1ETA,J1PHI,J1ET,ETRING(20)
      INTEGER NJ,NSUB,ISYS
      LOGICAL JTCS_GOOD,JTCSDO,JTCSIT,FOUND,OK
      INTEGER INDJET(MAX_JET),INDJTC(MAX_JET),II
      INTEGER KJET,TRANS(MAX_JET,2),NJOLD
      REAL ETJALL(MAX_JET),ETJTCS(MAX_JET)
      REAL CONESI
      INTEGER N1,N90,NCDC
      REAL    TEMPLATE(20)
      CHARACTER*16 ALGTYP(2)
      INTEGER ALGNUM,ALGN,ATYP
      DATA ALGTYP/'CONE_JET','NN_JET'/
      REAL DUMMY,CORRECT_JETS_USER_MITOOL
C
C: Escale variables
C
      REAL ERAT
      REAL EJET,ETJET,ETAJET,PHIJET,EMF,SIZ,ICDF,CHF,CONER
      REAL ENEW,ETNEW,ETANEW
      REAL ZEV
      INTEGER LJETS, GZJETS
      LOGICAL CAFIX,OWRITE
      DATA OWRITE /.TRUE./
C
C: NOTE!!! Run 1A offset is 9 cm  0 for 1B
C
      DATA ZOFF /-9.,0./
C
C: should add logicals to RCP
C
      LOGICAL DO_ZCOR,DO_UDVCOR,DO_OUTCOR
C
      INTEGER NTEMPLATE
      REAL UTEMPLATE(20)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
      IF(FIRST)THEN
C
C Get CAFIX RCP values for ESCALE correction
C
        CALL EZPICK('CAFIX_RCP')
        CALL EZGET_l('DO_ZSP_CORRECTION', DO_ZCOR, IER )
        IF ( IER.LT.0) CALL ERRMSG('QCD_UPK_JETS','CAFIX_RCP',
     &    ' DO_ZSP_CORRECTION not found ','F')
        CALL EZGET_l('DO_UND_CORRECTION', DO_UDVCOR, IER )
        IF ( IER.LT.0) CALL ERRMSG('QCD_UPK_JETS','CAFIX_RCP',
     &    ' DO_UND_CORRECTION not found ','F')
        CALL EZGET_l('DO_CONE_CORRECTION', DO_OUTCOR, IER )
        IF ( IER.LT.0) CALL ERRMSG('QCD_UPK_JETS','CAFIX_RCP',
     &    ' DO_CONE_CORRECTION not found ','F')
C
C Get QCD_UPK_JETS RCP values--only need for non-standard threshold
C
        CALL INRCP('QCD_UPK_JETS_RCP',IER)
        IF(IER.NE.0) CALL ERRMSG('QCD_UPK_JETS_RCP','QCD_UPK_JETS_RCP',
     &    'RCP FILE NOT PRESENT - DEFAULT VALUES USED','W')
        CALL EZPICK('QCD_UPK_JETS_RCP')
C
        CALL EZGET_i('NTEMPLATE', NTEMPLATE,IER)
        IF ( IER.LT.0) THEN
          CALL ERRMSG('QCD_UPK_JETS_RCP','NTEMPLATE',
     &      'NO VARIABLE FOUND','W')
          NTEMPLATE=0
        END IF
C
        CALL EZGET_rarr('UTEMPLATE', UTEMPLATE,IER)
        IF ( IER.LT.0) THEN
          CALL ERRMSG('QCD_UPK_JETS_RCP','UTEMPLATE',
     &      'NO VARIABLE FOUND','W')
        END IF
C
        CALL EZGET('Z_OFFSET_1A', ZOFFA,IER)
        IF ( IER.LT.0) THEN
          CALL ERRMSG('QCD_UPK_JETS_RCP','Z_OFFSET_1A',
     &      'NO VARIABLE FOUND--USE default ','W')
        ELSE
          ZOFF(1)=ZOFFA
        END IF
C
        CALL EZGET('Z_OFFSET_1B', ZOFFB,IER)
        IF ( IER.LT.0) THEN
          CALL ERRMSG('QCD_UPK_JETS_RCP','Z_OFFSET_1B',
     &      'NO VARIABLE FOUND--USE default ','W')
        ELSE
          ZOFF(2)=ZOFFB
        END IF
        CALL EZRSET
C
        FIRST = .FALSE.
C
      ENDIF
C
      CALL VZERO_i(TRANS,MAX_JET*2)
      CALL VZERO_i(INDJ,MAX_JET)
C
C Initialize
C
      NJETS=0
      DO IJET=1,MAX_JET
        EXJ(IJET)=-999.
        EYJ(IJET)=-999.
        EZJ(IJET)=-999.
        ENJ(IJET)=-999.
        ETJ(IJET)   =-999.
        PHIJ(IJET)  =-999.
        ETAJ(IJET)  =-999.
        WIDJ(IJET)  =-999.
        EMFRJ(IJET) =-999.
        IFLAGJ(IJET)=0
        NCELLJ(IJET)=-1
        ICDFRJ(IJET)=-999.
        CHFRJ(IJET) =-999.
        CELFRJ(IJET)=-999.
        JN90(IJET)  =-1
        JNTRK(IJET) =-1
        SEEDJ(IJET) =-999.
        PRECLUJ(IJET)=-999.
        NTKV1(IJET)=-1
        NTKV2(IJET)=-1
        GOOD_JET(IJET) =0
        ETUN(IJET)   =-999.
        ETLO(IJET)   =-999.
        ETHI(IJET)   =-999.
        DEMFR(IJET) =-999.
        DCHFR(IJET) =-999.
        DICFR(IJET) =-999.
        DJNEP(IJET) =-999.
      END DO
C
C Choice of jet-finding algorithm
C
      ALGN=ALGNUM
      CONER=ABS(RCONES(ALGN))
C
C Set algorithm choice
C
      CALL VZERO(TEMPLATE,20)
      IF(ALGN.LE.NCONES) THEN
        ATYP=1
C
C New low seed algo  6th word =0.7 cone 10th word =0.3 seed 12th word = 3.0
C Precluster; Need 2 words to specify this algorithm
C
        IF(RCONES(ALGN).GT..72.AND.RCONES(ALGN).LT..74) THEN
          TEMPLATE(1)=2.
          TEMPLATE(2)=6.
          TEMPLATE(3)=0.7
          TEMPLATE(4)=10.
          TEMPLATE(5)=0.3
        ELSE
          TEMPLATE(1)=1.
          TEMPLATE(2)=6.
          TEMPLATE(3)=RCONES(ALGN)
        END IF
C
C OVERWRITE W/ DEFAULTS FROM RCP FILE IF GIVEN
C
        DO I =1,NTEMPLATE
          TEMPLATE(I) = UTEMPLATE(I)
        ENDDO
      ELSE
C
C Merge or NN
C
        IF(ALGN.EQ.NCONES+1) THEN
          IF(NOMERG) THEN
            ATYP=1
            TEMPLATE(1)=2.
            TEMPLATE(2)=6.
            TEMPLATE(3)=.7
            TEMPLATE(4)=8.
            TEMPLATE(5)=1.0
          ELSE
            ATYP=2
            TEMPLATE(1)=0.
          END IF
C
C IF ALGN=NCONES+2 then both MERGE and NN are on and NN is last
C
        ELSE
          ATYP=2
          TEMPLATE(1)=0.
        END IF
      END IF
C
      CALL SET_CAPH(ALGTYP(ATYP),TEMPLATE,IER)
      IF(IER.NE.0) THEN
        CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +              'SET_CAPH failed','W')
        GO TO 998
      END IF
C
C  For more than one algorithm undo CAFIX since it will be called again
C
      IF(ALGN.GT.1.AND.ESCALE.AND.UPATH.EQ.'RECO') THEN
        LJETS=GZJETS()
        DO WHILE(LJETS.GT.0)
          CALL UNCORRECT_JETS_BANK(LJETS,OWRITE,IER)
          IF ( IER.LT.0)  CALL ERRMSG('QCD_UPK_JETS',
     &         'UNCORRECT_JETS_BANK','Error ','W')
          LJETS=LQ(LJETS)
        END DO
      END IF
C
C Get number of jets
C
      CALL GTJETS_TOTAL(NUM_JETS,IER)
C
C Check for weird number of jets
C
      IF(IER.NE.0) THEN
        CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +              'No jets bank found','W')
        GO TO 998
      END IF
      IF(NUM_JETS.LE.0) THEN
        CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +              'No jets found in event ','W')
        GO TO 998
      END IF
      IF(NUM_JETS.GT.MAX_JET) THEN
        CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +              'Too many jets found--only keeping MAX_JET','W')
      END IF
C
C Check that JTCS bank is okay if requested
C
      JTCS_GOOD=.TRUE.
      JTCSIT=JTCSON.AND.RCONES(ALGN).EQ.1.
      IF(JTCSIT) THEN
        IF(D0MDST) CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +                      ' JTCS may not work with D0MDST  ','W')
        CALL GTJTCS_TOTAL(CONESI,NJ,NSUB,IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +              ' JTCS error  ','W')
          JTCS_GOOD=.FALSE.
        END IF
        IF(CONESI.NE.1.0) THEN
          CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +              ' JTCS cone not 1.0  ','W')
          JTCS_GOOD=.FALSE.
        END IF
        IF(NSUB.NE.10) THEN
          CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +              ' JTCS nsubcone not 10  ','W')
          JTCS_GOOD=.FALSE.
        END IF
        IF(NJ.NE.NUM_JETS) THEN
          CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +              ' JTCS NJ is wrong  ','W')
          JTCS_GOOD=.FALSE.
        END IF
      END IF
      JTCSDO=JTCSIT.AND.JTCS_GOOD
C
C Max number of jets is 20
C
      NJETS=MIN(MAX_JET,NUM_JETS)
C
C Initialize JTCS stuff since filled for all algorithms
C
      IF(JTCSON) THEN
        DO I=1,NJETS
          ETR1(I)=-999.
          ETR2(I)=-999.
          ETR3(I)=-999.
          ETR4(I)=-999.
          ETR5(I)=-999.
          ETR6(I)=-999.
          ETR7(I)=-999.
          ETR8(I)=-999.
          ETR9(I)=-999.
          ETR10(I)=-999.
        ENDDO
      END IF
C
C Need vertex for Escale correction
C
      IF(TRK_NV.EQ.0) THEN
        ZEV=ZOFF(IRUN)
      ELSE
        ZEV=TRK_Z(1)
      END IF
      NJBAD=0
C
C For rapidity gap triggers use MITOOL instead of Luminosity for underlying
C event corrections
C
      IF(GAP_JET.AND.MUL_INT.LE.1) THEN
        DUMMY = CORRECT_JETS_USER_MITOOL(1)
      END IF
C
C Loop over jets and fill unsorted arrays
C
      IF(ESCALE.AND.UPATH.EQ.'RECO') THEN
C
C
C For ESCALE correction need two loops over jets--first to get uncorrected
C jet energy and then after CAFIX
C
        DO 100 IJ=1,NJETS
          CALL GTJETS(IJ,IVERS,E,THETA,PHI,ETA,IER)
C
          IF(IER.NE.0) THEN
            CALL ERRMSG('QCD_UPK_JETS','GTJETS',
     +                'Ith jet not found','F')
            GO TO 998
          END IF
          UN_ETUN(IJ)=E(5)
  100   CONTINUE
C
C Do Jet and MET correction
C
        OK=CAFIX()
        IF(.NOT.OK) CALL ERRMSG('QCD_UPK_JETS','CAFIX',
     &        'FAILURE!!','F')
C
C Loop over jets again to get CAFIXED jet information
C
        DO 101 IJ=1,NJETS
          CALL GTJETS(IJ,IVERS,E,THETA,PHI,ETA,IER)
C
          IF(IER.NE.0) THEN
            CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +                  'Ith jet not found','W')
            GO TO 998
          END IF
C
C Get other jet info
C
          CALL GTJETS_2(IJ,ISPL,N1,ICDFR,CHFR,HOTR,NCDC,N90,IER)
          CALL GTJETS_2B(SEED,PRECLU,JUL1,JUL2,JUL3)
          CALL GTJETS_2C(DEMF,DCHF,DICF,ERLO,ERHI)
          CALL GTJETS_2E(NV1,NV2)
C
C Save JNEP information -999 = no info  -100 = no JNEP otherwise save Delta ET
C
          IF(UPATH.EQ.'RECO'.AND.DO_ELC) THEN
            CALL GTJETS_2D(ET_JNEP)
            IF(ET_JNEP.LT.-95.) THEN
              UN_DJNEP(IJ) = ET_JNEP
            ELSE
              UN_DJNEP(IJ) = UN_ETUN(IJ) - ET_JNEP
            END IF
          END IF
C
          UN_EXJ(IJ)=E(1)
          UN_EYJ(IJ)=E(2)
          UN_EZJ(IJ)=E(3)
          UN_ENJ(IJ)=E(4)
          UN_ETJ(IJ)=E(5)
          UN_PHIJ(IJ)=PHI
          UN_ETAJ(IJ)=ETA
          UN_WIDJ(IJ)=E(6)
          UN_EMFRJ(IJ)=E(7)
          UN_IFLAGJ(IJ)=ISPL
          UN_NCELLJ(IJ)=N1
          UN_ICDFRJ(IJ)=ICDFR
          UN_CHFRJ(IJ)=CHFR
          UN_CELFRJ(IJ)=HOTR
          UN_JN90(IJ)=N90
          UN_JNTRK(IJ)=NCDC
          UN_SEEDJ(IJ) = SEED
          UN_PRECLUJ(IJ) = PRECLU
          UN_NTKV1(IJ)=NV1
          UN_NTKV2(IJ)=NV2
C
C Apply High and Low ESCALE corrections
C
          IF(ERLO.GE.0) THEN
            UN_ETLO(IJ)=UN_ETJ(IJ)*(1-ERLO)
          ELSE
            UN_ETLO(IJ)=-999.
          END IF
          IF(ERHI.GE.0) THEN
            UN_ETHI(IJ)=UN_ETJ(IJ)*(1+ERHI)
          ELSE
            UN_ETHI(IJ)=-999.
          END IF
C
          UN_DEMFR(IJ)=DEMF
          UN_DCHFR(IJ)=DCHF
          UN_DICFR(IJ)=DICF
          ETJALL(IJ)=UN_ETJ(IJ)
C
C: Fill JTCS bank
C
          IF(JTCSDO) THEN
            CALL GTJTCS(IJ,J1ETA,J1PHI,J1ET,ETRING,IER)
            ETJTCS(IJ)=J1ET
            DO I=1,10
              UN_ETANNU(IJ,I)=ETRING(I)
            ENDDO
          END IF
C
C If BADCHK=.TRUE. Flag bad jets for removal from list of jets
C In this case no energy scale correction and will only loop over NJETS-NJBAD
C
C NOTE: Using current algorithm for bad jet instead of NN results
C in 10% loss of bad jet rejection efficiency according to R. Astur
C
          IF(BADCHK) THEN
            IJET=IJ
            CALL QCD_BAD_DST(IJET,BAD_FLAG)
            IF(BAD_FLAG.GT.0) THEN
              NJBAD=NJBAD+1
              UN_ETJ(IJ)=0.
              GO TO 101
            ENDIF
          ENDIF
  101   CONTINUE
      ELSE
C
C: If not using CAFIX  just need one loop through jets
C
        DO 102 IJ=1,NJETS
          CALL GTJETS(IJ,IVERS,E,THETA,PHI,ETA,IER)
C
          IF(IER.NE.0) THEN
            CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +                  'Ith jet not found','W')
            GO TO 998
          END IF
C
C Get other jet info
C
          CALL GTJETS_2(IJ,ISPL,N1,ICDFR,CHFR,HOTR,NCDC,N90,IER)
          CALL GTJETS_2B(SEED,PRECLU,JUL1,JUL2,JUL3)
          CALL GTJETS_2E(NV1,NV2)
C
C Save JNEP information -999 = no info  -100 = no JNEP otherwise save Delta ET
C
          IF(UPATH.EQ.'RECO'.AND.DO_ELC) THEN
            CALL GTJETS_2D(ET_JNEP)
            IF(ET_JNEP.LT.-95.) THEN
              UN_DJNEP(IJ) = ET_JNEP
            ELSE
              UN_DJNEP(IJ) = E(5) - ET_JNEP
            END IF
          END IF
          UN_EXJ(IJ)=E(1)
          UN_EYJ(IJ)=E(2)
          UN_EZJ(IJ)=E(3)
          UN_ENJ(IJ)=E(4)
          UN_ETJ(IJ)=E(5)
          UN_ETUN(IJ)=E(5)
          UN_PHIJ(IJ)=PHI
          UN_ETAJ(IJ)=ETA
          UN_WIDJ(IJ)=E(6)
          UN_EMFRJ(IJ)=E(7)
          UN_IFLAGJ(IJ)=ISPL
          UN_NCELLJ(IJ)=N1
          UN_ICDFRJ(IJ)=ICDFR
          UN_CHFRJ(IJ)=CHFR
          UN_CELFRJ(IJ)=HOTR
          UN_JN90(IJ)=N90
          UN_JNTRK(IJ)=NCDC
          UN_SEEDJ(IJ) = SEED
          UN_PRECLUJ(IJ) = PRECLU
          UN_NTKV1(IJ)=NV1
          UN_NTKV2(IJ)=NV2
          ETJALL(IJ)=UN_ETJ(IJ)
C
C DO QCD_JET_CORRECTION Escale instead of CAFIX
C
          IF(ESCALE) THEN
C
C These variables not filled without CAFIX
C
            UN_DEMFR(IJ)=-999.
            UN_DCHFR(IJ)=-999.
            UN_DICFR(IJ)=-999.
C
C Do energy scale correction for systematics (with no CAFIX use ISYS)
C
            EJET=E(4)
            ETJET=E(5)
            ETAJET=ETA
            PHIJET=PHI
            EMF=E(7)
            SIZ=E(6)
            ICDF=ICDFR
            CHF=CHFR
            CALL QCD_JET_CORRECTION_2(EJET,ETJET,ETAJET,PHIJET,
     +        EMF,SIZ,CONER,ICDF,CHF)
C
C IF old MDST force in L0 FLAG and RUNNUM   removed on 3/13/96
C
C            IF(UPATH.EQ.'MDST') THEN
C              DUMMY = CORRECT_JETS_USER_MITOOL(MUL_INT)
C              CALL SCALE_FACTORS_RUNS(1,RUNNUM)
C            END IF
C
C Low correction
C
            ISYS=1
            LJETS=0
            CALL QCD_JET_CORRECTION(LJETS,DO_ZCOR,DO_UDVCOR,DO_OUTCOR,
     +                              ZEV,ISYS,ENEW,ETNEW,ETANEW,IER)
            IF(IER.NE.0) THEN
              CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +             'ESCALE correction error--jet not corrected ','F')
              GO TO 102
            END IF
            UN_ETLO(IJ)=ETNEW
C
C High Correction
C
            ISYS=2
            LJETS=0
            CALL QCD_JET_CORRECTION(LJETS,DO_ZCOR,DO_UDVCOR,DO_OUTCOR,
     +                              ZEV,ISYS,ENEW,ETNEW,ETANEW,IER)
            IF(IER.NE.0) THEN
              CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +             'ESCALE correction error--jet not corrected ','F')
              GO TO 102
            END IF
            UN_ETHI(IJ)=ETNEW
C
C Energy Scale correction on jet ET and energies
C
            ISYS=0
            LJETS=0
            CALL QCD_JET_CORRECTION(LJETS,DO_ZCOR,DO_UDVCOR,DO_OUTCOR,
     +                              ZEV,ISYS,ENEW,ETNEW,ETANEW,IER)
            IF(IER.NE.0) THEN
              CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +             'ESCALE correction error--jet not corrected ','F')
              GO TO 102
            END IF
            ERAT=ENEW/EJET
            UN_ETJ(IJ)=ETNEW
            UN_ETAJ(IJ)=ETANEW
            UN_EXJ(IJ)=UN_EXJ(IJ)*ERAT
            UN_EYJ(IJ)=UN_EYJ(IJ)*ERAT
            UN_EZJ(IJ)=UN_EZJ(IJ)*ERAT
            UN_ENJ(IJ)=UN_ENJ(IJ)*ERAT
          END IF !End of ESCALE IF
C
C: Fill JTCS bank
C
          IF(JTCSDO) THEN
            CALL GTJTCS(IJ,J1ETA,J1PHI,J1ET,ETRING,IER)
            ETJTCS(IJ)=J1ET
            DO I=1,10
              UN_ETANNU(IJ,I)=ETRING(I)
            ENDDO
          END IF
C
C If BADCHK=.TRUE. Flag bad jets for removal from list of jets
C In this case no energy scale correction and will only loop over NJETS-NJBAD
C
C NOTE: Using current algorithm for bad jet instead of NN results
C in 10% loss of bad jet rejection efficiency according to R. Astur
C
          IF(BADCHK) THEN
            IJET=IJ
            CALL QCD_BAD_DST(IJET,BAD_FLAG)
            IF(BAD_FLAG.GT.0) THEN
              NJBAD=NJBAD+1
              UN_ETJ(IJ)=0.
              GO TO 102
            ENDIF
          ENDIF
  102   CONTINUE
      END IF
C
C Sort jets in order of decresing ET
C
      CALL SORTZV(UN_ETJ,INDJ,NJETS,1,1,0)
C
C JTCS and JETS not in same order so need translation matrix
C
      IF(JTCSDO) THEN
        CALL VZERO_i(INDJET,MAX_JET)
        CALL VZERO_i(INDJTC,MAX_JET)
        CALL SORTZV(ETJALL,INDJET,NJETS,1,1,0)
        CALL SORTZV(ETJTCS,INDJTC,NJETS,1,1,0)
        DO II=1,NJETS
          TRANS(II,1)=INDJET(II)
          TRANS(II,2)=INDJTC(II)
        END DO
      END IF
C
C Adjust NJETS by number of bad jets
C
      NJOLD=NJETS
      NJETS=NJETS-NJBAD
C
C Store sorted jet info in common block
C
      DO IJET=1,NJETS
        JJET=INDJ(IJET)
        EXJ(IJET)=UN_EXJ(JJET)
        EYJ(IJET)=UN_EYJ(JJET)
        EZJ(IJET)=UN_EZJ(JJET)
        ENJ(IJET)=UN_ENJ(JJET)
        ETJ(IJET)   =UN_ETJ(JJET)
        PHIJ(IJET)  =UN_PHIJ(JJET)
        ETAJ(IJET)  =UN_ETAJ(JJET)
        WIDJ(IJET)  =UN_WIDJ(JJET)
        EMFRJ(IJET) =UN_EMFRJ(JJET)
        IFLAGJ(IJET)=UN_IFLAGJ(JJET)
        NCELLJ(IJET)=UN_NCELLJ(JJET)
        ICDFRJ(IJET)=UN_ICDFRJ(JJET)
        CHFRJ(IJET) =UN_CHFRJ(JJET)
        CELFRJ(IJET)=UN_CELFRJ(JJET)
        JN90(IJET)  =UN_JN90(JJET)
        JNTRK(IJET) =UN_JNTRK(JJET)
        SEEDJ(IJET) =UN_SEEDJ(JJET)
        PRECLUJ(IJET)=UN_PRECLUJ(JJET)
        NTKV1(IJET)=UN_NTKV1(JJET)
        NTKV2(IJET)=UN_NTKV2(JJET)
        GOOD_JET(IJET) =
     &    QCD_CHK_JET_QUAL(EMFRJ(IJET),CHFRJ(IJET),CELFRJ(IJET),
     &    ETAJ(IJET),ZEV)
        IF(ESCALE) THEN
          ETUN(IJET)   =UN_ETUN(JJET)
          ETLO(IJET)   =UN_ETLO(JJET)
          ETHI(IJET)   =UN_ETHI(JJET)
          DEMFR(IJET) =UN_DEMFR(JJET)
          DCHFR(IJET) =UN_DCHFR(JJET)
          DICFR(IJET) =UN_DICFR(JJET)
        END IF
        IF(UPATH.EQ.'RECO'.AND.DO_ELC) DJNEP(IJET) =UN_DJNEP(JJET)
        IF(JTCSDO) THEN
          FOUND=.FALSE.
          II=1
          DO WHILE (.not.FOUND.AND.II.LE.NJOLD)
            IF(JJET.EQ.TRANS(II,1)) THEN
              KJET=TRANS(II,2)
              FOUND=.TRUE.
            END IF
            II=II+1
          END DO
          IF(.not.FOUND) THEN
            CALL ERRMSG('QCD_UPK_JETS','QCD_UPK_JETS',
     +           'JTCS Index problem ','F')
          END IF
          ETR1(IJET)=UN_ETANNU(KJET,1)
          ETR2(IJET)=UN_ETANNU(KJET,2)
          ETR3(IJET)=UN_ETANNU(KJET,3)
          ETR4(IJET)=UN_ETANNU(KJET,4)
          ETR5(IJET)=UN_ETANNU(KJET,5)
          ETR6(IJET)=UN_ETANNU(KJET,6)
          ETR7(IJET)=UN_ETANNU(KJET,7)
          ETR8(IJET)=UN_ETANNU(KJET,8)
          ETR9(IJET)=UN_ETANNU(KJET,9)
          ETR10(IJET)=UN_ETANNU(KJET,10)
        END IF
      ENDDO
C
  998 CALL RESET_CAPH
C
  999 RETURN
      END
