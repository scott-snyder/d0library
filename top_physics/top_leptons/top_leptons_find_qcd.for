      SUBROUTINE TOP_LEPTONS_FINDQCD(NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,
     & NOPH,NOPH_UNCUT,NOJT,NOJT_UNCUT,QCD_FLAG,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control routine for QCD background studies for
C-                         t-tbar->di-lepton backgrounds
C-
C-   Inputs  :
C-             NOMU         - no. muons after cuts
C-             NOMU_UNCUT   - no. raw PMUO banks
C-             NOEL         - no. electrons after cuts
C-             NOEL_UNCUT   - no. raw PELC banks
C-             NOJT         - no. jets after cuts
C-             NOJT_UNCUT   - no. raw JETS banks
C-
C-   Outputs :
C-             QCD_FLAG     - (1)=1 if QCD candidate found
C-                          - (2)=1 if found as QCD e-mu event
C-                          - (3)=1 if found as QCD e-e event
C-                          - (4)=1 if found as QCD mu-mu event
C-                          - (5)=1 if found as QCD e-jet event
C-                          - (6)=1 if found as QCD mu-jet event
C-
C-   Created   2-MAY-1993   Stephen J. Wimpenny
C-   Modifed   4-Dec-1993   Cuts of min number of leptons for e-jets
C-                          and mu-jets finders removed. Redundant
C-                          variable removed.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL DO_FINDQCD_EMU,DO_FINDQCD_EE,DO_FINDQCD_MUMU
      LOGICAL DO_FINDQCD_EJET,DO_FINDQCD_MUJET
      LOGICAL IWANT,FIRST
C
      INTEGER NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,NOPH,NOPH_UNCUT
      INTEGER NOJT,NOJT_UNCUT
      INTEGER ITEMP,IER,QCD_FLAG(6)
C
      REAL MET_VEC
C
      DATA FIRST/.TRUE./
      DATA DO_FINDQCD_EE,DO_FINDQCD_EMU,DO_FINDQCD_MUMU/ 3*.FALSE./
      DATA DO_FINDQCD_EJET,DO_FINDQCD_MUJET/ 2*.FALSE./

      IF(FIRST) THEN
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
C
C *** Control Options - from TOP_LEPTONS_RCP -
C
        CALL EZGET('FINDQCD_EMU',DO_FINDQCD_EMU,IER)
        IF (IER.EQ.0) CALL EZGET('FINDQCD_EE',DO_FINDQCD_EE,IER)
        IF (IER.EQ.0) CALL EZGET('FINDQCD_MUMU',DO_FINDQCD_MUMU,IER)
        IF (IER.EQ.0) CALL EZGET('FINDQCD_MUJET',DO_FINDQCD_MUJET,IER)
        IF (IER.EQ.0) CALL EZGET('FINDQCD_EJET',DO_FINDQCD_EJET,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     1      'TOP_LEPTONS_FINDQCD',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      CALL VZERO(QCD_FLAG,4)
      IF(DO_FINDQCD_EMU) THEN
C
C *** Look for QCD-> e + mu candidates
C
        IWANT=.FALSE.
        IF( (NOEL.GT.0.AND.NOMU.GT.0).OR.
     1   (NOPH.GT.0.AND.NOMU.GT.0) ) CALL TOP_LEPTONS_FIND_QCDEMU(IWANT,
     1      NOMU,NOEL,NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          QCD_FLAG(1)=QCD_FLAG(1)+1
          QCD_FLAG(2)=QCD_FLAG(2)+1
        ENDIF
      ENDIF
      IF(DO_FINDQCD_EE) THEN
C
C *** Look for QCD-> e + e candidates
C
        IWANT=.FALSE.
        ITEMP=NOEL+NOPH
        IF( ITEMP.GT.0 ) CALL TOP_LEPTONS_FIND_QCDEE(IWANT,NOMU,NOEL,
     1      NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          QCD_FLAG(1)=QCD_FLAG(1)+1
          QCD_FLAG(3)=QCD_FLAG(3)+1
        ENDIF
      ENDIF
      IF(DO_FINDQCD_MUMU) THEN
C
C *** Look for QCD -> mu + mu candidates
C
        IWANT=.FALSE.
        IF( NOMU.GE.1 ) CALL TOP_LEPTONS_FIND_QCDMUMU(IWANT,NOMU,NOEL,
     1       NOPH,NOJT,NOMU_UNCUT,MET_VEC)
        IF(IWANT) THEN
          QCD_FLAG(1)=QCD_FLAG(1)+1
          QCD_FLAG(4)=QCD_FLAG(4)+1
        ENDIF
      ENDIF
      IF(DO_FINDQCD_EJET) THEN
C
C *** Look for QCD -> e + jets candidates
C
        IWANT=.FALSE.
        IF( NOEL.GE.0 ) CALL TOP_LEPTONS_FIND_QCDEJET(IWANT,NOMU,NOEL,
     1       NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          QCD_FLAG(1)=QCD_FLAG(1)+1
          QCD_FLAG(5)=QCD_FLAG(5)+1
        ENDIF
      ENDIF
      IF(DO_FINDQCD_MUJET) THEN
C
C *** Look for QCD -> mu + jets candidates
C
        IWANT=.FALSE.
        IF( NOMU.GE.0 ) CALL TOP_LEPTONS_FIND_QCDMUJET(IWANT,NOMU,NOEL,
     1       NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          QCD_FLAG(1)=QCD_FLAG(1)+1
          QCD_FLAG(6)=QCD_FLAG(6)+1
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
