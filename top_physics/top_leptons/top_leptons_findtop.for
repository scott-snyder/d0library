      SUBROUTINE TOP_LEPTONS_FINDTOP(NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,
     & NOPH,NOPH_UNCUT,NOJT,NOJT_UNCUT,TOP_FLAG,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control routine for t-tbar->di-lepton
C-                         selection (dileptons=ee,mumu,emu)
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
C-             TOP_FLAG     - (1)=1 if top candidate found
C-                          - (2)=1 if found as e-mu event
C-                          - (3)=1 if found as e-e event
C-                          - (4)=1 if found as mu-mu event
C-                          - (5)=1 if found as e-jet event
C-                          - (6)=1 if found as mu-jet event
C-
C-   Created  17-JUL-1992   Stephen J. Wimpenny
C-   Modified 18-Aug-1992   Top selection flags added
C-   Modified 18-Oct-1992   Top finder logicals added
C-   Modified 16-Nov-1992   W+Jets finders added
C-   Modified  4-Dec-1992   Bug in W+Jets(muon) call fixed
C-   Modified 29-Jan-1993   Hooks for two WWgamma search routines added
C-   Modified  1-Feb-1993   WWgamma finders removed
C-   Modified  1-MAr-1993   fix bug in select flag initialization for ljets
C-                          finders
C-   Modified  4-Dec-1993   Call to mu+jet finder changed - now no
C-                          requirement on muon (for background studies)
C-                          Call to e+jet finder changed - called if at
C-                          least one good electron OR photon is found.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL DO_FINDTOP_EMU,DO_FINDTOP_EE,DO_FINDTOP_MUMU
      LOGICAL DO_FINDTOP_EJET,DO_FINDTOP_MUJET
      LOGICAL IWANT,FIRST
C
      INTEGER NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,NOPH,NOPH_UNCUT
      INTEGER NOJT,NOJT_UNCUT
      INTEGER ITEMP,IER,TOP_FLAG(6)
C
      REAL MET_VEC(3)
C
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
C
C *** Control Options - from TOP_LEPTONS_RCP -
C
        CALL EZGET('FINDTOP_EE',DO_FINDTOP_EE,IER)
        IF (IER.EQ.0) CALL EZGET('FINDTOP_EMU',DO_FINDTOP_EMU,IER)
        IF (IER.EQ.0) CALL EZGET('FINDTOP_MUMU',DO_FINDTOP_MUMU,IER)
        IF (IER.EQ.0) CALL EZGET('FINDTOP_MUJET',DO_FINDTOP_MUJET,IER)
        IF (IER.EQ.0) CALL EZGET('FINDTOP_EJET',DO_FINDTOP_EJET,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     1      'TOP_LEPTONS_FINDTOP',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      CALL VZERO(TOP_FLAG,6)
      IF(DO_FINDTOP_EMU) THEN
C
C *** Look for top-> e + mu candidates
C
        IWANT=.FALSE.
        IF( (NOEL.GT.0.AND.NOMU.GT.0).OR.
     1   (NOPH.GT.0.AND.NOMU.GT.0) ) CALL TOP_LEPTONS_FIND_EMU(IWANT,
     1      NOMU,NOEL,NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          TOP_FLAG(1)=TOP_FLAG(1)+1
          TOP_FLAG(2)=TOP_FLAG(2)+1
        ENDIF
      ENDIF
      IF(DO_FINDTOP_EE) THEN
C
C *** Look for top-> e + e candidates
C
        IWANT=.FALSE.
        ITEMP=NOEL+NOPH
        IF( ITEMP.GT.0 ) CALL TOP_LEPTONS_FIND_EE(IWANT,NOMU,NOEL,NOPH,
     1      NOJT,MET_VEC)
        IF(IWANT) THEN
          TOP_FLAG(1)=TOP_FLAG(1)+1
          TOP_FLAG(3)=TOP_FLAG(3)+1
        ENDIF
      ENDIF
      IF(DO_FINDTOP_MUMU) THEN
C
C *** Look for top-> mu + mu candidates
C
        IWANT=.FALSE.
        IF( NOMU.GE.1 ) CALL TOP_LEPTONS_FIND_MUMU(IWANT,NOMU,NOEL,NOPH,
     1       NOJT,NOMU_UNCUT,MET_VEC)
        IF(IWANT) THEN
          TOP_FLAG(1)=TOP_FLAG(1)+1
          TOP_FLAG(4)=TOP_FLAG(4)+1
        ENDIF
      ENDIF
      IF(DO_FINDTOP_EJET) THEN
C
C *** Look for top-> e + jets candidates
C
        IWANT=.FALSE.
        ITEMP=NOEL+NOPH
        IF( ITEMP.GE.1 ) CALL TOP_LEPTONS_FIND_EJET(IWANT,NOMU,NOEL,
     1       NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          TOP_FLAG(1)=TOP_FLAG(1)+1
          TOP_FLAG(5)=TOP_FLAG(5)+1
        ENDIF
      ENDIF
      IF(DO_FINDTOP_MUJET) THEN
C
C *** Look for top-> mu + jets candidates
C
        IWANT=.FALSE.
        CALL TOP_LEPTONS_FIND_MUJET(IWANT,NOMU,NOEL,
     1       NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          TOP_FLAG(1)=TOP_FLAG(1)+1
          TOP_FLAG(6)=TOP_FLAG(6)+1
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
