      SUBROUTINE TOP_LEPTONS_FIND_WPAIR(NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,
     & NOPH,NOPH_UNCUT,NOJT,NOJT_UNCUT,WPAIR_FLAG,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control routine for W+W- -> dilepton
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
C-             WPAIR_FLAG      - (1)=1 if WW candidate found
C-                          - (2)=1 if found as e-mu event
C-                          - (3)=1 if found as e-e event
C-                          - (4)=1 if found as mu-mu event
C-                          - (5)=1 if found as e-jet event
C-                          - (6)=1 if found as mu-jet event
C-
C-   Created  17-JUL-1992   Stephen J. Wimpenny as Top -> Dileptons finder.
C-   Modified 18-Aug-1992   Top selection flags added
C-   Modified 18-Oct-1992   Top finder logicals added
C-   Modified 16-Nov-1992   W+Jets finders added
C-   Modified  4-Dec-1992   Bug in W+Jets(muon) call fixed
C-   Modified 29-Jan-1993   Hooks for two WWgamma search routines added
C-   Modified  1-Feb-1993   WWgamma finders removed
C-   Modified 07-May-1993   WW coding. Tom Diehl.
C-   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL DO_FINDWPAIR_EMU,DO_FINDWPAIR_EE,DO_FINDWPAIR_MUMU
      LOGICAL DO_FINDWPAIR_EJET,DO_FINDWPAIR_MUJET
      LOGICAL IWANT,CORR_JETS,FIRST
C
      INTEGER NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,NOPH,NOPH_UNCUT
      INTEGER NOJT,NOJT_UNCUT
      INTEGER ITEMP,IER,WPAIR_FLAG(6)
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
        CALL EZGET('FINDWPAIR_EE',DO_FINDWPAIR_EE,IER)
        IF (IER.EQ.0) CALL EZGET('FINDWPAIR_EMU',DO_FINDWPAIR_EMU,IER)
        IF (IER.EQ.0) CALL EZGET('FINDWPAIR_MUMU',DO_FINDWPAIR_MUMU,IER)
        IF (IER.EQ.0) 
     $    CALL EZGET('FINDWPAIR_MUJET',DO_FINDWPAIR_MUJET,IER)
        IF (IER.EQ.0) CALL EZGET('FINDWPAIR_EJET',DO_FINDWPAIR_EJET,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     1      'TOP_LEPTONS_FINDWPAIR',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      CALL VZERO(WPAIR_FLAG,6)
      IF(DO_FINDWPAIR_EMU) THEN
C
C *** Look for WW-> e + mu candidates
C
        IWANT=.FALSE.
        IF( (NOEL.GT.0.AND.NOMU.GT.0).OR.
     1   (NOPH.GT.0.AND.NOMU.GT.0) ) CALL TOP_LEPTONS_FIND_WPAIR_EMU(
     1      IWANT,NOMU,NOEL,NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          WPAIR_FLAG(1)=WPAIR_FLAG(1)+1
          WPAIR_FLAG(2)=WPAIR_FLAG(2)+1
        ENDIF
      ENDIF
      IF(DO_FINDWPAIR_EE) THEN
C
C *** Look for WW-> e + e candidates
C
        IWANT=.FALSE.
        ITEMP=NOEL+NOPH
        IF( ITEMP.GT.0 ) CALL TOP_LEPTONS_FIND_WPAIR_EE(
     1      IWANT,NOMU,NOEL,NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          WPAIR_FLAG(1)=WPAIR_FLAG(1)+1
          WPAIR_FLAG(3)=WPAIR_FLAG(3)+1
        ENDIF
      ENDIF
      IF(DO_FINDWPAIR_MUMU) THEN
C
C *** Look for WW-> mu + mu candidates
C
        IWANT=.FALSE.
        IF( NOMU.GE.1 ) CALL TOP_LEPTONS_FIND_WPAIR_MUMU(
     1       IWANT,NOMU,NOEL,NOPH,NOJT,NOMU_UNCUT,MET_VEC)
        IF(IWANT) THEN
          WPAIR_FLAG(1)=WPAIR_FLAG(1)+1
          WPAIR_FLAG(4)=WPAIR_FLAG(4)+1
        ENDIF
      ENDIF
      IF(DO_FINDWPAIR_EJET) THEN
C
C *** Look for WW-> e + jets candidates
C
        IWANT=.FALSE.
        IF( NOEL.GE.1 ) CALL TOP_LEPTONS_FIND_WPAIR_EJET(
     1       IWANT,NOMU,NOEL,NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          WPAIR_FLAG(1)=WPAIR_FLAG(1)+1
          WPAIR_FLAG(5)=WPAIR_FLAG(5)+1
        ENDIF
      ENDIF
      IF(DO_FINDWPAIR_MUJET) THEN
C
C *** Look for WW-> mu + jets candidates
C
        IWANT=.FALSE.
        IF( NOMU.GE.1 ) CALL TOP_LEPTONS_FIND_WPAIR_MUJET(
     1       IWANT,NOMU,NOEL,NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          WPAIR_FLAG(1)=WPAIR_FLAG(1)+1
          WPAIR_FLAG(6)=WPAIR_FLAG(6)+1
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
