      SUBROUTINE TOP_LEPTONS_FINDW(NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,
     1  NOPH,NOPH_UNCUT,NOJT,NOJT_UNCUT,W_FLAG,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control routine for W->mu(e)+neutrino
C-                                     selection
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
C-             W_FLAG     - (1)=1 if W candidate found
C-                        - (2)=1 if W->e candidate
C-                        - (3)=1 if W->mu candidate
C-
C-
C-   Created  29-JUL-1992   Stephen J. Wimpenny
C-   MOdifed  18-Aug-1992   W type logicals added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST,IWANT,DO_FIND_WMU,DO_FIND_WE
      INTEGER NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,NOPH,NOPH_UNCUT
      INTEGER NOJT,NOJT_UNCUT,IER,W_FLAG(3)
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
        CALL EZGET('FINDW_ENU',DO_FIND_WE,IER)
        IF (IER.EQ.0) CALL EZGET('FINDW_MUNU',DO_FIND_WMU,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     1      'TOP_LEPTONS_FINDW',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      CALL VZERO(W_FLAG,3)
      IF(DO_FIND_WMU) THEN
C
C *** Look for W -> mu nu candidates
C
        IWANT=.FALSE.
        IF(NOMU.GT.0) CALL TOP_LEPTONS_FIND_WMU(IWANT,NOMU,NOEL,
     1      NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          W_FLAG(1)=W_FLAG(1)+1
          W_FLAG(3)=1
        ENDIF
      ENDIF
      IF(DO_FIND_WE) THEN
C
C *** Look for W -> e nu candidates
C
        IWANT=.FALSE.
        IF(NOEL.GT.0) CALL TOP_LEPTONS_FIND_WE(IWANT,NOMU,NOEL,
     1      NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          W_FLAG(1)=W_FLAG(1)+1
          W_FLAG(2)=1
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
