      SUBROUTINE TOP_LEPTONS_FINDZ(NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,
     1  NOPH,NOPH_UNCUT,NOJT,NOJT_UNCUT,Z_FLAG,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control routine for Z0->l+l-
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
C-             Z_FLAG     - (1)=1 if Z candidate found
C-                        - (2)=1 if Z->ee candidate
C-                        - (3)=1 if Z->mumu candidate
C-
C-
C-   Created  29-Jan-1993   Stephen J. Wimpenny
C-   Modified 10-Feb-1993   RCP problem fixed (typo)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST,IWANT,DO_FIND_ZMUMU,DO_FIND_ZEE
      INTEGER NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,NOPH,NOPH_UNCUT
      INTEGER NOJT,NOJT_UNCUT,IER,Z_FLAG(3)
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
        CALL EZGET('FINDZ_EE',DO_FIND_ZEE,IER)
        IF (IER.EQ.0) CALL EZGET('FINDZ_MUMU',DO_FIND_ZMUMU,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     1      'TOP_LEPTONS_FINDZ',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      CALL VZERO(Z_FLAG,3)
      IF(DO_FIND_ZMUMU) THEN
C
C *** Look for Z0 -> mu mu candidates
C
        IWANT=.FALSE.
        IF(NOMU.GT.0) CALL TOP_LEPTONS_FIND_ZMUMU(IWANT,NOMU,NOEL,
     1      NOPH,NOJT,NOMU_UNCUT,MET_VEC)
        IF(IWANT) THEN
          Z_FLAG(1)=Z_FLAG(1)+1
          Z_FLAG(3)=1
        ENDIF
      ENDIF
      IF(DO_FIND_ZEE) THEN
C
C *** Look for Z0 -> e e candidates
C
        IWANT=.FALSE.
        IF(NOEL.GT.0) CALL TOP_LEPTONS_FIND_ZEE(IWANT,NOMU,NOEL,
     1      NOPH,NOJT,MET_VEC)
        IF(IWANT) THEN
          Z_FLAG(1)=Z_FLAG(1)+1
          Z_FLAG(2)=1
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
