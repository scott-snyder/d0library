      SUBROUTINE TOP_LEPTONS_FINDWWG(NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,
     & NOPH,NOPH_UNCUT,NOJT,NOJT_UNCUT,WWG_FLAG,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control routine for WWgamma event
C-                         selection (W->munu,enu)
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
C-             WWG_FLAG     - (1)=1 if WWG candidate found
C-                          - (2)=1 if found as e-nu event
C-                          - (3)=1 if found as mu-nu event
C-
C-   Created   1-Feb-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIND_WE_GAM,FIND_WMU_GAM
      LOGICAL IWANT,CORR_JETS,FIRST
      INTEGER NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,NOPH,NOPH_UNCUT
      INTEGER NOJT,NOJT_UNCUT
      INTEGER ITEMP,IER,WWG_FLAG(6)
      REAL MET_VEC
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
        CALL EZGET('FIND_WE_GAM',FIND_WE_GAM,IER)
        IF (IER.EQ.0) CALL EZGET('FIND_WMU_GAM',FIND_WMU_GAM,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     1      'TOP_LEPTONS_FINDWWG',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      CALL VZERO(WWG_FLAG,3)
C
      IF(FIND_WMU_GAM) THEN
C
C *** WWgamma search code : W->munu decay
C
        IWANT=.FALSE.
        IF( (NOEL.GT.0.AND.NOMU.GT.0).OR.
     1   (NOPH.GT.0.AND.NOMU.GT.0) ) 
     2     CALL TOP_LEPTONS_FIND_WMU_GAM(IWANT,NOMU,NOEL,NOPH,NOJT,
     3     MET_VEC)
        IF(IWANT) THEN
          WWG_FLAG(1)=WWG_FLAG(1)+1
          WWG_FLAG(3)=WWG_FLAG(3)+1
        ENDIF
      ENDIF
      IF(FIND_WE_GAM) THEN
C
C *** WWgamma search code : W->enu decay
C
        IWANT=.FALSE.
        ITEMP=NOEL+NOPH
        IF( ITEMP.GT.0 ) 
     1     CALL TOP_LEPTONS_FIND_WE_GAM(IWANT,NOMU,NOEL,NOPH,NOJT,
     2     MET_VEC)
        IF(IWANT) THEN
          WWG_FLAG(1)=WWG_FLAG(1)+1
          WWG_FLAG(2)=WWG_FLAG(2)+1
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
