      FUNCTION C2PFIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FINISH UP C2PMET
C-
C-   Returned value  : TRUE if all OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-APR-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      LOGICAL C2PFIN
C----------------------------------------------------------------------
      C2PFIN = .TRUE.
C
      CALL DHDIR('CAHITS_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Print HBOOK ID's
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','C2PFIN',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
        C2PFIN = .FALSE.
      ENDIF
C
      CALL HOPERA(2,'-',1,41,1.,1.)
      CALL HOPERA(22,'-',21,42,1.,1.)
C
  999 RETURN
      END
