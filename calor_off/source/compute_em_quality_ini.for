      LOGICAL FUNCTION COMPUTE_EM_QUALITY_INI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization routine of the electron/photon
C-                         selection pacakge
C-
C-   Returned value  : always returns TRUE - Fatal error if not successful
C-   Inputs  : None
C-   Outputs :
C-   Controls: Uses CLEANEM_RCP 
C-
C-   Created  27-OCT-1992   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER,LRCP
      LOGICAL OK
C----------------------------------------------------------------------
      COMPUTE_EM_QUALITY_INI = .TRUE.
      CALL EZLOC('CLEANEM_RCP',LRCP)
      OK = LRCP .GT. 0
      IF (.NOT. OK) THEN
        CALL INRCP('CLEANEM_RCP',IER)
        IF (IER.EQ.0) CALL EZPICK('CLEANEM_RCP')
        IF (IER.EQ.0) CALL EZERR(IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('COMPUTE_EM_QUALITY','INI',
     &      ' CLEANEM_RCP not found','F')
        ENDIF
        CALL EZRSET
      ENDIF
  999 RETURN
      END
