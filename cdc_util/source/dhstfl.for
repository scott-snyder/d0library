      SUBROUTINE DHSTFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : dummy routine for user to fill your own
C-                         histograms
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  28-JUN-1989   Qizhong Li-Demarteau
C-   Updated  21-DEC-1989   Qizhong Li-Demarteau  set up hbook_directory
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
C
C----------------------------------------------------------------------
C
C         Create/Set HBOOK directory for DTRAKS
C
      CALL DHDIR('DTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER .NE. 0) THEN
        CALL ERRMSG('DTRAKS','DHSTFL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
C   fill your histograms here
C
  999 RETURN
      END
