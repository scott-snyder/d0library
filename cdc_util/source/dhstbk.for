      SUBROUTINE DHSTBK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : dummy routine for user to book histograms
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  27-JUN-1989   Qizhong Li-Demarteau
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
      IF (IER.NE.0) THEN
        CALL ERRMSG('DTRAKS','DHSTBK',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
C   book your histograms here
C
  999 RETURN
      END
