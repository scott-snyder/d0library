      FUNCTION QCD_STA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOGICAL FILTER FOR ** QCM **  STREAM
C-        (QCD_STA = QCD STA stream - Just a few rare triggers ) 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-FEB-1994   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL   QCD_FILT_STRING, QCDJETS
      EXTERNAL  QCD_FILT_STRING, QCDJETS
      LOGICAL   QCD_STA, NOTWANTED
C----------------------------------------------------------------------
      QCD_STA   = .FALSE.                 ! Default is to fail
C
C: Pass if any of these pass
C
      NOTWANTED = .FALSE.
      IF ( QCD_FILT_STRING(NOTWANTED ,'EM1_GIS_HIGH', NOTWANTED,
     &  'EM1_GIS_HIGH') ) QCD_STA = .TRUE.
      IF ( QCD_FILT_STRING(NOTWANTED ,'JET_MAX', NOTWANTED,
     &  'JET_MAX') ) QCD_STA = .TRUE.
      IF ( QCD_FILT_STRING(NOTWANTED ,'JET_GAP_HHE', NOTWANTED,
     &  'JET_GAP_HHE') ) QCD_STA = .TRUE.
      IF ( QCD_FILT_STRING(NOTWANTED ,'JET_GAP_HME', NOTWANTED,
     &  'JET_GAP_HME') ) QCD_STA = .TRUE.
      IF ( QCD_FILT_STRING(NOTWANTED ,'JET_GAP_MHE', NOTWANTED,
     &  'JET_GAP_MHE') ) QCD_STA = .TRUE.


  999 RETURN
      END
