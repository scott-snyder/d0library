      FUNCTION QCD_NTR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOGICAL FILTER FOR ** QCN **  STREAM
C-        (QCD_NTR = QCD MDST stream - all notracking triggers except those
C-                                     with rapidity gaps) 
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
      LOGICAL   QCD_NTR
      LOGICAL   NOTWANTED1, NOTWANTED2
C----------------------------------------------------------------------
      QCD_NTR   = .FALSE.                 ! Default is to fail
C
C: Pass if any passed filters have the string '_QNT' in them but not
C: 'JET_GAP'
C
      NOTWANTED1   = .FALSE.
      NOTWANTED2   = .TRUE.
      IF ( QCD_FILT_STRING(NOTWANTED1,'_QNT', NOTWANTED2, 'JET_GAP') )
     &  QCD_NTR = .TRUE.

  999 RETURN
      END
