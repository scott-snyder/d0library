      FUNCTION QCD_QJT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOGICAL FILTER FOR ** QJT **  STREAM
C-        (QCD_QJT = QCD MDST stream - all tracking triggers except those
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
      LOGICAL   QCD_QJT
      LOGICAL   NOTWANTED1, NOTWANTED2
C----------------------------------------------------------------------
      QCD_QJT   = .FALSE.                 ! Default is to fail
C
C: Pass if any passed filters have neither the string '_QNT' NOR 
C: 'JET_GAP'
C
      NOTWANTED1   = .TRUE.
      NOTWANTED2   = .TRUE.
      IF ( QCD_FILT_STRING(NOTWANTED1,'_QNT', NOTWANTED2, 'JET_GAP') )
     &  QCD_QJT = .TRUE.

  999 RETURN
      END
