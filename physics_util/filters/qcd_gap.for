      FUNCTION QCD_GAP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOGICAL FILTER FOR ** QCR **  STREAM
C-        (QCD_GAP = QCD DST stream - Rapidity Gap triggers ) 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-FEB-1994   Richard V. Astur
C-   Updated   6-NOV-1995   Ki Suk Hahn  changed to not duplicate events with
C-                          QCD_SD filter. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL   QCD_FILT_STRING, QCDJETS
      EXTERNAL  QCD_FILT_STRING, QCDJETS
      LOGICAL   QCD_GAP, QCD_SD
      LOGICAL   NOTWANTED
C----------------------------------------------------------------------
      QCD_GAP   = .FALSE.                 ! Default is to fail
C
C: Pass if any passed filters have the string 'JET_GAP' in them
C
      NOTWANTED   = .FALSE.
      IF ( QCD_FILT_STRING(NOTWANTED,'JET_GAP', NOTWANTED, 'JET_GAP') )
     &  QCD_GAP = .TRUE.
      IF ( QCD_SD().EQ..TRUE. ) QCD_GAP = .FALSE.

  999 RETURN
      END
