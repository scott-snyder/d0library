      FUNCTION QCD_JJJ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOGICAL FILTER FOR ** QCJ **  STREAM
C-        (QCD_JJJ = QCD DST stream - all triggers except rapidity gap ) 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-FEB-1994   Richard V. Astur
C-   Updated   8-NOV-1995   Ki Suk Hahn  Modified to fail if QCD_NOL passes. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL   QCD_FILT_STRING, QCDJETS
      EXTERNAL  QCD_FILT_STRING, QCDJETS
      LOGICAL   QCD_JJJ, QCD_NOL
      LOGICAL   NOTWANTED
C----------------------------------------------------------------------
      QCD_JJJ   = .FALSE.                 ! Default is to fail
C
C: Pass if any passed filters don't have the string 'JET_GAP' in them
C
      NOTWANTED   = .TRUE.
      IF ( QCD_FILT_STRING(NOTWANTED,'JET_GAP', NOTWANTED, 'JET_GAP') )
     &  QCD_JJJ = .TRUE.

      IF ( QCD_NOL().EQ..TRUE. ) QCD_JJJ = .FALSE.

  999 RETURN
      END
