      FUNCTION QCD_NOL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOGICAL FILTER FOR ** QNL **  STREAM
C-    subset of QCD_JJJ with _NOL0
C-    
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  06-NOV-1995   Ki Suk Hahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL   QCD_FILT_STRING, QCDJETS
      EXTERNAL  QCD_FILT_STRING, QCDJETS
      LOGICAL   QCD_NOL
      LOGICAL   NOTWANTED
C----------------------------------------------------------------------
      QCD_NOL   = .FALSE.                 ! Default is to fail
C
C: Pass if any passed filters have the string '_NOL0' in them 
C: 
C
      NOTWANTED   = .FALSE.

      IF ( QCD_FILT_STRING(NOTWANTED,'_NOL0', NOTWANTED, '_NOL0') )
     &  QCD_NOL = .TRUE.

  999 RETURN
      END
