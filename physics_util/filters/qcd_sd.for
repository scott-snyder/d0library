      FUNCTION QCD_SD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOGICAL FILTER FOR ** QSD **  STREAM
C-    subset of QCD_GAP with JET_GAP_SD and JET_GAP_POM
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
      LOGICAL   QCD_SD
      LOGICAL   NOTWANTED
C----------------------------------------------------------------------
      QCD_SD   = .FALSE.                 ! Default is to fail
C
C: Pass if any passed filters have the string 'GAP_SD' or 'GAP_POM' in them 
C: 
C
      NOTWANTED   = .FALSE.

      IF ( QCD_FILT_STRING(NOTWANTED,'GAP_SD', NOTWANTED, 'GAP_SD') )
     &  QCD_SD = .TRUE.

C
C If JET_GAP_SD is not on, check if JET_GAP_POM is on
C
      IF(.NOT.QCD_SD) THEN
        IF ( QCD_FILT_STRING(NOTWANTED,'GAP_POM', NOTWANTED, 'GAP_POM')
     &    )
     &    QCD_SD = .TRUE.
      ENDIF

  999 RETURN
      END
