      SUBROUTINE TOP_LEPTONS_CLEANMU(LPMUO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Offline Muon id and Cosmic Ray Rejection
C-                         Steering Routine. Selection code version 2.0
C-
C-   Inputs  : 
C-              LPMUO - Pointer to PMUO bank to be tested
C-
C-   Outputs : 
C-              Sets a bit string detailing which cuts are in veto. This
C-              is stored in different locations depending which version
C-              of the PMUO Bank is in use.
C-
C-   Controls: 
C-              No Direct controls
C-
C-   Created  27-JAN-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER LPMUO,PMUO_VERS
C
      PMUO_VERS=IQ(LPMUO+1)
      IF(PMUO_VERS.GE.3) THEN
        CALL TOP_LEPTONS_MUON_CODE(LPMUO)
      ELSE
        CALL TOP_LEPTONS_MUON_OLD_CODE(LPMUO)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
