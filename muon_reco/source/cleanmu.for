      SUBROUTINE CLEANMU(LPMUO,STATUS,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Offline Muon id and Cosmic Ray Rejection
C-                                 Steering Routine. 
C-                            Selection code version 3.0
C-
C-   Inputs  : 
C-              LPMUO      (I) - Pointer to PMUO bank to be tested
C-
C-   Outputs : 
C-              STATUS     (I) - bit pattern (0/1) indicating which cuts
C-                               were satisfied/failed
C-              OK         (L) - .TRUE. if muon track passed all cuts 
C-                               specified by MUON_MASK
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
      LOGICAL OK
      INTEGER LPMUO,PMUO_VERS,STATUS,IOK
C
      OK=.FALSE.
      PMUO_VERS=IQ(LPMUO+1)
      IF(PMUO_VERS.GE.3) THEN
C
C *** code for PMUO versions 3 onwards (RECO 11 onwards)
C
        CALL MUON_SELECT(LPMUO,STATUS,IOK)
        IF(IOK.GT.0) OK=.TRUE.
      ELSE
C
C *** code for PMUO versions 1,2 (RECO versions up to and includind 10.13)
C
        CALL MUON_SELECT_OLD(LPMUO,STATUS,IOK)
        IF(IOK.GT.0) OK=.TRUE.
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
