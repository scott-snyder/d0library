      INTEGER FUNCTION ICRYO(IETA,IPHI,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine which cryostat a given set of 
C-                         eta,phi,layer indices belongs to 
C-              
C-   Returned value  :  0 => Indeterminate (bad arguments)
C-                      1 => ECN
C-                      2 => CC
C-                      3 => ECS
C-                      4 => ICD/MG
C-   Inputs  : IETA,IPHI,LAYER indices of the cell
C-   Outputs : None
C-   Controls: None
C-
C-   Created  30-SEP-1995   Dhiman Chakraborty
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER IETA,IPHI,LAYER
      INTEGER MAX_IETA_CC
      PARAMETER( MAX_IETA_CC = 13 )
C----------------------------------------------------------------------
      ICRYO = 0
      IF((ABS(IETA).GT.NETAL).OR.((IPHI.LT.1).OR.(IPHI.GT.NPHIL)).OR.
     &   ((LAYER.LT.1).OR.(LAYER.GT.NLYRL)))GOTO 999
      IF((LAYER.GE.MNLYMG).AND.(LAYER.LE.MXLYMG)) THEN
        ICRYO = 4
      ELSE
        IF(IETA.LT.-MAX_IETA_CC) ICRYO = 1
        IF(ABS(IETA).LE.MAX_IETA_CC) ICRYO = 2
        IF(IETA.GT.MAX_IETA_CC) ICRYO = 3
      ENDIF
  999 RETURN
      END
