      FUNCTION NOI_GAUSS(IETA,IPHI,ILYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURN ELECTRONICS CELL NOISE AS SIMPLE
C-                         gaussian of avg NOISE_AVG
C-                             and sigma NOISE_SIGMA
C-                         with slight variation for ICD and massless
C-                             gaps.
C-
C-   Returned value  : NOI_GAUSS = ELECTRONICS CELL NOISE IN GEV EQUIVALENT
C-   Inputs  : IETA,IPHI,ILYR OF THE CELL
C-   Outputs :
C-   Controls:
C-
C-   Created  15-JUL-1991   Peter Nemethy and Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      REAL NOI_GAUSS
      LOGICAL CEXIST
      REAL XC,YC,ZC
      REAL NOI_GAUSS1
      INTEGER IETA,IPHI,ILYR
      INTEGER IOK
      REAL AV,SIG,SIGGP
C----------------------------------------------------------------------
      NOI_GAUSS=0.0
      IF(CEXIST(IETA,IPHI,ILYR))THEN
        CALL CELXYZ(IETA,IPHI,ILYR,XC,YC,ZC,IOK)
        IF(IOK.EQ.0) THEN
          AV=NOISE_AVG
          SIG=NOISE_SIGMA
          SIGGP=SIG/3.
          IF(ILYR.EQ.9)THEN
            NOI_GAUSS=0.0
          ELSEIF(ILYR.EQ.8 .OR. ILYR.EQ.10)THEN
            NOI_GAUSS=NOI_GAUSS1(AV,SIGGP,RAN_VALUE)
          ELSE
            NOI_GAUSS=NOI_GAUSS1(AV,SIG,RAN_VALUE)
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
