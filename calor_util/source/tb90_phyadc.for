      SUBROUTINE TB90_PHYADC(ETAV,PHIV,LAYERV,ADCV,SEQV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert TB90 cal. PHYSICS address to ADC
C-                         address (and SEQUENTIAL address for use in 
C-                         subroutine C_PHY_GANG)
C-
C-   Inputs  : ETAV,PHIV,LAYERV - the PHYSICS address.
C-   Outputs : ADCV - the packed ADC address
C-             SEQV - the SEQUENTIAL ADC address
C-   Controls: none
C-
C-   Created  14-DEC-1989   
C-   by Andrew P. White
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TB_SORT_ORD.INC'
      INTEGER ETAV,PHIV,LAYERV,ADCV,SEQV
      INTEGER I,IP
C----------------------------------------------------------------------
C
C---  First find the ETA value
C
      SEQV=0
      ADCV=0
      DO 10 I=1,SIZE
        IP=I
        IF(ETA(I,1).EQ.ETAV) GO TO 50
   10 CONTINUE
      GO TO 999
C
C--- Now find the PHI value for this ETA.
C
   50 DO 20 I=IP,SIZE
        IP=I
        IF(PHI(I,1).EQ.PHIV) GO TO 100
        IF(ETA(I,1).NE.ETAV) GO TO 999
   20 CONTINUE
      GO TO 999
C
C--- Now find the LAYER value for this PHI.
C
  100 DO 30 I=IP,SIZE
        IP=I
        IF(LAYER(I,1).EQ.LAYERV) GO TO 500
        IF(PHI(I,1).NE.PHIV) GO TO 999
   30 CONTINUE
      GO TO 999
C
C--- Complete PHYSICS address has been located in table.
C--- Now extract the corresponding ADC address data.
C
  500 ADCV=ADCADD(IP,1)
      SEQV=SEQADC(IP,1)
C
  999 RETURN
      END
