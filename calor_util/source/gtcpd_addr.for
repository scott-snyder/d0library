      SUBROUTINE GTCPD_ADDR(TASK,CRATE,CARD,BLS,TOWER,DEPTH,SCALE,
     &  VALUE,SIGMA,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns Values and Sigmas for PED/?GAINS
C-
C-   Inputs  : TASK =1 PEDS, =3 GAINS
C-             CRATE = ADC CRATE
C-             CARD = ADC card number
C-             BLS
C-             TOWER
C-             DEPTH
C-             SCALE = 0 X8 Gain. =1 X1 Gain
C
C-   OUTPUTS : VALUE Value for channels
C-             SIGMA FOR channel
C-             IER   error code 0=OK
C-                   -1=bad ped bank
C-
C-   Created   23-MAR-1990   Chip Stewart , Dharmaratna
C-   Updated  13-NOV-1990   Jan Guida  Added CRATE argument to GT_PED_GNS 
C-   Updated  13-NOV-1993   Jan Guida  Add DATA statement for O_CRATE...(FLINT) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TASK,SCALE,CRATE,CARD,BLS,TOWER,DEPTH,IER
      INTEGER O_CRATE,O_CARD,O_SCALE,NSEQ
      INTEGER HEAD(30)
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
C
      REAL VAL(768),VALUE,SIGMA
      SAVE O_CRATE,O_CARD,O_SCALE
      DATA O_CRATE,O_CARD,O_SCALE/3*0/
C
C----------------------------------------------------------------------
      IER = 0
      IF(CRATE.NE.O_CRATE.OR.CARD.NE.O_CARD.OR.SCALE.NE.O_SCALE)THEN
        CALL GT_PED_GNS(TASK,SCALE,CRATE,CARD,HEAD,VAL)
        O_CRATE = CRATE
        O_CARD = CARD
        O_SCALE = SCALE
        IF (HEAD(6).LE.0 .OR.  HEAD(13).LE.0) IER = -1
        IF (IER.EQ.-1) THEN
          VALUE = 0
          SIGMA = 0
          GOTO 999
        END IF
      END IF
      NSEQ= BLS*NDEPTC*NEFC +
     &  TOWER*NDEPTC + DEPTH 
      VALUE = VAL(2*NSEQ+1)
      SIGMA = VAL(2*NSEQ+2)
C
  999 RETURN
      END
