      SUBROUTINE CADTFL(ISTAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank CADT -
C-                         the calorimeter CAD bank address look-up table
C-
C-   Inputs  : none
C-
C-   Outputs : ISTAT - error code - 0 if OK
C-   Controls: none
C-
C-   Created  20-SEP-1990 10:20:16.41  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      CHARACTER MSG_STRING*40
      INTEGER I,J,K,ISTAT,UNIT
      INTEGER PAKADR,PHYADR
      INTEGER SCALE,NEGL,ADDR
      INTEGER ICAD,ICRATE,JCRATE,ICARD,IBLS,ITOWER,IDEPTH
      INTEGER INDEX,IETA,IPHI,ILYR,IER
      INTEGER LCADT
      LOGICAL CEXIST
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
C----------------------------------------------------------------------
      ISTAT=0
C
C ****  LOOP OVER CAD ADDRESSES
C
      SCALE = 0
      NEGL  = 0
      ADDR = 0
      DO 300 ICAD = 1, 2
        DO 310 ICRATE = 0, NADCRC - 1
          CALL BKCADT (LCADT )
          JCRATE = (ADCR00-1+ICAD) + 10*ICRATE
          IC(LCADT+2) = JCRATE
          DO 320 ICARD = 0, NADCC - 1
            DO 330 IBLS = 0, NBLSC - 1
              DO 340 ITOWER = 0, NEFC - 1
                DO 350 IDEPTH = 0, NDEPTC - 1
                  CALL CADPAK(ICARD,IBLS,ITOWER,IDEPTH,SCALE,NEGL,ADDR)
                  INDEX = ADDR / 4
                  PAKADR = 0
                  PHYADR=PAKADR
                  IC(LCADT + INDEX + 3) = PHYADR
                  CALL CADPH(JCRATE,ICARD,IBLS,ITOWER,IDEPTH,
     &               IETA,IPHI,ILYR,IER)
                  IF (IER.NE.0) GOTO 350
                  IF (.NOT.CEXIST(IETA,IPHI,ILYR)) GOTO  350
C                 pack addresses
                  BYTES(BYTE4)=IETA
                  BYTES(BYTE3)=IPHI
                  BYTES(BYTE2)=ILYR
                  BYTES(BYTE1)  = 0
                  PHYADR=PAKADR
                  IC(LCADT + INDEX + 3) = PHYADR
  350           CONTINUE
  340         CONTINUE
  330       CONTINUE
  320     CONTINUE
  310   CONTINUE
  300 CONTINUE
      ISTAT = 0
  200 FORMAT(' ERROR GETTING FREE UNIT, RETURN')
  999 RETURN
      END
