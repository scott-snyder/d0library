      FUNCTION GTCHFR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SET UP THE CHFORM BLOCK FOR USE IN AUTO_FMT
C-
C-   Returned value  : IOK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   3-MAY-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:AUTOF.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C-------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBQ.INC'         ! ZEBRA COMMON BLOCKS
      INCLUDE 'D0$INC:MZCA.INC'
      INCLUDE 'D0$INC:MZCB.INC'
      INTEGER GTCHFR,DRPFRM,IOK,LDUM
C----------------------------------------------------------------------
      CALL GTZFRM(ISTOR,LBANK,CHFORM)  !only do it for a new bank
C LIFT FORMAT BANK.
      CALL MZBOOK(ISTOR,LFRMT,LDUM,2,'FRMT',1,1,BNKLEN,1,0)!STANDALONE BANK
C
      CALL DCD_CHFRM(CHFORM,QQ(LBANK+KQS+1),BNKLEN,
     &    QQ(KQS+LFRMT+1),IOK)     !Decodes CHFORM
C Decode CHFORM here INTO  FRMT.
      LBANKL = LBANK
      GTCHFR = IOK
      RETURN
      ENTRY DRPFRM()
      CALL MZDROP(ISTOR,LFRMT,' ')
      DRPFRM = 0                         ! OK DROP
  999 RETURN
      END
