      SUBROUTINE CADT_ADCPHY(ICRATE,ADC,BLS,ROTOW,DEPTH,
     &  IETA,IPHI,ILYR,IER)
C--------------------------------------------------------------------
C-
C-  Purpose and Methods : Converts an address in the ADC system to the
C-                        PHYsics system used in analysis. The inputs
C-                        are just the separated bit fields from the
C-                        address area of the raw data word. In the
C-                        current version, ICRATE, ADC, and DEPTH are
C-                        checked for validity of range. (The other
C-                        variables fill their bit fields and don't
C-                        need checking if provided by CADUPK.)
C-                        This routine has the functionality of CADPH
C-                        but uses  the CADT bank instead of code
C-                        and CDPLYR to determine IETA,IPHI, and ILYR.
C-
C-  Inputs  : ICRATE    ADC crate number         [NCRATE*10+BANK]
C-                           where NCRATE=0,5 and BANK=7,8
C-            ADC       ADC card number in crate [0,11]
C-            BLS       BLS number in ADC        [0,7]
C-            ROTOW     readout tower in BLS     [0,3]
C-            DEPTH     depth in readout tower   [0,11]
C-
C-  Outputs : IETAC     offline eta index        [-37,-1],[1,37]
C-            IPHIC     offline phi index        [1,64]
C-            ILYRC     offline radial index     [1,17]
C-            IER       return code: 0 = OK
C-                                  -1 = non-existant channel
C-                                  -2 = BAD ADDR < 0, ADDR>MAX_ADDR
C-                                       ADC,BLS,ROTOW,DEPTH
C-                                  -3 = BAD ICRATE < 0, ICRATE>5
C-   Controls: CADT bank
C-
C-   Created  12-MAR-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER  GZCADT,LCADT,CADT_LINK(2,0:5)
      INTEGER  NCAD,NCRATE,MAX_ADDR,ADC,BLS,ROTOW,DEPTH,ADDR
      INTEGER  IWORD,ICRATE,PCRATE,ICAD,IER,PAKADR
      INTEGER  IETA,ILYR,IPHI,IBITS
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
      CHARACTER MSG*80
      LOGICAL FIRST
      DATA FIRST/.TRUE./,PCRATE/0/
C----------------------------------------------------------------------
C
C ****  SETUP CADT LOOK-UP TABLE
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL STP_INZLNK
        LCADT = GZCADT ()
        DO NCAD = 1, 2
          DO  NCRATE = 0, 5
C
C ****  RESERVE LINK IN STP_ZLINKA FOR LINK TO NCAD,NCRATE
C
            CALL STP_GSLINK('CADT',CADT_LINK(NCAD,NCRATE) )
            STP_LSLINK(CADT_LINK(NCAD,NCRATE)) = LCADT
            IF (LCADT.LE.0) THEN
              CALL ERRMSG('CALOR_UNPACK','CAEPFL',
     &          'CADT CHAIN BAD','W')
              GOTO 999
            END IF
            LCADT = LC(LCADT)
            IF( LCADT.LE.0) THEN
              GOTO 77
            END IF
          END DO
        END DO
   77   LCADT = GZCADT ()
C
C ****  CHECK CAD VERSION TO FIX CADT TABLE IF SFTVSN.EQ.1
C
        CALL CADPAK(NADCC-1,NBLSC-1,NEFC-1, NDEPTC-1,0,0,MAX_ADDR)
        MAX_ADDR = IBITS(MAX_ADDR,2,13)
      ENDIF
      IER = 0
      CALL CADPAK(ADC,BLS,ROTOW,DEPTH,0,0,ADDR)
      ADDR = IBITS(ADDR,2,13)
      IF (ADDR.GT.MAX_ADDR) THEN
        WRITE(MSG,1004) ADDR
        CALL ERRMSG('BAD CAD ADDR','CADT_ADCPHY',MSG,'W')
        IER = -2
        GOTO 999
      ELSE IF(ADDR.LT.0) THEN
        WRITE(MSG,1004) ADDR
        CALL ERRMSG('BAD CAD ADDR','CADT_ADCPHY',MSG,'W')
        IER = -2
        GOTO 999
      END IF
C
C ****  ICRATE CAD1=07,17,27,37,47,57; CAD2=08,18,28,38,48,58;
C ****  5000CH TEST=67; QUADRANT TEST = 77; TB91 LOAD 2 =87
C
      IF(ICRATE.NE.PCRATE) THEN
        PCRATE = ICRATE
        NCRATE = ICRATE / 10
        NCAD = MOD(ICRATE,10)-6
        IF (D0VSN.EQ.64 .AND. CALVSN.EQ.2) THEN   !TB90L2 ICRATE=87
          NCAD   = 1  !CAD1 bank
          NCRATE = 0  !ONE CRATE
        END IF
        IF( (NCRATE.GT.5).OR.(NCRATE.LT.0)) THEN
          PCRATE = -1
          IER = - 3
          GOTO 999
        END IF
        ICAD = NCAD
      END IF
      PAKADR = IC(STP_LSLINK(CADT_LINK(ICAD,NCRATE)) + ADDR + 3)
      IETA  = BYTES(BYTE4)
      ILYR  = BYTES(BYTE2)
      IPHI  = BYTES(BYTE3)
      IF (ILYR.LT.1) IER =  -1
  999 RETURN
 1004 FORMAT(1X,' ADDR = ',Z12.8)
      END
