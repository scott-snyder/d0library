      SUBROUTINE PRT_PED_GNS(PRUNIT,TASK,CRATE,CARD,IGN,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print CPD1,CPD8,CGN1,CGN8 banks
C-
C-   Inputs  : PRUNIT = Print unit
C-             TASK   = 1,2 Print  Pedestal bank. =3 Print Gains bank
C-             CRATE  = ADC crate number
C-             CARD   = ADC card number. If CFL = 'ALL',CARD not used
C-             IGN    = 0 Gain X8, =1 Gain X1
C-             CFL    = flag to control printout. 
C-                    = 'ONE' For one bank only
C-                    = 'ALL' All such banks
C-             IFL    = 0 Header printout only
C-             IFL    = >0 list of values and sigmas
C-
C-   Created  26-DEC-1987   Rajendran Raja
C-   Modified  2-MAR-1989   Jan Guida,   PRTPDG now in D0$PARAMS
C-   Updated  21-NOV-1990   Jan Guida  Added CRATE argument, and ability 
C-                                      to do multiple crates 
C-   Updated   7-JUN-1991   Jan Guida  Pedestals:  TASK = 1 or 2 (add 2)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TASK,IGN,CARD,LINKH,LINK,LZFIND,PRUNIT,IFL,CRATE
      CHARACTER*(*) CFL
      CHARACTER*4 BANK
      CHARACTER*80 STRING
      INTEGER NDATA,LOC,LOC1,NCHA,K,I
      DATA NCHA/4/
C
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPD1.LINK'
      INCLUDE 'D0$LINKS:IZCPD8.LINK'
      INCLUDE 'D0$LINKS:IZCGN1.LINK'
      INCLUDE 'D0$LINKS:IZCGN8.LINK'
C----------------------------------------------------------------------
      IF ( TASK.LT.3 ) THEN      !Pedestals
        LCPDH = LZFIND(IDVSTP,LCPDH,CRATE,9)   !Finds Bank with Crate
        IF (LCPDH.EQ.0) THEN
          WRITE(STRING,5)CRATE
    5     FORMAT(' ERROR in PRT_PED_GNS: no pedestal bank for crate ',
     &      I2)
          CALL INTMSG(STRING)
          GO TO 999
        ENDIF
        IF ( IGN.EQ.0 ) THEN     !X8 gains
          LINKH = LC(LCPDH-IZCPD8)
          BANK = 'CPD8'
        ELSEIF ( IGN.EQ.1 ) THEN !X1 gains
          LINKH = LC(LCPDH-IZCPD1)
          BANK = 'CPD1'
        ENDIF
      ELSE                       !Gains
        LCGNH = LZFIND(IDVSTP,LCGNH,CRATE,9)   !Finds Bank with Crate
        IF (LCGNH.EQ.0) THEN
          WRITE(STRING,6)CRATE
    6     FORMAT(' ERROR in PRT_PED_GNS: no gain bank for crate ',I2)
          CALL INTMSG(STRING)
          GO TO 999
        ENDIF
        IF ( IGN.EQ.0 ) THEN     !X8 gains
          LINKH = LC(LCGNH-IZCGN8)
          BANK = 'CGN8'
        ELSEIF ( IGN.EQ.1 ) THEN !X1 gains
          LINKH = LC(LCGNH-IZCGN1)
          BANK = 'CGN1'
        ENDIF
      ENDIF
C
      LINK = LINKH
      IF(CFL.EQ.'ONE')LINK  = LZFIND(IDVSTP,LINKH,CARD,11)
C Finds Bank with Card
C
 1    IF(LINK.GT.0) THEN
        WRITE(PRUNIT,101) BANK,LINK,IC(LINK-5)
        WRITE(PRUNIT,102) (IC(LINK+I),I=1,13),
     +       (I,IC(LINK+13+I),MEANING(I),I=1,16)
        IF(IFL.GT.0) THEN
          NDATA=(IC(LINK-1)-NHEAD)/NCHA/2
          DO 10 I = 1,NDATA
            LOC=(I-1)*NCHA+1
            LOC1=LINK+NHEAD+(I-1)*NCHA*2
            WRITE(PRUNIT,104) BANK,LOC,(C(LOC1+K),K=1,2*NCHA)
 10       CONTINUE
        ENDIF
C
        IF(CFL.NE.'ONE') THEN
          LINK=LC(LINK)
          GOTO 1
        ENDIF   
      ENDIF
      RETURN
C
 101  FORMAT(/,
     $' ========================================================='/
     $  6X,A,   ' bank for an ADC card: '/
     $'      LINK =',I8,'  NCLINK =',I8/
     $' ========================================================='/)
 102  FORMAT(
     $' Type                          = ',I8/
     $' Status                        = ',I8/
     $' Controller Word (hexadec)     = ',Z8/
     $' MIN_RUN_NUM                   = ',I8/
     $' MAX_RUN_NUM                   = ',I8/
     $' ACT_RUN_NUM                   = ',I8/
     $' DATE                          = ',I8/
     $' TIME                          = ',I8/
     $' Crate ID (octal)              = ',O8/
     $' ADC_CARD Indx                 = ',I8/
     $' ADC_CARD #                    = ',I8/
     $' ALL_CHANNELS                  = ',I8/
     $' OK_CHANNELS                   = ',I8/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Bad_channeL flag ',I2,' = ',I8,2X,A,/
     $' Spare        = ',I8///' Individual Channels:'/)
C
 104  FORMAT(
     $   A,'(',I4,') = ',F8.2,' +/- ',F5.2,';  ',F8.2,' +/- ',F5.2,
     $';  ',F8.2,' +/- ',F5.2,';  ',F8.2,' +/- ',F5.2)
C
  999 RETURN
      END
