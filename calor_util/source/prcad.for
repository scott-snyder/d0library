      SUBROUTINE PRCAD(LUN,LLCAD,NCAD,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out for CAD banks - Calorimeter ADC RAW DATA
C-
C-   Inputs  :PRUNIT= unit number for printout
C-            LLCAD = bank address
C-                   = 1 for CAD1 print
C-                   = 2 for CAD2 print
C-            NCAD = bank number      (not used)
C-            CFL   = flag to control printout (not used)
C-            IFL   = flag to control printout
C-                  = 0 RAW HEX DUMP
C-                  = 1 print full title for each NON ZERO ADC word
C-
C-   Outputs : none
C-   Controls: IFL  = flag to control printout
C-                  = 0 RAW HEX DUMP
C-                  = 1 print full title for each NON ZERO ADC word
C-
C-   Created  1-APR-1989   Wyatt Merritt, Chip Stewart
C-   Updated  27-APR-1990   Chip Stewart  - MODERNIZED USING GTCAD ROUTINES 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:HEADER.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
C
      LOGICAL FIRST,OK,FIRST_CELL
      LOGICAL CSINGL,CDOUBL,CFORC1,CFORC8,CPDDAT,CZRSUP,
     &  CSNSUP,CRENRM,CPDSUB,CRTERR
C
      INTEGER LLCAD,LCAD,LUN,POINTER,SYNC,CONTRL,VERT,NCAD,IFL
      INTEGER CLV1ID,BLSMOD,ADCMOD,CIERR,CRSTAT,CRVERT
      INTEGER CSYNC,CRATE,NCRDS,SYNFLG,UNPK(20)
      INTEGER ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM,JCRATE,KCRATE
      INTEGER IETAC,IPHIC,ILYRC,PULSHT,IDATA,I,J,ERR,K,NN,N
      INTEGER ICRATE,WCNT,KCOND,LDAT,CRATE1,CRATE2,NCRATE,NCH
      INTEGER JBYT,JBIT,INAME,ND,HEADER_LEN,VERSION,PULSER,IER
      CHARACTER*(*) CFL
      CHARACTER*4 NAME
C
C---------------------------------------------------------
      DATA FIRST /.TRUE./
      DATA SYNFLG /65535/  ! octal 177777
C---------------------------------------------------------
C
      IF (FIRST) THEN
        CALL ERRMSG('CALORIMETER','PRCAD',' PRCAD called.','I')
      ENDIF
C
C--   check if bank does exist
C
        IF(LLCAD.EQ.1) THEN
          LCAD = LQ(LHEAD - IZCAD1)
        ELSE IF(LLCAD.EQ.2) THEN
          LCAD = LQ(LHEAD - IZCAD2)
        ELSE IF(LCAD.EQ.0) THEN
          GO TO 999
        END IF
        ND = IQ(LCAD-1)
        CALL DHTOC (4,IQ(LCAD-4),NAME)  ! Get bank name 
        WRITE (LUN,800) NAME,'NL',IQ(LCAD-3),'NS',IQ(LCAD-2),'ND',
     &    IQ(LCAD-1)
  800 FORMAT( //A10,3(5X,A5,I6))
      IF(IFL.EQ.0) THEN
        DO 45 J = 1,ND+8,8
          K = J + 7
          WRITE (LUN,6000) (IQ(LCAD+NN),NN=J,K)        
 6000     FORMAT(8(1X,Z9.8))                                
   45   CONTINUE
        GOTO 999
      END IF
C
C--   Decode header and print
C
      NCRATE = 1
      ICRATE = 0
   20 CALL GTCAD_HEADER(LLCAD,-NCRATE,HEADER_LEN,SYNC,
     &  CONTRL,VERSION,VERT,PULSER,IER)
      NCRATE = NCRATE + 1
      WRITE (LUN,901) 'HEADER_LEN','SYNC','CONTRL',
     &  'VERSION','STAT/VERT','PULSER'
      IF(HEADER_LEN.LT.5) WRITE (LUN,900) HEADER_LEN,SYNC,CONTRL,
     &  VERSION,VERT
      IF(HEADER_LEN.EQ.5) WRITE (LUN,900) HEADER_LEN,SYNC,CONTRL,
     &  VERSION,VERT,PULSER
  900 FORMAT( 6Z12.9)  
  901 FORMAT(/,6A12)
C
      CSYNC = JBYT(SYNC,1,16)
      IF (CSYNC .NE. SYNFLG) THEN
        WRITE (LUN,902)
  902   FORMAT(' SYNC WORD NOT CORRECT.')
      ENDIF
      CLV1ID = JBYT(SYNC,17,16)
      CRATE = JBYT(CONTRL,25,8)
      NCRDS = JBYT(CONTRL,17,8)
      BLSMOD = JBYT(CONTRL,9,4)
      ADCMOD = JBYT(CONTRL,1,5)
      CSINGL = JBIT(CONTRL,12).NE.0
      CDOUBL = JBIT(CONTRL,11).NE.0
      CFORC1 = JBIT(CONTRL,10).NE.0 .AND. JBIT(CONTRL,9).NE.0
      CFORC8 = JBIT(CONTRL,10).NE.0 .AND. JBIT(CONTRL,9).EQ.0
      CPDDAT = JBIT(CONTRL,1).EQ.0
      CZRSUP = JBIT(CONTRL,3).NE.0
      CSNSUP = JBIT(CONTRL,2).NE.0
      CRENRM = JBIT(CONTRL,5).NE.0
      CPDSUB = JBIT(CONTRL,4).NE.0
      CRTERR = VERT.LT.0
      CIERR = JBYT(VERT,25,7)
      CRSTAT = JBYT(VERT,17,8)
      CRVERT = JBYT(VERT,1,7)
      IF (JBIT(VERT,8).NE.0) CRVERT = -CRVERT
      WRITE (LUN,1000) CRATE,NCRDS
      WRITE (LUN,1001) CLV1ID,BLSMOD,ADCMOD,CIERR,CRSTAT,CRVERT
      WRITE (LUN,1002) CSINGL,CDOUBL,CFORC1,CFORC8,CPDDAT,CZRSUP,
     &  CSNSUP,CRENRM,CPDSUB
 1000 FORMAT('  CRATE ',I4,'  # ADCS -1 ',I4)
 1001 FORMAT(' CLV1ID ',I4,' BLSMOD ',I4,' ADCMOD' ,I4,
     &  ' CIERR ',I4,' CRSTAT ',I4,' CRVERT ',I4)
 1002 FORMAT(' SINGL ',L1,' DOUBL ',L1,' X1 ',L1,' X8 ',L1,' PDDAT ',
     &  L1,' ZS ',L1,' SS ',L1,' RN ',L1,' PDSUB ',L1)
C
C
      CALL GTCAD_TOTAL(LLCAD,NCH,IER)
      FIRST_CELL = .TRUE.
      N = 0
      DO 50 I = 0 , NCH
        CALL GTCAD(LLCAD,FIRST_CELL,JCRATE,IDATA,IER)
        IF (IER.NE.0) GOTO 50
        FIRST_CELL = .FALSE.
        LDAT = JBYT(IDATA,1,16)
        IF(LDAT.EQ.0) GOTO 40
        CALL CADUPK(JCRATE,IDATA,KCRATE,ADC,BLS,ROTOW,DEPTH,SCALE,
     &      NEGLIM)
        CALL CADPH(KCRATE,ADC,BLS,ROTOW,DEPTH,
     &        IETAC,IPHIC,ILYRC,KCOND)
        IF (KCOND .NE. 0) THEN
          WRITE (LUN,2000) KCRATE,ADC,BLS,ROTOW,DEPTH
 2000     FORMAT(' ERROR IN CADPH: ',5(I3,1X))
          GO TO 40
        ENDIF
        CALL CALPH(IDATA,PULSHT)
        IF (PULSHT .EQ. 0) GO TO 40
        N = N + 1
	J = J + 10
        IF (J.EQ.20) J = 0
        UNPK(1+J) = IDATA
        UNPK(2+J) = KCRATE
        UNPK(3+J) = ADC
        UNPK(4+J) = BLS
        UNPK(5+J) = ROTOW
        UNPK(6+J) = DEPTH
        UNPK(7+J) = PULSHT
        UNPK(8+J) = IETAC
        UNPK(9+J) = IPHIC
        UNPK(10+J)= ILYRC
        IF(MOD(N,50).EQ.0) WRITE (LUN,3001)
     &    'WORD','CRATE','CARD','BLS','TOWER','DEP','PH','ETA','PHI',
     &    'LYR',
     &    'WORD','CRATE','CARD','BLS','TOWER','DEP','PH','ETA','PHI',
     &    'LYR'
        IF(J.EQ.10) THEN
          WRITE (LUN,3000) (UNPK(K),K=1,20)
        ELSE IF(I.EQ.NCH) THEN
          WRITE (LUN,3000) (UNPK(K),K=1,10)
        END IF
 3000   FORMAT( 2(1X,Z10.9,5I4,I6,3I4))
 3001   FORMAT( 2(1X,A10,5A4,A6,3A4))
   40 CONTINUE
   50 CONTINUE
  999 RETURN
C
      END
