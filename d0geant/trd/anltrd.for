      LOGICAL FUNCTION ANLTRD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TRD POST_EVENT ANALYSIS CALLED AT THE END OF
C-                          EACH EVENT BY ANLCEN CALLED BY GUOUT
C-
C-
C-   Inputs  : GTLY ZEBRA BANK (TRD USER'S BANKS)
C-   Outputs :QUANTITIES IN COMMON /TRHITW/
C-
C-   Created  13-JAN-1988   A. ZYLBERSTEJN
C-   Updated  14-JUL-1989   Harrison B. Prosper
C-   Made into pbd logical function
C-   Updated  22-APR-1991   A. Zylberstejn   Use TRHIST to fill or not histos
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:TRHITW.INC/LIST'
      INCLUDE 'D0$INC:TRINFO.INC'
      INCLUDE 'D0$INC:WORKSP.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER I,IE,IFOIS,IST,IW,LL,NSECT(2),NW,NWS
      REAL EN,ENSECT(16,2)
      INTEGER LGTRH,GZGTRH,LGTLY
C
      DATA IFOIS/0/
C
C----------------------------------------------------------------------
      ANLTRD = .TRUE.
      IF ( DCEN .LT. 4 ) GOTO 999
      IF ( DTRD .LT. 4 ) GOTO 999
C
C
      IFOIS=IFOIS+1
      IF(IDEBUG.GE.1)THEN
        IF (PTRD.GE.4) THEN
          CALL PRGTRH(LOUT)                  !PRINT USER'S BANKS
          IF(STRD(4).EQ.1 )CALL PRTRDH(LOUT) !PRINT IDEALIZED BANKS
        ENDIF
        IF (PTRD.GE.2)CALL PRCDD4(LOUT)   !PRINT FADC CONTENT
C      CALL GTRDFA !Sharon routine
      ENDIF
      LGTRH = GZGTRH()
      IF ( LGTRH .EQ. 0 ) THEN
        WRITE(LOUT,*)' PROBLEM_TRD IN ANLTRD: BANK LGTRH NOT BOOKED'
        GO TO 999
      ENDIF
C  COMPUTE THE NUMBER OF HIT WIRES IN EACH LAYER
      CALL VZERO(NBTHIT,6)
      CALL VZERO(ENSECT,32)
      DO 100 IST=1,3  !LOOP ON THE STACKS
        LGTLY=LQ(LGTRH-IST)
        IF(LGTLY.LE.0)THEN
          WRITE(LOUT,*)' PROBLEM_TRD IN ANLTRD: BANK LGTLY NOT BOOKED'
          GO TO 999
        ENDIF
        LL=LGTLY
        CALL VZERO(IWS,512)
        NW=0
        NWS=0
   20   IF(LL.LE.0)GO TO 96
        DO 30 I=15,18  !LOOP ON THE HIT ANODES
          IW=MOD(IQ(LL+I),1000)
          IF(IW.LE.0)GO TO 32
          IE=IQ(LL+I)/1000
          EN=FLOAT(IE)/100.
          IF(IST.EQ.3)IW=(IW-1)/2+1
          NSECT(1)=(IW-1)/16+1
          ENSECT(NSECT(1),1)=ENSECT(NSECT(1),1)+EN
C       print*,' IW,NSECT',IW,NSECT(1)
          IF(IWS(IW).LE.0)THEN      !  DEFINE A NEW WIRE
            NW=NW+1
            IWS(IW)=NW
            NUMTWH(NW,IST,1)=IW
            ENTWH (NW,IST,1)=EN
          ELSE                      !ADD UP ENERGY ON SAME WIRE
            ENTWH(IWS(IW),IST,1)=ENTWH(IWS(IW),IST,1)+EN
          ENDIF
   30   CONTINUE
   32   NBTHIT(IST,1)=NW
        IF(STRD(2).NE.1.)THEN
          DO 40 I=19,24  !LOOP ON THE HIT CATHODE STRIPS
            IW=MOD(IQ(LL+I),1000)
C            PRINT*,' CATHODE NON DECODEE',IW
            IF(IW.LE.0)GO TO 42
            IE=IQ(LL+I)/1000
            EN=FLOAT(IE)/100.
            IF(IST.EQ.3)IW=(IW-1)/2+1
            NSECT(2)=(IW-1)/16+1
C            PRINT*,' CATHODE DECODEE,IE',IE,' EN',EN
            ENSECT(NSECT(2),2)=ENSECT(NSECT(2),2)+EN
C            PRINT*,' IW,NSECT(2)',IW,NSECT(2)
            IF(IWS(IW+256).LE.0)THEN      !  DEFINE A NEW STRIP
              NWS=NWS+1
C              PRINT*,' NEW STRIP,NUMERO',NWS
              IWS(IW+256)=NWS
              NUMTWH(NWS,IST,2)=IW
              ENTWH (NWS,IST,2)=EN
            ELSE                      !ADD UP ENERGY ON SAME STRIP
              ENTWH(IWS(IW+256),IST,2)=ENTWH(IWS(IW+256),IST,2)+EN
            ENDIF
   40     CONTINUE
   42     NBTHIT(IST,2)=NWS
C          PRINT*,' NWS',NWS
        ENDIF
        LL=LQ(LL)
        GO TO 20
   96   CONTINUE
        IF(TRHIST)THEN
          CALL HF1(7007+IST,FLOAT(NW), 1.)
          CALL HF1(7207+IST,FLOAT(NWS),1.)
          EN=0.
          DO 68 I=1,NW
            EN=EN+ENTWH(I,IST,1)
            CALL HF1(7013+IST,ENTWH(I,IST,1),1.) !ETOT PER WIRE
   68     CONTINUE
          CALL HF1(7016+IST,EN,1.)!ETOT PER LAYER
          IF(STRD(2).NE.1.)THEN !CATHODES
            EN=0.
            DO 78 I=1,NWS  
              EN=EN+ENTWH(I,IST,2)
C            CALL HF1(7013+IST,ENTWH(I,IST,1),1.) !ETOT PER WIRE
   78       CONTINUE
C           CALL HF1(7016+IST,EN,1.)!ETOT PER LAYER
          ENDIF  !      CATHODES
        END IF
  100 CONTINUE
  999 RETURN
      END
