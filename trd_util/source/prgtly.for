      SUBROUTINE PRGTLY(LUNOUT,ILAY)
C ----------------------------------------------------------------------
C -
C -   PURPOSE AND METHODS : PRINT OF TRD ZEBRA BANK GTLY(USER'S HITS
C-                          IN LAYER ILAY)
C -
C -   INPUTS  :LUNOUT = unit for output
C_
C -   OUTPUTS :
C -
C-   Created  11-JAN-1988   A. ZYLBERSTEJN
C-   Updated  13-SEP-1988   A. ZYLBERSTEJN
C-   Updated  30-AUG-1989   A. Zylberstejn  Introduce logical unit for print
C-                                          as argument of the routine
C-
C-
C ----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C      INCLUDE 'D0$INC:GTRHLN.INC/LIST'
      INCLUDE 'D0$INC:WORKSP.INC/LIST'
      INCLUDE 'D0$LINKS:IZGTRH.LINK/LIST'
      INCLUDE 'D0$LINKS:IZGEAN.LINK/LIST'
      INCLUDE 'D0$LINKS:IZGHIT.LINK/LIST'
      INTEGER LGHIT,LUNOUT,NERR
C
      INTEGER I,ILAY,LL,IE,IW,IADR(256,2),IHIT(256,2),NFRE,ITY
      INTEGER J,JW,K,KW,KS,NF,NWHIT(2),INDEX(300),IWIR(3),IM
      CHARACTER*4 PARTYP(50)
      REAL EHIT(256,2),ENER(3),EN
      REAL ESTR
      INTEGER ITRA,ISTR,GZGTRH,LGTRH,LGTLY
C
      DATA PARTYP/'GAMM',2*'ELEC','  NU',2*'MUON','PI 0',2*'PION',
     &  '  KL','  K+','  K-','NEUT',2*'PROT','  KS',' ETA','LAMB',
     &  2*'SIGM',30*'    '/
      DATA NERR/0/
C      -----------------------------------------------------------------
      IF(LUNOUT.LE.0)GO TO 999 ! CHECK OUTPUT UNIT
      LGTRH = GZGTRH()
      IF ( LGTRH .EQ. 0) THEN !Check GTRH
        NERR=NERR+1
        IF(NERR.LE.20)
     +    CALL INTMSG(' PROBLEM_TRD IN PRGTLY: BANK LGTRH NOT BOOKED')
        GO TO 999
      ENDIF
C
      LGTLY=LQ(LGTRH-ILAY)
      IF(LGTLY.LE.0 )THEN
        NERR=NERR+1
        IF(NERR.LE.20)
     &    CALL INTMSG(' PROBLEM_TRD IN PRGTLY: BANK LGTLY NOT BOOKED')
        GO TO 999
      END IF
      LL=LGTLY
      CALL VZERO(IADR,512)
      NWHIT(1)=0
      NWHIT(2)=0
      NFRE=0
   20 IF(LL.LE.0)GO TO 100
      KW=0
C  LOOP ON THE  RECORDED HITS IN GTLY BANK
      DO 30 I=1,4
        IW=MOD(IQ(LL+14+I),1000)
        IF(IW.LE.0)GO TO 32
        IE=IQ(LL+14+I)/1000
        KW=KW+1
        EN=FLOAT(IE)/100.
        JW=IW
        IF(ILAY.EQ.3)JW=(IW-1)/2+1
        IF(IADR(JW,1).LE.0)THEN
          NWHIT(1)=NWHIT(1)+1
          EHIT(NWHIT(1),1)=EN
          IADR(JW,1)=NWHIT(1)
          IHIT(NWHIT(1),1)=JW
        ELSE
          J=IADR(JW,1)
          EHIT(J,1)=EHIT(J,1)+EN
        END IF
   30 CONTINUE
   32 CONTINUE
      KS=0
C  LOOP ON THE  RECORDED HITS IN GTLY BANK FOR THE CATHODE STRIPS
      IF(STRD(2).NE.1.)THEN
        DO 40 I=1,6
          IW=MOD(IQ(LL+18+I),1000)
          IF(IW.LE.0)GO TO 42
          IE=IQ(LL+18+I)/1000
          KS=KS+1
          EN=FLOAT(IE)/100.
          JW=IW
          IF(IADR(JW,2).LE.0)THEN
            NWHIT(2)=NWHIT(2)+1
            EHIT(NWHIT(2),2)=EN
            IADR(JW,2)=NWHIT(2)
            IHIT(NWHIT(2),2)=JW
          ELSE
            J=IADR(JW,2)
            EHIT(J,2)=EHIT(J,2)+EN
          END IF
   40   CONTINUE
   42   CONTINUE
      END IF   !CATHODES
      IF(MOD(NFRE,13).EQ.0)THEN
        WRITE(LUNOUT,1034)
        WRITE(LUNOUT, 1036)
      END IF
      NFRE=NFRE+1
      WRITE(LUNOUT,1046)
      ITY=IQ(LL+14)
      ITRA=IQ(LL+25)
      IM=MIN0(KW,3)
      DO 99 I=1,IM
        IWIR(I)=MOD(IQ(LL+14+I),1000)
        J=IQ(LL+14+I)/1000
        ENER(I)=FLOAT(J)/100.
   99 CONTINUE
      ISTR=MOD(IQ(LL+19),1000)
      J=IQ(LL+19)/1000
      ESTR=FLOAT(J)/100.
      WRITE(LUNOUT,1038) ITRA,PARTYP(ITY),
     &          Q(LL+10),IWIR(1),ENER(1),ISTR,ESTR,Q(LL+9),
     &          SQRT(Q(LL+7)**2+ Q(LL+8)**2)
      ISTR=MOD(IQ(LL+20),1000)
      J=IQ(LL+20)/1000
      ESTR=FLOAT(J)/100.
      IF(KW.GE.2)THEN
        WRITE(LUNOUT,1050) IWIR(2),ENER(2),ISTR,ESTR,Q(LL+3),
     &          SQRT(Q(LL+1)**2+Q(LL+2)**2)
      ELSE
        WRITE(LUNOUT,1099) ISTR,ESTR, Q(LL+3),SQRT(Q(LL+1)**2+
     &          Q(LL+2)**2)
      END IF
      ISTR=MOD(IQ(LL+21),1000)
      J=IQ(LL+21)/1000
      ESTR=FLOAT(J)/100.
      IF(KW.GE.3)THEN
        WRITE(LUNOUT,1052) IWIR(3),ENER(3),ISTR,ESTR,Q(LL+6),
     &          SQRT(Q(LL+4)**2+Q(LL+5)**2)
      ELSE
        WRITE(LUNOUT,1042) ISTR,ESTR,Q(LL+6),
     &          SQRT(Q(LL+4)**2+Q(LL+5)**2)
      END IF
  465 CONTINUE
 1034 FORMAT('1I        GEANT           I             TRD
     &  HITS             I      POSITIONS        I',/
     &  ' I------------------------I-----------------------',
     &'------------I-----------------------I')
 1036 FORMAT(' I TRACK I TYPE I   E/M   I   HIT  I
     &  DEPOS. I   HIT  I DEPOS. I     Z     I     R     I',/,
     &  ' I       I      I         I  WIRES I
     &  ENERGY I STRIPS I ENERGY I           I           I',/,
     &  ' I       I      I         I NUMBER I',
     &'        I NUMBER I        I           I           I')
 1038 FORMAT(' ','I',I5,2X,'I ',A4,1X,
     &'I',F7.1,'  I ',I4,3X,'I',F7.2,1X,
     &'I ',I4,3X,'I',F7.2,1X,'I',F8.2,3X,'I',F8.2,3X,'I ORIGIN')
 1099 FORMAT(' ','I',5X,2X,'I ',5X,'I',7X,'  I',7X,' I',8X
     &,'I ',I4,3X,'I',F7.2,1X,'I',F8.2,3X,'I',F8.2,3X,'I ENTRANCE')
 1042 FORMAT(' I',7X,'I ',4X,' I         I        I',8X
     &,'I ',I4,3X,'I',F7.2,1X,'I',F8.2,3X,'I',F8.2,3X,'I EXIT')
 1046 FORMAT(' I-------I------I---------I--------I--------I',
     &'--------I--------I-----------I-----------I')
 1050 FORMAT(' ','I',5X,2X,'I',6X,'I',8X,' I ',I4,3X,'I',F7.2,1X
     &,'I ',I4,3X,'I',F7.2,1X,'I',F8.2,3X,'I',F8.2,3X,'I ENTRANCE')
 1052 FORMAT(' ','I',5X,2X,'I',6X,'I',8X,' I ',I4,3X,'I',F7.2,1X
     &,'I ',I4,3X,'I',F7.2,1X,'I',F8.2,3X,'I',F8.2,3X,'I EXIT')
      LL=LQ(LL)
      GO TO 20
  100 CONTINUE
      IF(PTRD.LT.4)GO TO 999
      WRITE(LUNOUT,*)'        LAYER',ILAY ,'NUMBER OF HIT WIRES',NWHIT
      DO 320 K=1,2
        IF (K.EQ.2 .AND. STRD(2).EQ.1.) GO TO 320
        IF(K.EQ.1)WRITE(LUNOUT,*)' ANODE WIRES '
        IF(K.EQ.2)WRITE(LUNOUT,*)' CATHODE STRIPS'
        IF(NWHIT(K).LE.0)GO TO 320
        WRITE (LUNOUT,1032)
C       SORTING
        CALL UCOPY(IHIT(1,K),IWS(2001),NWHIT(K))
        CALL UCOPY(EHIT(1,K),WS(2501),NWHIT(K))
        CALL SORTZV(IHIT(1,K),INDEX,NWHIT(K),-1,0,0)
        DO 199 I=1,NWHIT(K)
          J=INDEX(I)
          IHIT(I,K)=IWS(2000+J)
          EHIT(I,K)= WS(2500+J)
  199   CONTINUE
C
        NF=NWHIT(K)/5
        IF (NF.GT.0) THEN
          DO 160 I=1,NF
            J=5*(I-1)
            WRITE(LUNOUT,1060) (IHIT(J+JW,K ),EHIT(J+JW,K),JW=1,5)
  160     CONTINUE
        END IF
        IF(MOD(NWHIT(K),5).NE.0)THEN
          WRITE(LUNOUT,1060) (IHIT(J,K),EHIT(J,K),J=5*NF+1,NWHIT(K))
        END IF
  320 CONTINUE
  999 RETURN
 1000 FORMAT(' --ISAJET TRACK NUMBER:',I4,' GEANT INFORMATION:
     &        TRACK NUMBER:',I4, ' STACK NUMBER:',I3,'  TYPE:',I3,
     &        ' E/M',G10.4)
 1010 FORMAT(' TOTAL',I4,F8.2)
 1020 FORMAT(8X,I4,F8.2)
 1032 FORMAT(5(' WIRE NB. ENERGY(KeV)'))
 1060 FORMAT(5(I8,3X,F6.2,3X))
      END
