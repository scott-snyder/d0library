      SUBROUTINE CATDFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and Fill in the contents of the bank CATD
C-                         from the hardware towers energy bank CATE
C-
C-   Controls:
C-
C-   Modified  1-MAR-1993 Nobuaki Oshima 
C-         Fixed a bug on MUTWR(Max=64) exceed checking. 
C-   Created   9-DEC-1991 Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$LINKS:IZMUCA.LINK'
C
      INTEGER   LCATE,LCATD,GZCATE,GZCATD
      INTEGER   LPMUO,GZPMUO,LMUON,LMUCA
      INTEGER   NCH,NREP,IVER,IENR,IETA,JETA,IPHI,IER
      INTEGER   I,IC,IPNT,INX,IE,IP,IL,IEND,ISTAT
      INTEGER   EMPNT,HDPNT,MUOPNT,NV
      INTEGER   IMNETEM,IMNETHD,IMNMUOE,NREPET,NCELLS,IDLETA
      INTEGER   MUOFLG(NPHIL,2*NETAL)
      INTEGER   NTOWR
      INTEGER   PACTWR
      INTEGER   EMTWR(256),HADTWR(256),MUTWR(64)
      REAL      E(4),EHAD,MNETEM,MNETHD,MNMUOE,ENERGY,ET
      REAL      EXEM(NPHIL,2*NETAL),EXTOT(NPHIL,2*NETAL)
      REAL      EYEM(NPHIL,2*NETAL),EYTOT(NPHIL,2*NETAL)
      REAL      EZEM(NPHIL,2*NETAL),EZTOT(NPHIL,2*NETAL)
      REAL      EEM(NPHIL,2*NETAL), ETOT(NPHIL,2*NETAL)
      REAL      XC,YC,ZC,ZOT,ZV(10),DZ(10),THETAN,PHI,THETA
      REAL      ETANOM,ETATRU,DELETA,EUNIT,DEUNIT
C
      REAL    SMALL
      PARAMETER( SMALL = 1.0E-5 )
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA EUNIT,DEUNIT / .1, .01/
C----------------------------------------------------------------------
C-
C--- do initialization here if necessary.
C-
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CATD_RCP')
        CALL EZERR(IER)
        IF (IER .EQ. 0) THEN
          CALL EZGET('CATD_MINET_EM',MNETEM,IER)
          CALL EZGET('CATD_MINET_HAD',MNETHD,IER)
          CALL EZGET('CATD_MINE_MUON',MNMUOE,IER)
          CALL EZRSET
        ELSE
          CALL ERRMSG('CATD','CATDFL',
     &        ' NO_RCP - PROCESSING SKIPS CATDFL','W')
          GO TO 999
        ENDIF
      ENDIF
C-
C--- scan PMUO bank to get information about associated cells from
C--- MUCA bank and save IETA and IPHI of the tower which contains
C--- these cells.
C-
      CALL VZERO_i(MUOFLG, NPHIL*2*NETAL)
      LPMUO = GZPMUO(1)
   10 IF (LPMUO .LE. 0) GO TO 40
      LMUON = LQ(LPMUO-3)
      IF (LMUON .LE. 0) GO TO 20
      LMUCA = LQ(LMUON-IZMUCA)
      IF (LMUCA .LE. 0) GO TO 20
      NREPET = IQ(LMUCA+2)
      NCELLS = IQ(LMUCA+3)
      DO IC = 4,NCELLS,NREPET
        IPNT = LMUCA + IC
        IE = IQ(IPNT)
        IP = IQ(IPNT+1)
        IL = IQ(IPNT+2)
        IF(IE .LT. 0) THEN
          IE = IE + NETAL + 1
        ELSE
          IE = IE + NETAL
        ENDIF
        MUOFLG(IP,IE)  = 1
      ENDDO
   20 CONTINUE
      LPMUO = LQ(LPMUO)
      GO TO 10
   40 CONTINUE
C-
C--- check CATE bank is available or not, then scan CATE bank
C--- and fill array EXEM,EYEM,EZEM,EEM,EXTOT,EYTOT,EZTOT and ETOT
C-
      LCATE = GZCATE()
      IF (LCATE .GT. 0) THEN
        CALL VZERO(EXEM, NPHIL*2*NETAL)
        CALL VZERO(EXTOT,NPHIL*2*NETAL)
        CALL VZERO(EYEM, NPHIL*2*NETAL)
        CALL VZERO(EYTOT,NPHIL*2*NETAL)
        CALL VZERO(EZEM, NPHIL*2*NETAL)
        CALL VZERO(EZTOT,NPHIL*2*NETAL)
        CALL VZERO(EEM, NPHIL*2*NETAL)
        CALL VZERO(ETOT,NPHIL*2*NETAL)
        NCH  = IQ(LCATE-1)
        IVER = IQ(LCATE+1)
        NREP = IQ(LCATE+2)
C-
        DO I = 4,NCH,NREP
          INX  = LCATE + I
          E(1) =  Q(INX)
          E(2) =  Q(INX+1)
          E(3) =  Q(INX+2)
          E(4) =  Q(INX+3)
          IETA = IQ(INX+8)
          IPHI = IQ(INX+9)
          IF (IETA .LT. 0) THEN
            JETA = IETA + NETAL + 1
          ELSE
            JETA = IETA + NETAL
          ENDIF
          IF (IQ(INX+10) .EQ. 1) THEN
            EXEM(IPHI,JETA)  = E(1)
            EYEM(IPHI,JETA)  = E(2)
            EZEM(IPHI,JETA)  = E(3)
            EEM(IPHI,JETA)   = E(4)
          ELSEIF(IQ(INX+10).EQ.2) THEN
            EXTOT(IPHI,JETA) = E(1)
            EYTOT(IPHI,JETA) = E(2)
            EZTOT(IPHI,JETA) = E(3)
            ETOT(IPHI,JETA)  = E(4)
          ENDIF
        ENDDO
      ELSE
        CALL ERRMSG('CATD','CATDFL',
     &       ' NO CATE BANKS ARE AVAILABLE FOR CATDFL','W')
        GO TO 999
      ENDIF
C-
C--- get vertex information
C-
      CALL ZVERTE(NV,ZV,DZ)
      IF(NV.EQ.0) THEN
        CALL ERRMSG('CATD','CATDFL',
     &              'NO_VERTICES - Z set to 0','W')
        ZV(1)=0.0
      ENDIF
C---
C--- Fill array EMTWR and HADTWR from EM arrays and TOT arrays
C---
      EMPNT  = 0
      HDPNT  = 0
      IEND = 2*NETAL
      DO 200 IE = 1,IEND
        DO 200 IP = 1,NPHIL
C-
C--- check EM towers
          PACTWR = 0
          E(1) = EXEM(IP,IE)
          E(2) = EYEM(IP,IE)
          E(3) = EZEM(IP,IE)
          E(4) = EEM(IP,IE)
          ET = SQRT(E(1)*E(1)+E(2)*E(2))
          IF (ET.LT.MNETEM .OR. E(4).LE.0.)    GO TO 100
          EMPNT = EMPNT + 1
          IF (EMPNT .GT. 256) THEN
            CALL ERRMSG('CATD','CATDFL',
     &        ' NUMBER OF EM TOWERS EXCEEDED(MAX=256)','W')
            GO TO 100
          ENDIF
          IF (IE .LE. NETAL) THEN
            IETA = IE - NETAL - 1
          ELSE
            IETA = IE - NETAL
          ENDIF
C--- calculate delta ETA(= true ETA - nominal ETA)
          CALL CELXYZ(IETA,IPHI, 1,XC,YC,ZC,ISTAT)
          IF(ISTAT .EQ. 0) THEN
            ZC  = ZC - ZV(1)
            ZOT = (ZC+SMALL)/(SQRT(XC*XC+YC*YC+ZC*ZC)+SMALL)
            THETAN = ACOS(ZOT)
            ETANOM = -ALOG(TAN(THETAN/2.)+SMALL)
            CALL ETOETA(E,PHI,THETA,ETATRU)
            DELETA = ETATRU - ETANOM
            IDLETA = ABS(NINT(DELETA/DEUNIT))
          ENDIF
C-
          IENR = NINT(E(4)/EUNIT)
          CALL SBYT(IE,  PACTWR, 1, 7)
          CALL SBYT(IP,  PACTWR, 8, 7)
          IF(DELETA .LT. 0.) CALL SBIT1(PACTWR,15)
          CALL SBYT(IDLETA,PACTWR,16, 4)
          CALL SBYT(IENR,PACTWR,20,13)
          EMTWR(EMPNT) = PACTWR
C-
C--- check HAD towers
C-
  100     CONTINUE
          PACTWR = 0
          E(1) = EXTOT(IP,IE) - EXEM(IP,IE)
          E(2) = EYTOT(IP,IE) - EYEM(IP,IE)
          E(3) = EZTOT(IP,IE) - EZEM(IP,IE)
          E(4) = ETOT(IP,IE) - EEM(IP,IE)
          ET = SQRT(E(1)*E(1)+E(2)*E(2))
          IF (ET.LT.MNETHD .OR. E(4).LE.0.)          GO TO 200
          HDPNT = HDPNT + 1
          IF (HDPNT .GT. 256) THEN
            CALL ERRMSG('CATD','CATDFL',
     &        ' NUMBER OF HAD TOWERS EXCEEDED(MAX=256)','W')
            GO TO 200
          ENDIF
          IF (IE .LE. NETAL) THEN
            IETA = IE - NETAL - 1
          ELSE
            IETA = IE - NETAL
          ENDIF
C--- calculate delta ETA(= true ETA - nominal ETA)
          CALL CELXYZ(IETA,IPHI,11,XC,YC,ZC,ISTAT)
          IF(ISTAT .EQ. 0) THEN
            ZC  = ZC - ZV(1)
            ZOT = (ZC+SMALL)/(SQRT(XC*XC+YC*YC+ZC*ZC)+SMALL)
            THETAN = ACOS(ZOT)
            ETANOM = -ALOG(TAN(THETAN/2.)+SMALL)
            CALL ETOETA(E,PHI,THETA,ETATRU)
            DELETA = ETATRU - ETANOM
            IDLETA = ABS(NINT(DELETA/DEUNIT))
          ENDIF
C-
          IENR = NINT(E(4)/EUNIT)
          CALL SBYT(IE,  PACTWR, 1, 7)
          CALL SBYT(IP,  PACTWR, 8, 7)
          IF(DELETA .LT. 0.) CALL SBIT1(PACTWR,15)
          CALL SBYT(IDLETA,PACTWR,16, 4)
          CALL SBYT(IENR,PACTWR,20,13)
          HADTWR(HDPNT) = PACTWR
  200 CONTINUE
C---
C--- scan MUOFLG to fill the towers associated with PMUO
C--- into the bank CATD
C---
      MUOPNT = 0
      IEND = 2*NETAL
      DO 400 IE = 1,IEND
        DO 400 IP = 1,NPHIL
          IF (MUOFLG(IP,IE) .EQ. 1) THEN
            E(1) = EXTOT(IP,IE)
            E(2) = EYTOT(IP,IE)
            E(3) = EZTOT(IP,IE)
            E(4) = ETOT(IP,IE)
            IF (E(4) .LT. MNMUOE) GO TO 400
            PACTWR = 0
            MUOPNT = MUOPNT + 1
            IF (MUOPNT .GT. 64) THEN
              CALL ERRMSG('CATD','CATDFL',
     &          'NUMBER OF EM TOWERS(PMUO) EXCEEDED(MAX=64)','W')
              GO TO 400
            ENDIF
            IF (IE .LE. NETAL) THEN
              IETA = IE - NETAL - 1
            ELSE
              IETA = IE - NETAL
            ENDIF
C--- calculate delta ETA(= true ETA - nominal ETA)
            CALL CELXYZ(IETA,IPHI,11,XC,YC,ZC,ISTAT)
            IF(ISTAT .EQ. 0) THEN
              ZC  = ZC - ZV(1)
              ZOT = (ZC+SMALL)/(SQRT(XC*XC+YC*YC+ZC*ZC)+SMALL)
              THETAN = ACOS(ZOT)
              ETANOM = -ALOG(TAN(THETAN/2.)+SMALL)
              CALL ETOETA(E,PHI,THETA,ETATRU)
              DELETA = ETATRU - ETANOM
              IDLETA = ABS(NINT(DELETA/DEUNIT))
            ENDIF
C-
            IENR = NINT(E(4)/EUNIT)
            CALL SBYT(IE,  PACTWR, 1, 7)
            CALL SBYT(IP,  PACTWR, 8, 7)
            IF(DELETA .LT. 0.) CALL SBIT1(PACTWR,15)
            CALL SBYT(IDLETA,PACTWR,16, 4)
            CALL SBYT(IENR,PACTWR,20,13)
            MUTWR(MUOPNT) = PACTWR
          ENDIF
  400 CONTINUE
C---
C--- Book and fill the bank CATD here...
C---
      IF (EMPNT .GT. 256) EMPNT = 256
      IF (HDPNT .GT. 256) HDPNT = 256
      IF (MUOPNT .GT. 64) MUOPNT = 64
      NTOWR = EMPNT + HDPNT + MUOPNT
      CALL BKCATD(LCATD,NTOWR)
C-
      IF (LCATD .GT. 0) THEN
C... version number
        IQ(LCATD+1) = IVER + 1000
        IMNETEM = MNETEM/EUNIT
        IMNETHD = MNETHD/EUNIT
        IMNMUOE = MNMUOE/EUNIT
C... pointer to EM sector
        IQ(LCATD+2) = 8
C... pointer to HAD sector
        IQ(LCATD+3) = 9 + EMPNT
C... pointer to MUON sector
        IQ(LCATD+4) = 10+EMPNT+HDPNT
C... min. EM  Et to be saved
        IQ(LCATD+5) = IMNETEM
C... min. HAD Et to be saved
        IQ(LCATD+6) = IMNETHD
C... min. TOT E to be saved for PMUO
        IQ(LCATD+7) = IMNMUOE
C... no. of EM towers
        IQ(LCATD+8) = EMPNT
        CALL UCOPY(EMTWR, IQ(LCATD+9), EMPNT)
C... no. of HAD towers
        IQ(LCATD+9+EMPNT) = HDPNT
        CALL UCOPY(HADTWR, IQ(LCATD+10+EMPNT), HDPNT)
C... no. of towers asso. with PMUO
        IQ(LCATD+10+EMPNT+HDPNT) = MUOPNT
        CALL UCOPY(MUTWR, IQ(LCATD+11+EMPNT+HDPNT), MUOPNT)
      ENDIF
C-
C----------------------------------------------------------------------
  999 RETURN
      END
