      SUBROUTINE MAKE_TTEE_NTUPLE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-NOV-1993   Balamurali V
C-   Updated  23-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VERT.INC'
      INCLUDE 'D0$INC:ELEC.INC'
      INCLUDE 'D0$INC:PHOT.INC'
      INCLUDE 'D0$INC:MET.INC'
      INCLUDE 'D0$INC:JET.INC'
C
      INTEGER NTVAR
      PARAMETER (NTVAR=202)
      INTEGER I,J,K,L,IE,IP,LFLAG,IER,NTID,I1,I2,IEOLD,IEOLF
      REAL    PAR(NTVAR),DPHI,DPHIMET(2),EEMASS(2),MT(2)
      REAL    JET_ET,DETA,ET_THRES,ETCUT
      REAL    NJET_20,NJETCC_20,NJETEC_20,NJET_15,NJETCC_15
      REAL    NJETEC_15,NJETCC_THRES(30),NJETEC_THRES(30)
      CHARACTER*8 TAGS(NTVAR),NTFILE
      LOGICAL FIRST,FAKE_BKG,REQ_TRKMATCH
      DATA    FIRST/.TRUE./
      DATA    TAGS/
     &'RUN     ','EVENT   ','L1BIT   ','L2BIT0  ','L2BIT1  ','L2BIT2  ',
     &'L2BIT3  ','XING    ','UBLANK  ','NVERTEX ','ZVERTEX ','NGOODE  ',
     &'ETE1    ','PHIE1   ','ETAE1   ','THE1    ','DETAE1  ','CHISQE1 ',
     &'T1      ','FISO1   ','NCELLE1 ','QUALE1_ ','QUALE1  ',
     &'SIGETE1 ','CORRE1  ','FEM1    ',
     &'ETISOL1 ','NTRKE1  ','DTRKE1  ','DEDX1   ','NTRKCON1','RDPHI1  ',
     &'DZ_DR1  ','MTCHSIG1','TRDINFO1','TRDMEAN1','MT1     ','DPHIMET1',
     &'ETE2    ','PHIE2   ','ETAE2   ','THE2    ','DETAE2  ','CHISQE2 ',
     &'T2      ','FISO2   ','NCELLE2 ','QUALE2_ ','QUALE2  ',
     &'SIGETE2 ','CORRE2  ','FEM2    ',
     &'ETISOL2 ','NTRKE2  ','DTRKE2  ','DEDX2   ','NTRKCON2','RDPHI2  ',
     &'DZ_DR2  ','MTCHSIG2','TRDINFO2','TRDMEAN2','MT2     ','DPHIMET2',
     &'BAD     ','MEE     ','MEEC    ','DPHI    ','MET2    ',
     &'METX    ','METC2   ','METXC   ','M3P     ','SUMET2  ','SIGMET2 ',
     &'SIGMETS ','PTMIN   ','SPHER   ','APLANR  ','NUM_JET ','ETJ1    ',
     &'ETCJ1   ','PHIJ1   ','ETAJ1   ','FICDJ1  ','FEMJ1   ','NCJ1    ',
     &'DETAJ1  ','ETJ2    ','ETCJ2   ','PHIJ2   ','ETAJ2   ','FICDJ2  ',
     &'FEMJ2   ','NCJ2    ','DETAJ2  ','ETJ3    ','ETCJ3   ','PHIJ3   ',
     &'ETAJ3   ','FICDJ3  ','FEMJ3   ','NCJ3    ','DETAJ3  ','ETJ4    ',
     &'ETCJ4   ','PHIJ4   ','ETAJ4   ','FICDJ4  ','FEMJ4   ','NCJ4    ',
     &'DETAJ4  ','ETJ5    ','ETCJ5   ','PHIJ5   ','ETAJ5   ','FICDJ5  ',
     &'FEMJ5   ','NCJ5    ','DETAJ5  ','ETJ6    ','ETCJ6   ','PHIJ6   ',
     &'ETAJ6   ','FICDJ6  ','FEMJ6   ','NCJ6    ','DETAJ6  ','ETJ7    ',
     &'ETCJ7   ','PHIJ7   ','ETAJ7   ','FICDJ7  ','FEMJ7   ','NCJ7    ',
     &'DETAJ7  ',
     &'NFCTHR1 ','NFCTHR2 ','NFCTHR3 ','NFCTHR4 ','NFCTHR5 ','NFCTHR6 ',
     &'NFCTHR7 ','NFCTHR8 ','NFCTHR9 ','NFCTHR10','NFCTHR11','NFCTHR12',
     &'NFCTHR13','NFCTHR14','NFCTHR15','NFCTHR16','NFCTHR17','NFCTHR18',
     &'NFCTHR19','NFCTHR20','NFCTHR21','NFCTHR22','NFCTHR23','NFCTHR24',
     &'NFCTHR25','NFCTHR26','NFCTHR27','NFCTHR28','NFCTHR29','NFCTHR30',
     &'NFETHR1 ','NFETHR2 ','NFETHR3 ','NFETHR4 ','NFETHR5 ','NFETHR6 ',
     &'NFETHR7 ','NFETHR8 ','NFETHR9 ','NFETHR10','NFETHR11','NFETHR12',
     &'NFETHR13','NFETHR14','NFETHR15','NFETHR16','NFETHR17','NFETHR18',
     &'NFETHR19','NFETHR20','NFETHR21','NFETHR22','NFETHR23','NFETHR24',
     &'NFETHR25','NFETHR26','NFETHR27','NFETHR28','NFETHR29','NFETHR30',
     &'NTOTJT15','NTOTJT20','NTJTC15 ','NTJTC20 ','KTJTC15 ','KTJTC20 '/
C
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL EZPICK('TTEE_RCP')
        CALL EZGET('ETCUT',ETCUT,IER)
        CALL EZGET('FAKE_BKG',FAKE_BKG,IER)
        CALL EZGET('REQ_TRKMATCH',REQ_TRKMATCH,IER)
        CALL EZRSET
        CALL HCDIR('//PAWC',' ')
        NTFILE = 'TTEE_NT4'
        CALL NTUPLE_FILE_OPEN(999,.TRUE.,NTFILE,8191,'TTEE',IER)
        CALL NTUPLE_SET_ID(1,1)
        CALL NTUPLE_BOOK('TTEE',NTVAR,TAGS,'TTEE ANALYSIS',NTID,IER)
        IF(IER.NE.0)THEN
          CALL ERRMSG('Error in booking Nt or opening Nt file',
     &      'Make_ttee_ntuple',' ','F')
        ENDIF
        FIRST=.FALSE.
      ENDIF
C
C ** Initialize variables
      DO I = 1,NTVAR
        PAR(I) = 0.0
      ENDDO
      IF(FAKE_BKG)THEN
        DO I = 1,30
          NJETCC_THRES(I) = 0
          NJETEC_THRES(I) = 0
        ENDDO
      ENDIF
      NJET_15 = 0
      NJET_20 = 0
      NJETCC_15 = 0
      NJETEC_15 = 0
      NJETCC_20 = 0
      NJETEC_20 = 0
      IEOLD = 0
      IEOLF = 0
      I1 = 0
      I2 = 0
C
C ** General params
      DO I=1,9
        PAR(I) = RUN(I)
      ENDDO
      PAR(10) = FLOAT(NVERT)
      PAR(11) = ZVERT
C
C ** Electron/Gamma params
      PAR(12) = NELEC-NFAKE_ELEC
      IF(NELEC .EQ. 1)THEN
        IF(NPHOT .LT. 1)LFLAG=0
        IE = ABS(IGELEC(1))
        IP = MAX(IGPHOT(1),1)
c$$$        IF(REQ_TRKMATCH.AND.(IGELEC(1).GT.0))GOTO 90
        IF(RELEC(1,IE) .LT. RPHOT(1,IP) .and. .not.
     &     (REQ_TRKMATCH.AND.(IGELEC(1).GT.0)))THEN
          LFLAG = 3
          I1 = IP
          I2 = IE
          DO J = 1,8
            PAR(12+J) = RPHOT(J,ip)
          ENDDO
          PAR(21) = iPHOT(1,ip)
          PAR(22) = iPHOT(2,ip)
          PAR(23) = iPHOT(3,ip)
          PAR(24) = RPHOT(9,ip)
          PAR(25) = RPHOT(10,ip)
          PAR(26) = RPHOT(11,ip)
          PAR(27) = RPHOT(12,ip)
          PAR(28) = iPHOT(4,ip)
          par(30) = rphot(14,ip)
          par(32) = rphot(15,ip)
C
          DO J = 1,8
            PAR(38+J) = RELEC(J,ie)
          ENDDO
          PAR(47) = iELEC(1,ie)
          PAR(48) = iELEC(2,ie)
          PAR(49) = iELEC(3,ie)
          PAR(50) = RELEC(9,ie)
          PAR(51) = RELEC(10,ie)
          PAR(52) = RELEC(11,ie)
          PAR(53) = RELEC(12,ie)
          PAR(54) = iELEC(4,ie)
          PAR(55) = iELEC(5,ie)
          par(56) = relec(14,ie)
          par(57) = ielec(6,ie)
          DO J = 15,21
            PAR(43+J) = RELEC(J,ie)
          ENDDO
        ELSE
   90     CONTINUE
          LFLAG = 2
          I1 = IE
          I2 = IP
          DO J = 1,8
            PAR(12+J) = RELEC(J,ie)
          ENDDO
          PAR(21) = iELEC(1,ie)
          PAR(22) = iELEC(2,ie)
          PAR(23) = iELEC(3,ie)
          PAR(24) = RELEC(9,ie)
          PAR(25) = RELEC(10,ie)
          PAR(26) = RELEC(11,ie)
          PAR(27) = RELEC(12,ie)
          PAR(28) = iELEC(4,ie)
          PAR(29) = iELEC(5,ie)
          par(30) = relec(14,ie)
          par(31) = ielec(6,ie)
          DO J = 15,21
            PAR(17+J) = RELEC(J,ie)
          ENDDO
C
          DO J = 1,8
            PAR(38+J) = RPHOT(J,ip)
          ENDDO
          PAR(47) = iPHOT(1,ip)
          PAR(48) = iPHOT(2,ip)
          PAR(49) = iPHOT(3,ip)
          PAR(50) = RPHOT(9,ip)
          PAR(51) = RPHOT(10,ip)
          PAR(52) = RPHOT(11,ip)
          PAR(53) = RPHOT(12,ip)
          PAR(54) = iPHOT(4,ip)
          par(56) = rphot(14,ip)
          par(58) = rphot(15,ip)
        ENDIF
      ELSEIF(NELEC .GT. 1)THEN
        LFLAG = 1
        DO I = 1,2
          DO J = 1,NELEC
            IE = IGELEC(J)
            IF(IE.GE.1.AND.IE.NE.IEOLD)GOTO 200
          ENDDO
          DO J = 1,NELEC
            IE = ABS(IGELEC(J))
            IF(IE.NE.IEOLF.AND.IE.NE.IEOLD)GOTO 100
          ENDDO
  100     CONTINUE
          IEOLF = IE
  200     CONTINUE
          IEOLD = IE
          IF(I1 .EQ. 0)I1=IE
          I2 = IE
          DO J = 1,8
            PAR(12+J+(i-1)*26) = RELEC(J,IE)
          ENDDO
          PAR(21+(i-1)*26) = iELEC(1,IE)
          PAR(22+(i-1)*26) = iELEC(2,IE)
          PAR(23+(i-1)*26) = iELEC(3,IE)
          PAR(24+(i-1)*26) = RELEC(9,IE)
          PAR(25+(i-1)*26) = RELEC(10,IE)
          PAR(26+(i-1)*26) = RELEC(11,IE)
          PAR(27+(i-1)*26) = RELEC(12,IE)
          PAR(28+(i-1)*26) = iELEC(4,IE)
          PAR(29+(i-1)*26) = iELEC(5,IE)
          par(30+(i-1)*26) = relec(14,ie)
          par(31+(i-1)*26) = ielec(6,ie)
          DO J = 15,21
            PAR(17+J+(i-1)*26) = RELEC(J,IE)
          ENDDO
        ENDDO
      ELSE
        IF(nphot.GE.2)THEN
          LFLAG = 4
          DO I = 1,2
            Ip = IGphot(i)
            IF(I .EQ. 1)I1=Ip
            IF(I .EQ. 2)I2=Ip
            DO J = 1,8
              PAR(12+J+(i-1)*26) = RPHOT(J,ip)
            ENDDO
            PAR(21+(i-1)*26) = iPHOT(1,ip)
            PAR(22+(i-1)*26) = iPHOT(2,ip)
            PAR(23+(i-1)*26) = iPHOT(3,ip)
            PAR(24+(i-1)*26) = RPHOT(9,ip)
            PAR(25+(i-1)*26) = RPHOT(10,ip)
            PAR(26+(i-1)*26) = RPHOT(11,ip)
            PAR(27+(i-1)*26) = RPHOT(12,ip)
            PAR(28+(i-1)*26) = iPHOT(4,ip)
            par(30+(i-1)*26) = rphot(14,ip)
            par(32+(i-1)*26) = rphot(15,ip)
          ENDDO
        ELSE
          LFLAG = 0
        ENDIF
      ENDIF
C
C ** Params calculated from above 2 leptons
      IF(LFLAG .NE. 0)THEN
        CALL GET_DILEP_QUAN(LFLAG,I1,I2,DPHI,DPHIMET,EEMASS,MT)
      ENDIF
C
      PAR(37) = MT(1)
      PAR(38) = DPHIMET(1)
      PAR(63) = MT(2)
      PAR(64) = DPHIMET(2)
      PAR(66) = EEMASS(1)
      PAR(67) = EEMASS(2)
      PAR(68) = DPHI
c
C ** Missing_Et params
      PAR(69) = META(1,2)          ! Met from pnut2
      PAR(72) = META(4,2)          ! Both corrections applied to PAR(69)
      PAR(74) = META(3,2)          ! Scal Met from pnut2
      PAR(75) = META(8,2)
c
C ** Jet params
      IF(NUM_JET .GT. 7)NUM_JET=7
      PAR(80) = NUM_JET
      DO I= 1,NUM_JET
        K = IJET(I)
        DO J = 1,8
          PAR(80+J+(I-1)*8) = JET(J,K)
        ENDDO
C
C ** Number of jets over certain Et for Fake BKG calculations
        JET_ET = JET(2,K)
        DETA   = JET(8,K)
        IF(FAKE_BKG)THEN
          DO L = 1,30
            ET_THRES = ETCUT+FLOAT((L-1))
            IF(JET_ET .GT. ET_THRES)THEN
              IF(ABS(DETA).LE.1.2)THEN
                NJETCC_THRES(L) = NJETCC_THRES(L)+1
              ELSEIF(ABS(DETA).GT.1.2.AND.ABS(DETA).LE.2.5)THEN
                NJETEC_THRES(L) = NJETEC_THRES(L)+1
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        IF(JET_ET.GT.15.)THEN
          NJET_15=NJET_15+1
          IF(ABS(DETA).LE.1.2)THEN
            NJETCC_15=NJETCC_15+1
          ELSEIF(ABS(DETA).GT.1.2.AND.ABS(DETA).LE.2.5)THEN
            NJETEC_15=NJETEC_15+1
          ENDIF
        ENDIF
        IF(JET_ET.GT.20.)THEN
          NJET_20=NJET_20+1
          IF(ABS(DETA).LE.1.2)THEN
            NJETCC_20=NJETCC_20+1
          ELSEIF(ABS(DETA).GT.1.2.AND.ABS(DETA).LE.2.5)THEN
            NJETEC_20=NJETEC_20+1
          ENDIF
        ENDIF
      ENDDO
      DO I = 1,30
        PAR(136+I) = NJETCC_THRES(I)
        PAR(166+I) = NJETEC_THRES(I)
      ENDDO
      PAR(197)=NJET_15
      PAR(198)=NJET_20
      PAR(199)=NJETCC_15
      PAR(200)=NJETCC_20
      PAR(201)=NJETEC_15
      PAR(202)=NJETEC_20
C
      CALL NTUPLE_FILL('TTEE',NTID,PAR,IER)
      IF(IER.NE.0)THEN
        CALL ERRMSG('Error while filling Nt','Make_ttee_ntuple',' ','F')
      ENDIF
C
  999 RETURN
      END
