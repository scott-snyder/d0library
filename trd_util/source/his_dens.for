      SUBROUTINE HIS_DENS
     #(EN,NA,PAS,VRAIS_PI,VRAIS_FA,VRAIS_EL,VRAIS_CO,
     #           VRAIS_PIP,VRAIS_FAP,VRAIS_ELP,VRAIS_COP)
C
C---------------------------------------------------------------
C-    Inputs: EN(1 TO 4) : ENERGY IN THE 3 TRD LAYERS AND IN THE CDC
C-            NA(1 TO 3) : Number of TRD Cells hit.
C-            PAS(1 TO 4): Step of the energy distributions
C-
C-    Outputs: VRAIS_PI,VRAIS_FA,VRAIS_EL,VRAIS_CO: Likelihood values for Pion,
C-             Fake, Electron, and Conversion hypothesis without CDC dE/dX cuts 
C-
C-            VRAIS_PIP,VRAIS_FAP,VRAIS_ELP,VRAIS_COP:
C-             Likelihood values for Pion, Fake, Electron, and Conversion
C-             hypothesis with CDC dE/dX cuts
C------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      REAL VRAIS_PI,VRAIS_EL,VRAIS_FA,VRAIS_CO
      REAL VRAIS_PIP,VRAIS_ELP,VRAIS_FAP,VRAIS_COP
      REAL PAS(4),EN(4)
      INTEGER NA(3)
C
      REAL ENTRD_PIMAX,ENTRD_PIMIN,ENCDC_PIMAX,ENCDC_PIMIN
      REAL ENTRD_FAMAX,ENTRD_FAMIN,ENCDC_FAMAX,ENCDC_FAMIN
      REAL ENTRD_ELMAX,ENTRD_ELMIN,ENCDC_ELMAX,ENCDC_ELMIN
      REAL ENTRD_COMAX,ENTRD_COMIN,ENCDC_COMAX,ENCDC_COMIN
      INTEGER MMAX_TRD,MMAX_CDC
C
      REAL PRO_CDC_PI(101,2),PRO_CDC_EL(101,2),PRO_CDC_FA(101,2)
      REAL PRO_CDC_CO(101,2)
      REAL PRO_CDC_PIP(101,2),PRO_CDC_ELP(101,2),PRO_CDC_FAP(101,2)
      REAL PRO_CDC_COP(101,2)
      REAL PRO_L1_PI(101,2),PRO_L2_PI(101,2),PRO_L3_PI(101,2)
      REAL PRO_L1_EL(101,2),PRO_L2_EL(101,2),PRO_L3_EL(101,2)
      REAL PRO_L1_FA(101,2),PRO_L2_FA(101,2),PRO_L3_FA(101,2)
      REAL PRO_L1_CO(101,2),PRO_L2_CO(101,2),PRO_L3_CO(101,2)
C
      INTEGER LTDPI,GZTDPI,LTDEL,GZTDEL,LTDFA,GZTDFA,LTDCO,GZTDCO
      INTEGER K,J,I,MEN(4),NATOT
      REAL AL_L1,AL_L2,AL_L3,AL_CDC,AL_CDCP,TOT_CDC(2)
      REAL PAP(4),PAF(4),PAE(4),PAC(4)
      REAL XXX,RNDM
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST) THEN
        FIRST=.FALSE.
c
c             Probability Distributions 
c
c***************   MINIMUM BIAS  *****************************8
C
        CALL VZERO(PRO_CDC_PI,202)
        CALL VZERO(PRO_L1_PI,202)
        CALL VZERO(PRO_L2_PI,202)
        CALL VZERO(PRO_L3_PI,202)
        LTDPI=GZTDPI()
        IF(LTDPI.LE.0) THEN 
          GO TO 999
        END IF
        MMAX_TRD=INT(C(LTDPI+2))+1
        ENTRD_PIMIN=C(LTDPI+3)
        ENTRD_PIMAX=C(LTDPI+4)
        PAP(1)=0.
        IF(MMAX_TRD-1.GT.0) THEN 
          PAP(1)=(ENTRD_PIMAX-ENTRD_PIMIN)/FLOAT(MMAX_TRD-1)
        END IF
        MMAX_CDC=INT(C(LTDPI+5))+1
        ENCDC_PIMIN=C(LTDPI+6)
        ENCDC_PIMAX=C(LTDPI+7)
        PAP(4)=0.
        IF(MMAX_CDC-1.GT.0) THEN 
          PAP(4)=(ENCDC_PIMAX-ENCDC_PIMIN)/FLOAT(MMAX_CDC-1)
        END IF
        TOT_CDC(1)=0.
        TOT_CDC(2)=0.
        DO J=1,MMAX_TRD
          PRO_CDC_PI(J,1)=C(LTDPI+12+(J-1)*8+7)
          PRO_CDC_PI(J,2)=C(LTDPI+12+(J-1)*8+8)
          IF(J.LT.39..OR.J.GT.73) THEN
            TOT_CDC(1)=TOT_CDC(1)+PRO_CDC_PI(J,1)
            TOT_CDC(2)=TOT_CDC(2)+PRO_CDC_PI(J,2)
          END IF
          DO K=1,2
            PRO_L1_PI(J,K)=C(LTDPI+12+(J-1)*8+(K-1)*3+1)
            PRO_L2_PI(J,K)=C(LTDPI+12+(J-1)*8+(K-1)*3+2)
            PRO_L3_PI(J,K)=C(LTDPI+12+(J-1)*8+(K-1)*3+3)
          END DO
        END DO
        DO J=1,MMAX_CDC
          PRO_CDC_PIP(J,1)=PRO_CDC_PI(J,1)/TOT_CDC(1)
          PRO_CDC_PIP(J,2)=PRO_CDC_PI(J,2)/TOT_CDC(2)
        END DO
c
c***************   PIONS (FA)  *****************************8
c
C
        CALL VZERO(PRO_CDC_FA,202)
        CALL VZERO(PRO_L1_FA,202)
        CALL VZERO(PRO_L2_FA,202)
        CALL VZERO(PRO_L3_FA,202)
        LTDFA=GZTDFA()
        IF(LTDFA.LE.0) THEN
          GO TO 999
        END IF
        MMAX_TRD=INT(C(LTDFA+2))+1
        ENTRD_FAMIN=C(LTDFA+3)
        ENTRD_FAMAX=C(LTDFA+4)
        PAF(1)=0.
        IF(MMAX_TRD-1.GT.0) THEN
          PAF(1)=(ENTRD_FAMAX-ENTRD_FAMIN)/FLOAT(MMAX_TRD-1)
        END IF
        MMAX_CDC=INT(C(LTDFA+5))+1
        ENCDC_FAMIN=C(LTDFA+6)
        ENCDC_FAMAX=C(LTDFA+7)
        PAF(4)=0.
        IF(MMAX_CDC-1.GT.0) THEN 
          PAF(4)=(ENCDC_FAMAX-ENCDC_FAMIN)/FLOAT(MMAX_CDC-1)
        END IF
        TOT_CDC(1)=0.
        TOT_CDC(2)=0.
        DO J=1,MMAX_TRD
          PRO_CDC_FA(J,1)=C(LTDFA+12+(J-1)*8+7)
          PRO_CDC_FA(J,2)=C(LTDFA+12+(J-1)*8+8)
          IF(J.LT.39..OR.J.GT.73) THEN
            TOT_CDC(1)=TOT_CDC(1)+PRO_CDC_FA(J,1)
            TOT_CDC(2)=TOT_CDC(2)+PRO_CDC_FA(J,2)
          END IF
          DO K=1,2
            PRO_L1_FA(J,K)=C(LTDFA+12+(J-1)*8+(K-1)*3+1)
            PRO_L2_FA(J,K)=C(LTDFA+12+(J-1)*8+(K-1)*3+2)
            PRO_L3_FA(J,K)=C(LTDFA+12+(J-1)*8+(K-1)*3+3)
          END DO
        END DO
        DO J=1,MMAX_CDC
          PRO_CDC_FAP(J,1)=PRO_CDC_FA(J,1)/TOT_CDC(1)
          PRO_CDC_FAP(J,2)=PRO_CDC_FA(J,2)/TOT_CDC(2)
        END DO
C
C--------------- ELECTRONS ----------------------
C
        CALL VZERO(PRO_CDC_EL,202)
        CALL VZERO(PRO_L1_EL,202)
        CALL VZERO(PRO_L2_EL,202)
        CALL VZERO(PRO_L3_EL,202)
        LTDEL=GZTDEL()
        IF(LTDEL.LE.0) THEN
          GO TO 999
        END IF
        MMAX_TRD=INT(C(LTDEL+2))+1
        ENTRD_ELMIN=C(LTDEL+3)
        ENTRD_ELMAX=C(LTDEL+4)
        PAE(1)=0.
        IF(MMAX_TRD-1.GT.0) THEN 
          PAE(1)=(ENTRD_ELMAX-ENTRD_ELMIN)/FLOAT(MMAX_TRD-1)
        END IF
        MMAX_CDC=INT(C(LTDEL+5))+1
        ENCDC_ELMIN=C(LTDEL+6)
        ENCDC_ELMAX=C(LTDEL+7)
        PAE(4)=0.
        IF(MMAX_CDC-1.GT.0) THEN 
          PAE(4)=(ENCDC_ELMAX-ENCDC_ELMIN)/FLOAT(MMAX_CDC-1)
        END IF
        TOT_CDC(1)=0.
        TOT_CDC(2)=0.
        DO J=1,MMAX_TRD
          PRO_CDC_EL(J,1)=C(LTDEL+12+(J-1)*8+7)
          PRO_CDC_EL(J,2)=C(LTDEL+12+(J-1)*8+8)
          IF(J.LT.39..OR.J.GT.73) THEN
            TOT_CDC(1)=TOT_CDC(1)+PRO_CDC_EL(J,1)
            TOT_CDC(2)=TOT_CDC(2)+PRO_CDC_EL(J,2)
          END IF
          DO K=1,2
            PRO_L1_EL(J,K)=C(LTDEL+12+(J-1)*8+(K-1)*3+1)
            PRO_L2_EL(J,K)=C(LTDEL+12+(J-1)*8+(K-1)*3+2)
            PRO_L3_EL(J,K)=C(LTDEL+12+(J-1)*8+(K-1)*3+3)
          END DO
        END DO
        DO J=1,MMAX_CDC
          PRO_CDC_ELP(J,1)=PRO_CDC_EL(J,1)/TOT_CDC(1)
          PRO_CDC_ELP(J,2)=PRO_CDC_EL(J,2)/TOT_CDC(2)
        END DO
C
C ***************   CONV. *************************
C
        CALL VZERO(PRO_CDC_CO,202)
        CALL VZERO(PRO_L1_CO,202)
        CALL VZERO(PRO_L2_CO,202)
        CALL VZERO(PRO_L3_CO,202)
        LTDCO=GZTDCO()
        IF(LTDCO.LE.0) THEN
          GO TO 999
        END IF
        MMAX_TRD=INT(C(LTDCO+2))+1
        ENTRD_COMIN=C(LTDCO+3)
        ENTRD_COMAX=C(LTDCO+4)
        PAC(1)=0.
        IF(MMAX_TRD-1.GT.0) THEN 
          PAC(1)=(ENTRD_COMAX-ENTRD_COMIN)/FLOAT(MMAX_TRD-1)
        END IF
        MMAX_CDC=INT(C(LTDCO+5))+1
        ENCDC_COMIN=C(LTDCO+6)
        ENCDC_COMAX=C(LTDCO+7)
        PAC(4)=0.
        IF(MMAX_CDC-1.GT.0) THEN 
          PAC(4)=(ENCDC_COMAX-ENCDC_COMIN)/FLOAT(MMAX_CDC-1)
        END IF
        TOT_CDC(1)=0.
        TOT_CDC(2)=0.
        DO J=1,MMAX_TRD
          PRO_CDC_CO(J,1)=C(LTDCO+12+(J-1)*8+7)
          PRO_CDC_CO(J,2)=C(LTDCO+12+(J-1)*8+8)
          IF(J.LT.39..OR.J.GT.73) THEN
            TOT_CDC(1)=TOT_CDC(1)+PRO_CDC_CO(J,1)
            TOT_CDC(2)=TOT_CDC(2)+PRO_CDC_CO(J,2)
          END IF
          DO K=1,2
            PRO_L1_CO(J,K)=C(LTDCO+12+(J-1)*8+(K-1)*3+1)
            PRO_L2_CO(J,K)=C(LTDCO+12+(J-1)*8+(K-1)*3+2)
            PRO_L3_CO(J,K)=C(LTDCO+12+(J-1)*8+(K-1)*3+3)
          END DO
        END DO
        DO J=1,MMAX_CDC
          PRO_CDC_COP(J,1)=PRO_CDC_CO(J,1)/TOT_CDC(1)
          PRO_CDC_COP(J,2)=PRO_CDC_CO(J,2)/TOT_CDC(2)
        END DO
C
        IF((PAP(1).EQ.PAF(1)).AND.(PAP(1).EQ.PAE(1)).AND.
     #  (PAP(1).EQ.PAC(1))) THEN
          PAS(1)=PAP(1)
          PAS(2)=PAP(1)
          PAS(3)=PAP(1)
        END IF
        IF((PAP(4).EQ.PAF(4)).AND.(PAP(4).EQ.PAE(4)).AND.
     #  (PAP(4).EQ.PAC(4))) THEN
          PAS(4)=PAP(4)
        END IF
      END IF
C
      VRAIS_PI=-100.
      VRAIS_FA=-100.
      VRAIS_EL=-100.
      VRAIS_CO=-100.
      VRAIS_PIP=-100.
      VRAIS_FAP=-100.
      VRAIS_ELP=-100.
      VRAIS_COP=-100.
      DO I=1,4
        MEN(I)=INT(EN(I)/PAS(I))+1
        IF(MEN(I).GT.101) MEN(I)=101
      END DO
c      write(6,*)' na=',na(1),na(2),na(3)
      NATOT=NA(1)+NA(2)+NA(3)
      IF(NATOT.LE.4)NATOT=1
      IF(NATOT.GT.4)NATOT=2
C 
C ********   PION ( MIN BIA) LIKELIHOOD ********************
C
      IF(PRO_L1_PI(MEN(1),NA(1)).GT.0.00005) THEN
        AL_L1=-ALOG(PRO_L1_PI(MEN(1),NA(1)))
      ELSE
        AL_L1=10.
      END IF
      IF(PRO_L2_PI(MEN(2),NA(2)).GT.0.00005) THEN
        AL_L2=-ALOG(PRO_L2_PI(MEN(2),NA(2)))
      ELSE
        AL_L2=10.
      END IF
      IF(PRO_L3_PI(MEN(3),NA(3)).GT.0.00005) THEN
        AL_L3=-ALOG(PRO_L3_PI(MEN(3),NA(3)))
      ELSE
        AL_L3=10.
      END IF
      IF(PRO_CDC_PI(MEN(4),NATOT).GT.0.00005) THEN
        AL_CDC=-ALOG(PRO_CDC_PI(MEN(4),NATOT))
      ELSE
        AL_CDC=10.
      END IF
      IF(PRO_CDC_PIP(MEN(4),NATOT).GT.0.00005) THEN
        AL_CDCP=-ALOG(PRO_CDC_PIP(MEN(4),NATOT))
      ELSE
        AL_CDCP=10.
      END IF
      VRAIS_PI=AL_L1+AL_L2+AL_L3+AL_CDC
c      VRAIS_PIP=AL_L1+AL_L2+AL_L3
      VRAIS_PIP=AL_L1+AL_L2+AL_L3+AL_CDCP
C
C ************** ELECTRON LIKELIHOOD **************
C
      IF(PRO_L1_EL(MEN(1),NA(1)).GT.0.00005) THEN
        AL_L1=-ALOG(PRO_L1_EL(MEN(1),NA(1)))
      ELSE
        AL_L1=10.
      END IF
      IF(PRO_L2_EL(MEN(2),NA(2)).GT.0.00005) THEN
        AL_L2=-ALOG(PRO_L2_EL(MEN(2),NA(2)))
      ELSE
        AL_L2=10.
      END IF
      IF(PRO_L3_EL(MEN(3),NA(3)).GT.0.00005) THEN
        AL_L3=-ALOG(PRO_L3_EL(MEN(3),NA(3)))
      ELSE
        AL_L3=10.
      END IF
      IF(PRO_CDC_EL(MEN(4),NATOT).GT.0.00005) THEN
        AL_CDC=-ALOG(PRO_CDC_EL(MEN(4),NATOT))
      ELSE
        AL_CDC=10.
      END IF
      IF(PRO_CDC_ELP(MEN(4),NATOT).GT.0.00005) THEN
        AL_CDCP=-ALOG(PRO_CDC_ELP(MEN(4),NATOT))
      ELSE
        AL_CDCP=10.
      END IF
      VRAIS_EL=AL_L1+AL_L2+AL_L3+AL_CDC
c      VRAIS_ELP=AL_L1+AL_L2+AL_L3
      VRAIS_ELP=AL_L1+AL_L2+AL_L3+AL_CDCP
C
C ****************  FAKE LIKELIHOOD *************************
C
      IF(PRO_L1_FA(MEN(1),NA(1)).GT.0.00005) THEN
        AL_L1=-ALOG(PRO_L1_FA(MEN(1),NA(1)))
      ELSE
        AL_L1=10.
      END IF
      IF(PRO_L2_FA(MEN(2),NA(2)).GT.0.00005) THEN
        AL_L2=-ALOG(PRO_L2_FA(MEN(2),NA(2)))
      ELSE
        AL_L2=10.
      END IF
      IF(PRO_L3_FA(MEN(3),NA(3)).GT.0.00005) THEN
        AL_L3=-ALOG(PRO_L3_FA(MEN(3),NA(3)))
      ELSE
        AL_L3=10.
      END IF
      IF(PRO_CDC_FA(MEN(4),NATOT).GT.0.00005) THEN
        AL_CDC=-ALOG(PRO_CDC_FA(MEN(4),NATOT))
      ELSE
        AL_CDC=10.
      END IF
      IF(PRO_CDC_FAP(MEN(4),NATOT).GT.0.00005) THEN
        AL_CDCP=-ALOG(PRO_CDC_FAP(MEN(4),NATOT))
      ELSE
        AL_CDCP=10.
      END IF
      VRAIS_FA=AL_L1+AL_L2+AL_L3+AL_CDC
c      VRAIS_FAP=AL_L1+AL_L2+AL_L3
      VRAIS_FAP=AL_L1+AL_L2+AL_L3+AL_CDCP
C
C ********************  CONVERSION LIKELIHOOD ************************
C
      IF(PRO_L1_CO(MEN(1),NA(1)).GT.0.00005) THEN
        AL_L1=-ALOG(PRO_L1_CO(MEN(1),NA(1)))
      ELSE
        AL_L1=10.
      END IF
      IF(PRO_L2_CO(MEN(2),NA(2)).GT.0.00005) THEN
        AL_L2=-ALOG(PRO_L2_CO(MEN(2),NA(2)))
      ELSE
        AL_L2=10.
      END IF
      IF(PRO_L3_CO(MEN(3),NA(3)).GT.0.00005) THEN
        AL_L3=-ALOG(PRO_L3_CO(MEN(3),NA(3)))
      ELSE
        AL_L3=10.
      END IF
      IF(PRO_CDC_CO(MEN(4),NATOT).GT.0.00005) THEN
        AL_CDC=-ALOG(PRO_CDC_CO(MEN(4),NATOT))
      ELSE
        AL_CDC=10.
      END IF 
      IF(PRO_CDC_COP(MEN(4),NATOT).GT.0.00005) THEN
        AL_CDCP=-ALOG(PRO_CDC_COP(MEN(4),NATOT))
      ELSE
        AL_CDCP=10.
      END IF 
      VRAIS_CO=0.5*AL_L2+AL_L3+AL_CDC
c      VRAIS_COP=AL_L2+AL_L3
      VRAIS_COP=0.5*AL_L2+AL_L3+AL_CDCP
C
  999 CONTINUE
C
      END
