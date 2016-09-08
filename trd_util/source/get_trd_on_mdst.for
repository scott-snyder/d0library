      SUBROUTINE GET_TRD_ON_MDST(LDUMM,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : unpack_trd information on micro_dst
C-
C-   Inputs  : real_trd :packed information read on micro_dst
C-             nint     :nb. of values
C-   Outputs :
C-        I_IN_TPRL(LAYER,3)= local density (4 bits)
C-        I_IN_TPRL(LAYER,4)= nb. of hit anodes
C-        I_IN_TPRL(LAYER,5)= nb. of hit cathodes
C-        I_IN_TPRL(LAYER,6)= nb. of clusters
C-        ENERGY_FIRED_CELLS(LAYER)= energies in 3 layers
C-        R_IN_TPRL(LAYER,45)= Tmin
C-        ENERGY_FIRED_CELLS(4)=total energy 3 layers
C-        ENERGY_FIRED_CELLS(5)=trunc. mean
C-   Controls:
C-
C-   Created  13-JUL-1994   A. ZYLBERSTEJN
C-   Updated  17-FEB-1995   A. ZYLBERSTEJN  decode micro dst bank
C-   Updated  14-FEB-1996   L. T. Goss   call TRD_NUM_LAYERS for geom info.
C-   Updated  20-MAR-2004   sss - compile with g77
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INCLUDE 'D0$INC:TRD_DST_ENERGIES.INC'
      INCLUDE 'D0$INC:trdlik.INC'
      INCLUDE 'D0$INC:WORD_IN_TPRL.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZEBWRK.INC'
      INTEGER LENGWS
      PARAMETER (LENGWS=5000)
C      COMMON/WORKSP/WS(LENGWS)
      REAL WS(LENGWS)
      INTEGER IWS(LENGWS)
      EQUIVALENCE(WS,IWS,W(1001))
      INTEGER JBYT,K,LAYER,LTDST,GZTDST,IETA
      INTEGER NINT,INT_TRD,TGEO
      LOGICAL ACCEPTANCE,DOPRINT,TRD_BADRUN,BAD_SECTOR,TRD_DO_PRINT
      LOGICAL BADTRACK(10),CC,ENVIR
      REAL EPSL,EPSL_2,LIK1,LIK2,EPST
      INTEGER LCLUS,LDUMM,LOUT,TRUNIT,MINT
      INTEGER LPPHO_TEMP,LCACL_TEMP,GZPPHO,GZUDST,LUDST,UDST_VER
      PARAMETER (NINT = 8)
      CHARACTER*6 CLUS_VAR_NAME(NINT)
      CHARACTER*6 PELC_VAR_NAME(NINT),PPHO_VAR_NAME(NINT)
      CHARACTER*4 CLUS_NAME,BANK
      REAL ECDC,UDST_VALUE
      REAL REAL_TRD(10)
      LOGICAL GEOMETRY(3)
      INTEGER IER,IFOIS, HITS(3),LDTRK,LZTRK,LCACL
      DATA PELC_VAR_NAME/'TRDACC','TRDEFF','TRD11E','TRD12E','TRD21E',
     &                   'TRD22E','TRD31E','TRD32E'/
      DATA PPHO_VAR_NAME/'TRDACP','TRDEFP','TRD11P','TRD12P','TRD21P',
     &                   'TRD22P','TRD31P','TRD32P'/
      DATA IFOIS/0/

      integer ihpelc/4HPELC/
      integer ihppho/4HPPHO/
C----------------------------------------------------------------------
      LUDST = GZUDST()
      UDST_VER = IQ(LUDST+1)
      IF (UDST_VER.GE.2) THEN
        MINT = 8
      ELSE
        MINT = 2
      ENDIF
C
      IFOIS=IFOIS+1
      LOUT=TRUNIT()
      DOPRINT=TRD_DO_PRINT()
C      DOPRINT=IFOIS.LE.50
      ACCEPTANCE = .FALSE.
      DO K = 1,3
        GEOMETRY(K) = .FALSE.
      ENDDO
      BAD_SECTOR = .FALSE.
      LCLUS = 0
      DO K = 1,NINT
        REAL_TRD(K) = 0
      ENDDO
C
      CALL UHTOC(IQ(LDUMM-4),4,BANK,4)
      IF(BANK.EQ.'PELC') THEN
        LCLUS = LDUMM
        DO K = 1,NINT
          CLUS_VAR_NAME(K) = PELC_VAR_NAME(K)
        ENDDO
        CLUS_NAME = 'PELC'
      ELSEIF(BANK.EQ.'PPHO') THEN
        LCLUS = LDUMM
        DO K = 1,NINT
          CLUS_VAR_NAME(K) = PPHO_VAR_NAME(K)
        ENDDO
        CLUS_NAME = 'PPHO'
      ELSEIF(BANK.EQ.'TRDT') THEN
        LZTRK = LQ(LDUMM - 4)
        LCACL = LQ(LDUMM - 5)
        IF(LZTRK.NE.0) THEN
          LCLUS = LQ(LZTRK-4)
          IF (LCLUS.NE.0) THEN
            IF (IQ(LCLUS-4).EQ.iHPELC) THEN
              DO K = 1,NINT
                CLUS_VAR_NAME(K) = PELC_VAR_NAME(K)
              ENDDO
              CLUS_NAME = 'PELC'
C take care of PPHOs
            ELSEIF (IQ(LCLUS-4).EQ.iHPPHO) THEN
              DO K = 1,NINT
                CLUS_VAR_NAME(K) = PPHO_VAR_NAME(K)
              ENDDO
              CLUS_NAME = 'PPHO'
            ENDIF
          ENDIF
        ELSE
C take care of PPHOs
          DO K = 1,NINT
            CLUS_VAR_NAME(K) = PPHO_VAR_NAME(K)
          ENDDO
          CLUS_NAME = 'PPHO'
          LPPHO_TEMP = GZPPHO()
          LCACL_TEMP = 0
          DO WHILE(LPPHO_TEMP.NE.0.AND.LCACL.NE.LCACL_TEMP)
            LCACL_TEMP = LQ(LPPHO_TEMP - 2)
            IF (LCACL_TEMP.EQ.LCACL) LCLUS = LPPHO_TEMP
            LPPHO_TEMP = LQ(LPPHO_TEMP)
          ENDDO
        ENDIF
      ENDIF
C
      CC=.FALSE.
      IF (LCLUS.GT.0) THEN
        CALL TRD_NUM_LAYERS(LCLUS,GEOMETRY,BADTRACK,TGEO)
        BAD_SECTOR = BADTRACK(2)
        IETA=Q(LCLUS+19)
        CC=IABS(IETA).LE.12
      ENDIF
C
      ACCEPTANCE= GEOMETRY(1).AND.GEOMETRY(3).AND.
     &  .NOT.BAD_SECTOR.AND..NOT.TRD_BADRUN()
      DO K=1,MINT
        REAL_TRD(K)=UDST_VALUE(CLUS_NAME,CLUS_VAR_NAME(K),IQ(LCLUS-5),
     &    IER)
        IF(IER.NE.0)GO TO 999
      END DO
      K=2
      ENERGY_FIRED_CELLS(4)=0.
      ENERGY_FIRED_CELLS(5)=0.
      DO LAYER=1,3
        K=K+1
        INT_TRD=REAL_TRD(K)
        ENERGY_FIRED_CELLS(LAYER)=JBYT(INT_TRD,1,9)*.1
        R_IN_TPRL(LAYER,45)= JBYT(INT_TRD,10,6)! Tmin
        ENERGY_FIRED_CELLS(4)=ENERGY_FIRED_CELLS(4)+
     &    ENERGY_FIRED_CELLS(LAYER) !total energy
      END DO
      ENERGY_FIRED_CELLS(5)=ENERGY_FIRED_CELLS(4)-
     &  AMAX1(ENERGY_FIRED_CELLS(1),ENERGY_FIRED_CELLS(2),
     &  ENERGY_FIRED_CELLS(3))  !truncated mean
      DO LAYER=1,3
        K=K+1
        INT_TRD=REAL_TRD(K)
        I_IN_TPRL(LAYER,3)=JBYT(INT_TRD,1,4)!local density (4 bits)
        I_IN_TPRL(LAYER,4)=JBYT(INT_TRD,5,4) ! nb. of hit anodes
        I_IN_TPRL(LAYER,5)=JBYT(INT_TRD,9,4) !nb. of hit cathodes
        I_IN_TPRL(LAYER,6)=JBYT(INT_TRD,13,4)!nb. of clusters
        HITS(LAYER)=I_IN_TPRL(LAYER,4)
      END DO
      EPST = REAL_TRD(2)
C fetch de/dx from bank dtrk
      LDTRK = 0
      IF (LZTRK.NE.0) LDTRK=LQ(LZTRK-7)
C      EPSL=-99.
      ECDC=0.
      IF(LDTRK.NE.0 ) ECDC=Q(LDTRK+20)
      ENVIR= .NOT.BAD_SECTOR .AND. .NOT.TRD_BADRUN()
      CALL TDSTFL(ACCEPTANCE,EPST,EPSL,EPSL_2,LIK1,LIK2)
      IWS(1)=0
      IF(CC)IWS(1)=1
      IF (UDST_VER.GE.3) THEN
        CALL TRD_ELECTRON_PION(ENERGY_FIRED_CELLS,ECDC,GEOMETRY,HITS,
     &    ACCEPTANCE,EPST,EPSL,EPSL_2,LIK1,LIK2)
      ELSE
        ACCEPTANCE = (REAL_TRD(1).NE.0).AND.(TGEO.EQ.3)
        IF (.NOT.ACCEPTANCE) EPST = 999.
        LTDST=GZTDST()
        Q(LTDST+24) = 0.
        IF (ACCEPTANCE) Q(LTDST+24) = 3.
        Q(LTDST+8) = EPST
      ENDIF
C      IF(CC)THEN
C        ACCEPTANCE=ACCEPTANCE .AND. Q(LDUMM+31).GT.0.5
C      ELSE
C        ACCEPTANCE=ACCEPTANCE .AND. Q(LTDST+24).NE.0.
C      END IF
      LTDST=GZTDST()
      Q(LTDST+7) = 0.
      IF (ACCEPTANCE)         Q(LTDST+7) = 1.
  999 RETURN
      END
