      SUBROUTINE TRD_CALURAPT(TEMPTRD,ATMPRES,ILAY,COR_URAPT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correction  for temperature, pressure  and gain
C-                         variations in the 3 TRD layers
C                          using the 29 keV signal in the chambers
C-   Returned value  :
C-   Inputs  :             TEMPTRD,ATMPRES,ILAY
C-   Outputs :             COR_URAPT,IER
C-   Controls:               IER=1 AND COR_URAPT=1.
C-                                    IF
C-                           Atmpres < 900. or Atmpres >1050.
C-                                    or
C-                           Temptrd < 15.  or Temptrd > 35.
C-                                    or
C-                                LHEAD.LE.0
C-
C-
C- decision tree
C  -------------          Thit=0
C                         /      \
C                        yes     no
C                       /          \
C                   cdd4=0         if THIT vs >4
C                     /  \                /    \
C                   yes  no             yes    no
C                   /      \            /        \
C   READ : RCP or CDFIX   data base   THIT      cdd4=0
C                                                /   \
C                                               no    yes
C                                              /       \
C                                            data b.   RCP
C-
C-   Created  29-SEP-1994   Y. Ducros
C-   Updated  30-SEP-1994   Alain PLUQUET  Read reference from TRD.RCP
C-   Updated  17-OCT-1994   Y. Ducros removes test on event date
C-   Updated  11-JAN-1995   Lewis Taylor Goss read Uranium data from CDFIX DB
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:URAN_COR.INC'
      INCLUDE 'D0$INC:zebstp.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INTEGER IDAT_EVENT,ITIM_EVENT,J,LAYER,NWARN,NWARN1
      INTEGER LOUT,TRUNIT
      INTEGER TIME_OF_RUN(2),LOC
      INTEGER*2 UNPACKED_TIME(7)
      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND
      REAL    DATINF,DATSUP,QJT_EVENT
      INTEGER I,K,M,N,ILAY,R,RUN_NUMBER_ZONE
      REAL COR_URA,V0_ANO,V0_POT,COR_URAPT
      REAL TEMPTRD,ATMPRES,CORPT_EVENT
      INTEGER INDIC,LEN,NKEYS
      INTEGER LTHIT,GZTHIT,LCDD4
      PARAMETER(NKEYS=12)
      INTEGER KEYS(7+NKEYS,2)
      INTEGER N_ZONES
      PARAMETER (M=300)
      CHARACTER*7 URANIUM_DB
      REAL HV_ARRAY(6,M)
      INTEGER RUN_LIMITS_HV(2,M),N_ZONES_HV
      INTEGER IDAT,ITIM,IER,RUN_NO,RUNNO,LTRDT,GZTRDT
      REAL HV(2,3),HP(2,3),PRES(2),TEMP(2),UR(2,3)
      REAL HV_STOR(3,M),HP_STOR(3,M),PRES_STOR(M),TEMP_STOR(M)
      REAL QJT_UR_STOR(M),UR_STOR(3,M)
      INTEGER URANIUM_RUN_STOR(M),URANIUM_RUN(2),IRUN(2)
      REAL    DENOM
      LOGICAL OK,SYS$NUMTIM,FIRST,DOPRINT,TRD_DO_PRINT
      LOGICAL READ_FROM_DB,READ_FROM_RCP,READ_FROM_THIT
      EXTERNAL SYS$NUMTIM
      REAL ALPHA,BETA,TREF,PREF,C0_POT(3),U0
      DATA ALPHA / 6.2 /
      DATA BETA / 15. /
      DATA TREF / 20. /
      DATA PREF / 1013. /
      DATA C0_POT / 0.32, 0.18, 0.36 /
      DATA U0 / 1000. /
      DATA FIRST /.TRUE./
      DATA RUN_NO /0/
      SAVE FIRST,RUN_NO,URANIUM_RUN_STOR,UR_STOR,PRES_STOR,TEMP_STOR,
     &  QJT_UR_STOR,HV_STOR,HP_STOR
      IF (FIRST) THEN
        LOUT=TRUNIT()
        NWARN=0
        NWARN1=0
        CALL EZLOC ('TRD_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP ('TRD_RCP',IER)
        CALL EZPICK ('TRD_RCP')
        CALL GET_TRD_COR_BY_RUN
     &    ('TRD_RCP','TRD_RCP','REF_HV',HV_ARRAY,
     &    6,RUN_LIMITS_HV,N_ZONES_HV)
        CALL EZGETS ('URANIUM_DB',1,URANIUM_DB,LEN,IER)
        IF (URANIUM_DB.EQ.'TRD.RCP') THEN     ! READ FROM RCP FILE
          CALL EZGETA ('URANIUM',0,0,0,N,IER)
          N_ZONES=N/14
          DO I=1,N_ZONES
            K=(I-1)*14
            CALL EZGETA ('URANIUM',K+1,K+1,1,URANIUM_RUN_STOR(I),IER)
            CALL EZGETA ('URANIUM',K+2,K+4,1,UR_STOR(1,I),IER)
            CALL EZGETA ('URANIUM',K+5,K+5,1,PRES_STOR(I),IER)
            CALL EZGETA ('URANIUM',K+6,K+6,1,TEMP_STOR(I),IER)
            CALL EZGETA ('URANIUM',K+7,K+7,1,IDAT,IER)
            CALL EZGETA ('URANIUM',K+8,K+8,1,ITIM,IER)
            CALL EZGETA ('URANIUM',K+9,K+11,1,HV_STOR(1,I),IER)
            CALL EZGETA ('URANIUM',K+12,K+14,1,HP_STOR(1,I),IER)
            CALL CLDR(92,IDAT,ITIM,QJT_UR_STOR(I))
          ENDDO
        ENDIF
        CALL EZRSET
C
        DATINF=0.
        DATSUP=0.
        COR_URA=0.
        INDIC=0
        FIRST=.FALSE.
        DOPRINT=.TRUE.
        IF(LOUT.LE.0)DOPRINT=.FALSE.
      ENDIF
      DOPRINT=TRD_DO_PRINT().AND.DOPRINT
      COR_URAPT=1.
      IER=0
      LAYER=ILAY
      IF(ILAY.GT.3)LAYER=ILAY-3
      TIME_OF_RUN(1) = IQ(LHEAD+4)
      TIME_OF_RUN(2) = IQ(LHEAD+5)
      OK     = SYS$NUMTIM(UNPACKED_TIME(1),TIME_OF_RUN(1))
      YEAR   = UNPACKED_TIME(1) - (UNPACKED_TIME(1)/100)*100
      MONTH  = UNPACKED_TIME(2)
      DAY    = UNPACKED_TIME(3)
      IDAT_EVENT   = 10000*YEAR+100*MONTH+DAY
      HOUR   = UNPACKED_TIME(4)
      MINUTE = UNPACKED_TIME(5)
      SECOND = UNPACKED_TIME(6)
      ITIM_EVENT   = HOUR*10000+100*MINUTE+SECOND
      CALL CLDR(92,IDAT_EVENT,ITIM_EVENT,QJT_EVENT)
      IF(LHEAD.LE.0) THEN
        IER=1
        GO TO 999
      END IF
      IF (RUN_NO.NE.RUNNO()) THEN
        READ_FROM_DB=.FALSE.
        READ_FROM_RCP=.FALSE.
        READ_FROM_THIT=.FALSE.
        LTHIT=GZTHIT()
        LCDD4 = LQ(LHEAD-IZCDD4)
        IF(LTHIT.LE.0)THEN
          IF(LCDD4.NE.0)THEN ! raw data are there
            READ_FROM_DB=.TRUE.
          ELSE
            IF (URANIUM_DB.EQ.'TRD.RCP') READ_FROM_RCP=.TRUE.
            IF (URANIUM_DB(1:5).EQ.'CDFIX') READ_FROM_DB=.TRUE.
          ENDIF
        ELSE !THIT has ben defined
          IF(MOD(IQ(LTHIT+1),10).GE.4)THEN
            READ_FROM_THIT=.TRUE.
          ELSE ! version of THIT<4
            IF(LCDD4.NE.0)THEN ! raw data are there
              READ_FROM_DB=.TRUE.
            ELSE
              IF (URANIUM_DB.EQ.'TRD.RCP') READ_FROM_RCP=.TRUE.
              IF (URANIUM_DB(1:5).EQ.'CDFIX') READ_FROM_DB=.TRUE.
            ENDIF
          END IF
        END IF
        IF(DOPRINT)WRITE(LOUT,*)
     &    'READ_FROM_DB,READ_FROM_RCp,READ_FROM_THIT',
     &    READ_FROM_DB,READ_FROM_RCP,READ_FROM_THIT
        IF(.NOT.READ_FROM_DB .AND. .NOT.READ_FROM_RCP .AND.
     &    .NOT.READ_FROM_THIT)THEN
          OVER_ALL_COR(LAYER) = 1.
          COR_URAPT = 1.
          NWARN=NWARN+1
          IF(NWARN.LT.10) CALL ERRMSG(
     &      'Cant find coorrection for uranium','TRD_CALURAPT',' ','w')
          GO TO 999
        END IF
        IF(READ_FROM_DB)THEN
          IF(DOPRINT)WRITE(LOUT,*)' run_no',RUN_NO,' read data base'
          CALL D0DBL3_DBPKTS(IDAT_EVENT,ITIM_EVENT,KEYS(3,1))
          KEYS(4,1) = KEYS(3,1)
          CALL FETCH_URAN_CDFIX(KEYS)
          DO I = 1,2
            CALL D0DBL3_DBUPTS(IDAT,ITIM,KEYS(3,I))
            URANIUM_RUN(I) = KEYS(8,I)
            PRES(I)        = FLOAT(KEYS(12,I))/10.
            TEMP(I)        = FLOAT(KEYS(13,I))/10.
            CALL CLDR(92,IDAT,ITIM,QJT_UR(I))
            DO J = 1,3
              UR(I,J)     = FLOAT(KEYS( 8+J,I))/10.
              HV(I,J)     = FLOAT(KEYS(13+J,I))/10.
              HP(I,J)     = FLOAT(KEYS(16+J,I))/10.
            ENDDO
          ENDDO
        ELSEIF(READ_FROM_THIT)THEN ! read uranium info from TROP kept on  THIT
          CALL FETCH_URAN_TROP(URANIUM_RUN,UR, PRES,TEMP,HV,HP,QJT_UR)
C
        ELSEIF(READ_FROM_RCP)THEN! no thit bank. read the information in trd.rcp
          DO I = 1,N_ZONES - 1
            IF (QJT_EVENT.LE.QJT_UR_STOR(I).AND.I.EQ.1) THEN
              IRUN(1) = 1
              IRUN(2) = 2
            ELSEIF(QJT_EVENT.GT.QJT_UR_STOR(I).AND.QJT_EVENT.LE.
     &          QJT_UR_STOR(I+1)) THEN
              IRUN(1) = I
              IRUN(2) = I + 1
            ELSEIF(I.EQ.N_ZONES-1.AND.QJT_EVENT.GT.QJT_UR_STOR(I+1))
     &          THEN
              IRUN(1) = N_ZONES
              IRUN(2) = N_ZONES
            ENDIF
          ENDDO
          DO I = 1,2
            URANIUM_RUN(I) = URANIUM_RUN_STOR(IRUN(I))
            PRES(I)        = PRES_STOR(IRUN(I))
            TEMP(I)        = TEMP_STOR(IRUN(I))
            QJT_UR(I)      = QJT_UR_STOR(IRUN(I))
            DO J = 1,3
              UR(I,J)    = UR_STOR(J,IRUN(I))
              HV(I,J)    = HV_STOR(J,IRUN(I))
              HP(I,J)    = HP_STOR(J,IRUN(I))
            ENDDO
          ENDDO
        END IF
        RUN_NO = RUNNO()
      END IF
C
      IF(DOPRINT)WRITE(LOUT,*)' in trd_calurapt,atmpres,TEMPTRD',
     &  ATMPRES,TEMPTRD
      IF(ATMPRES.LT.900..OR.ATMPRES.GT.1050.) THEN
        IER=1
        GO TO 999
      END IF
      IF(TEMPTRD.LT.15..OR.TEMPTRD.GT.35.) THEN
        IER=1
        GO TO 999
      END IF
      J=0
      DO 800 I=1,2
        UR_COR(I,LAYER)=0.
        IF(DOPRINT)WRITE(LOUT,*)' in trd_calurapt,i',I,' press,temp',
     &    PRES(I),TEMP(I),' uranium_run',URANIUM_RUN(I)
        IF(PRES(I).LT.900. .OR. PRES(I).GT.1050.)THEN
          GO TO 800
        END IF
        J=J+1
        CORPT(I)=((PRES(I)*(273.+TREF)/((TEMP(I)+273.)*PREF))**ALPHA)
        R = RUN_NUMBER_ZONE(RUN_LIMITS_HV,URANIUM_RUN(I))
        V0_ANO = HV_ARRAY(LAYER,R)
        V0_POT = HV_ARRAY(LAYER+3,R)
        IF(DOPRINT)WRITE(LOUT,*)' in trd_calurapt,i,layer',I,LAYER,
     &    ' hv',HV(I,LAYER), ' hp',HP(I,LAYER)
        CORHV(I,LAYER)=(V0_ANO/(HV(I,LAYER)-C0_POT(LAYER)*(HP(I,LAYER)
     &        -V0_POT)))**BETA
        UR_COR(I,LAYER)=UR(I,LAYER)*CORPT(I)*CORHV(I,LAYER)/U0
  800 CONTINUE
C
      CORPT_EVENT=((ATMPRES*(273.+TREF))/((TEMPTRD+273.)*PREF))
     &      **ALPHA
C      IF(QJT_EVENT.GT.DATINF.AND.QJT_EVENT.LE.DATSUP.AND.
C     #   INDIC.EQ.1) GO TO 998
      IF(J.NE.2)THEN ! problem with uranium runs
        NWARN1=NWARN1+1
        IF(DOPRINT)WRITE(LOUT,*)' layer',LAYER,' j',J,' nwarn1',
     &    NWARN1
        IF(NWARN1.LT.10)
     &      CALL ERRMSG('incorrect value for uranium data  ',
     &      'TRD_CALURAPT',' ','w')
        IF(J.NE.1)THEN ! no correct value
          IER=2
          GO TO 999
        END IF
        K=1 ! at least one set of uranium values is correct
        IF(UR_COR(1,LAYER).LE.0.)K=2
        COR_URA=1./UR_COR(K,LAYER)
        IF(DOPRINT)WRITE(LOUT,*)' cor-uranium',UR_COR(K,LAYER)
        GO TO 998
      END IF
      IF(QJT_EVENT.LE.QJT_UR(1)) THEN
        IF(UR_COR(1,LAYER).LE.0.) THEN
          IER=1
          GO TO 999
        END IF
        DATINF=635.
        DATSUP=QJT_EVENT
        COR_URA=1./UR_COR(1,LAYER)
        GO TO 998
      ENDIF
      IF(QJT_EVENT.GT.QJT_UR(1).AND.QJT_EVENT.LE.QJT_UR(2)) THEN
        GOTO    997
      ENDIF
      IF(QJT_EVENT.GT.QJT_UR(2)) THEN
        IF(UR_COR(2,LAYER).LE.0.) THEN
          IER=1
          GO TO 999
        END IF
        DATINF=QJT_UR(2)
        DATSUP=9999.
        COR_URA=1./UR_COR(2,LAYER)
        GO TO 998
      END IF
C --------------------------------------------------------------------
  997 CONTINUE
      IF(QJT_UR(2).EQ.QJT_UR(1)) THEN
        IF(UR_COR(1,LAYER).GT.0.) THEN
          COR_URA=1./UR_COR(1,LAYER)
          DATINF=QJT_UR(1)
          DATSUP=QJT_UR(2)
          GO TO 998
        ELSE
          IER=1
          GO TO 999
        END IF
      END IF
C
      DENOM=UR_COR(1,LAYER)+(QJT_EVENT-QJT_UR(1))*(UR_COR(2,LAYER)-
     &      UR_COR(1,LAYER))/(QJT_UR(2)-QJT_UR(1))
      IF(DENOM.EQ.0.) THEN
        IER=1
        GO TO 999
      END IF
      DATINF=QJT_UR(1)
      DATSUP=QJT_UR(2)
      COR_URA=1./DENOM
C
  998 INDIC=1
      OVER_ALL_COR(LAYER) = CORPT_EVENT*COR_URA
      COR_URAPT = OVER_ALL_COR(LAYER)
      IF(DOPRINT)WRITE(LOUT,*)' in trd_calurapt,layer',LAYER,
     &    'COR_URAPT',COR_URAPT
      RETURN
  999 INDIC=0
      RETURN
      END
