      SUBROUTINE TRD_DST_COR
     &  (LTRDT,VERSION,CORRECTION,ENERGY,DIAGNOSTIC_COR,RWD,IWD,EPICOR)
C ---------------------------------------------------------------------
C
C    Purpose and Methods : handles TRD corrections at DST level
C
C    Inputs  :
C       LTRDT         integer      LINK to TRDT
C
C    Outputs :
C       VERSION       real        version of TPRL
C       CORRECTION    logical(10) status word for corrections
C                                 (TRUE=done, FALSE=problem)
C                                 CORRECTION(1)  electronic gain correction
C                                 CORRECTION(2)  EPICOR calibration
C                                 CORRECTION(3)  additive pedestal correction
C                                 CORRECTION(4)  pedestal substraction
C                                 CORRECTION(5)  sector corection
C                                 CORRECTION(6)  wire correction
C                                 CORRECTION(7)  high voltage correction
C                                 CORRECTION(8)  angular correction
C                                 CORRECTION(9)  gas correction
C
C       ENERGY         real(5)    energies in MIP
C                                 ENERGY(1) energy layer 1
C                                 ENERGY(2) energy layer 2
C                                 ENERGY(3) energy layer 3
C                                 ENERGY(4) total energy
C                                 ENERGY(5) truncated energy
C       DIAGNOSTIC_COR integer    coded word for special cases
C                                 0               <=> OK
C                                 bit 1 (LSB) =1  <=> canary out of bounds
C                                 bit 2       =1  <=> wrong status word
C                                 bit 3       =1  <=> wrong version
C                                 bit 4       =1  <=> error in elements
C                                 bit 5       =1  <=> error in banks
C       RWD             real(3,100)     output of UNPACK_TPRL for 3 layers
C       IWD             integer(3,100)  output of UNPACK_TPRL for 3 layers
C       EPICOR         real(3)    EPICOR for layers 1,2,3
C
C    Controls: TRD_ANALYSIS.RCP,TRD_RCP
C
C    Created   8-APR-1993   Alain PLUQUET
C-   Updated  16-JUL-1993   Alain PLUQUET  adds RCP source for GAS correction
C-   Updated  30-SEP-1993   Alain PLUQUET  takes into account mipto5gev for old
C-                                         data (clusters only)
C-   Updated  15-OCT-1993   Alain PLUQUET  adds DEBUG mode
C-   Updated  25-NOV-1993   Alain PLUQUET  adds RCP source for SEC correction
C-   Updated   4-MAR-1994   Alain PLUQUET  fixes bug (CELE-->CEPI),
C-                                         avoids crash due to a bug in RECO
C-                                         (wrong status word for wires 0)
C-   Updated   9-MAR-1994   A. Zylberstejn  :Temporary fix for null correction
C-   Updated  24-JUN-1994   Alain PLUQUET  split and simplify
C-   Updated  29-SEP-1994   Alain PLUQUET  allow OVERWRITE/MAXIMUM actions
C-                                         for EPI,GAS,HVT,SEC corrections
C-                                         Call CTCOR for run1A with protection
C-   Updated   8-NOV-1994   A. ZYLBERSTEJN  test on CTCOR value
C-   Updated  16-DEC-1994   Alain PLUQUET  avoid crash due to corrupted
C-                                         TRD_STPFILE.DAT .
C-                                         DO nothing for MC data
C-   Updated  14-APR-1995   A. Zylberstejn  correct a bug: replace IQ by IC to
C-            compute LTCY
C-   Updated  23-MAY-1995   A. Zylberstejn  protect for PTRD=0,TTRD=0 for run1b
C-   Updated   9-JUN-1995   A. Zylberstejn  remove test on TRDT version 3
C-   Updated   9-JUL-1995   A. Zylberstejn  introduce new epicor for mc data
C ---------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:TRD_COR_VALUES.INC'
      INCLUDE 'D0$INC:ZEBWRK.INC'
C      INCLUDE 'D0$INC:WORKSP.INC'
C   -------COMMON,WORKSP :DUMMY COMMON TO USE ARRAYS NOT TRANSMITTED FROM
C                         ONE SUBROUTINE TO ANOTHER.MAY BE REPLACED BY
C                         'ZEBRA' WORKING SPACE.
      INTEGER LENGWS
      PARAMETER (LENGWS=5000)
C      COMMON/WORKSP/WS(LENGWS)
      REAL WS(LENGWS)
      INTEGER IWS(LENGWS)
      EQUIVALENCE(WS,IWS,W(1001))
      INCLUDE 'D0$LINKS:IZTCY1.LINK'
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER RECOVERSION,PASS,RUNNI,RUNNO
      CHARACTER*3 NAME(10)
      CHARACTER*10 ACTION(10)
      CHARACTER*20 WORD
      CHARACTER*70 MESSAGE
      REAL A,HTANO,HTWIN,HTPOT,TTRD,PTRD
      REAL CEPI,CSEC,CGAS,CHVT,CTCOR,CELE
      REAL VERSION,COLD(4),ENERGY(5),EPICOR(3),RWD(3,300)
      REAL OLD_CORRECTION(10,4),OLD_FADC(4),EPICOR_MC(3)
      INTEGER DIAGNOSTIC_COR,NA,JBIT,WIRE,IER,LENGTH,LOUT,TRUNIT
      INTEGER LAYER,I,LTRDT,LTPRL,SECTOR,IWD(3,NWORD),IUCOMP,IS
      LOGICAL FIRST,DEBUG_MODE,CORRECTION(10),OKW(4),RUN1A
      LOGICAL DOPRINT, TRD_DO_PRINT,DO_CORRECTION,DO_COR
      INTEGER GZTCAN,LTCY1,LOC,IFOIS,J,NEVTI
      REAL ENRJCAN,ENRJREF,TEMPCAN,TEMPTRD,TEMPREF
      REAL ENRAP,TCANRAP,TTRDRAP,RELATIVE_AGEING
      REAL RCVI ! recoversion
      LOGICAL LCFITOK,MONTE_CARLO_DATA,MC_DATA
      REAL CHGN(3),SECTGN(16,3)
      DATA CHGN/1.00 ,  0.85,   0.83/
      DATA SECTGN                            /
     +1.13,1.09,1.19,1.16,1.17,1.18,1.12,1.00,  !CHAMB 1
     +1.02,1.12,1.16,1.14,1.18,1.07,1.07,1.08,
     +0.96,0.89,0.98,0.94,0.98,1.00,0.94,0.91,  !CHAMB 2
     +0.90,0.92,0.95,0.95,1.01,0.94,0.98,0.94,
     +1.00,0.89,0.93,0.87,0.93,0.94,0.91,0.89,  !CHAMB 3
     +0.92,0.91,0.94,1.05,0.99,0.86,0.94,0.94/
C-------------------------------------------------------------------------------
      DATA FIRST/.TRUE./
      DATA NAME
     &/'ELE','EPI','APC','PED','SEC','WIR','HVT','ANG','GAS','CDM'/
      DATA NEVTI/0/
      IF (FIRST) THEN
        FIRST=.FALSE.
        IFOIS=0
        LOUT=TRUNIT()
        MC_DATA= IQ(LHEAD+1) .GT. 1000
C        call ezrset
        CALL EZLOC ('TRD_ANALYSIS_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP ('TRD_ANALYSIS_RCP',IER)
        CALL EZPICK('TRD_ANALYSIS_RCP')
        CALL EZGET('DEBUG_MODE',DEBUG_MODE,IER)
        IF (DEBUG_MODE) CALL INTMSG(
     &' TRD_ANALYSIS package is called with the following options :')
        DO_CORRECTION=.TRUE.
        CALL EZGET('ENERGY_CORRECTION',DO_COR,IER)
        IF(IER.EQ.0)DO_CORRECTION=DO_COR
        DO I=1,9
          WORD=NAME(I)//'_CORRECTION'
          ACTION(I)='NOCHANGE'
          IF(DO_CORRECTION)THEN
            CALL EZGETS (WORD,1,ACTION(I),LENGTH,IER)
            CALL CLTOU(ACTION(I))
            IF (DEBUG_MODE) THEN
              WRITE (MESSAGE,'(1X,A3,2(1X,A10),33X)')
     &          NAME(I),ACTION(I)
              CALL INTMSG(MESSAGE)
            ENDIF
          END IF
        ENDDO
        IF(MC_DATA)THEN
          ACTION(1)='OVERWRITE'
          CALL EZGET('MC_EPICOR',EPICOR_MC,IER)
        ENDIF
        CALL EZRSET
        LCFITOK=.FALSE.
        RUNNI=0
        RCVI=0.
        IF(RUN1A())THEN
          LTCAN=GZTCAN()
          IF (LTCAN.GT.0) THEN
            IF(IC(LTCAN-2).GE.IZTCY1)THEN
              LTCY1=LC(LTCAN-IZTCY1)
            ELSE
              LTCY1=0
            ENDIF
            IF (LTCY1.GT.0)   THEN
              LCFITOK=.TRUE.
            ELSE
              LCFITOK=.FALSE.
              CALL ERRMSG
     &          (' TRD_DST_COR','TRD_DST_COR','Bank TCY1 not found','W')
            ENDIF
          ELSE
            LCFITOK=.FALSE.
            CALL ERRMSG
     &        (' TRD_DST_COR','TRD_DST_COR','Bank TCAN not found','W')
          ENDIF
        END IF
      ENDIF
C-------------------------------------------------------------------------------
C     initialyze
C-------------------------------------------------------------------------------
      IFOIS=IFOIS+1
      DOPRINT=TRD_DO_PRINT()
      CALL VFILL(ENERGY,5,0.)
      CALL VFILL(EPICOR,3,0.)
      DIAGNOSTIC_COR=0
      IF(NEVTI.NE.IQ(LHEAD+9))THEN
        CALL THIT_GET
        NEVTI=IQ(LHEAD+9)
      END IF
      CALL RECO_VERSION(RECOVERSION,PASS)
      IF(RUNNO().NE.RUNNI .OR. FLOAT(RECOVERSION)+PASS*0.01.NE.RCVI)
     &    THEN
        RCVI=FLOAT(RECOVERSION)+PASS*0.01
        RUNNI=RUNNO()
        IF(DOPRINT)
     +    WRITE(LOUT,*)'new run',RUNNI,' new reco version',RCVI
        VERSION=0.
        DO LAYER=1,3
          LTPRL=LQ(LTRDT-LAYER)
          IF(LTPRL.NE.0)        VERSION=Q(LTPRL+1)
        END DO
        IF(VERSION.LE.0.)RETURN
c        VERSION=Q(LTPRL+1)
        IF(VERSION.GE.2)RECOVERSION=12
        IF(DOPRINT)
     +    WRITE(LOUT,*)' reco version',RECOVERSION,PASS
        IF(RECOVERSION.LT.11)THEN
          CORRECTION(1)=.TRUE.
          CORRECTION(2)=.TRUE.
          CORRECTION(3)=.FALSE.
          CORRECTION(4)=.TRUE.
          CORRECTION(5)=.FALSE.
          CORRECTION(6)=.FALSE.
          CORRECTION(7)=.FALSE.
          CORRECTION(8)=.TRUE.
        END IF
      END IF
      DO LAYER=1,3
C        CALL VZERO(INTEGER_WORD,NWORD)
C        CALL VFILL(REAL_WORD,NWORD,0.)
        LTPRL=LQ(LTRDT-LAYER)
        IF(DOPRINT)
     +    WRITE(LOUT,*)' in trd_dst_cor,layer',LAYER,' ltprl',LTPRL
        IF (LTPRL.GT.0) THEN
          DO I=1,NWORD
            INTEGER_WORD(I)=IWD(LAYER,I)
            REAL_WORD(I)=RWD(LAYER,I)
          END DO
C          CALL UNPACK_TPRL(LTPRL,VERSION,REAL_WORD,INTEGER_WORD,IER)
C-------------------------------------------------------------------------------
C  read status word and number of hit anodes
C-------------------------------------------------------------------------------
          VERSION=Q(LTPRL+1)
          IF(RECOVERSION.GE.11)THEN
            DO I=1,9
              CORRECTION(I)=JBIT(IQ(LTPRL+4),I).EQ.1
            ENDDO
            NA=IWD(LAYER,4)
            IF(DOPRINT)
     +        WRITE(LOUT,*)' in trd_dst_cor,na',NA
            IF(DOPRINT)
     +        WRITE(LOUT,*)' in trd_dst_cor corrections',CORRECTION
          ELSE
            NA=IQ(LTPRL+14)
            VERSION=Q(LTPRL+1)
            IF(DOPRINT)
     +        WRITE(LOUT,*)' recoversion est inf a 11'
          END IF
          IF(DOPRINT)
     +      WRITE(LOUT,*)' na',NA,    ' version',VERSION
          IF(NA.GT.4)THEN
            CALL ERRMSG('number of hit anodes too big ','TRD_DST_COR',
     &                  ' TPRL bank may be clobbered','W')
            CALL SBIT1(DIAGNOSTIC_COR,5)
            GO TO 999
          END IF
C-------------------------------------------------------------------------------
C if known bug then exit
C-------------------------------------------------------------------------------
          IF (VERSION.LT.2.0) THEN      ! wrong version
            IF(DOPRINT)
     +        WRITE(LOUT,*)'layer',LAYER,' q(ltprl+4)',Q(LTPRL+4),
     &        ' correction tprl',Q(LTPRL+4),
     &        ' correction theta',SIN(Q(LZFIT+13))
            IF(.NOT.MC_DATA)THEN
              CALL SBIT1(DIAGNOSTIC_COR,3)
              GOTO 999
            END IF
          ELSE
            IF(DOPRINT)
     +        WRITE(LOUT,*)' IQ(LTPRL+4)',IQ(LTPRL+4)
C-------------------------------------------------------------------------------
C if corrupted corrections then exit
C-------------------------------------------------------------------------------
            IF (IQ(LTPRL+4).EQ.511) THEN  ! wrong status
              CALL SBIT1(DIAGNOSTIC_COR,2)
              GOTO 999
            ENDIF
          ENDIF
          IF (NA.GT.0) THEN
            IF(VERSION.LT.2.)THEN
              DO I=1,NA
                CALL VFILL(OLD_CORRECTION(1,I),9,1.)
                WIRE=Q(LTPRL+17+I)
                SECTOR=(WIRE-1)/16+1
                IF(LAYER.EQ.3)SECTOR=(WIRE-1)/32+1
                IF(DOPRINT)
     +            WRITE(LOUT,*)'wire',WIRE,' sector',SECTOR,
     &            ' sector correction', SECTGN(SECTOR,LAYER)
                IF(SECTGN(SECTOR,LAYER).NE.0.)THEN
                  IF(DOPRINT)
     +              WRITE(LOUT,*)' cor corrige sans angle',Q(LTPRL+4)/
     &              SECTGN(SECTOR,LAYER),' cor corrige avec angle',
     &              Q(LTPRL+4)/ SECTGN(SECTOR,LAYER)/SIN(Q(LZFIT+13))
                  OLD_CORRECTION(1,I)=1./SECTGN(SECTOR,LAYER)
                  OLD_CORRECTION(2,I)=Q(LTPRL+4)/SECTGN(SECTOR,LAYER)
                END IF
                OLD_CORRECTION(3,I)=0.
                OLD_CORRECTION(4,I)=0.
                OLD_CORRECTION(8,I)=SIN(Q(LZFIT+13))! angular correction
                COLD(I)=Q(LTPRL+4)/OLD_CORRECTION(8,I)
                OLD_FADC(I)=RWD(LAYER,50+I)*COLD(I)
                CALL THIT_UNPACK(WIRE,LAYER)
                IF(DOPRINT)
     +            WRITE(LOUT,*)' energy ',RWD(LAYER,50+I),' old_fadc',
     &            OLD_FADC(I),' raw',WS(2001),' cold',COLD(I)
                OKW(I)=.TRUE.
C              WRITE(LOUT,*)' in TRD_DST_COR,layer wire',LAYER,
C     &           WIRE,' old corrections',
C     &           (OLD_CORRECTION(Is,i),Is=1,9)
              END DO
            ELSE! reco version>10
              IWS(2005)=LAYER
              CALL TRD_SEPARATE_ELEMENTS (REAL_WORD,INTEGER_WORD,
     &          CORRECTION,OLD_CORRECTION,OLD_FADC,COLD,OKW)
            END IF
            DO WIRE=1,NA
              CALL UCOPY(OLD_CORRECTION(1,WIRE),
     &            OLD_VALUES(1,WIRE,LAYER),10)
              CALL UCOPY(OLD_CORRECTION(1,WIRE),
     &            NEW_VALUES(1,WIRE,LAYER),10)
              WS(3000+(LAYER-1)*10+WIRE)=RWD(LAYER,50+WIRE)
              IF (.NOT.OKW(WIRE)) THEN
                CALL SBIT1(DIAGNOSTIC_COR,4)
                GOTO 999
              ENDIF
C             IF(DOPRINT)then
C              CALL THIT_UNPACK(IWD(LAYER,50+WIRE),LAYER)
C     +          WRITE(LOUT,*)' in TRD_DST_COR,layer wire',LAYER,
C     &          IWD(LAYER,50+WIRE),' old corrections',
C     &          (OLD_CORRECTION(I,WIRE),I=1,9),' raw data',
C     &          WS(2001),'old_fadc',OLD_FADC(WIRE)
C              end if
            ENDDO
C-------------------------------------------------------------------------------
C EPICOR correction
C-------------------------------------------------------------------------------
            IF(DOPRINT)
     +        WRITE(LOUT,*)' in trd_dst_cor,action(2) ',ACTION(2),
     &        ' correction(2) ',CORRECTION(2)
            IF (ACTION(2).EQ.'OVERWRITE'.OR.
     &            (ACTION(2).EQ.'MAXIMUM'.AND..NOT.CORRECTION(2))) THEN
              IF(MC_DATA)THEN
                IER=0
                CEPI=EPICOR_MC(LAYER)
                IF(DOPRINT)
     +            WRITE(LOUT,*)' dans trd_dst_cor,cepi',CEPI
              ELSE
                CALL GET_TRD_COR_EPI(LAYER,CEPI,IER)
              END IF
              IF (IER.EQ.0) THEN
                IF(CEPI.NE.0.)THEN
                  DO WIRE=1,NA
                    OLD_CORRECTION(2,WIRE)=CEPI
                  ENDDO
                END IF
                CORRECTION(2)=.TRUE.
              ENDIF
            ELSEIF (ACTION(2).EQ.'CANCEL') THEN
              DO WIRE=1,NA
                OLD_CORRECTION(2,WIRE)=1.
              ENDDO
              CORRECTION(2)=.FALSE.
            ENDIF
            IF(DOPRINT)
     +        WRITE(LOUT,*)'OLD_CORRECTION(2)',(OLD_CORRECTION(2,WIRE),
     &        WIRE=1,NA)
C-------------------------------------------------------------------------------
C Electronic Gain Correction
C-------------------------------------------------------------------------------
            IF(DOPRINT)
     +        WRITE(LOUT,*)' action(1)',ACTION(1)
            IF (ACTION(1).EQ.'OVERWRITE')THEN
              DO WIRE=1,NA
                CALL GET_TRD_COR_ELE(LAYER,IWD(LAYER,50+WIRE),
     &            CELE,IER)
                IF (IER.EQ.0) OLD_CORRECTION(1,WIRE)=CELE
              ENDDO
              CORRECTION(1)=.TRUE.
            ELSEIF (ACTION(1).EQ.'CANCEL') THEN
              DO WIRE=1,NA
                OLD_CORRECTION(1,WIRE)=1.
              ENDDO
              CORRECTION(1)=.FALSE.
            ENDIF
            IF(DOPRINT)
     +        WRITE(LOUT,*)'OLD_CORRECTION(1)',(OLD_CORRECTION(1,WIRE),
     &        WIRE=1,NA)
C-------------------------------------------------------------------------------
C Additive pedestal correction
C-------------------------------------------------------------------------------
            IF (ACTION(3).EQ.'CANCEL') THEN
              DO WIRE=1,NA
                OLD_CORRECTION(3,WIRE)=0.
              ENDDO
              CORRECTION(3)=.FALSE.
            ENDIF
C-------------------------------------------------------------------------------
C Pedestal substraction
C-------------------------------------------------------------------------------
            IF (ACTION(4).EQ.'CANCEL') THEN
              DO WIRE=1,NA
                OLD_CORRECTION(4,WIRE)=0.
              ENDDO
              CORRECTION(4)=.FALSE.
            ENDIF
C-------------------------------------------------------------------------------
C Wire correction
C-------------------------------------------------------------------------------
            IF (ACTION(6).EQ.'CANCEL') THEN
              DO WIRE=1,NA
                OLD_CORRECTION(6,WIRE)=1.
              ENDDO
              CORRECTION(6)=.FALSE.
            ENDIF
C-------------------------------------------------------------------------------
C Angular correction
C-------------------------------------------------------------------------------
            IF (ACTION(8).EQ.'CANCEL') THEN
              DO WIRE=1,NA
                OLD_CORRECTION(8,WIRE)=1.
              ENDDO
              CORRECTION(8)=.FALSE.
            ENDIF
C-------------------------------------------------------------------------------
C Sector correction
C-------------------------------------------------------------------------------
            IF(.NOT.MC_DATA)THEN ! do no perform sector correction for MC data
              IF (ACTION(5).EQ.'OVERWRITE'.OR.
     &            (ACTION(5).EQ.'MAXIMUM'.AND..NOT.CORRECTION(5))) THEN
                DO WIRE=1,NA
                  SECTOR=(IWD(LAYER,50+WIRE)-1)/16+1
                  CALL GET_TRD_COR_SEC(LAYER,SECTOR,2,CSEC,IER)
                  IF (IER.EQ.0) THEN
                    OLD_CORRECTION(5,WIRE)=CSEC
                  ENDIF
                ENDDO
                CORRECTION(5)=.TRUE.
              ELSEIF (ACTION(5).EQ.'CANCEL') THEN
                DO WIRE=1,NA
                  OLD_CORRECTION(5,WIRE)=1.
                ENDDO
                CORRECTION(5)=.FALSE.
              ENDIF
C-------------------------------------------------------------------------------
C High voltage correction
C-------------------------------------------------------------------------------
              IF (ACTION(7).EQ.'OVERWRITE'.OR.
     &            (ACTION(7).EQ.'MAXIMUM'.AND..NOT.CORRECTION(7))) THEN
                DO WIRE=1,NA
                  HTANO=RWD(LAYER,3*WIRE)
                  HTWIN=RWD(LAYER,3*WIRE+1)
                  HTPOT=RWD(LAYER,3*WIRE+2)
                  CALL TRD_CORHV(CHVT,LAYER,HTANO,HTPOT,IER)
                  IF (IER.EQ.0) THEN
                    OLD_CORRECTION(7,WIRE)=CHVT
                    CORRECTION(7)=.TRUE.
                  ENDIF
                ENDDO
              ELSEIF (ACTION(7).EQ.'CANCEL') THEN
                DO WIRE=1,NA
                  OLD_CORRECTION(7,WIRE)=1.
                ENDDO
                CORRECTION(7)=.FALSE.
              ENDIF
C-------------------------------------------------------------------------------
C Gas correction
C-------------------------------------------------------------------------------
              IF (ACTION(9).EQ.'OVERWRITE'.OR.
     &              (ACTION(9).EQ.'MAXIMUM'.AND..NOT.CORRECTION(9)))
     &              THEN
                IF (RUN1A()) THEN
                  A=0.
                  IF (LCFITOK) THEN
                    A=CTCOR()
                    IF(A.GT.0.)THEN
                      CGAS=A*RELATIVE_AGEING()
                      IER=0 ! reset error flag to 0
                    END IF
                  END IF
                  IF(A.LE.0.)THEN
                    TEMPCAN=Q(LTRDT+21)+273.15
                    TEMPTRD=Q(LTRDT+22)+273.15
                    ENRJCAN=Q(LTRDT+11)
                    TEMPREF=293.15
                    ENRJREF=650.
                    IF (ENRJCAN.EQ.0.) THEN
                      ENRJCAN=650. ! approximates CANARY
                    ENDIF
                    IF (TEMPCAN.EQ.0.) THEN
                      TEMPCAN=293.15 ! approximates temperature
                    ENDIF
                    IF (TEMPTRD.EQ.0.) THEN
                      TEMPTRD=293.15 ! approximates temperature
                    ENDIF
                    IF (ENRJCAN.GT.0..AND.TEMPTRD.GT.0..AND.
     &                TEMPCAN.GT.0..AND.ENRJREF.GT.0..AND.
     &                TEMPREF.GT.0.) THEN
                      ENRAP=ENRJCAN/ENRJREF
                      TCANRAP=TEMPCAN/TEMPREF
                      TTRDRAP=TEMPTRD/TEMPREF
                      IF((ENRJCAN.LT.200..OR.ENRJCAN.GT.900.).OR.
     &                  ABS(TTRDRAP-1.).GT.0.5.OR.
     &                  ABS(TCANRAP-1.).GT.0.5) THEN
                        CGAS=1.
                        IER=1
                        CALL ERRMSG('TRD_DST_COR','TRD_DST_COR',
     &                    ' Canary values out of bound','W')
                      ELSE
                        CGAS=(ENRJREF/ENRJCAN)*
     &                    ((TEMPCAN/TEMPTRD)**7.1)*RELATIVE_AGEING()
                        IER=0
                      ENDIF
                    ELSE
                      CGAS=1.
                      IER=1
                      CALL ERRMSG('TRD_DST_COR','TRD_DST_COR',
     &                  ' No P,T,canary data','W')
                    ENDIF
                  ENDIF
                ELSE !run 1 b
                  TTRD=Q(LTRDT+22)
                  IF (Q(LTRDT+1).GE.2) THEN
                    PTRD=Q(LTRDT+13)
                  ELSE
                    PTRD=(Q(LTRDT+13)-900.)*10.+4.1
                    IF(PTRD.LE.4.1) PTRD=980.
                  ENDIF
                  IF (Q(LTRDT+1).LT.3) THEN
                    IF(TTRD.LT.10.)TTRD=20.
                    IF(PTRD.LT.800.)PTRD=980.
                    CALL TRD_CALURAPT(TTRD,PTRD,LAYER,CGAS,IER)
                    IF(IER.NE.0)CALL ERRMSG('Error in TRD_calurapt',
     &                'TRD_DST_COR','no gas correction','W')
                  ELSE
                    CGAS=OLD_CORRECTION(9,1)
                    IER = 0
                  ENDIF
                ENDIF
                IF (IER.EQ.0 .AND. Q(LTRDT+1).LT.3.) THEN
                  CORRECTION(9)=.TRUE.
                  DO WIRE=1,NA
                    OLD_CORRECTION(9,WIRE)=CGAS
                  ENDDO
C                END IF
C                ELSEIF ((IER.EQ.0).AND.(Q(LTRDT+1).GE.3)) THEN
C                  CORRECTION(9)=.FALSE.
C                  DO WIRE=1,NA
C                    OLD_CORRECTION(9,WIRE)=1.
C                  ENDDO
                ELSE
                  CALL SBIT1(DIAGNOSTIC_COR,1)
                ENDIF
              ELSEIF (ACTION(9).EQ.'CANCEL') THEN
                DO WIRE=1,NA
                  OLD_CORRECTION(9,WIRE)=1.
                ENDDO
                CORRECTION(9)=.FALSE.
              ENDIF
            END IF ! end of test on MC data
C-------------------------------------------------------------------------------
C computes new energy
C-------------------------------------------------------------------------------
            IF(DOPRINT)
     +        WRITE(LOUT,*)' in TRD_DST_COR, energies in',
     &        (RWD(LAYER,50+WIRE),WIRE=1,NA),' layer',LAYER,
     &        ' wires',(IWD(LAYER,50+WIRE),WIRE=1,NA),
     &        ' old_fadc ',(OLD_FADC(WIRE),WIRE=1,NA),' layer',LAYER
C            IF(DOPRINT)
C     +        WRITE(LOUT,*)
C     +        ' avant  TRD_COMBINE_ELEMENTS, old_corrections',
C     &        OLD_CORRECTION
            CALL TRD_COMBINE_ELEMENTS
     &          (REAL_WORD,INTEGER_WORD,OLD_CORRECTION,OLD_FADC,COLD,
     &          OKW)
            DO WIRE=1,NA
              ENERGY(LAYER)=ENERGY(LAYER)+REAL_WORD(50+WIRE)
              RWD(LAYER,50+WIRE)=REAL_WORD(50+WIRE)
              CALL UCOPY(OLD_CORRECTION(1,WIRE),
     &          NEW_VALUES(1,WIRE,LAYER),10)
              IF(DOPRINT)THEN
                CALL THIT_UNPACK(IWD(LAYER,50+WIRE),LAYER)
                WRITE(LOUT,*)' in trd_dst_cor raw',WS(2001),
     &            ' OLD_VALUES'
                WRITE(LOUT,'(10F8.3)')(OLD_VALUES(I,WIRE,LAYER),I=1,10)
                WRITE(LOUT,*)' NEW_VALUES'
                WRITE(LOUT,'(10F8.3)')(NEW_VALUES(I,WIRE,LAYER),I=1,10)
                WRITE(LOUT,*)' in TRD_DST_COR, energy in',
     &            WS(3000+(LAYER-1)*10+WIRE),' energies out',
     &            REAL_WORD(50+WIRE)
C                WRITE(LOUT,*)' de/e ',WS((LAYER-1)*10+WIRE)
              END IF
            ENDDO
            RWD(LAYER,46)=ENERGY(LAYER)
            IWD(LAYER,12)=0
            DO I=1,9
              IF(CORRECTION(I))THEN
                CALL SBIT1(IWD(LAYER,12),I)
              END IF
            END DO
            EPICOR(LAYER)=REAL_WORD(1)
            RWD(LAYER,1)=REAL_WORD(1)
          ENDIF   ! IF (NA.GT.0)
        ENDIF   ! IF (LTPRL.GT.0)
      ENDDO   ! DO LAYER=1,3

      ENERGY(4)=ENERGY(1)+ENERGY(2)+ENERGY(3)
      ENERGY(5)=ENERGY(4)-MAX(ENERGY(1),ENERGY(2),ENERGY(3))
  999 RETURN
      END
