      SUBROUTINE FIT2_NTUPLE(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  :  NTUPLE_ID   (I)   ID of the Ntuple
C-   Outputs :  IER         (I)   IER=0 Everything is OK
C-   Controls: 
C-
C-   Created  11-OCT-1993   Pushpa C. Bhat
C-
C----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C----------------------------------------------------------------------
      INTEGER IRUN,IEVENT,LFIT2,I,J,IER,GZFIT2
      INTEGER RUNNO,EVONUM,NTUPLE_ID
      INTEGER LJETS,GZJETS,NJETS,LVERT,GZVERT
      REAL    AMEX,AMEY,AMET,ET,ETA,JET_ETMIN,JETS_MAX_ETA
      REAL    TEMPLATE(5,4),ZVTX,THETA,PHI,EM_FRAC,DETA
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:FIT_TWO.INC'
      INCLUDE 'D0$LINKS:IZFIT2.LINK'
      DATA JET_ETMIN/10./
      DATA JETS_MAX_ETA/4./
      DATA TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2
C----------------------------------------------------------------------
C
C ****  WRITE OUT NTUPLE VARIABLES
C
      IRUN=RUNNO()
      IEVENT=EVONUM()
      PIN(1)=IRUN
      PIN(2)=IEVENT
      LFIT2=GZFIT2()
      DO I=1,6
        PIN(I+2)=Q(LFIT2+I+4)
      ENDDO
      PIN(9)=Q(LFIT2+17)
      PIN(10)=Q(LFIT2+18)
      PIN(11)=Q(LFIT2+21)
      PIN(12)=Q(LFIT2+22)
      PIN(13)=Q(LFIT2+23)
      PIN(14)=PIN(12)
      PIN(15)=Q(LFIT2+24)
      PIN(16)=Q(LFIT2+25)
      PIN(17)=Q(LFIT2+23)
      PIN(18)=Q(LFIT2+25)
      PIN(19)=Q(LFIT2+26)
      PIN(20)=Q(LFIT2+27)
      PIN(21)=Q(LFIT2+28)
      PIN(22)=Q(LFIT2+29)
      PIN(23)=Q(LFIT2+28)
      PIN(24)=Q(LFIT2+30)
      PIN(25)=Q(LFIT2+31)
      PIN(26)=Q(LFIT2+29)
      PIN(27)=Q(LFIT2+31)
      PIN(28)=Q(LFIT2+32)
      PIN(29)=Q(LFIT2+33)
      PIN(30)=Q(LFIT2+34)
      PIN(31)=Q(LFIT2+34)
      PIN(32)=Q(LFIT2+35)
      PIN(33)=Q(LFIT2+36)
      AMEX=Q(LFIT2+17)-Q(LFIT2+5)-Q(LFIT2+8)
      AMEY=Q(LFIT2+18)-Q(LFIT2+6)-Q(LFIT2+9)
      IF(DO_MUONS)THEN
        AMEX=Q(LFIT2+17) 
        AMEY=Q(LFIT2+18) 
      ENDIF
      PIN(49)=AMEX
      PIN(50)=AMEY
      AMET=(AMEX**2.+AMEY**2.)
      IF(AMET.GT.0)THEN
        AMET=SQRT(AMET)
      ELSE
        AMET=0.
      ENDIF
      IF(ABS(AMEX).GT.0)THEN
        PIN(34)= SQRT(PIN(29))*AMET/ABS(AMEX)
      ELSE
        PIN(34)=-9999.
      ENDIF
      DO I=1,6
        PIN(34+I)=Q(LFIT2+10+I)
      ENDDO
      PIN(41)=Q(LFIT2+19)
      PIN(42)=Q(LFIT2+20)
      AMEX=Q(LFIT2+19)-Q(LFIT2+11)-Q(LFIT2+14)
      AMEY=Q(LFIT2+20)-Q(LFIT2+12)-Q(LFIT2+15)
      IF(DO_MUONS)THEN
        AMEX=Q(LFIT2+19) 
        AMEY=Q(LFIT2+20)
      ENDIF
      AMET=(AMEX**2.+AMEY**2.)
      IF(AMET.GT.0)THEN
        AMET=SQRT(AMET)
      ELSE
        AMET=0.
      ENDIF
      PIN(43)=AMET
      PIN(44)=Q(LFIT2+4)
      PIN(45)=PROB(Q(LFIT2+4),2)
      IF(BW_CONSTR.OR.BW_CONSTR_RES.OR.BW_CONSTR_SPC.OR.FIX_ZMASS)
     & PIN(45)=PROB(Q(LFIT2+4),3)
      PIN(46)=Q(LFIT2+37)
      PIN(47)=Q(LFIT2+38)
      PIN(48)=Q(LFIT2+39)
C
C
C ****  LOOK AT JETS FOR THIS EVENT
C

      LVERT=GZVERT(1)
      IF(LVERT.GT.0) ZVTX=Q(LVERT+5)
      IF(ZVTX.EQ.0) ZVTX=-1000.
      NJETS=0.
      CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
      LJETS=GZJETS()
      IF(LJETS.NE.0) THEN
        CALL ZSORT(IXCOM,LJETS,6)
        LJETS=GZJETS()
        CALL ZTOPSY(IXCOM,LJETS)
        LJETS=GZJETS()
        DO WHILE (LJETS.GT.0)
          IF(LQ(LJETS-3).EQ.LEPPHO_PNTR(1).OR.
     &       LQ(LJETS-3).EQ.LEPPHO_PNTR(2))GOTO 800
          EM_FRAC=Q(LJETS+14)
          IF(EM_FRAC.LT..05) GOTO 800   ! typical hot cell in FH or CC
          PHI=Q(LJETS+8)
          THETA=Q(LJETS+7)
          CALL DET_ETA(ZVTX,THETA,DETA)
C             hot cell in ECEM
          IF(PHI.GT.4.75.AND.PHI.LT.4.8.AND.DETA.GT.2.2
     &        .AND.DETA.LT.2.7.AND.EM_FRAC.GT..93) GOTO 800
          ET =SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
          ETA = Q(LJETS+9)
          IF (ET.GE.JET_ETMIN) THEN
            IF (ETA.LT.JETS_MAX_ETA) THEN
              NJETS=NJETS+1
              IF(NJETS.EQ.1)JET_ET_MAX=ET
            ENDIF
          ENDIF
  800     CONTINUE
          LJETS=LQ(LJETS)          ! pointer to next jet
        ENDDO
      ENDIF
      CALL RESET_CAPH
C
      PIN(51)=NJETS
      PIN(52)=JET_ET_MAX
C
C ****  Stretch functions
C
      DO I=1,8
        PIN(52+I)=Q(LFIT2+39+I)
      ENDDO
      CALL WRITE_NTUPLE(IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('FIT_TWO','WRITE_NTUPLE',
     &      'Cannot write out Ntuple','W')
        ENDIF
C
  999 RETURN
      END

