      SUBROUTINE TRD_REWB(LTRDT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Rewrite TRDT and TPRL bank after trd_analysis
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-JUN-1995   A. ZYLBERSTEJN
C-   Updated  24-JUL-1995   A. ZYLBERSTEJN
C-   Updated  30-SEP-1995   Lewis Taylor Goss  Don't overwrite ACCEPTANCE, etc. 
C-   Updated   8-NOV-1995   L.T. Goss Modified to run on RECO 12.20 DST's
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:TPRL_VERSION_NB.INC'
      INCLUDE 'D0$INC:TRDT_VERSION_NB.INC'
      INCLUDE 'D0$INC:TRD_NWORD.INC'
C      INCLUDE 'D0$INC:provl.INC'
      INCLUDE 'D0$INC:TRD_PHI_Z.INC'
      INCLUDE 'D0$INC:URAN_COR.INC'
      INCLUDE 'D0$INC:worksp.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZTDST,I,IER,LAYER,LTRDT,LTANA,LTDST,LTPRL,RUNI,RUNNO
      INTEGER IZ,LOUT,NA,NC,TRUNIT
      REAL VERSION
      LOGICAL FIRST,RUN1A,DOPRINT
      DATA FIRST/.TRUE./
      DATA DOPRINT/.FALSE./
C      DATA LOUT/6/
      DATA RUNI/0/
C----------------------------------------------------------------------
      LTDST=GZTDST()
C      doprint=.true.
      IF(RUNI.NE.RUNNO())THEN
        VERSION=TPRL_VERS+.512
        IF(RUN1A())        VERSION=TPRL_VERS+.256
        IF(RUNI.EQ.0)THEN
          LOUT=TRUNIT()
C          lout=6
          IF(LOUT.LE.0)DOPRINT=.FALSE.
        END IF
        RUNI=RUNNO()
      END IF
C do TRDT stuff
      IF (Q(LTRDT+1).LT.4) THEN
        Q(LTRDT+1) =TRDT_VERS
        IWS(4003) =IQ(LTRDT-5)  !BANK NUMBER= TRD track number
        Q(LTRDT+4)=Q(LTDST+5)   !Sum of energies layer 1+2+3
        Q(LTRDT+5)=Q(LTDST+6)   !Truncated energy
        Q(LTRDT+8)=Q(LTDST+11)  !TRD only Likelihood 
        Q(LTRDT+9)=Q(LTDST+10)  !TRD+CDC Likelihood
        Q(LTRDT+17)=Q(LTDST+20) !electron efficiency (computed with TRD only
C                               ! likelihood
        Q(LTRDT+18)=Q(LTDST+8)  !electron efficiency (computed with trunc. mean)
        Q(LTRDT+19)=Q(LTDST+9)  !electron efficiency (computed with TRD+CDC
C                               ! likelihood)
C Put uranium stuff in banks
        Q(LTRDT+26) = QJT_UR(1)   ! Lower uranium run packed time
        Q(LTRDT+27) = QJT_UR(2)   ! Upper uranium run packed time
        Q(LTRDT+28) = CORPT(1)    ! Lower uranium run p,t correction
        Q(LTRDT+29) = CORPT(2)    ! Upper uranium run p,t correction
        Q(LTRDT+30) = 1.          ! set status bit
        Q(LTRDT+31) = Q(LTDST+7)  ! acceptance
        Q(LTRDT+32) = Q(LTDST+8)  ! epsilon_t
C
        DO LAYER = 1,3
          Q(LTRDT+32+LAYER) = CORHV(2,LAYER)
        ENDDO
        DO I = 1,2
          DO LAYER=1,3
            Q(LTRDT+35+LAYER+(I-1)*3) = UR_COR(I,LAYER)
          ENDDO
        ENDDO
      ENDIF
C do TPRL stuff
      DO LAYER=1,3
        LTPRL=LQ(LTRDT-LAYER)
        IF(LTPRL.NE.0)THEN
          IF (Q(LTPRL+1).LT.4) THEN
            LTDST=GZTDST()
            LTANA=LQ(LTDST-LAYER)
            IF(LTANA.LE.0)THEN
              IF(DOPRINT)WRITE(LOUT,*)' ltana=0 pour layer',LAYER
              GO TO 999
            END IF
            DO I=1,NWORD
              REAL_WORD(I)=Q(LTANA+I)
              INTEGER_WORD(I)=IQ(LTANA+300+I)
            END DO
C  substract one to the cell numbers before packing
            NA=IQ(LTANA+304)
            NC=IQ(LTANA+305)
            IF(NA.NE.0)THEN
              DO I=1,NA
                INTEGER_WORD(50+I)=IQ(LTANA+350+I)-1
              END DO
            END IF
            IF(NC.NE.0)THEN
              DO I=1,NC
                INTEGER_WORD(50+NA+I)=IQ(LTANA+350+NA+I)-1
              END DO
            END IF
            IF(DOPRINT)WRITE(LOUT,*)' real-word(46)= energy',
     &        REAL_WORD(46),'old ener', Q(LTPRL+12),' new ener',
     &        Q(LTANA+46)
            IQ(LTPRL+4)=IQ(LTANA+300+12)
            Q(LTPRL+12)=Q(LTANA+46)
            Q(LTPRL+13)=Q(LTANA+47)
C            IF(DOPRINT)WRITE(LOUT,*)' ltprl avant  pack_tprl',LTPRL,
C     &        ' ltrdt,ltdst',LTRDT,LTDST,' version',VERSION
            Q(LTPRL+1)=VERSION
            CALL PACK_TPRL(LTPRL,VERSION,REAL_WORD,INTEGER_WORD,IER)
            LTRDT=LQ(LTPRL+1)
            LTDST=GZTDST()
C            IF(DOPRINT)WRITE(LOUT,*)' ltprl apres pack_tprl',LTPRL,
C     &        ' ltrdt,ltdst',LTRDT,LTDST,' version',VERSION
C
C write in 23 , phi*100,r*10,z*10. set bit 31 to 1 is z is negative
C
            IZ=MIN(IABS(INT(Z_TRD(LAYER)*10.)),1000)
            IQ(LTPRL+23)=INT(PHI_TRD(LAYER)*100.)
            CALL SBYT(INT(R_TRD(LAYER)*10.),IQ(LTPRL+23),11,10)
            CALL SBYT(IZ, IQ(LTPRL+23),21,10)
            IF(Z_TRD(LAYER).LE.0.)CALL SBIT1(IQ(LTPRL+23),31)
C            IF(DOPRINT)WRITE(LOUT,*)' in ,layer',LAYER,' phi,r,z',
C     &            PHI_TRD(LAYER),R_TRD(LAYER),Z_TRD(LAYER)
          ENDIF
        END IF
      END DO
  999 RETURN
      END
