      SUBROUTINE NOI_TRIG2_PILEUP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add pileup to CELL_EN array for NOISY
C-                         package level 2 pileup
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-JAN-1992   Allen I. Mincer
C-   Modified 28-JAN-1994   Allen I. Mincer uncomment tracking call
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBNOI.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      INCLUDE 'D0$INC:EVENT_HEAD_LINKS.INC'
      INCLUDE 'D0$INC:NOISY_STORE_LINKS.INC'
      INTEGER NPILE
      INTEGER I,J,K,NLO,NHI
      INTEGER JJJ,OLD_HEAD,OLD_HEADR,NEW_HEAD,NEW_HEADR
      INTEGER LAST_EVENT_NUM,NEW_EVENT_NUM
      INTEGER IETA,IPHI,ILYR
      INTEGER LCAEP,GZCAEP,NRP,NCH,PT_CAEP
      REAL ENERGY
      INTEGER NUMBUK
      REAL WGT
      LOGICAL FIRST,OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        NPILE=0
        WEIGHT(0)=1.0
        DO J=1,KMAX
          WEIGHT(J)=0.0
        ENDDO
        FIRST=.FALSE.
      ENDIF
C ***
C *** copy events saved by NOI_TRIG1_PILEUP into ZEBRA IXDVR division
C ***
      NLO=1
      NHI=NUMBPILE
      DO K=NLO,NHI
        J=K+1
        IF(K.EQ.NLO)THEN
          OLD_HEADR=SRUN_HEAD
          CALL NOI_EVENT_SELECT(K)
          NEW_HEADR=LHEADR
          CALL MZCOPY(IDVNOI,OLD_HEADR,IXDVR,NEW_HEADR,0,' ')
          NEW_HEADR=LQ(NEW_HEADR)
          CALL NOI_EVENT_HEAD_LINK(2,JJJ,NEW_HEADR,2)
        ENDIF
        OLD_HEAD=SEVENT_HEAD(J)
        CALL NOI_EVENT_SELECT(K)
        NEW_HEAD=LHEAD
        CALL MZCOPY(IDVNOI,OLD_HEAD,IXMAIN,NEW_HEAD,0,' ')
        NEW_HEAD=LQ(NEW_HEAD)
        CALL NOI_EVENT_HEAD_LINK(J,NEW_HEAD,JJJ,1)
      ENDDO
      CALL MZDROP(IDVNOI,SRUN_HEAD,'L')
      CALL MZDROP(IDVNOI,SEVENT_HEAD(2),'L')
      SRUN_HEAD=0
      DO J=NLO,NHI
        SEVENT_HEAD(J)=0
      ENDDO
C ***
C *** ALL PILEUP EVENTS READ IN, NOW ADD THEM TO CELLS WITH PROPER
C ***     BUCKET WEIGHTS
C
C *** FIND EVENT WEIGHTS
C
      CALL NOI_TRIG2_VALCYC
C
C *** FILL CELL ENERGY ARRAY
C
      DO 710 J=1,NUMBPILE
        NEW_EVENT_NUM=J+1
        CALL NOI_EVENT_SELECT(NEW_EVENT_NUM)
C ***
C ***   CALL TRACKING PILEUP PACKAGE 
C ***
        IF(DO_TRACK_PILEUP .AND. IWBUCK(J).EQ.80)THEN
          CALL NOI_TRACK_PILE(NEW_EVENT_NUM)
          CALL NOI_EVENT_SELECT(NEW_EVENT_NUM)
        ENDIF
        LCAEP=GZCAEP()
        IF(LCAEP.LE.0) THEN
          CALL ERRMSG(' NOISY','NOI_TRIG2_PILEUP',
     &      ' NO CAEP BANKS FOR PILEUP EVENT','W')
        ELSE
          WGT=WEIGHT(J)
          IF(WGT.NE.0.0)THEN
            NRP=IQ(LCAEP+2)
            NCH=IQ(LCAEP+3)
            DO I=1,NCH
              PT_CAEP=LCAEP+(I-1)*NRP
              CALL CAEP_INDICES(IQ(PT_CAEP+4),IETA,IPHI,ILYR) ! unpack address
              ENERGY=Q(PT_CAEP+5)
              CELL_EN(IETA,IPHI,ILYR)=
     &            CELL_EN(IETA,IPHI,ILYR)+ENERGY*WGT
            ENDDO
          ENDIF
        ENDIF
  710 CONTINUE
C *** DROP NUMBSWITCH NOISE EVENTS AND RESET COUNTER VALUES
      NPILE=NUMBPILE-NUMBSWITCH
      IF(NUMBSWITCH.GT.0) THEN
        IF(NPILE.GT.0)THEN
          DO K=1,NUMBSWITCH
            J=K+1
            CALL NOI_EVENT_DROP(J,1)
          ENDDO
          NLO=NUMBSWITCH+1
          NHI=NUMBPILE
          DO K=NLO,NHI
            J=K+1
            CALL NOI_EVENT_SELECT(J)
C
            NEW_EVENT_NUM=J-NUMBSWITCH
            LAST_EVENT_NUM=NEW_EVENT_NUM-1
C ***
C ***       copy saved events into ZEBRA division where they will be
C ***       saved for use in the next event
C ***
            IF(K.EQ.NLO)THEN
              OLD_HEADR=LNOIH
              CALL MZCOPY(IXDVR,LHEADR,IDVNOI,OLD_HEADR,1,' ')
              SRUN_HEAD=OLD_HEADR
              OLD_HEAD=SRUN_HEAD
            ELSE
              OLD_HEAD=SEVENT_HEAD(LAST_EVENT_NUM)
            ENDIF
            CALL MZCOPY(IXMAIN,LHEAD,IDVNOI,OLD_HEAD,0,' ')
            NEW_HEAD=LNS(OLD_HEAD)
            SEVENT_HEAD(NEW_EVENT_NUM)=NEW_HEAD
            CALL NOI_EVENT_DROP(J,1)
          ENDDO
          CALL NOI_EVENT_DROP(2,2)
        ELSE
          NPILE=0
        ENDIF
      ENDIF
      CALL NOI_EVENT_SELECT(1)
C----------------------------------------------------------------------
  999 RETURN
      END
