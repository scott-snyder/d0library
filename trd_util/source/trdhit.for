      SUBROUTINE TRDHIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Unpacks TRD hits
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  30-MAY-1990   J.Fr.Glicenstein
C-   Updated  29-JUN-1992   A. Zylberstejn  Use THIT  bank
C-   Updated  20-SEP-1993   A. Zylberstejn  updated for 512 cellls in layer 3
C-   Updated  11-JAN-1994   A. Zylberstejn : Get rid of all corrections of
C-                                             energy
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FADCCN.INC'
      INCLUDE 'D0$INC:UNPTRD.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:TRHITW.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER DT0,ISUPFLAT,IPRIME,IER,NBMOT,IMOT,VERSION
      INTEGER CHA1,ICH,IWIRE,UBIT,TDATA(NMFADC+10),IERR,IW,KKK
      INTEGER EXTD,LOUT,TRUNIT,TWIRCOR,TCHNB
      INTEGER I,II,GZTHIT,LSH,NCL,JBYT,JJ,LTHIT,NTOT,TDMIN,TDMAX
      REAL WG,PEDES,FDATA(NMFADC+10),CORE(3)
C     real GAIN_TRD(NW1+NW2+NW3)
C
      REAL TRHITS(NTOT_WIRE_TRD)
      REAL VSUM
      CHARACTER*3 C3
      LOGICAL DOCOR,FIRST,RUN1A,DOPRINT,TRD_DO_PRINT
      INTEGER LAYER,K,DEPTH
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        CALL EZPICK('TRDHIT_RCP')
        CALL EZGET_i('TRIGGER_TIME',DT0,IER)
        CALL EZGET_i('SUPPRESS_FLAT_CHAN',ISUPFLAT,IER)
        CALL EZGET_i('CORRECTION_<EPI>',I,IER)
        CALL UHTOC(I,3,C3,3)
        DOCOR=C3.EQ.'y' .OR. C3.EQ.'Y' .OR. C3.EQ.'YES'
C        CALL VFILL(GAIN_TRD,768,1.)
        IF(DOCOR) THEN
          CALL EZGET('EPICOR',CORE(1),IER)
C          DO ICH=1,3  ! Get gains
C            DO IWIRE=1,NWIRE_PER_LAYER(ICH)
C              CALL TRGGN('BID',IWIRE,ICH,WG,IERR)
C              IF(IERR.EQ.0)GAIN_TRD(TCHNB(IWIRE,ICH))=WG
C            END DO
C          END DO
        ELSE
          CORE(1)=200.
          CORE(2)=200.
          CORE(3)=200.
        END IF
        CALL EZRSET
        DEPTH = DT0 + NTFADC
        KKK = DT0 + 3
        LOUT=TRUNIT()
        FIRST = .FALSE.
      ENDIF
      LTHIT = GZTHIT( )
      IF(LTHIT.LE.0)THEN
        CALL ERRMSG('Bank THIT not booked','TRDHIT',' use CDD4','W')
        GO TO 60
      END IF
      DOPRINT=TRD_DO_PRINT()
      CALL VZERO_i(NBTHIT,6)
      CALL VZERO(NUMTWH,6*NMWHIT)
      CALL VZERO(ENTWH,6*NMWHIT)
C
C     CALL PRTHIT(6,LTHIT,0,'all',0)
      VERSION=IQ(LTHIT+1)
      NTOT=2
      IF(DOPRINT)WRITE(LOUT,*)' enter trdhit with lthit=',LTHIT
      DO I=1,IQ(LTHIT+2)  ! Loop on the hits
        LSH=LTHIT+NTOT
        IF(DOPRINT)WRITE(LOUT,*)' in loop in trdhit,lsh,ntot',LSH,NTOT
        II=IQ(LSH+1)
        IF(DOPRINT)WRITE(LOUT,*)' ii',II,' im lsh+1=',LSH+1
        K=JBYT(II,1,1)+1 ! k=1 anode,k=2 cathode
        EXTD=JBYT(II,17,1)
        IF(VERSION.GE.2)THEN
          IWIRE= JBYT(II,2,9)+1
          LAYER=JBYT(II,11,2)+1 +(K-1)*3! layer
          NCL=JBYT(II,13,4)! number of clusters
          IF(NCL.NE.0)NTOT=NTOT+NCL*(1+EXTD)
          IF(DOPRINT)WRITE(LOUT,*)' ncl,ntot',NCL,NTOT,' extd',EXTD
        ELSE
          IWIRE= JBYT(II,2,8)+1
          LAYER=JBYT(II,10,2)+1 +(K-1)*3! layer
          NCL=JBYT(II,12,3)! number of clusters
          IF(NCL.NE.0)NTOT=NTOT+2
        END IF
C       Removes bad hits
        IF(DOPRINT)WRITE(LOUT,*)'in trdhit,layer',LAYER,' wire',IWIRE,
     &    ' ntot',NTOT, ' lsh',LSH
        IF (LAYER.LT.1.OR.LAYER.GT.3) GOTO 50
        NBTHIT(LAYER,K) = NBTHIT(LAYER,K)+1
        NUMTWH(NBTHIT(LAYER,K),LAYER,K) = IWIRE
        ENTWH(NBTHIT(LAYER,K),LAYER,K) = FLOAT(IQ(LSH+2))*.1 ! Energy
   50   CONTINUE
        NTOT=NTOT+2
      END DO
      IF(DOPRINT)THEN
        WRITE(LOUT,*)'                     from THIT'
        WRITE(LOUT,*)'                     ---------'
        DO LAYER =  1,  3
          K=1
          DO 54 K=1,2
            WRITE(LOUT,*)' CHAMBRE',LAYER+(K-1)*2,
     &            ' NB. of hit THIT',NBTHIT(LAYER,K)
            IF(NBTHIT(LAYER,K).LE.0)GO TO 54
            WRITE(LOUT,*)' hit wires'
            WRITE(LOUT,'(10F8.0)')
     +        (NUMTWH(I,LAYER,K) ,I=1,NBTHIT(LAYER,K))
            WRITE(LOUT,*)' energies'
            WRITE(LOUT,'(6g10.4)')
     +        (ENTWH(I,LAYER,K) ,I=1,NBTHIT(LAYER,K))
   54     CONTINUE
        END DO
      ELSE
        GO TO 999
      END IF
   60 CONTINUE ! THIT not defined: use CDD4
      IF(LQ(LHEAD-IZCDD4).LE.0)GO TO 999
      CALL VZERO_i(NBTHIT,6)
      CALL VZERO(NUMTWH,6*NMWHIT)
      CALL VZERO(ENTWH,6*NMWHIT)
      PEDES = 0.
      WG    = 1.
      K = 1
C
      DO 100 ICH = 1,6
        IPRIME = ICH
        LAYER = ICH
        IF(ICH .GE. 4) THEN
          K = 2
          LAYER = ICH-3
        ENDIF
        DO 80 IW = 1,NWIRE_PER_LAYER(ICH)
          IWIRE = IW
C         IF (IQ(LHEAD+1).LT.1000) THEN
C          CALL T_COR_CABLE(IPRIME,IWIRE)  !   Fixes the cabling problems
C         ENDIF                            ! in real data
          IF(RUN1A() .AND. .NOT.MCDATA .AND. ICH.EQ.3 .AND.
     &                          IW.LE.16)IWIRE=TWIRCOR(IW)
          CALL TCODER(CHA1,IPRIME-1,IWIRE-1,UBIT,2)
          CALL ZDEXPD(4,CHA1,TDATA)
          NBMOT = TDATA(1)
          IF (TDATA(1).GT.0) THEN
C***
            IF(DOPRINT)WRITE(LOUT,*)'wire layer',IW,ICH,' nbmot',NBMOT
            NBMOT=MIN0(128,NBMOT)
            CALL VFLOAT(TDATA(3),FDATA(1),NBMOT)
            IF (TRDPEDES) THEN
              CALL TRGPED('BID',IWIRE,IPRIME,PEDES,IERR)
              IF(PEDES.LE.0.)PEDES=.1*VSUM(FDATA,10)
****  Substract pedestals
              CALL VBIAS(FDATA,-PEDES,FDATA,NBMOT)
            ENDIF
            NBTHIT(LAYER,K) = NBTHIT(LAYER,K)+1
            NUMTWH(NBTHIT(LAYER,K),LAYER,K) = IWIRE
            ENTWH(NBTHIT(LAYER,K),LAYER,K) = VSUM(FDATA, NBMOT)
C
          ENDIF
   80   CONTINUE
  100 CONTINUE
      IF(.NOT.DOPRINT)GO TO 999
      WRITE(LOUT,*)'                     from CDD4'
      WRITE(LOUT,*)'                     ---------'
      DO LAYER =  1,  3
C        K=1
        DO 64 K=1,2
          WRITE(LOUT,*)' CHAMBRE',LAYER+(K-1)*2,
     &            ' NB. of hit THIT',NBTHIT(LAYER,K)
          IF(NBTHIT(LAYER,K).LE.0)GO TO 64
          WRITE(LOUT,*)' hit wires'
          WRITE(LOUT,'(10F8.0)')
     +        (NUMTWH(I,LAYER,K) ,I=1,NBTHIT(LAYER,K))
          WRITE(LOUT,*)' energies'
          WRITE(LOUT,'(6g10.4)')
     +        (ENTWH(I,LAYER,K) ,I=1,NBTHIT(LAYER,K))
   64   CONTINUE
      END DO
  999 CONTINUE
      RETURN
      END
