      SUBROUTINE TRH_OUT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Handle TRD hits out of reconstructed tracks
C-                         For the time being only anodes are analysed
C-                         (15-Sept-91)
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-APR-1989   A. Zylberstejn
C-   Updated  15-SEP-1991   A. Zylberstejn :Corrections to deal with THIT bank
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CLUREC.INC'
      INCLUDE 'D0$INC:FIRSHT.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:TRENER.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$INC:TRWCOD.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LOUT,IER,LTRDT,GZTRDT,LZTRDH,GZTRDH,LZTPRL
      INTEGER CHA1,LZTLYR,IW,ID,IERR,LJ,IUCOMP,UBIT,TDATA(518),WIRE
      INTEGER LSHF,LTHIT,GZTHIT,NCL,NTOT,NTHIT,NBHIT
      INTEGER JJ,LSH,TYPE,LAYER,TRACK_NUMBER
      REAL ENERGY,PEDES,VI
      INTEGER I,ITR,ICH,IDENT,ISHFT,J,TRUNIT,KFADC,NDFADC,NCLUST(3)
      INTEGER IR,II,JBYT
      EQUIVALENCE (VI , II )
      LOGICAL DOCOR,DOCLUS,DOPRINT,FIRST,DO_HISTO
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        CALL EZPICK('TRD_RCP')
        DO_HISTO=.FALSE.
        CALL EZGET('HSTBOK',IWS,IERR)
        IF(IERR.EQ.0)DO_HISTO=IWS(1).EQ.1HY .OR. IWS(1).EQ.1HY
     &    .OR. IWS(1).EQ.3HYES
        CALL EZGET('ETOTM',ETOTM,IER)
        NDFADC=TVALIN(3)
        DOCLUS=.FALSE.
        CALL EZGET('CLUSTER_RECONS',I,IER)
        IF(I.EQ.1HY .OR. I.EQ.1HY .OR. I.EQ.3HYES)DOCLUS=.TRUE.
C        DOCOR=.FALSE.
C        CALL EZGET('CORRECTION_<EPI>',I,IER)
        DOPRINT=SWTDBG.EQ.1 .AND. LOUT.NE.0
        CALL EZRSET
      END IF
      IF(DO_HISTO)CALL HCDIR('//PAWC/TRD',' ')  ! go to TRD directory
      DOPRINT=DOPRINT.AND.TNEVDG.GT.0.AND.ITR.LE.TNTRDG
      LTHIT=GZTHIT()
      IF(LTHIT.LE.0)GO TO 999
      IF(DOPRINT)THEN
        WRITE(LOUT,1001)IQ(LTHIT+2)
 1001   FORMAT(' TRD hit Bank TRHIT for',I3,' hits')
        WRITE(LOUT,1002)
 1002   FORMAT(' layer  wire   Energy   N clus.  Track nb.')
      END IF
      NTOT=2
      DO 254 I=1,IQ(LTHIT+2)  ! Loop on the hits
        LSH=LTHIT+NTOT
        II=IQ(LSH+1)
        LAYER=JBYT(II,10,2)+1
        TYPE=JBYT(II,1,1)
        WIRE= JBYT(II,2,8)+1
        NCL=JBYT(II,12,3)! number of clusters
        TRACK_NUMBER=JBYT(II,25,28)
        IF(DOPRINT)
     +    WRITE(LOUT,1004)TYPE*3+LAYER,WIRE,FLOAT(IQ(LSH+2))*.1,NCL
        IF(TRACK_NUMBER.NE.0)GO TO 254
        NTOT=NTOT+2
        IF(NCL.NE.0)THEN
          NTOT=NTOT+2
          IF(DO_HISTO)THEN
            DO IR =  1, MIN0(NCL,4)
              IBCEN(IR)=JBYT(IQ(LSH+3),(IR-1)*8+1,8)
              ECLR(IR)=FLOAT(JBYT(IQ(LSH+4), (IR-1)*8+1,8))
              IF(DO_HISTO)CALL HF1(660+ICH+FIRSHT,ECLR(IR),1.)
            END DO
          END IF
        END IF
 1004   FORMAT(2X,I2,4X,I4,1X,F8.1,5X,I3,8X,I5,8(I4,F6.1))
C  Histogram for off-track coded wires
        CALL HF1(FIRSHT+590+ICH,FLOAT(I),1.)
C
        IF(.NOT.DOPRINT)GO TO 254! Drawing of off_track wires
        IW=WIRE
        WIRE=IW
        IF(COSMIC1 .AND.ICH.EQ.3)THEN
          IF(IW.GE.49  .AND. IW.LE.56)WIRE=I+8
          IF(IW.GE.57  .AND. IW.LE.64) WIRE=I-8
          IF(IW.GE.80  .AND. IW.LE.88)WIRE=I+8
          IF(IW.GE.89  .AND. IW.LE.96) WIRE=IW-8
          IF(IW.GE.193 .AND. IW.LE.200)WIRE=IW-8
          IF(IW.GE.65  .AND. IW.LE.72) WIRE=IW+8
          IF(IW.GE.73  .AND. IW.LE.80 )WIRE=IW-8
        END IF
        CALL TCODER(CHA1,ICH-1,WIRE-1,UBIT,2)
        CALL ZDEXPD(4,CHA1,TDATA)
        IF (TDATA(1).EQ.0)GO TO 254
        CALL TRGPED('BID',IW,ICH,PEDES,IERR) ! Get pedestals
        IF(IERR.NE.0)THEN
          WRITE(LOUT,*)' Error in TRGPED,ich,iwi',ICH,IW
          PEDES=0
        END IF
        CALL VFLOAT(TDATA(3),WS,TDATA(1))
        CALL VBIAS(WS,-PEDES,WS,TDATA(1))
        WRITE(LOUT,*)' Off-track wire',IW,' chambre',ICH,
     &          ' channels 1 to 120. pedes',PEDES,' etot',Q(LZTLYR+3)
        CALL DRAWPT(WS,120,2)
        WRITE(LOUT,*)' Off-track wire',IW,' chambre',ICH,
     &         ' Channel 121 to 240'
        CALL DRAWPT(WS(121),120,2)
        IF(NCL.NE.0)
     +    WRITE(LOUT,1006)JJ,(IBCEN(IR),ECLR(IR), IR=1,MIN0(4,NCL))
 1006   FORMAT(' nb. of clusters',I2,' position, energy',4(2G12.4))
  254 CONTINUE
  999 RETURN
      END
