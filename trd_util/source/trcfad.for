      SUBROUTINE  TRCFAD(TRACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :  TRACK = TRACK NUMBER
C-   Controls:
C-
C-   Created  26-APR-1989   A. Zylberstejn
C-   Updated  15-MAR-1994   A. Zylberstejn   Correct for misplaced call to
C-                                           TRDTMIN
C-   Updated   9-MAY-1994   Alain PLUQUET    sector correction version 2
C-   Updated  29-SEP-1994   Alain PLUQUET   called GET_TRD_COR_GAS with a new
C-                                          argument (LAYER)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CLUREC.INC'
      INCLUDE 'D0$INC:FADCCN.INC'
      INCLUDE 'D0$INC:FADTRD.INC'
      INCLUDE 'D0$INC:FIRSHT.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:THIT_INFO.INC'
      INCLUDE 'D0$INC:TRENER.INC'
      INCLUDE 'D0$INC:VARTRD.INC'
      INCLUDE 'D0$INC:TCLUS_PER_WIRE.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:TRDCOR.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INCLUDE 'D0$INC:ZEBWRK.INC'
      INTEGER LENGWS
      PARAMETER (LENGWS=5000)
      REAL WS(LENGWS)
      INTEGER IWS(LENGWS)
      EQUIVALENCE(WS,IWS,W(1001))
      INTEGER I,J,K,CHA1,IER,ITYP,UBIT,ADJ(2),ISWHSG(12)
      INTEGER TDATA(NMFADC+10)
      INTEGER LAYER,HIT,WI,WIRE,TRACK,SECT,SECTOR,GOOD_HIT
      INTEGER TWIRCOR,NERR,NPLANE
      INTEGER LVSIMI,LVSIMX,NBW,TCHNB
      INTEGER NBIN_FADC
      INTEGER LTHIT,GZTHIT
      INTEGER LOUT,TRUNIT
      INTEGER FFADCB,LFADCB,NBFADC
      REAL    MIPTO5G,TRDTMIN,VSUM,VMAX,CORRECTION
      LOGICAL FIRST,HEXIST,CATHODE_ANALYSIS,DOCLUS,DOPRINT,TRD_DO_PRINT
      CHARACTER*3 C3
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        LOUT=TRUNIT()
        FIRST=.FALSE.
        NERR=0
        CALL EZPICK('TRD_RCP')
        NBIN_FADC=128
        CALL EZGET_i('NBINS_PHYSICS',I,IER)
        IF(IER.EQ.0)NBIN_FADC=I
C        CALL EZGET('GENERAL_TRD_HISTOS',ISWHSG(1),IER)
        CALL EZGET_l('CATHODE_ANALYSIS',CATHODE_ANALYSIS,IER)
        DOCLUS=.FALSE.
        CALL EZGET_i('CLUSTER_RECONS',I,IER)
        CALL UHTOC(I,3,C3,3)
        IF(C3.EQ.'y' .OR. C3.EQ.'Y' .OR. C3.EQ.'YES')DOCLUS=.TRUE.
        CALL EZGET('MIP_TO_5_GEV',MIPTO5G,IER)
        FFADCB=1
        LFADCB=NBIN_FADC
        CALL EZGET_i('FIRST_FADC_BIN',I,IER)
        IF(IER.EQ.0)        FFADCB=I
        CALL EZGET_i('LAST_FADC_BIN',I,IER)
        IF(IER.EQ.0)        LFADCB=I
        NBFADC=LFADCB-FFADCB+1
        CALL EZGET('MIP_TO_5_GEV',MIPTO5G,IER)
        CALL EZRSET
        IF (CATHODE_ANALYSIS) THEN
          NPLANE=6
        ELSE
          NPLANE=3
        ENDIF
      ENDIF
      IF(TRACK.GE.59)THEN
        CALL ERRMSG
     &    (' Problem_TRD: Too many tracks in the TRD','TRCFAD',' ','W')
        RETURN
      END IF
      CALL RESET_TRDCOR()
C-------------------------------------------------------------------------------
C initilizations for this track
C-------------------------------------------------------------------------------
      ETOTAL(1,TRACK)=0.
      ETOTAL(2,TRACK)=0.
      NTWIRE(1,TRACK)=0
      NTWIRE(2,TRACK)=0
C      CALL VZERO(NBPNT(1,TRACK),6)
C-------------------------------------------------------------------------------
C loop on layers
C-------------------------------------------------------------------------------
      LTHIT=GZTHIT()
      DOPRINT=TRD_DO_PRINT()
      IF(DOPRINT)WRITE(LOUT,'(a30,i3,/,a30)')' enter trcfad track:',
     &    TRACK,' ----------------'
      DO 160 LAYER=1,NPLANE
        NTOT_CL(LAYER)=0
        GOOD_HIT=0
        TRETOT(LAYER,TRACK)=0.
C        CALL VZERO(FADCTR(1,LAYER,1),NMFADC)
        IF (NBHWIR(LAYER,TRACK).LE.0) GO TO 160
        IF (LAYER.LE.3) THEN
          ITYP=1
        ELSE
          ITYP=2
        ENDIF
C-------------------------------------------------------------------------------
C Calibration and corrections at layer level
C-------------------------------------------------------------------------------
        CALL GET_TRD_COR_ANG(TRACK,CANG,EANG)
        CALL GET_TRD_COR_EPI(LAYER,CEPI,EEPI)
        CALL GET_TRD_COR_APC(LAYER,CAPC,EAPC)
        CALL GET_TRD_COR_GAS(LAYER,CGAS,TCAN,TTRD,PCAN,PTRD,GCAN,EGAS)
C-------------------------------------------------------------------------------
C loop on hit wires
C-------------------------------------------------------------------------------
        EMAXCH(LAYER,TRACK)=-1000.
        NBW=0
        IF(DOPRINT)WRITE(LOUT,*)'layer,track',LAYER,TRACK,'nbhwir',
     &    NBHWIR(LAYER,TRACK)
        DO 80 HIT=1,NBHWIR(LAYER,TRACK)
          WIRE=WIRNBH(HIT,LAYER,TRACK)
          IF(DOPRINT)WRITE(LOUT,*)'wire',WIRE
          NCL_PER_WIRE(HIT,LAYER)=0
          IF(WIRE.LE.0.OR.WIRE.GT.NWIRE_PER_LAYER(LAYER))THEN
            IF(NERR.LE.20) CALL ERRMSG
     +        (' Problem_TRD: wrong wire number ','TRCFAD',' ','W')
            NERR=NERR+1
            GO TO 80
          END IF
          IF (.NOT.TWCOD(TCHNB(WIRE,LAYER))) GO TO 80
          GOOD_HIT=GOOD_HIT+1
C-------------------------------------------------------------------------------
C calibration and corrections at wire level
C-------------------------------------------------------------------------------
          SECT=(WIRE-1)/16+1
          IF(LAYER.EQ.3)SECT=(SECT-1)/2+1
          CALL GET_TRD_COR_SEC(LAYER,SECT,2,CSEC,ESEC)
          CALL GET_TRD_COR_WIR(LAYER,WIRE,0,CWIR,EWIR)
          CALL GET_TRD_COR_HVT(LAYER,WIRE,CHVT,HVA,HVP,HVW,EHVT)
          CALL GET_TRD_COR_ELE(LAYER,WIRE,CELE,EELE)
C-------------------------------------------------------------------------------
C computes energy with all calibration and correction factors
C-------------------------------------------------------------------------------
          CORRECTION=CELE*CGAS*CANG/CEPI*CSEC*CWIR*CHVT
          WI=WIRE
          IF (IQ(LHEAD+6).LT. 68000 .AND. .NOT.MCDATA.AND.LAYER.EQ.3)
     +        WI=TWIRCOR(WIRE) ! correct for miscabling
c          PRINT*,' in trcfad,wire',WIRE,' twircor',WI
          IF(LTHIT.LE.0)THEN
            IF(LQ(LHEAD-IZCDD4).LE.0)THEN
              CALL ERRMSG (' Problem_TRD: not enough raw data info',
     &          'TRCFAD',' ','W')
              GO TO 999
            END IF
            CALL TCODER(CHA1,LAYER-1,WI-1,UBIT,2)
            CALL ZDEXPD(4,CHA1,TDATA)
            IF (TDATA(1).LT.100) GOTO 80
            CALL VFLOAT(TDATA(3),WS,TDATA(1))
            CALL GET_TRD_COR_PED(LAYER,WI,WS,CPED,EPED)
C            print*,' in trcfad,cped',cped
            CALL WRITE_TRDCOR(LAYER,TRACK,GOOD_HIT)
            IF (JSWITC(1).EQ.1)
     +        CALL TLYRFL(LAYER,TRACK,WIRE,TDATA(3),TDATA(1))!fill TLYR
            CALL VBIAS(WS,-CPED+CAPC,WS(1001),NBIN_FADC)
            TMIN_TRD(HIT,LAYER)=TRDTMIN(WS(1001)) ! tzero
            ETOTWI(HIT,LAYER,TRACK)= VSUM(WS(1000+FFADCB),NBFADC)
C  compute clusters
            NCREC=0
            IF(DOCLUS)THEN
              J=0
              CALL CLUSTF(WS(1000+FFADCB),LAYER*1000+NBFADC)
            END IF
          ELSE
            IF(LQ(LHEAD-IZCDD4).NE.0)THEN
              CALL TCODER(CHA1,LAYER-1,WI-1,UBIT,2)
              CALL ZDEXPD(4,CHA1,TDATA)
              IF (TDATA(1).LT.100) GOTO 80
              CALL VFLOAT(TDATA(3),WS,TDATA(1))
              CALL GET_TRD_COR_PED(LAYER,WI,WS,CPED,EPED)
            END IF
            IWS(2002)=TRACK ! transmit track number to put in THIT
            CALL THIT_UNPACK(WIRE,LAYER)
            TMIN_TRD(HIT,LAYER)=WS(2003)
            ETOTWI(HIT,LAYER,TRACK)=WS(2001)
            IF(DOPRINT)WRITE(LOUT,*)
     &          ' in trcfad from THIT,hit,layer',HIT,LAYER,
     &          'tmin_trd',TMIN_TRD(HIT,LAYER),' etot',
     &          ETOTWI(HIT,LAYER,TRACK)
          END IF
          ETOTWI(HIT,LAYER,TRACK)=ETOTWI(HIT,LAYER,TRACK)*CORRECTION
          TRETOT(LAYER,TRACK)= ETOTWI(HIT,LAYER,TRACK)+
     &        TRETOT(LAYER,TRACK)
          NCL_PER_WIRE(HIT,LAYER)=NCREC
          IF(NCREC.NE.0)THEN
            DO I=1,NCREC
              IF(NBW.LT.NCLMAX)THEN
                NBW=NBW+1
                ECL_PER_WIRE(NBW,LAYER)=ECLR(I)*CORRECTION
                LEFT_CL(NBW,LAYER)=IBLFT(I)
                RIGHT_CL(NBW,LAYER)=IBRGHT(I)
                HEIGTH_CL(NBW,LAYER)=YSUP(I)
                CENTER_CL(NBW,LAYER)=IBCEN(I)
              END IF
            END DO
            NCREC=NBW
          END IF
          IF(ETOTWI(HIT,LAYER,TRACK).GT. EMAXCH(LAYER,TRACK))THEN
            NWIRMA(LAYER,TRACK)=HIT
            EMAXCH(LAYER,TRACK)=ETOTWI(HIT,LAYER,TRACK)
          END IF
          CALL WRITE_TRDCOR(LAYER,TRACK,HIT)
   80   CONTINUE ! end of loop on hit wires
        NTOT_CL(LAYER)=NBW
C-------------------------------------------------------------------------------
C wire number with energy max (number,energy,tzero)
C-------------------------------------------------------------------------------
C        NWIRMA(LAYER,TRACK)=
C     &    LVMAX(ETOTWI(1,LAYER,TRACK),NBPNT(LAYER,TRACK))
C        EMAXCH(LAYER,TRACK)= ETOTWI(NWIRMA(LAYER,TRACK),LAYER,TRACK)
        TMINIM(LAYER,TRACK)=TMIN_TRD(NWIRMA(LAYER,TRACK),LAYER)
        IF(DOPRINT)WRITE(LOUT,*)' nwirma',NWIRMA(LAYER,TRACK),
     &    'tminim',TMINIM(LAYER,TRACK)
C-------------------------------------------------------------------------------
C fills dq/dt histogram
C-------------------------------------------------------------------------------
        IF(HEXIST(FIRSHT+640+LAYER).AND.
     +      LAYER.LE.3.AND.TRETOT(LAYER,TRACK).GT.1.) THEN
          DO I=1,128
            CALL HF1(FIRSHT+640+LAYER,FLOAT(I),WS(1000+I))
          END DO
        END IF
C-------------------------------------------------------------------------------
C total energy and number of hit layers
C-------------------------------------------------------------------------------
        IF(NBHWIR(LAYER,TRACK).NE.0) THEN
          ETOTAL(ITYP,TRACK)=ETOTAL(ITYP,TRACK)+TRETOT(LAYER,TRACK)
          NTWIRE(ITYP,TRACK)=NTWIRE(ITYP,TRACK)+1
        ENDIF
C-------------------------------------------------------------------------------
C  Fill the banks with energy of the adjacent wires
C-------------------------------------------------------------------------------
        I=LVSIMI(WIRNBH(1,LAYER,TRACK),NBHWIR(LAYER,TRACK),1)
        ADJ(1)=WIRNBH(I,LAYER,TRACK)-1
        I=LVSIMX(WIRNBH(1,LAYER,TRACK),NBHWIR(LAYER,TRACK),1)
        ADJ(2)=WIRNBH(I,LAYER,TRACK)+1
        IF (ADJ(1).LE.0)   ADJ(1)=NWIRE_PER_LAYER(LAYER)
        IF (ADJ(2).GT.NWIRE_PER_LAYER(LAYER)) ADJ(2)=1
        DO 140 K=1,2
          WIRE=ADJ(K)
          IF (IQ(LHEAD+9).LT. 68000 .AND. .NOT.MCDATA.AND.LAYER.EQ.3)
     &                                             WIRE=TWIRCOR(WIRE)
          ETOTWI(NBINFO+K-2,LAYER,TRACK)=0.
          IF (.NOT. TWCOD(TCHNB(WIRE,LAYER))) GO TO 140
          SECTOR=(WIRE-1)/16+1
          CALL GET_TRD_COR_SEC(LAYER,SECTOR,2,CSEC,ESEC)
          CALL GET_TRD_COR_WIR(LAYER,WIRE,0,CWIR,EWIR)
          CALL GET_TRD_COR_HVT(LAYER,WIRE,CHVT,HVA,HVP,HVW,EHVT)
          CALL GET_TRD_COR_ELE(LAYER,WIRE,CELE,EELE)
          CORRECTION=CELE*CGAS*CANG/CEPI*CSEC*CWIR*CHVT
          IF(LTHIT.LE.0)THEN
            IF(LQ(LHEAD-IZCDD4).LE.0)THEN
              CALL ERRMSG (' Problem_TRD: not enough raw data info',
     &          'TRCFAD',' ','W')
              GO TO 999
            END IF
            CALL TCODER(CHA1,LAYER-1,ADJ(K)-1,UBIT,2)
            CALL ZDEXPD(4,CHA1,TDATA)
            CALL VFLOAT(TDATA(3),WS,NBIN_FADC)
            CALL GET_TRD_COR_PED(LAYER,WIRE,TDATA,CPED,EPED)
            CALL VBIAS(WS,-CPED+CAPC,WS,NBIN_FADC)
            CALL VSCALE(WS,CORRECTION,WS,NBIN_FADC)
            ETOTWI(NBINFO+K-2,LAYER,TRACK)=
     &        VSUM(WS,NBIN_FADC)*CORRECTION
          ELSE! Pick up the information from THIT bank
            WS(2002)=0
            CALL THIT_UNPACK(WIRE,LAYER)
            ETOTWI(NBINFO+K-2,LAYER,TRACK)=WS(2001)*CORRECTION
          END IF
  140   CONTINUE
  160 CONTINUE  ! end of loop on layers
C-------------------------------------------------------------------------------
C truncated energy
C-------------------------------------------------------------------------------
      ETRUNC(1,TRACK)=ETOTAL(1,TRACK)-VMAX(TRETOT(1,TRACK),3)
C      ETRUNC(2,TRACK)=ETOTAL(2,TRACK)-VMAX(TRETOT(2,TRACK),3)
  999 CONTINUE
      IF(DOPRINT)WRITE(LOUT,'(a30,i3,/,a30)')' exit TRCFAD track:',
     &    TRACK,' ----------------'
      RETURN
      END
