      SUBROUTINE MUHIST_CALIB(MUCALIB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills histograms of signed residual and
C-                         coarse adjustment of T0 for each module.
C-                         Also fills histograms for signed residual
C-                         and fine adjustment of DT0.
C-
C-   Inputs  : MUCALIB     calibration .rcp value
C-   Outputs : NONE
C-   Controls:
C-
C-   Created   6-DEC-1991  C.Gerber,D.Hedin,E.James
C-   Updated  22-JAN-1992  C.G. add histos for number of hits per module
C-   DH 2/92 add t-->d plots
C    DH 3/92 only do t-->d for modules in MUNMOD3
C    DH 3/92 add dumping utilities
C    DH 3/92. use deviation in middle of cell for coarse T0
C    DH 4/92 save run/time, add call to MUFITZ; 5/92 call to PADS
C    DH 8/92 add odd/even deltaT deviations
C    PQ 8/92 use actual wire length for dividing into thirds
C    DH 3/93 allow missing deltaT for drift 3 misses
C    PQ 10/93 change loop over raw hits to accommodate new MUD1 format
C    DH 6/94 redo a-lyer deltaT logic
C    RM 8/94 update for new t0 tuning code
C    DH 9/94 fix error on calling deltaT tuning code
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER MUCALIB,IOR,ORIEN
      INTEGER NSAM,IFW3,ISPARE,IWADD,ID1,ID,MODN,IWR
      INTEGER NRAW,IMUD1,NCEL,LAT,IADC(8)
      INTEGER IMOD,IPLN,IWIR,IERR,IHIT,JHIT,I,J,JTIME,JDATE,K
      INTEGER NMODU,MUNMOD3,MODNUM,MODHITS,MB
      REAL CORT1,CORT2,CORP1,CORP2,CORDT1,ELCAL,ELFE,SPR1,SPR2,
     &     CORDT2,DDIS1,DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR
      INTEGER KK,NTRAKS,ITRAK,NPTRAK,QUAD,IFW1,IFW2
      REAL XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     &     YCOSOM,ZCOSOM,CHSQBV,CHSNBV,RMOM,RMOMER
      INTEGER IHMUOH,ITSIGN,IDELT,IPAD,IFWH1,IFWH2,INRAW,IORIEN,NHWIR
      REAL DEL,XP,YP,ZP,DDIS,VECT,WLEN,VOFF
      INTEGER SI,IORIENT
      PARAMETER (SI=40)
      REAL XHTRAK(SI),YHTRAK(SI),ZHTRAK(SI),XHCWIR(SI),HTDIV1(SI),
     A  XDR(SI),DEL012,CHI4
      REAL DELTA_T0, THTRAK(SI), GTRAK(SI), HIT_ANGLE(SI), DEL123
      REAL COST, T01, TRES1, T02, TRES2, DT01, DTSLP1, DT02, DTSLP2
      REAL DCI(3), DCO(3), XYZGI(3), XYZGO(3), SLBI, SLBO
      REAL SLNBI, SLNBO, XYZLI(3), XYZLO(3)
      INTEGER LAYER, MULAYR
      INTEGER MTRAK(SI),PTRAK(SI),WTRAK(SI),IDT(SI),IS(SI)
      REAL RES1,RES2,CHI,FITPOS,IOE,IOE2,IOE3,IOE4,RES3,RES4
      INTEGER LWIR,JHP1,NMGEO(164)
      INTEGER MUPRINT,STPPRINT,FIRST,DUM,OFFTIM,RUN,LTIME
      CHARACTER*8 STIME
      DATA FIRST/0/
      IF(FIRST.EQ.0) THEN
        FIRST=1
        LTIME=OFFTIM()
        CALL IDATE(I,J,K)
        JDATE=I*10000+J*100+K
        CALL TIME(STIME)
        READ(STIME,'(I2,1X,I2,1X,I2)') I,J,K
        JTIME=I*10000+J*100+K
        RUN=IQ(LHEAD+6)
        CALL MUJUNK(0,RUN,LTIME,JTIME,JDATE,0.,0.,0.,0.)
        CALL EZGET('MUPRINT',MUPRINT,IERR)
        CALL EZGET('STPPRINT',STPPRINT,IERR)
        IF(STPPRINT.GT.0) THEN   ! DUMP OUT STP BANKS
          NMODU = MUNMOD3(0,DUM)         ! number of modules
          DO  I = 1,NMODU               ! loop over modules
            MODNUM = MUNMOD3(1,I)
            NMGEO(I)=MODNUM
            CALL PRMDFT(STPPRINT,0,MODNUM,'CURRENT',0)
            CALL PRMTIM(STPPRINT,0,MODNUM,'CURRENT',0)
            CALL PRMDTM(STPPRINT,0,MODNUM,'CURRENT',0)
            CALL PRMGAN(STPPRINT,0,MODNUM,'CURRENT',0)
          ENDDO
          CALL PRMGEO(STPPRINT,0,NMGEO,'CURRENT',NMODU)
        ENDIF
      ENDIF
CC   DUMP OUT EVENT
      IF(MUPRINT.GT.0) CALL MUPRT(MUPRINT)
C --------------------------------------------------
C --- FILL HISTOGRAMS WITH RAW TIMES
C --------------------------------------------------
C
C - initialize
C
	CALL MUDMOD(0,NRAW,JHIT,IMUD1)
C
C - loop over selected modules
C
	NMODU = MUNMOD3(0,DUM)  
	DO  I = 1,NMODU         
	  MODNUM = MUNMOD3(1,I)
C
C - get number of mud1 hits
C
	  CALL MUDMOD(MODNUM,NRAW,JHIT,IMUD1)
C
C - loop over hits and fill histogram of raw times
C
	  DO J = 1,NRAW
	    IHIT = JHIT
	    CALL MUDHIT(IHIT,JHIT,NCEL,LAT,IADC)
	    CALL HFILL(24000+MODNUM,FLOAT(IADC(1)),0.,1.)	! IADC(1) is TIME1
	  ENDDO
	ENDDO
C --------------------------------------------------
C --- LOOP OVER TRACKS AND HITS ON TRACKS
C --- DO T0 AND DT0 CALCULATIONS AND FILL HISTOGRAMS
C --------------------------------------------------
      CALL GTMTRH(NTRAKS)                  ! see how many tracks
      IF(NTRAKS.GT.0) THEN
        DO ITRAK=1,NTRAKS                  ! loop over tracks
C
          CALL GTMUOT(ITRAK,NPTRAK,NSAM,QUAD,IFW1,IFW2,IFW3,ISPARE,
     &         XI,YI,ZI,XMAGC,YMAGC,
     &         ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,ZCOSOM,
     &         CHSQBV,CHSNBV,RMOM,RMOMER,ELCAL,ELFE,SPR1,SPR2)
C
C-------- Cut on track chi**2
C
          IF (CHSQBV .LE. 0.2) THEN
C
C---------- Get slopes of track
C
            DCI(1) = XCOSIM
            DCI(2) = YCOSIM
            DCI(3) = ZCOSIM
            DCO(1) = XCOSOM
            DCO(2) = YCOSOM
            DCO(3) = ZCOSOM
            XYZGI(1) = XI
            XYZGI(2) = YI
            XYZGI(3) = ZI
            XYZGO(1) = XMAGC
            XYZGO(2) = YMAGC
            XYZGO(3) = ZMAGC
            CALL MUUNRT(QUAD,DCI,DCO,XYZGI,XYZGO,SLBI,SLNBI,SLBO,SLNBO,
     &        XYZLI,XYZLO)
            JHIT=0
            DO IHIT=1,NPTRAK              !loop over hits
C
              CALL GTMHTT(ITRAK,IHIT,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
C
              CALL GTMUOH(IHMUOH,IWADD,IFWH1,IFWH2,INRAW,IORIEN,NHWIR,
     &                  CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,
     &                  DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR)
C
              CALL MUADD(IWADD,IMOD,IPLN,IWIR,IERR)
              LAYER = MULAYR(IMOD)
              ID1=20000+IMOD
              ID=IMOD*100
              MB=IWIR/8
              CALL MUGEOM(IMOD,IPLN,IERR,VECT,WLEN,VOFF,IORIENT)
C
C       Histogram for Tzero adjustment. Use TIME1 only.
C
              IF (MUCALIB.EQ.0.OR.MUCALIB.EQ.1) THEN
                CALL HFILL(ID1+2000,DDIS1,0.,1.)
                CALL HFILL(ID1+3000,CORT1,0.,1.)
                DDIS=DDIS1
                IF(IABS(ITSIGN).EQ.2) DDIS=DDIS2
                IF(IABS(ITSIGN).NE.0) THEN
C
CC      Do deviations in bend view
C
                  ORIEN=IABS(IORIEN)
                  IF (ORIEN.EQ.1) THEN
                    ZP=ZMAGC+(XCWIR-XMAGC)*ZCOSOM/XCOSOM
                    IF(IMOD.LE.99) ZP=ZI+(XCWIR-XI)*ZCOSIM/XCOSIM
                    DEL=ZCWIR+ITSIGN*DDIS/IABS(ITSIGN)-ZP
                  ENDIF
                  IF (ORIEN.EQ.2) THEN
                    ZP=ZMAGC+(YCWIR-YMAGC)*ZCOSOM/YCOSOM
                    IF(IMOD.LE.99) ZP=ZI+(YCWIR-YI)*ZCOSIM/YCOSIM
                    DEL=ZCWIR+ITSIGN*DDIS/IABS(ITSIGN)-ZP
                  ENDIF
                  IF (ORIEN.EQ.3) THEN
                    XP=XMAGC+(ZCWIR-ZMAGC)*XCOSOM/ZCOSOM
                    IF(IMOD.LE.99) XP=XI+(ZCWIR-ZI)*XCOSIM/ZCOSIM
                    DEL=XCWIR+ITSIGN*DDIS/IABS(ITSIGN)-XP
                  ENDIF
                  IF (ORIEN.EQ.4) THEN
                    YP=YMAGC+(ZCWIR-ZMAGC)*YCOSOM/ZCOSOM
                    IF(IMOD.LE.99) YP=YI+(ZCWIR-ZI)*YCOSIM/ZCOSIM
                    DEL=YCWIR+ITSIGN*DDIS/IABS(ITSIGN)-YP
                  ENDIF
C
CC      Check if distance hit-wire is less or equal than 1 cm.
C
                  IF(ABS(ITSIGN).EQ.2.AND.DDIS2.GE..05) THEN
                    KK=DDIS2/.25+1.
                    IF(KK.GE.5.AND.KK.LE.16) THEN
                      CALL HFILL(ID1+1000,ITSIGN*DEL/IABS(ITSIGN),0.,1.)
                    ENDIF
                  ENDIF
                  IF(ABS(ITSIGN).EQ.1.AND.DDIS1.GE..05) THEN
                    KK=DDIS1/.25+1.
                    IF(KK.GE.5.AND.KK.LE.16) THEN
                      CALL HFILL(ID1,ITSIGN*DEL,0.,1.)
                    ENDIF
CCC      do time-->distance histograms
                    IF(MUNMOD3(2,IMOD).GT.0) THEN
                      IF(KK.GE.1.AND.KK.LE.20) THEN
                        CALL HFILL(30000+KK,ITSIGN*DEL,0.,1.)
                        IF(KK.LE.10) THEN
                          CALL HFILL(30000+IMOD*10,ITSIGN*DEL,0.,1.)
                        ELSE
                          CALL HFILL(30001+IMOD*10,ITSIGN*DEL,0.,1.)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
C
CC      Put hit info for dt0 analysis into array variables
CC      if mucalib rcp value equals 0,2
C
              IOR=IORIEN/IABS(IORIEN)
              IF (MUCALIB.EQ.0.OR.MUCALIB.EQ.2) THEN
                CALL HFILL(16000+IMOD,IOR*TDIV1,0.,1.)
              ENDIF
              IF(MUCALIB.EQ.0.OR.MUCALIB.EQ.1.OR.MUCALIB.EQ.2) THEN
                ORIEN=IABS(IORIEN)
                IDELT=IABS(IDELT)
c                IF (IABS(ITSIGN).GE.1.AND.IDELT.EQ.1) THEN
                IF (IABS(ITSIGN).GE.1) THEN
                  JHIT=JHIT+1
                  MTRAK(JHIT)=IMOD
                  PTRAK(JHIT)=IPLN
                  WTRAK(JHIT)=IWIR
                  IS(JHIT)=ITSIGN
                  XDR(JHIT)=DDIS1
                  THTRAK(JHIT) = CORT1
C
C---------------- Get gain for this channel
C
                  CALL MUGTCN(IMOD,IPLN,IWIR,T01,TRES1,T02,TRES2,
     &              DT01,DTSLP1,DT02,DTSLP2)
                  GTRAK(JHIT) = TRES1
                  IF(IABS(ITSIGN).EQ.2) THEN
                    XDR(JHIT)=DDIS2
                    XDR(JHIT)=DDIS2
                    THTRAK(JHIT) = CORT2
                    GTRAK(JHIT) = TRES2
                  ENDIF
C
C---------------  Calculate incident angle for this hit
C
                  IF (LAYER.EQ.1) THEN      ! A LAYER
                    COST=1./SQRT(1.+SLBI**2)
                  ELSE                    ! BC LAYER
                    COST=1./SQRT(1.+SLBO**2)
                  ENDIF
                  HIT_ANGLE(JHIT) = ACOS(COST)
                  IF(XDR(JHIT).LT..1) IS(JHIT)=0
                  HTDIV1(JHIT)=-999999.
                  IF(IDELT.EQ.1) HTDIV1(JHIT)=TDIV1
                  IDT(JHIT)=0
                  IF (ABS(TDIV1).LT.400.) IDT(JHIT)=1
                  IF (ORIEN.EQ.1) THEN
                    XHTRAK(JHIT)=TDIV1+YCWIR
                    YHTRAK(JHIT)=XCWIR
                    ZHTRAK(JHIT)=ZCWIR
                    XHCWIR(JHIT)=YCWIR
                    YP=YMAGC+(XCWIR-XMAGC)*YCOSOM/XCOSOM
                    IF(IMOD.LE.99) YP=YI+(XCWIR-XI)*YCOSIM/XCOSIM
                    DEL=YCWIR+TDIV1-YP
                  ENDIF
                  IF (ORIEN.EQ.2) THEN
                    XHTRAK(JHIT)=TDIV1+XCWIR
                    YHTRAK(JHIT)=YCWIR
                    ZHTRAK(JHIT)=ZCWIR
                    XHCWIR(JHIT)=XCWIR
                    XP=XMAGC+(YCWIR-YMAGC)*XCOSOM/YCOSOM
                    IF(IMOD.LE.99) XP=XI+(YCWIR-YI)*XCOSIM/YCOSIM
                    DEL=XCWIR+TDIV1-XP
                  ENDIF
                  IF (ORIEN.EQ.3) THEN
                    XHTRAK(JHIT)=TDIV1+YCWIR
                    YHTRAK(JHIT)=ZCWIR
                    ZHTRAK(JHIT)=XCWIR
                    XHCWIR(JHIT)=YCWIR
                    YP=YMAGC+(ZCWIR-ZMAGC)*YCOSOM/ZCOSOM
                    IF(IMOD.LE.99) YP=YI+(ZCWIR-ZI)*YCOSIM/ZCOSIM
                    DEL=YCWIR+TDIV1-YP
                  ENDIF
                  IF (ORIEN.EQ.4) THEN
                    XHTRAK(JHIT)=TDIV1+XCWIR
                    YHTRAK(JHIT)=ZCWIR
                    ZHTRAK(JHIT)=YCWIR
                    XHCWIR(JHIT)=XCWIR
                    XP=XMAGC+(ZCWIR-ZMAGC)*XCOSOM/ZCOSOM
                    IF(IMOD.LE.99) XP=XI+(ZCWIR-ZI)*XCOSIM/ZCOSIM
                    DEL=XCWIR+TDIV1-XP
                  ENDIF
CC      Do deviations in nonbend view
CC	See which third of the chamber the track is in
                  IF (ORIEN.EQ.1 .OR. ORIEN.EQ.3) THEN
                    KK = (YP-YCWIR)*IOR/(WLEN/3.) + 2.5
                  ELSE
                    KK = (XP-XCWIR)*IOR/(WLEN/3.) + 2.5
                  ENDIF
                  IWR=MOD(IWIR,2)+1     ! EVEN OR ODD
                  IF(KK.GE.1.AND.KK.LE.3) THEN
                    CALL HFILL(16000+KK*1000+IMOD,IOR*DEL,0.,1.)
                    IF(MUNMOD3(2,IMOD).GT.0) THEN
                      CALL HFILL(16000,IOR*DEL,0.,1.)
                      CALL HFILL(16000+KK*1000,IOR*DEL,0.,1.)
                    ENDIF
                  ENDIF
                  IF(KK.GE.2.AND.KK.LE.3) THEN
                    CALL HFILL(10000+(IWR-1)*2000+
     A                         KK*1000+IMOD,IOR*DEL,0.,1.)
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
C
CC   Call subroutines for dt0 analysis
C
            IF(MUCALIB.EQ.0.OR.MUCALIB.EQ.2) THEN
              NMODU=MUNMOD3(0,0)
              DO I=1,NMODU
                MODN=MUNMOD3(1,I)
                MODHITS=0
                DO J=1,JHIT
                  IF (MTRAK(J).EQ.MODN) MODHITS=MODHITS+1
                ENDDO
                IF (MODHITS.GE.3) THEN
                  CALL MUFITX_CALIB(JHIT,MODN,MTRAK,PTRAK,WTRAK,
     X   IDT,XHTRAK,YHTRAK,ZHTRAK,RES1,RES2,RES3,RES4,CHI,LWIR,
     X              IOE,IOE2,IOE3,IOE4,JHP1,FITPOS)

         IF(JHP1.GT.0) CALL NEWDT0(MTRAK(JHP1),PTRAK(JHP1),WTRAK(JHP1),
     X              XHCWIR(JHP1),HTDIV1(JHP1),MODN,RES1,RES2,RES3,
     X              RES4,IOE,IOE2,IOE3,IOE4,FITPOS)
                ENDIF
              ENDDO
            ENDIF
CC   DO T0 ANALYSIS
            IF(MUCALIB.EQ.0.OR.MUCALIB.EQ.1) THEN
              NMODU=MUNMOD3(0,0)
              DO I=1,NMODU
                MODN=MUNMOD3(1,I)
                MODHITS=0
                DO J=1,JHIT
                  IF (MTRAK(J).EQ.MODN) MODHITS=MODHITS+1
                ENDDO
                IF (MODHITS.GE.3) THEN
                  CALL MUFITZ(JHIT,MODN,MTRAK,PTRAK,WTRAK,
     &              IS,XHTRAK,YHTRAK,ZHTRAK,THTRAK,HIT_ANGLE,
     &              GTRAK,XDR,DEL012,DEL123,CHI4,DELTA_T0,LWIR)
                  IF(LWIR.GE.0) THEN
                    IF (DEL012 .NE. -999999.)
     &                CALL HFILL(25000+MODN,DEL012,0.,1.)
                    IF (DELTA_T0 .NE. 999999.)
     &                CALL HFILL(26000+MODN,DELTA_T0,0.,1.)
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDIF
CCC    DO PAD ANALYSIS
      IF(MUCALIB.EQ.0.OR.MUCALIB.EQ.3) CALL PADS
C
C 999 RETURN
      END
