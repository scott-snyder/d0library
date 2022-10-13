      SUBROUTINE PADS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods: To set up for calculation of pad-wire
C-                        resolutions using sigma 3-miss method.
C-                        (See subroutine PADSUMRY).
C-                        Requires tracks having one hit per plane 
C-                        for each module that the track passes through.
C-                        
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   4-FEB-1992   c.r.murphy
C-   Modified 28-Oct-1992   P.Z.Quintas. change call to MUGGAN
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
C  Local Declarations:
C  ====================
C
      INTEGER NSAM,IFW3,ISPARE,IWADD,ID1,ID,MODN
      INTEGER IMOD,IPLN,IWIR,IERR,IHIT,JHIT,I,J,K,L,M
      INTEGER IORENT
      INTEGER NMODU,MUNMOD,MODNUM,MODHITS
      REAL WLEN,VOFF,VECT(3),DX,CWIR
      REAL CORT1,CORT2,CORP1,CORP2,CORDT1,ELCAL,ELFE,SPR1,SPR2,
     &     CORDT2,DDIS1,DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR
C
      INTEGER NTRAKS,ITRAK,NPTRAK,QUAD,IFW1,IFW2
      REAL XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     &     YCOSOM,ZCOSOM,CHSQBV,CHSNBV,RMOM,RMOMER
C
      INTEGER IHMUOH,ITSIGN,IDELT,IPAD,IFWH1,IFWH2,INRAW,IORIEN,NHWIR
      INTEGER LMUOH,GZMUOH,LMGEO,GZMGEO,EMOD,MODULE(10),NUMPLNS
      INTEGER MULAYR,NHIT,NMT,NPT(10),NVER
      INTEGER LMUD1,GZMUD1,NMUD1,NADD1,I16,IAND,PAD
      LOGICAL OK_EMOD(10),PFLAG
      INTEGER NPLN(10,4),P,WIREADD(10,4)
      REAL X(10,4),Y(10,4),TDIV(10,4),DELT(10,4)
      REAL QQ(10,4),Q1(10,4),Q2(10,4),PADNUM(10,4)
      REAL WAVELEN,PADREP,PLNCOORD,WIRECOORD
      REAL SX,SY,SXY,SX2,SLOPE(10),INTRCPT(10)
      REAL PRB,PROB,SIG,CHI2,S,SIGMA2
      REAL XT,YT,DIFF
      REAL X2T,Y2T,MISS3,SLOPE3,INTRCPT3
      REAL THETA,PHI,ACOS,ATAN2
      REAL RPAD_BE,RPAD_AE,RPAD_BO,RPAD_AO,PED(8)
      REAL GAIN1,GAIN2,GAIN3,GAIN4,PED1,PED2,PED3,PED4
      INTEGER MUNMOD3,DUM,NUMMODS,HID
      LOGICAL PROCEED,WRITE_EVENT_CRP
      DATA PADREP/60.96/
      DATA I16/o'177777'/
C
C ************************BEGIN PAD ANALYSIS*********************
C
      CALL GTMTRH(NTRAKS)                  ! see how many tracks
      IF(NTRAKS.LE.0) GO TO 999
      DO ITRAK=1,NTRAKS                  ! loop over tracks
C
        CALL GTMUOT(ITRAK,NPTRAK,NSAM,QUAD,IFW1,IFW2,IFW3,ISPARE,
     &         XI,YI,ZI,XMAGC,YMAGC,
     &         ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,ZCOSOM,
     &         CHSQBV,CHSNBV,RMOM,RMOMER,ELCAL,ELFE,SPR1,SPR2)
C
CC      Make some track cuts
C
        IF(ABS(RMOM).LT.3.) GO TO 30
C
        NUMMODS=MUNMOD3(0,DUM)
        EMOD=-1            !current module # along track
        NMT=0              ! # of different mods hit along track
        CALL VZERO_i(NPLN,40)
        CALL VZERO_i(WIREADD,40)
        CALL VZERO(X,40)
        CALL VZERO(Y,40)
        CALL VZERO(Q1,40)
        CALL VZERO(Q2,40)
        CALL VZERO(QQ,40)
        CALL VZERO(TDIV,40)
        CALL VZERO(DELT,40)
        CALL VZERO(PADNUM,40)
        CALL VZERO_i(NPT,10)
        CALL VZERO_i(MODULE,10)
        CALL VZERO(SLOPE,10)
        CALL VZERO(INTRCPT,10)
        DO I=1,10
          OK_EMOD(I)=.FALSE.   !set true if each plane in mod has
        ENDDO                  !one hit
C
        DO IHIT=1,NPTRAK              !loop over hits
C
          PROCEED=.FALSE.
          CALL GTMHTT(ITRAK,IHIT,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
C
          CALL GTMUOH(IHMUOH,IWADD,IFWH1,IFWH2,INRAW,IORIEN,NHWIR,
     &                  CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,
     &                  DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR)
C
          CALL MUADD(IWADD,IMOD,IPLN,IWIR,IERR)
          DO M=1,NUMMODS
            IF (MUNMOD3(1,M).EQ.IMOD) PROCEED=.TRUE.
          ENDDO
C
          IF (.NOT.PROCEED) GO TO 10
          CALL MUGEOM(IMOD,IPLN,IWIR,VECT,WLEN,VOFF,IORENT)
C
          IORIEN=IABS(IORIEN)
          IDELT=IABS(IDELT)
          LMUD1=GZMUD1()
          LMUOH=GZMUOH()
          NHIT=LMUOH+28*(IHMUOH-1)
          IF (IMOD.NE.EMOD) THEN
            NMT=NMT+1             ! count mods along track
            NPT(NMT)=0            ! # of hits for this mod
            EMOD=IMOD             ! current module along track
            MODULE(NMT)=EMOD      ! store for later histograms
            LMGEO=GZMGEO(EMOD)
            NUMPLNS=IC(LMGEO+10)  ! # of planes for this module
            P=1
            NPLN(NMT,P)=IPLN+1
            PFLAG=.FALSE.
            IF (IERR.EQ.0.AND.IDELT.EQ.1.AND.IPAD.NE.0) THEN
              IF (IPAD.LT.0) NVER=1
              IF (IPAD.GT.0) NVER=2
              WAVELEN=PADREP*(IABS(IPAD)-1)
              IF (IORIEN.EQ.1) THEN
                IF (MULAYR(EMOD).EQ.1.AND.ABS(XCOSIM).LT..25) GO TO 10
                IF (MULAYR(EMOD).NE.1.AND.ABS(XCOSOM).LT..25) GO TO 10
                PLNCOORD=XCWIR
                WIRECOORD=YCWIR+Q(NHIT+18+NVER)+WAVELEN
                CWIR=YCWIR
              ELSE IF (IORIEN.EQ.2) THEN
                IF (MULAYR(EMOD).EQ.1.AND.ABS(YCOSIM).LT..25) GO TO 10
                IF (MULAYR(EMOD).NE.1.AND.ABS(YCOSOM).LT..25) GO TO 10
                PLNCOORD=YCWIR
                WIRECOORD=XCWIR+Q(NHIT+18+NVER)+WAVELEN
                CWIR=XCWIR
              ELSE IF (IORIEN.EQ.3) THEN
                IF (MULAYR(EMOD).EQ.1.AND.ABS(ZCOSIM).LT..25) GO TO 10
                IF (MULAYR(EMOD).NE.1.AND.ABS(ZCOSOM).LT..25) GO TO 10
                PLNCOORD=ZCWIR
                WIRECOORD=YCWIR+Q(NHIT+18+NVER)+WAVELEN
                CWIR=YCWIR
              ELSE IF (IORIEN.EQ.4) THEN
                IF (MULAYR(EMOD).EQ.1.AND.ABS(ZCOSIM).LT..25) GO TO 10
                IF (MULAYR(EMOD).NE.1.AND.ABS(ZCOSOM).LT..25) GO TO 10
                PLNCOORD=ZCWIR
                WIRECOORD=XCWIR+Q(NHIT+18+NVER)+WAVELEN
                CWIR=XCWIR
              ENDIF
              NPT(NMT)=NPT(NMT)+1
              X(NMT,NPT(NMT))=PLNCOORD
              Y(NMT,NPT(NMT))=WIRECOORD
              Q1(NMT,NPT(NMT))=CORP1
              Q2(NMT,NPT(NMT))=CORP2
              QQ(NMT,NPT(NMT))=(CORP1-CORP2)/(CORP1+CORP2)
              TDIV(NMT,NPT(NMT))=Q(NHIT+16+IDELT)
              DELT(NMT,NPT(NMT))=Q(NHIT+12+IDELT)
              PADNUM(NMT,NPT(NMT))=IPAD
              WIREADD(NMT,NPT(NMT))=IWADD
              NMUD1=IQ(NHIT+4)
              NADD1=LMUD1+NMUD1
              PAD=IQ(NADD1+4)
              RPAD_BE=FLOAT(IAND(I16,PAD))
              PAD=IQ(NADD1+5)
              RPAD_AE=FLOAT(IAND(I16,PAD))
              PAD=IQ(NADD1+8)
              RPAD_BO=FLOAT(IAND(I16,PAD))
              PAD=IQ(NADD1+9)
              RPAD_AO=FLOAT(IAND(I16,PAD))
C-OLD              CALL MUGPED(IMOD,IPLN,IWIR,PED)
C-OLD              CALL MUGGAN(IMOD,IPLN,IWIR,GAIN1,GAIN2)
	      CALL MUGGAN(IMOD,IPLN,IWIR,GAIN1,GAIN2,GAIN3,GAIN4,
     x                                   PED1,PED2,PED3,PED4)
              ID=41000+EMOD
C-OLD              CALL HFILL(ID,PED(1),0.,1.)
C-OLD              CALL HFILL(ID,PED(2),0.,1.)
              CALL HFILL(ID,PED1,0.,1.)
              CALL HFILL(ID,PED2,0.,1.)
              ID=42000+EMOD
C-OLD              CALL HFILL(ID,PED(5),0.,1.)
C-OLD              CALL HFILL(ID,PED(6),0.,1.)
	      CALL HFILL(ID,PED3,0.,1.)
	      CALL HFILL(ID,PED4,0.,1.)
              ID=43000+EMOD
              CALL HFILL(ID,GAIN1,0.,1.)
              ID=44000+EMOD
              CALL HFILL(ID,GAIN2,0.,1.)
              ID=45000+EMOD
              CALL HFILL(ID,RPAD_AE+RPAD_BE,0.,1.)
              ID=46000+EMOD
              CALL HFILL(ID,RPAD_AO+RPAD_BO,0.,1.)
              ID=47000+EMOD
              CALL HFILL(ID,QQ(NMT,NPT(NMT)),0.,1.)
              CALL GETDX(WIRECOORD,CWIR,WAVELEN,WLEN,VOFF,
     &                                     IORENT,NVER,DX)
              ID=48000+EMOD
              CALL HFILL(ID,ABS(DX),0.,1.)
              ID=49000+EMOD
              CALL HFILL(ID,WIRECOORD-CWIR,0.,1.)
            ENDIF
          ELSE
            P=P+1
            IF (P.GT.NUMPLNS) GO TO 10
            NPLN(NMT,P)=IPLN+1
            DO I=1,P-1
              IF (NPLN(NMT,P).EQ.NPLN(NMT,I)) PFLAG=.TRUE.
            ENDDO
            IF (PFLAG) GO TO 10
            IF (OK_EMOD(NMT)) GO TO 10
            IF (IERR.EQ.0.AND.IDELT.EQ.1.AND.IPAD.NE.0) THEN
              IF (IPAD.LT.0) NVER=1
              IF (IPAD.GT.0) NVER=2
              WAVELEN=PADREP*(IABS(IPAD)-1)
              IF (IORIEN.EQ.1) THEN
                IF (MULAYR(EMOD).EQ.1.AND.ABS(XCOSIM).LT..25) GO TO 10
                IF (MULAYR(EMOD).NE.1.AND.ABS(XCOSOM).LT..25) GO TO 10
                PLNCOORD=XCWIR
                WIRECOORD=YCWIR+Q(NHIT+18+NVER)+WAVELEN
                CWIR=YCWIR
              ELSE IF (IORIEN.EQ.2) THEN
                IF (MULAYR(EMOD).EQ.1.AND.ABS(YCOSIM).LT..25) GO TO 10
                IF (MULAYR(EMOD).NE.1.AND.ABS(YCOSOM).LT..25) GO TO 10
                PLNCOORD=YCWIR
                WIRECOORD=XCWIR+Q(NHIT+18+NVER)+WAVELEN
                CWIR=XCWIR
              ELSE IF (IORIEN.EQ.3) THEN
                IF (MULAYR(EMOD).EQ.1.AND.ABS(ZCOSIM).LT..25) GO TO 10
                IF (MULAYR(EMOD).NE.1.AND.ABS(ZCOSOM).LT..25) GO TO 10
                PLNCOORD=ZCWIR
                WIRECOORD=YCWIR+Q(NHIT+18+NVER)+WAVELEN
                CWIR=YCWIR
              ELSE IF (IORIEN.EQ.4) THEN
                IF (MULAYR(EMOD).EQ.1.AND.ABS(ZCOSIM).LT..25) GO TO 10
                IF (MULAYR(EMOD).NE.1.AND.ABS(ZCOSOM).LT..25) GO TO 10
                PLNCOORD=ZCWIR
                WIRECOORD=XCWIR+Q(NHIT+18+NVER)+WAVELEN
                CWIR=XCWIR
              ENDIF
              NPT(NMT)=NPT(NMT)+1
              X(NMT,NPT(NMT))=PLNCOORD
              Y(NMT,NPT(NMT))=WIRECOORD
              Q1(NMT,NPT(NMT))=CORP1
              Q2(NMT,NPT(NMT))=CORP2
              QQ(NMT,NPT(NMT))=(CORP1-CORP2)/(CORP1+CORP2)
              TDIV(NMT,NPT(NMT))=Q(NHIT+16+IDELT)
              DELT(NMT,NPT(NMT))=Q(NHIT+12+IDELT)
              PADNUM(NMT,NPT(NMT))=IPAD
              WIREADD(NMT,NPT(NMT))=IWADD
              NMUD1=IQ(NHIT+4)
              NADD1=LMUD1+NMUD1
              PAD=IQ(NADD1+4)
              RPAD_BE=FLOAT(IAND(I16,PAD))
              PAD=IQ(NADD1+5)
              RPAD_AE=FLOAT(IAND(I16,PAD))
              PAD=IQ(NADD1+8)
              RPAD_BO=FLOAT(IAND(I16,PAD))
              PAD=IQ(NADD1+9)
              RPAD_AO=FLOAT(IAND(I16,PAD))
C-OLD              CALL MUGPED(IMOD,IPLN,IWIR,PED)
C-OLD              CALL MUGGAN(IMOD,IPLN,IWIR,GAIN1,GAIN2)
	      CALL MUGGAN(IMOD,IPLN,IWIR,GAIN1,GAIN2,GAIN3,GAIN4,
     x                                   PED1,PED2,PED3,PED4)
              ID=41000+EMOD
C-OLD              CALL HFILL(ID,PED(1),0.,1.)
C-OLD              CALL HFILL(ID,PED(2),0.,1.)
              CALL HFILL(ID,PED1,0.,1.)
              CALL HFILL(ID,PED2,0.,1.)
              ID=42000+EMOD
C-OLD              CALL HFILL(ID,PED(5),0.,1.)
C-OLD              CALL HFILL(ID,PED(6),0.,1.)
	      CALL HFILL(ID,PED3,0.,1.)
	      CALL HFILL(ID,PED4,0.,1.)
              ID=43000+EMOD
              CALL HFILL(ID,GAIN1,0.,1.)
              ID=44000+EMOD
              CALL HFILL(ID,GAIN2,0.,1.)
              ID=45000+EMOD
              CALL HFILL(ID,RPAD_AE+RPAD_BE,0.,1.)
              ID=46000+EMOD
              CALL HFILL(ID,RPAD_AO+RPAD_BO,0.,1.)
              ID=47000+EMOD
              CALL HFILL(ID,QQ(NMT,NPT(NMT)),0.,1.)
              CALL GETDX(WIRECOORD,CWIR,WAVELEN,WLEN,VOFF,
     &                                     IORENT,NVER,DX)
              ID=48000+EMOD
              CALL HFILL(ID,ABS(DX),0.,1.)
              ID=49000+EMOD
              CALL HFILL(ID,WIRECOORD-CWIR,0.,1.)
              IF(.NOT.PFLAG.AND.(NPT(NMT).EQ.NUMPLNS))
     +          OK_EMOD(NMT)=.TRUE.
            ENDIF
          ENDIF
   10     CONTINUE
        ENDDO                                !end loop over hits
        
C
C  ***************NOW DO SIGMA 3-MISS FOR THE CHAMBERS**************
C
        IF (NMT.EQ.0) GO TO 30
        DO I=1,NMT
          IF (.NOT.OK_EMOD(I)) GO TO 25
          IF ((X(I,3)-X(I,1)).EQ.0.) GO TO 25
          SLOPE3=(Y(I,3)-Y(I,1))/(X(I,3)-X(I,1))
          INTRCPT3=(Y(I,1)*X(I,3)-Y(I,3)*X(I,1))/(X(I,3)-X(I,1))
          X2T=X(I,2)
          Y2T=SLOPE3*X2T+INTRCPT3
          MISS3=Y(I,2)-Y2T
          ID=40000+MODULE(I)
          CALL HFILL(ID,MISS3,0.,1.)
   25     CONTINUE
        ENDDO
C
C
C
   30   CONTINUE
      ENDDO                         !end loop over tracks
C
  999 RETURN
      END
