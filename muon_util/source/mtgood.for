      SUBROUTINE MTGOOD(IQQ,P,XI,YI,ZI,XMAGC,YMAGC,ZMAGC,
     A  XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,ZCOSOM,CHSQBV,
     A  CHSNBV,CHSQBC,CHSQDT,IFW1,IFW2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCSEE IF A GIVEN MUON TRACK IS GOOD. INPUTS ARE THE
CC  QUADRANT, MOMENTMUM,X-Y-Z LOCATIONS AND DIRECTION COSINES,
CC  AND THE 4 QUALITY OF FIT VALUES
CC  OUTPUT: IFW2 = 0  THEN GOOD
CC                 BIT 1 SET    BAD BEND FIT
CC                 BIT 2 SET    BAD NONBEND FIT
CC                 BIT 3 SET    BAD BEND VERTEX
CC                 BIT 4 SET    BAD NONBEND VERTEX
CC                 BIT 5,6 SET    MISSING MODULE ON A TRACK
CC   D. Hedin 9/90
CC   DH 8/91 fix bug in defining bend residual-->change cut in this routine
CC   DH 10/91 add some momentum and angular dependence;use 'best' quad
CC   DH 2/92 slightly different for level 2
CC   DH 5/92 tighten vertex pointing slightly; add different if pad
CC           in fits. all 'quality of fit' cuts will need tuning
CC   TD 8/20/92 Compare VERXYZ(3) with mu projection.
CC   DH 8/21/92 USE IFW1 FOR 2/3 MODULE TRACKS
CC   DH 9/92 tune nonbend cuts, remove p-dependence for now
CC   DH 12/92 turn back on p-dependence. tighter if pad fits
CC   DH 1/93 make end/central different. tighten nonbend
CC   DH 3/93 minor bug in end nonbend, redo low momentum
CC   DH 4/93 IFW1=10 done correctly
CC   DH 1/95 use deltaT and BC in end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT NONE
C
      INTEGER IQ,IFW1,IFW2,IQQ,FIRST,MUVERT,IER,IPDFIT
      REAL P,XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     A  YCOSOM,ZCOSOM,CHSQBV,CHSNBV,XV,ZV,CBND,CNBND,DELXVC,DELZVC,
     A  DELXVE,DELZVE,DELX,CPNBND,FACTOR,FACTOR2,DELZ,PP
      REAL CHSQBC,CHSQDT,VERTEX(3)                    !VERTEX
      INTEGER IVER,NV 
      DATA FIRST/0/
      DATA CBND,CNBND,CPNBND/1.1,100.,7./ ! RESIDUAL CUTS BEND,NONBEND,PADNB
      DATA DELZVC,DELZVE/80.,80./! PROJ. TO VERTEX BEND (CENT+END)
      DATA DELXVC,DELXVE/75.,100./! PROJ. TO VERTEX NONBEND (CENT+END)P=7
      IF(FIRST.EQ.0) THEN
        FIRST=1
        CALL EZGET('MUVERT',MUVERT,IER)
        CALL EZGET('IPDFIT',IPDFIT,IER)
      ENDIF
      FACTOR=1.
      IF(IFW1.NE.0.AND.IFW1.NE.10) FACTOR=.7 ! TIGHTER FOR 2-MOD:FIT QUALITY
      FACTOR2=1.
      IF(IFW1.NE.0.AND.IFW1.NE.10) FACTOR2=.85 ! TIGHTER FOR 2-MOD:VTX PROJ
      IFW2=0
      IQ=MOD(IQQ,100)
CCC  BEND VIEW QUALITY OF FIT: may want to add some p-dependence
      IF(CHSQBV.LT.500..AND.CHSNBV.LT.700.) THEN
        IF(IQ.LE.4) THEN
          IF(CHSQBV.LT.0..OR.CHSQBV.GT.FACTOR*CBND) IFW2=IFW2+1
        ELSE
          IF(IFW1.EQ.0.OR.IFW1.EQ.10) THEN
            IF(CHSQBV.LT.0..OR.CHSQBV.GT.FACTOR*CBND.OR.
     A      CHSQBC.LT.0..OR.CHSQBC.GT.FACTOR*CBND*1.2) IFW2=IFW2+1
          ELSE
            IF(CHSQBV.LT.0..OR.CHSQBV.GT.FACTOR*CBND.OR.
     A      CHSQBC.GT.FACTOR*CBND*1.2) IFW2=IFW2+1
          ENDIF
        ENDIF
CCC  NON-BEND VIEW QUALITY OF FIT
        IF(IPDFIT.EQ.1) THEN     ! no pads in fit
          IF(CHSNBV.LT.0..OR.CHSNBV.GT.CNBND) IFW2=IFW2+2
        ELSE
          IF(IQ.LE.4) THEN
            IF(CHSNBV.LT.0..OR.CHSNBV.GT.FACTOR*CPNBND) IFW2=IFW2+2
          ELSE
           IF(CHSNBV.LT.0..OR.CHSNBV.GT.FACTOR*CPNBND.OR.
     A     CHSQDT.LT.0..OR.CHSQDT.GT.FACTOR*CNBND) IFW2=IFW2+2
          ENDIF
        ENDIF
C
CC  PROJECT TO POINT (0,0,VERTEX(3)) ZV IS BEND, XV IS NONBEND
CC  
        CALL VERXYZ(IVER,VERTEX,NV)
        IF(IQ.EQ.1.OR.IQ.EQ.3) THEN
          XV=YMAGC-XMAGC*YCOSOM/XCOSOM
          ZV=ZMAGC-XMAGC*ZCOSIM/XCOSIM-VERTEX(3)
        ELSE IF(IQ.EQ.2.OR.IQ.EQ.4) THEN
          XV=XMAGC-YMAGC*XCOSOM/YCOSOM
          ZV=ZMAGC-YMAGC*ZCOSIM/YCOSIM-VERTEX(3)
        ELSE IF(IQ.EQ.5.OR.IQ.EQ.7.OR.IQ.EQ.9.OR.IQ.EQ.11) THEN
          XV=YMAGC+(VERTEX(3)-ZMAGC)*YCOSOM/ZCOSOM
          ZV=XMAGC+(VERTEX(3)-ZMAGC)*XCOSIM/ZCOSIM
        ELSE
          XV=XMAGC+(VERTEX(3)-ZMAGC)*XCOSOM/ZCOSOM
          ZV=YMAGC+(VERTEX(3)-ZMAGC)*YCOSIM/ZCOSIM
        ENDIF
        IF(MUVERT.EQ.1) THEN
          XV=0.
          ZV=0.                       ! MUVERT=1 Random Cosmics
        ENDIF  
        PP=ABS(P)
        IF(PP.LE.3.1) PP=3.1
        IF(IQ.LE.4) THEN                   ! CENTRAL
          IF(PP.LE.20.) THEN
            DELZ=200./PP + DELZVC-10.
          ELSE
            DELZ=DELZVC
          ENDIF
          IF(PP.LE.42.) THEN
            DELX=400./(PP-2.)-10.+DELXVC/3.
          ELSE
            DELX=DELXVC/3.
          ENDIF
C   no p dependence unless PAD fits are used
          IF(IPDFIT.EQ.1) DELX=DELXVE
          IF(ABS(ZV).GT.DELZ) IFW2=IFW2+4
          IF(ABS(XV).GT.FACTOR2*DELX) IFW2=IFW2+8
        ELSE                               ! ENDS
          IF(PP.LE.43.) THEN
            DELX=600./(PP-3.)-15. +DELXVE/3.
          ELSE
            DELX=DELXVE/3.
          ENDIF
C   no p dependence unless PAD fits are used; L2 AND OFFLINE THE SAME
          IF(IPDFIT.EQ.1) DELX=DELXVE
          IF(PP.LE.20.) THEN
            DELZ=200./PP + DELZVE-10.
          ELSE
            DELZ=DELZVE
          ENDIF
          IF(ABS(ZV).GT.DELZ) IFW2=IFW2+4
          IF(ABS(XV).GT.FACTOR2*DELX) IFW2=IFW2+8
        ENDIF
      ELSE
        IFW2=15
      ENDIF
      RETURN
      END
