      SUBROUTINE PSAM_TK(IVIEW,IFLAG)
C========================================================================
C
C  Description:
C  ============
C  Displays an track from the SAMUS detector using information
C  from MUOT ZEBRA bank if SAMONLY=.TRUE. (otherwise track displayed by MUODIS)
C  =======
C  IVIEW - tells the default view  (HORIZONTAL-VERTICAL)
C         1  Z Y         7  Z -Y
C         2  X Y         8  X -Y
C         3  X Z         9  X -Z
C         4  Y Z        10  Y -Z
C         5  Y X        11  Y -X
C         6  Z X        12  Z -X
C-
C- IFLAG - <0 Draw only tracks in -Z region
C-         >0 Draw only tracks in +Z region.
C-
C
C  Revision History:
C  =================
C       7-29-91  C.Y.  Put in CUT views for sideviews.
C                      Eliminated use of QUAD in MUOT for geometric sectors-
C                      now use hit in magnet center.
C       5-NOV-1991   Lupe Howell  Goto end statement changed to include
C                    call to ezrset
C========================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C    Local Declarations:
C    ===================
      INTEGER QUAD,XQUAD,LMUHT,GZMUHT,NSAMUS,IFW3,ISPARE
      INTEGER I,J,K,L,M                     ! Loop variables
      INTEGER NTRACK                        ! No. tracks
      INTEGER TRKNUM                        ! TRACK LABEL NUMBER
      INTEGER NPTRK                         ! No. of points on track
      INTEGER NHCTR                         ! No. of hit trigger counters
      INTEGER IVIEW,MODK,NTEMP,IPHITS
      INTEGER IFLAG
      INTEGER IFW1,XIFW1,IFW2,XIFW2,DRAWHITS,DRAWTRK,IER
      INTEGER DRAWCELLS,ICUT,NPLOT
      INTEGER LMUON,GZMUON,LPMUO,GZPMUO
      REAL XTRKI(2),YTRKI(2),ZTRKI(2),NM,MR            ! INNER TRACK
      REAL XTRKO(2),YTRKO(2),ZTRKO(2)                  ! OUTTER TRACK
      REAL XMAGC,YMAGC,ZMAGC,ELCAL,ELFE,SPARE1
      REAL XCOSOM,YCOSOM,ZCOSOM,SPARE2
      REAL XCOSIM,YCOSIM,ZCOSIM
      REAL CHSQBV,CHSNBV,MOM,MOMER,SIGN
      REAL RMAX,RMIN,YMAX,ZMAX,ZMIN,RCAL
      REAL RHO,THETA,ZEE
      REAL XI,YI,ZI,PX,PY,PZ,PT,PTMIN,DPT,XCHARSIZE,YCHARSIZE
      REAL XXI,XYI,XZI
      REAL XHTRAK(40),YHTRAK(40),ZHTRAK(40),IHWADD(40)
      REAL DISTAMC,XNUMDIR,YNUMDIR,PHINUM,RNUM,XNUM,YNUM
      REAL ORIGIN,XO,YO,ZO,PHITK
      REAL VPORTXMIN,VPORTXMAX,VPORTYMIN,VPORTYMAX
      CHARACTER*3 COLORS(3)
      CHARACTER*15 LABELS(3),LABMUON
      CHARACTER*3 ICOL(6),JCOL,CNUM,CTRKNUM,MC
      CHARACTER*30 STR1,IBLANK
      REAL VWSAVE(85)
      LOGICAL ALYR,EZERROR,SAMONLY
C
      DATA YMAX,ZMAX,ZMIN/62.,965.2,400./
      DATA RCAL/230./
      DATA XCHARSIZE,YCHARSIZE/10,10/
      DATA RNUM/245/
      DATA ICUT/1/
      DATA COLORS /'GRE','BLU','CYA'/
      DATA LABELS /' WAMUS TK',' SAMUS TK',' WA+SA TK'/
      DATA LABMUON/' MUON TK'/
      DATA IBLANK/'                              '/
C
C    Include Statements:
C    ===================
      REAL PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC'
C
C
C    Executable Code:
C    ================
      NTRACK=0
      NPLOT=0
C
C
C
C ****  Picking the right RCP bank
C
      CALL EZPICK('PX_SAMDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PSAM_TK',
     &         'Bank PX_SAMDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PUGET_l('SAMUS ONLY',SAMONLY)
      IF(.NOT.SAMONLY)GO TO 900
C    Get number of tracks from MTRH (muon track header bank)
C    =======================================================
      CALL GTMTRH(NTRACK)
      IF(NTRACK.LE.0)GO TO 900
C
C    IF DON'T DESIRE TRACK, SKIP DRAWING TRACK
C    =========================================
      CALL PUGET_i ('SAMUS DRAW TRACKS',DRAWTRK)
      IF (DRAWTRK.EQ.0) GOTO 900   ! 1=DRAW EC, 2=DRAW ALL/ 0=DON'T DRAW
C
      CALL PUOPEN
C    Loop over  tracks and draw them
C    ================================================
      DO 45 K = 1,NTRACK
C
C    GET MUON INFORMATION FROM MUOT
C    ==============================
        CALL GTMUOT(K,NPTRK,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,
     X    XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     X    YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,SPARE1,
     X    SPARE2)
C-
C--- Reset IFW1 flag
C-
        IFW1 = MOD(IFW1,10)
C-
C
C For WAMUS TRACKS CHECK IF TRACK IS GOOD QUALITY
C
        IF (NSAMUS.EQ.0 .AND. IFW1.NE.5) THEN
C For WAMUS TRACKS, check if anglea right for this IVIEW
C
          PHITK=ATAN2(YCOSOM,XCOSOM)
          IF(IVIEW.EQ.1.AND. .NOT.((1./4.)*PI.LE.PHITK.AND.
     &      PHITK.LE.(3./4.)*PI
     &      .OR. (-3./4.)*PI.LT.PHITK.AND.PHITK.LT.(-1./4.)*PI ))GO TO
     &      45
          IF(IVIEW.EQ.3.AND. .NOT.((-1./4.)*PI.LE.PHITK.AND.
     &      PHITK.LE.(1./4.)*PI
     &      .OR.-1*PI.LT.PHITK.AND.PHITK.LT.(-3./4.)*PI
     &      .OR.(3./4.)*PI.LT.PHITK.AND.PHITK.LT.PI))GO TO 45
        ENDIF
C check if track Z matches FLAG
        IF(ZI.GT.0..AND.IFLAG.LT.0)GO TO 45
        IF(ZI.LT.0..AND.IFLAG.GT.0)GO TO 45
C IF ZI=0, check sign of ZMAGC
        IF(ZI.EQ.0)THEN
          IF(ZMAGC.EQ.0.)GO TO 45
          IF(ZMAGC.GT.0..AND.IFLAG.LT.0)GO TO 45
          IF(ZMAGC.LT.0..AND.IFLAG.GT.0)GO TO 45
        ENDIF
C Draw WAMUS only IF THETA.LT. 45 DEG
        IF (NSAMUS.EQ.0 .AND. IFW1.NE.5) THEN
          RHO=SQRT(XCOSOM**2+YCOSOM**2)
          ZEE=ABS(ZCOSOM)
          THETA=ATAN2(ZEE,RHO)
          IF(THETA.LT.(1./4.)*PI)GO TO 45
        ENDIF
C
C     SET COLOR OF TRACKS
C DARK BLUE IF SAMUS HITS ONLY
C CYANNE IF HAS BOTH SAMUS AND WAMUS HITS
C GREEN IF WAMUS ONLY
C     ===================================
        SIGN=MOM/ABS(MOM)
        MOM=ABS(MOM)
        IF (IFW1.EQ.1.OR.IFW1.EQ.4) THEN    ! NO A-LAYER HIT
          PX=MOM*XCOSOM
          PY=MOM*YCOSOM
          PZ=MOM*ZCOSOM
        ELSE                                  ! HAVE A-LAYER HIT
          PX=MOM*XCOSIM
          PY=MOM*YCOSIM
          PZ=MOM*ZCOSIM
        ENDIF
        PT=SQRT(PX**2+PY**2)
        CALL PUGETV('TRACK PTMIN',PTMIN)
        CALL PUGETV('TRACK DPT',DPT)
        IF (PT.LT.PTMIN) GOTO 45
        IF(NPTRK.GT.0.AND.NSAMUS.EQ.0)THEN
          JCOL='GRE'
        ELSE IF(NPTRK.EQ.0.AND.NSAMUS.GT.0)THEN
          JCOL='BLU'
        ELSE
          JCOL='CYA'
        ENDIF
C for End Cap, draw all tracks green
        IF(DRAWTRK.EQ.1)THEN
          JCOL='GRE'
        ENDIF
        CALL PXCOLR(JCOL)
C
C       POINT TRKO(1) IS AT THE MAGNET CENTER
C       =====================================
        IF (IFW1 .NE. 5) THEN
          XTRKO(1)=XMAGC
          YTRKO(1)=YMAGC
          ZTRKO(1)=ZMAGC
C For End Cap Plots, draw muon track from MAGNET to outside of CAL
          IF(DRAWTRK.EQ.1.OR.IFW1.EQ.1.OR.IFW1.EQ.4)THEN
            RHO=SQRT(XCOSOM**2+YCOSOM**2)
            ZEE=RCAL/RHO
            XTRKO(2) = (ZEE*XCOSOM) + XMAGC
            YTRKO(2) = (ZEE*YCOSOM) + YMAGC
            ZTRKO(2) = (ZEE*ZCOSOM) + ZMAGC
C
C       POINT TRKO(2) IS EXTRAPOLATED OUTWARD FROM MAGNET CENTER
C       ========================================================
          ELSE
            RMAX = ABS(ZMAX/ZCOSOM)
          ENDIF
          XTRKO(2) = (RMAX*XCOSOM) + XMAGC
          YTRKO(2) = (RMAX*YCOSOM) + YMAGC
          ZTRKO(2) = (RMAX*ZCOSOM) + ZMAGC
        ELSE
          DO I=1,2
            XTRKO(I) = 0.
            YTRKO(I) = 0.
            ZTRKO(I) = 0.
          ENDDO
        ENDIF
C
C       INNER TRACK (TRKI) IS EXTRAPOLATED
C       OUTWARD TO MAGNET CENTER (TRKI(2)) & INWARD TO INTERACTION REGION
C       (TRKI(1)).
C       =================================================================
        DISTAMC=SQRT((XMAGC-XI)**2+(YMAGC-YI)**2+(ZMAGC-ZI)**2)
        XTRKI(2)=XI+DISTAMC*XCOSIM
        YTRKI(2)=YI+DISTAMC*YCOSIM
        ZTRKI(2)=ZI+DISTAMC*ZCOSIM
C If no A-layer hit, extrapolate to vertex
        IF(IFW1.EQ.1.OR.IFW1.EQ.4)THEN
          RMAX=SQRT(XI**2+YI**2+ZI**2)
        ELSE
          RMAX = ABS(ZMIN/ZCOSIM)
        ENDIF
        XTRKI(1)=XI-RMAX*XCOSIM
        YTRKI(1)=YI-RMAX*YCOSIM
        ZTRKI(1)=ZI-RMAX*ZCOSIM
C
C       TAKE CARE OF INVERTED VIEWS
C       ===========================
        IF(IVIEW.EQ.7.OR.IVIEW.EQ.8) THEN
          YTRKI(1)=-YTRKI(1)
          YTRKI(2)=-YTRKI(2)
          YTRKO(1)=-YTRKO(1)
          YTRKO(2)=-YTRKO(2)
        ELSE IF(IVIEW.EQ.9.OR.IVIEW.EQ.10) THEN
          ZTRKI(1)=-ZTRKI(1)
          ZTRKI(2)=-ZTRKI(2)
          ZTRKO(1)=-ZTRKO(1)
          ZTRKO(2)=-ZTRKO(2)
        ELSE IF(IVIEW.EQ.11.OR.IVIEW.EQ.12) THEN
          XTRKI(1)=-XTRKI(1)
          XTRKI(2)=-XTRKI(2)
          XTRKO(1)=-XTRKO(1)
          XTRKO(2)=-XTRKO(2)
        ENDIF
C Don't draw track labels for END CAP views
        IF(DRAWTRK.NE.2)GO TO 40
C
C       CALCULATE POSITION OF TRACK NUMBER LABEL
C       ========================================
        IF (IVIEW.EQ.1) THEN
          XNUMDIR=ZTRKI(2)
          YNUMDIR=YTRKI(2)
        ELSEIF (IVIEW.EQ.2) THEN
          XNUMDIR=XTRKI(2)
          YNUMDIR=YTRKI(2)
        ELSEIF (IVIEW.EQ.3) THEN
          XNUMDIR=ZTRKI(2)
          YNUMDIR=XTRKI(2)
        ELSEIF (IVIEW.EQ.4) THEN
          XNUMDIR=YTRKI(2)
          YNUMDIR=ZTRKI(2)
        ELSEIF (IVIEW.EQ.5) THEN
          XNUMDIR=YTRKI(2)
          YNUMDIR=XTRKI(2)
        ELSEIF (IVIEW.EQ.6) THEN
          XNUMDIR=ZTRKI(2)
          YNUMDIR=XTRKI(2)
        ELSEIF (IVIEW.EQ.7) THEN
          XNUMDIR=ZTRKI(2)
          YNUMDIR=-YTRKI(2)
        ELSEIF (IVIEW.EQ.8) THEN
          XNUMDIR=XTRKI(2)
          YNUMDIR=-YTRKI(2)
        ELSEIF (IVIEW.EQ.9) THEN
          XNUMDIR=XTRKI(2)
          YNUMDIR=-ZTRKI(2)
        ELSEIF (IVIEW.EQ.10) THEN
          XNUMDIR=YTRKI(2)
          YNUMDIR=-ZTRKI(2)
        ELSEIF (IVIEW.EQ.11) THEN
          XNUMDIR=YTRKI(2)
          YNUMDIR=-XTRKI(2)
        ELSEIF (IVIEW.EQ.12) THEN
          XNUMDIR=ZTRKI(2)
          YNUMDIR=-XTRKI(2)
        ENDIF
        PHINUM=ATAN2(YNUMDIR,XNUMDIR)
        IF (-PI.LE.PHINUM.AND.PHINUM.LT.-PI/2.OR.
     &        0.LT.PHINUM.AND.PHINUM.LE.PI/2.) THEN
          XNUM=RNUM*COS(PHINUM)-3*XCHARSIZE
          YNUM=RNUM*SIN(PHINUM)
        ELSE
          XNUM=RNUM*COS(PHINUM)-3*XCHARSIZE
          YNUM=RNUM*SIN(PHINUM)-YCHARSIZE
        ENDIF
        CALL JMOVE(XNUM,YNUM)
C       TRACK NUMBER WILL BE NEGATIVE IF IT HAS NO A-LAYER HIT
        IF (XIFW1.EQ.1) THEN
          TRKNUM=-K                   ! NO A-LAYER HIT
        ELSE
          TRKNUM=K                    ! HAVE A-LAYER HIT
        ENDIF
        WRITE(CNUM,25)TRKNUM
        READ(CNUM,35)CTRKNUM
   25   FORMAT(I3)
   35   FORMAT(A3)
        CALL JSIZE(XCHARSIZE,YCHARSIZE)
        CALL J3STRG(CTRKNUM)
C
   40   NPLOT=NPLOT+1
        IF (IVIEW .EQ. 1) THEN
          CALL PXLINE(0,2,ZTRKI,YTRKI,XTRKI)
          CALL PXLINE(0,2,ZTRKO,YTRKO,XTRKO)
        ELSEIF (IVIEW .EQ. 2) THEN
C only draw inner track if not end cap only
          IF(DRAWTRK.EQ.2)THEN
            CALL PXLINE(0,2,XTRKI,YTRKI,ZTRKI)
          ENDIF
          CALL PXLINE(0,2,XTRKO,YTRKO,ZTRKO)
        ELSEIF (IVIEW .EQ. 3) THEN
          CALL PXLINE(0,2,ZTRKI,XTRKI,YTRKI)
          CALL PXLINE(0,2,ZTRKO,XTRKO,YTRKO)
        ELSEIF (IVIEW .EQ. 4) THEN
          CALL PXLINE(0,2,YTRKI,ZTRKI,XTRKI)
          CALL PXLINE(0,2,YTRKO,ZTRKO,XTRKO)
        ELSEIF (IVIEW .EQ. 5) THEN
          CALL PXLINE(0,2,YTRKI,XTRKI,ZTRKI)
C         CALL PXLINE(0,2,YTRKO,XTRKO,ZTRKO)
        ELSEIF (IVIEW .EQ. 6) THEN
          CALL PXLINE(0,2,ZTRKI,XTRKI,YTRKI)
C         CALL PXLINE(0,2,ZTRKO,XTRKO,YTRKO)
        ELSEIF (IVIEW .EQ. 7) THEN
          CALL PXLINE(0,2,ZTRKI,YTRKI,XTRKI)
C         CALL PXLINE(0,2,ZTRKO,YTRKO,XTRKO)
        ELSEIF (IVIEW .EQ. 8) THEN
          CALL PXLINE(0,2,XTRKI,YTRKI,ZTRKI)
C         CALL PXLINE(0,2,XTRKO,YTRKO,ZTRKO)
        ELSEIF (IVIEW .EQ. 9) THEN
          CALL PXLINE(0,2,XTRKI,ZTRKI,YTRKI)
C         CALL PXLINE(0,2,XTRKO,ZTRKO,YTRKO)
        ELSEIF (IVIEW .LE. 10) THEN
          CALL PXLINE(0,2,YTRKI,ZTRKI,XTRKI)
C         CALL PXLINE(0,2,YTRKO,ZTRKO,XTRKO)
        ELSEIF (IVIEW .EQ. 11) THEN
          CALL PXLINE(0,2,YTRKI,XTRKI,ZTRKI)
C         CALL PXLINE(0,2,YTRKO,XTRKO,ZTRKO)
        ELSEIF (IVIEW .EQ. 12) THEN
          CALL PXLINE(0,2,ZTRKI,XTRKI,YTRKI)
C         CALL PXLINE(0,2,ZTRKO,XTRKO,YTRKO)
        ENDIF
   45 CONTINUE
C
   50 CALL JRCLOS        !(SEGMENT ISEGEV+IVIEW)
      STR1=IBLANK
C get viewport limits
      CALL J4RGET(2,VPORTXMIN,VPORTXMAX,VPORTYMIN,VPORTYMAX)
      IF(VPORTYMAX.GT..7.AND.VPORTXMIN.LT.-.4)THEN
        IF(DRAWTRK.EQ.2)THEN
          WRITE (STR1,55) PTMIN
   55     FORMAT('TRK PTMIN = ',F6.2)
        ENDIF
      ENDIF
C no legend for End Cap view
      IF(VPORTXMAX.GT..4.AND.VPORTYMIN.LT.-.8.AND.NPLOT.GT.0)THEN
        CALL JVSAVE(VWSAVE)
        IF(DRAWTRK.EQ.2)THEN
          CALL LEGEND_LINE(COLORS,LABELS,3 )
          CALL PUMESS(STR1)
        ELSE IF(DRAWTRK.EQ.1)THEN
          CALL LEGEND_LINE(COLORS(1),LABMUON,1)
        ENDIF
        CALL JVLOAD(VWSAVE)
      ENDIF
C
C ****  Reset RCP bank
C
  900 CONTINUE
      CALL EZRSET
C
  999 RETURN
      END
