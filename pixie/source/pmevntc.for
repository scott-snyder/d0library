      SUBROUTINE PMEVNTC(IVIEW,IFLAG)
C========================================================================
C
C  Description:
C  ============
C  Displays an event in from MUON COSMIC TEST SETUP, using information
C  from ZEBRA banks
C  =======
C  IVIEW - tells the default view  (HORIZONTAL-VERTICAL)
C         1  Z Y         7  Z -Y
C         2  X Y         8  X -Y
C         3  X Z         9  X -Z
C         4  Y Z        10  Y -Z
C         5  Y X        11  Y -X
C         6  Z X        12  Z -X
C  IFLAG - 0 for all layers
C          1 for A-layer only
C
C-  Draws the reconstructed muon track using information from either
C-  MUOT, MUON, or PMUO.  The switch is:
C-      -- "MUON TK BANK"  1=MUOT / 2=MUON / 3=PMUO
C
C  Revision History:
C  =================
C  D. HEDIN  ??-JAN-91 CHANGE GTMUOT
C  C.Y.      03-AUG-91 Kill QUAD to determine physical quadrants.
C                      Use XIFL2 to not draw bad hit.
C                      Kill "TRK PTMIN=..."
C  N. Oshima 05-SEP-91 Put selection to draw tracks + fix bug at PTMIN
C                      checking.
C  C.Y.      25-OCT-91 Killed PMUO & MUON dependence on MUOT.
C                      Eliminated use of QUAD in MUOT totally.
C                      Eliminate use of ORIGIN in PMUO & MUON.
C  C.Y.      31-OCT-91 Changed RNUM for endview.
C  S.H.       3-AUG-92 Check quality flag of track
C- N.O.,C.Y. 19-JAN-93 Draw dashed lines for tracks inside magnet w/ no A-layer
C-                     hits.
C- T. McKibben 29-OCT-93 Changed call to GTMUHT for run 1B compatibility
C-   Updated  15-NOV-1993   BH Added variable NMSCT to call GTMUHT
C- TMcK 04-DEC-1993 run 1B mods made to correct version of PMEVNTC
C-   Updated  23-MAR-2004   compile with g77.
C========================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C    Local Declarations:
C    ===================
      INTEGER MAXMUD1,MAXMUOH
      INTEGER NWRAW,NWPRO,NWMOD,NSRAW,NSPRO,NSMOD !Run 1B GTMUHT
      INTEGER NMUHP,NMUOF,NVERS,LPMUOF(460),NMSCT !Run 1B GTMUHT
      INTEGER QUAD,LMUHT,GZMUHT,NSAMUS,IFW3,ISPARE
      INTEGER I,J,K,L,M                     ! Loop variables
      INTEGER NTRACK                        ! No. tracks
      INTEGER TRKNUM                        ! TRACK LABEL NUMBER
      INTEGER NPTRK                         ! No. of points on track
      INTEGER NHCTR                         ! No. of hit trigger counters
      INTEGER IVIEW,MODK,IPHITS
      INTEGER IFLAG,MUTKQUAL
      INTEGER IFW1,IFW2,DRAWHITS,MUBANK,DRAWTRK,IER
      INTEGER DRAWCELLS,ICUT, IDBOX,IPOS
      INTEGER LMUON,GZMUON,LPMUO,GZPMUO,LMUOT,GZMUOT
      INTEGER TKLAB
      REAL XTRKI(2),YTRKI(2),ZTRKI(2),NM,MR            ! INNER TRACK
      REAL XTRKO(2),YTRKO(2),ZTRKO(2)                  ! OUTTER TRACK
      REAL XMAGC,YMAGC,ZMAGC,ELCAL,ELFE,SPARE1
      REAL XCOSOM,YCOSOM,ZCOSOM,SPARE2
      REAL XCOSIM,YCOSIM,ZCOSIM
      REAL CHSQBV,CHSNBV,MOM,MOMER
      REAL RMAX,RMIN,AORG(3)
      REAL XI,YI,ZI,PX,PY,PZ,PT,PTMIN,DPT,XCHARSIZE,YCHARSIZE
      REAL XMN,XMX,YMN,YMX
      REAL XHTRAK(40),YHTRAK(40),ZHTRAK(40),IHWADD(40)
      REAL DISTALYR,DISTAMC,DISTMC,XNUMDIR,YNUMDIR,PHINUM,RNUM,XNUM,YNUM
      REAL XMAX_MUOT,YMAX_MUOT,ZMAX_MUOT,X_ALYR,Y_ALYR,Z_ALYR
      REAL XNUMMAX,YNUMMAX,ZNUMMAX,RNUM_END
      REAL XO,YO,ZO,YMAX,ZMAX,PHITK,SIGN,THETA1,THETA2
      CHARACTER*3 ICOL(6),JCOL,MC,CNUM0,CTRKNUM0
      CHARACTER*8 CNUM,CTRKNUM
      CHARACTER*22 LINE(40)
      CHARACTER*4 BKNAME
      LOGICAL ALYR,EZERROR,MUONLY,MUFIT,PU_PICK_ACTIVE,IOK
C
      DATA XCHARSIZE,YCHARSIZE/15,15/
      DATA RNUM_END/245/
      DATA XMAX_MUOT,YMAX_MUOT,ZMAX_MUOT/581.66,581.66,965.2/
      DATA X_ALYR,Y_ALYR,Z_ALYR/309.88,281.94,381./
      DATA XNUMMAX,YNUMMAX,ZNUMMAX/371.8,370.5,515.62/  ! MAG. CENTER
      DATA ICUT/1/
      DATA LINE(2) /' TRKID     P      PT  '/
C
      SAVE XMN,XMX,YMN,YMX,MUONLY
C
C    Include Statements:
C    ===================
      INCLUDE 'D0$INC:PXMHTK.INC/NOLIST'
      REAL PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC'
C============================================================================
C
C
C    Executable Code:
C    ================
      NTRACK=0

C
C
C    Loop over and mark processed hits if desired
C    ============================================
C
C ****  Picking the right RCP bank
C
      CALL EZPICK('PX_MUODIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PMEVNTC',
     &         'Bank PX_MUODIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PUGETV('MUON ONLY',MUONLY)
      CALL PUGETV('MUON TK BANK',MUBANK)
      CALL PUGETV('MUON TK FIT',MUFIT)
      CALL PUGETV('TRACK PTMIN',PTMIN)
      CALL PUGETV('TRACK DPT',DPT)
      CALL PUGETV('MUON DRAW CELLS',DRAWCELLS)
      CALL PUGETV('MUON DRAW HITS',DRAWHITS)
      CALL PUGETV('MUON TK QUAL',MUTKQUAL)
      CALL PUGETV('MUON MAX MUD1',MAXMUD1)
      CALL PUGETV('MUON LABEL TKS',TKLAB)
      IF(MAXMUD1.LE.0)MAXMUD1=9999
      CALL PUGETV('MUON MAX MUOH',MAXMUOH)
      IF(MAXMUOH.LE.0)MAXMUOH=9999
      CALL PUGETV('MUON DRAW TRACKS',DRAWTRK)
      IF (.NOT. MUONLY) THEN
        IF(MUBANK.EQ.1) THEN
          BKNAME = 'MUOT'
        ELSEIF(MUBANK.EQ.2) THEN
          BKNAME = 'MUON'
        ELSEIF(MUBANK.EQ.3) THEN
          BKNAME = 'PMUO'
        ELSE
          BKNAME = ' '
        ENDIF
        WRITE(LINE(1),100) BKNAME
  100   FORMAT(' MUON TRK BANK: ',A4)
      ENDIF
      IPOS = 0
C
C    Get processed hit info from ZEBRA bank
C    ======================================
      CALL GTMUHT(NWRAW,NWPRO,NWMOD,NSRAW,NSPRO,NSMOD,
     &            NMUHP,NMUOF,NMSCT,NVERS,LPMUOF)
      CALL PUOPEN
C
C    DRAW MUDULES AND HITS, THEN...
C    Loop over hit on tracks and tracks and draw them
C    ================================================
      IF (DRAWCELLS.GT.0) THEN
        IF(NWRAW.LT.MAXMUD1)THEN
          CALL PMMUD1(IVIEW,IFLAG,ICUT)
        ELSE
          CALL INTMSG(' !!!!WARNING -too many MUD1 hits to display')
        ENDIF
      ENDIF
      IF (DRAWHITS.GT.0)  THEN
        IF(NWPRO.LT.MAXMUOH)THEN
          CALL PMHITC(IVIEW,IFLAG) ! 1=DRAW/0=DON'T DRAW
        ELSE
          CALL INTMSG(' !!!!WARNING -too many MUOH hits to display')
        ENDIF
      ENDIF
C
C    IF DON'T DESIRE TRACK, SKIP DRAWING TRACK
C    =========================================
      IF (DRAWTRK.EQ.0) GOTO 50           ! 1=DRAW / 0=DON'T DRAW
      IPOS = 2
      K=1
C
C    FIND WHICH MUON BANK TO USE
C    ===========================
C
      lmuon = 0
      lpmuo = 0
 800  continue
      IF (MUBANK.EQ.2) THEN
C
C    GET MOMENTUM INFORMATION FROM MUON
C    ==================================
        if (lmuon .eq. 0) LMUON=GZMUON(1)
        IF (LMUON.EQ.0) THEN
          CALL JRCLOS
          CALL PUMESS('MUON BANK DOES NOT EXIST')
          GOTO 900
        ENDIF
   10   CONTINUE
C Check muon track quality
        IF(IQ(LMUON+6).GT.0.AND.MUTKQUAL.GT.0)GO TO 45        
        PX=Q(LMUON+11)
        PY=Q(LMUON+12)
        PZ=Q(LMUON+13)
        MOM=Q(LMUON+14)
        IF (MOM.EQ.0) THEN
          CALL ERRMSG('PIXIE','PMEVNTC','MOM=0 IN MUON BANK','W')
          GOTO 45
        ENDIF
        SIGN=MOM/ABS(MOM)             ! UPDATED 10-7-91
        MOM=ABS(MOM)                  ! UPDATED 10-7-91
        PT=Q(LMUON+15)
C          ORIGIN=IQ(LMUON+8)
C          ORIGIN=1                      ! TEMPORARY TILL MURECO FIXED
        XO=Q(LMUON+37)                ! CHANGE 9-23-91
        YO=Q(LMUON+38)                ! CHANGE 9-23-91
        ZO=Q(LMUON+39)                ! CHANGE 9-23-91
        XCOSIM=PX/MOM
        YCOSIM=PY/MOM
        ZCOSIM=PZ/MOM
      ELSEIF (MUBANK.EQ.3) THEN
C
C    GET MOMENTUM INFORMATION FROM PMUO
C    ==================================
        if (lpmuo .eq. 0) LPMUO=GZPMUO(1)
        IF (LPMUO.EQ.0) THEN
          CALL JRCLOS
          CALL PUMESS('PMUO BANK DOES NOT EXIST')
          GOTO 900
        ENDIF
   20   CONTINUE
        IF(IQ(LPMUO+9).GT.0.AND.MUTKQUAL.GT.0)GO TO 45
        PX=Q(LPMUO+10)                ! UPDATED 9-23-91
        PY=Q(LPMUO+11)                ! UPDATED 9-23-91
        PZ=Q(LPMUO+12)                ! UPDATED 9-23-91
        MOM=Q(LPMUO+13)               ! UPDATED 9-23-91
        IF (MOM.EQ.0) THEN
          CALL ERRMSG('PIXIE','PMEVNTC','MOM=0 IN PMUO BANK','W')
          GOTO 45
        ENDIF
        SIGN=MOM/ABS(MOM)             ! UPDATED 10-7-91
        MOM=ABS(MOM)                  ! UPDATED 10-7-91
        PT=Q(LPMUO+14)                ! UPDATED 9-23-91
C          ORIGIN=IQ(LPMUO+5)            ! UPDATED 9-23-91
C          ORIGIN=1                      ! TEMPORARY TILL MURECO FIXED
        XO=Q(LPMUO+25)                ! UPDATED 9-23-91
        YO=Q(LPMUO+26)                ! UPDATED 9-23-91
        ZO=Q(LPMUO+27)                ! UPDATED 9-23-91
        XCOSIM=PX/MOM
        YCOSIM=PY/MOM
        ZCOSIM=PZ/MOM
      ELSE
C
C    GET MUON INFORMATION FROM MUOT
C    ==============================
   30   CONTINUE
C    Get number of tracks from MTRH (muon track header bank) ONLY for MUOT
C    =====================================================================
        CALL GTMTRH(NTRACK)
        IF(NTRACK.LE.0)GO TO 50
C Check that MUOT bank exists
        LMUOT=GZMUOT(0)
        IF(LMUOT.LE.0)GO TO 50
        CALL GTMUOT(K,NPTRK,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,
     X      XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     X      YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,SPARE1,
     X      SPARE2)
C-
C--- Reset IFW1 flag
C-
        IFW1 = MOD(IFW1,10)
C-
C CHECK TRACK QUALITY (IFLGW4.EQ.0)
        IF(ISPARE.GT.0.AND.MUTKQUAL.GT.0)GO TO 45
C    DRAW GOOD FIT TRACKS ONLY )
C    =======================================================
        IF (MUFIT .AND. (CHSQBV.GT.900.))   GO TO 45
C
        IF (MOM.EQ.0) THEN
          CALL ERRMSG('PIXIE','PMEVNTC','MOM=0 IN MUOT BANK','W')
          GOTO 45
        ENDIF
        SIGN=MOM/ABS(MOM)
        MOM=ABS(MOM)
        IF (IFW1.EQ.1 .OR. IFW1.EQ.4) THEN    ! NO A-LAYER HIT
          PX=MOM*XCOSOM
          PY=MOM*YCOSOM
          PZ=MOM*ZCOSOM
        ELSE                                  ! HAVE A-LAYER HIT
          PX=MOM*XCOSIM
          PY=MOM*YCOSIM
          PZ=MOM*ZCOSIM
        ENDIF
        PT=SQRT(PX**2+PY**2)
      ENDIF
C
C    DISPLAY MOMENTUM INFORMATION OF ALL MUON TRACKS
C    ===============================================
      IF (.NOT. MUONLY) THEN
        IPOS = IPOS + 1
        WRITE(LINE(IPOS),110) K,SIGN*MOM,PT
  110   FORMAT(I4,2X,2F8.2)
      ENDIF
C
C CHECK IF QUAD is right for this IVIEW
C
      PHITK=ATAN2(PX,PY)
      PHITK=ABS(PHITK)
      THETA1=ATAN2(PY,PZ)
      THETA2=ATAN2(PX,PZ)
      THETA1=ABS(THETA1)
      THETA2=ABS(THETA2)
      IOK=.FALSE.
C FOR Y-Z VIEW, test PHITK,THETA2
      IF(IVIEW.EQ.1)THEN
        IF(PHITK.LE..30*PI.OR.PHITK.GE..70*PI)IOK=.TRUE. 
        IF(THETA2.LE..30*PI.OR.THETA2.GE..70*PI)IOK=.TRUE. 
C FOR X-Y VIEW, TEST THETA1,THETA2
      ELSE IF(IVIEW.EQ.2)THEN 
        IF(THETA1.GE..20*PI.AND.THETA1.LE..80*PI)IOK=.TRUE.
        IF(THETA2.GE..20*PI.AND.THETA2.LE..80*PI)IOK=.TRUE.
C FOR Z-X VIEW TEST PHITK,THETA1
      ELSE IF(IVIEW.EQ.3)THEN
        IF(PHITK.GE..20*PI.AND.PHITK.LE..80*PI)IOK=.TRUE.
        IF(THETA1.LE..30*PI.OR.THETA1.GE..70*PI)IOK=.TRUE. 
      ENDIF
      IF(.NOT.IOK)GO TO 45
C
C     SET COLOR OF TRACKS BY THE PT VALUE IF MUON ONLY=.TRUE.
C     ===================================
      IF (PT .LT. PTMIN) GO TO 45
      IF (.NOT. MUONLY) THEN
        JCOL='GRE'
        GO TO 15
      ENDIF
      IF (PT.LT.DPT) THEN
        JCOL='BLU'
      ELSE
        JCOL='GRE'
      ENDIF
   15 CALL PXCOLR(JCOL)
C
      IF (MUBANK.LE.1) THEN
C       CALCULATE ENDPOINTS OF TRACK(S) TO BE DRAWN
C       ===========================================
        DISTALYR=SQRT(XI**2+YI**2+ZI**2)
        DISTMC=SQRT(XMAGC**2+YMAGC**2+ZMAGC**2)
        DISTAMC=SQRT((XMAGC-XI)**2+(YMAGC-YI)**2+(ZMAGC-ZI)**2)
C
C       POINT TRKO(1) IS AT THE MAGNET CENTER
C       =====================================
        IF (IFW1 .NE. 5) THEN
          XTRKO(1)=XMAGC
          YTRKO(1)=YMAGC
          ZTRKO(1)=ZMAGC
C
C       POINT TRKO(2) IS EXTRAPOLATED OUTWARD FROM MAGNET CENTER
C       ========================================================
          IF (IVIEW.EQ.1.AND.ABS(PY/PZ).GT.(Y_ALYR/Z_ALYR) .OR.
     &      IVIEW.EQ.3.AND.ABS(PX/PZ).GT.(X_ALYR/Z_ALYR)) THEN !CENTRAL TRK
            IF ((1./4.)*PI.LE.PHITK.AND.PHITK.LE.(3./4.)*PI .OR.
     &         (-3./4.)*PI.LT.PHITK.AND.PHITK.LT.(-1./4.)*PI ) THEN
              RMAX=ABS(YMAX_MUOT/YCOSOM)
            ELSE
              RMAX=ABS(XMAX_MUOT/XCOSOM)
            ENDIF
          ELSE                                   ! END TRACK
            RMAX = ABS(ZMAX_MUOT/ZCOSOM)
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
C       INNER TRACK (TRKI) IS EXTRAPOLATED OUTWARD TO MAGNET CENTER (TRKI(2))
C       & INWARD TO INTERACTION REGION(TRKI(1)).
C       =================================================================
        XTRKI(2)=XI+DISTAMC*XCOSIM
        YTRKI(2)=YI+DISTAMC*YCOSIM
        ZTRKI(2)=ZI+DISTAMC*ZCOSIM
        XTRKI(1)=XI-DISTALYR*XCOSIM
        YTRKI(1)=YI-DISTALYR*YCOSIM
        ZTRKI(1)=ZI-DISTALYR*ZCOSIM
      ELSEIF (MUBANK.EQ.2.OR.MUBANK.EQ.3) THEN
C          IF (ORIGIN.EQ.1) THEN         ! ORIGIN=VERTEX
        IF (IVIEW.EQ.1.AND.ABS(PY/PZ).GT.(Y_ALYR/Z_ALYR) .OR.
     &        IVIEW.EQ.3.AND.ABS(PX/PZ).GT.(X_ALYR/Z_ALYR)) THEN !CENTRAL TRK
          IF ((1./4.)*PI.LE.PHITK.AND.PHITK.LE.(3./4.)*PI .OR.
     &           (-3./4.)*PI.LT.PHITK.AND.PHITK.LT.(-1./4.)*PI ) THEN
            DISTALYR=ABS(YNUMMAX/YCOSIM)
          ELSE
            DISTALYR=ABS(XNUMMAX/XCOSIM)
          ENDIF
        ELSE                                ! END TRACK
          DISTALYR=ABS(ZNUMMAX/ZCOSIM)
        ENDIF
        XTRKI(1)=XO
        YTRKI(1)=YO
        ZTRKI(1)=ZO
        XTRKI(2)=XO+DISTALYR*XCOSIM
        YTRKI(2)=YO+DISTALYR*YCOSIM
        ZTRKI(2)=ZO+DISTALYR*ZCOSIM
C          ELSEIF (ORIGIN.EQ.2) THEN     ! ORIGIN=A-LAYER HIT
C            DISTALYR=SQRT(XO**2+YO**2+ZO**2)
C            XTRKI(1)=XO-DISTALYR*XCOSIM
C            YTRKI(1)=YO-DISTALYR*YCOSIM
C            ZTRKI(1)=ZO-DISTALYR*ZCOSIM
C            XTRKI(2)=XO
C            YTRKI(2)=YO
C            ZTRKI(2)=ZO
C          ELSE
C            CALL JRCLOS
C            CALL PUMESS(
C     &        'ORIGIN OF MUON TRACK IN MUON OR PMUO NOT DEFINED')
C            GOTO 900
C          ENDIF
      ENDIF
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
      IF (IVIEW.EQ.2) THEN
        RNUM=255.0
C          IF (ABS(PY/PX).GT.(Y_ALYR/X_ALYR)) THEN
C            RNUM=ABS(YNUMMAX/SIN(PHINUM))
C          ELSE
C            RNUM=ABS(XNUMMAX/COS(PHINUM))
C          ENDIF
      ELSEIF (IVIEW.EQ.1) THEN
        RNUM=255.0
        IF (ABS(PY/PZ).GT.(Y_ALYR/Z_ALYR)) THEN
          RNUM=ABS(YNUMMAX/SIN(PHINUM))
        ELSE
          RNUM=ABS(ZNUMMAX/COS(PHINUM))
        ENDIF
      ELSEIF (IVIEW.EQ.3) THEN
        IF (ABS(PX/PZ).GT.(X_ALYR/Z_ALYR)) THEN
          RNUM=ABS(XNUMMAX/SIN(PHINUM))
        ELSE
          RNUM=ABS(ZNUMMAX/COS(PHINUM))
        ENDIF
      ELSE
        CALL JRCLOS
        CALL PUMESS('IVIEW choice out of useful range')
        GOTO 900
      ENDIF
      IF (-PI.LE.PHINUM.AND.PHINUM.LT.-PI/2.OR.
     &        0.LT.PHINUM.AND.PHINUM.LE.PI/2.) THEN
        XNUM=RNUM*COS(PHINUM)-XCHARSIZE
        YNUM=RNUM*SIN(PHINUM)
      ELSE
        XNUM=RNUM*COS(PHINUM)
        YNUM=RNUM*SIN(PHINUM)
      ENDIF
C
C DISPLAY TRACK NUMBER AND PT
C
      CALL JMOVE(XNUM,YNUM)
      TRKNUM=K
      CALL JSIZE(XCHARSIZE,YCHARSIZE)
      IF(TKLAB.EQ.2.AND.PT.GT.0.AND.PT.LT.100.)THEN
        WRITE(CNUM,25) TRKNUM, PT
        READ(CNUM,35) CTRKNUM
   25   FORMAT(I3,1X,F4.0)
   35   FORMAT(A8)
        CALL J3STRG(CTRKNUM)
      ELSEIF(TKLAB.GT.0)THEN
        WRITE(CNUM0,26)TRKNUM
        READ(CNUM0,36)CTRKNUM0
   26   FORMAT(I3)
   36   FORMAT(A3)
        CALL J3STRG(CTRKNUM0)
      ENDIF
C
      IF (IVIEW .EQ. 1) THEN
        IF (MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)) THEN
          CALL PXLINE(2,2,ZTRKI,YTRKI,XTRKI)
        ELSE
          CALL PXLINE(0,2,ZTRKI,YTRKI,XTRKI)
        ENDIF
        IF (MUBANK.LE.1) CALL PXLINE(0,2,ZTRKO,YTRKO,XTRKO)
      ELSEIF (IVIEW .EQ. 2) THEN
        IF (MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)) THEN
          CALL PXLINE(2,2,XTRKI,YTRKI,ZTRKI)
        ELSE
          CALL PXLINE(0,2,XTRKI,YTRKI,ZTRKI)
        ENDIF
        IF (MUBANK.LE.1) CALL PXLINE(0,2,XTRKO,YTRKO,ZTRKO)
      ELSEIF (IVIEW .EQ. 3) THEN
        IF (MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)) THEN
          CALL PXLINE(2,2,ZTRKI,XTRKI,YTRKI)
        ELSE
          CALL PXLINE(0,2,ZTRKI,XTRKI,YTRKI)
        ENDIF
        IF (MUBANK.LE.1) CALL PXLINE(0,2,ZTRKO,XTRKO,YTRKO)
      ELSEIF (IVIEW .EQ. 4) THEN
        IF (MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)) THEN
          CALL PXLINE(2,2,YTRKI,ZTRKI,XTRKI)
        ELSE
          CALL PXLINE(0,2,YTRKI,ZTRKI,XTRKI)
        ENDIF
        IF (MUBANK.LE.1) CALL PXLINE(0,2,YTRKO,ZTRKO,XTRKO)
      ELSEIF (IVIEW .EQ. 5) THEN
        IF (MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)) THEN
          CALL PXLINE(2,2,YTRKI,XTRKI,ZTRKI)
        ELSE
          CALL PXLINE(0,2,YTRKI,XTRKI,ZTRKI)
        ENDIF
        IF (MUBANK.LE.1) CALL PXLINE(0,2,YTRKO,XTRKO,ZTRKO)
      ELSEIF (IVIEW .EQ. 6) THEN
        IF (MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)) THEN
          CALL PXLINE(2,2,ZTRKI,XTRKI,YTRKI)
        ELSE
          CALL PXLINE(0,2,ZTRKI,XTRKI,YTRKI)
        ENDIF
        IF (MUBANK.LE.1) CALL PXLINE(0,2,ZTRKO,XTRKO,YTRKO)
      ELSEIF (IVIEW .EQ. 7) THEN
        IF (MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)) THEN
          CALL PXLINE(2,2,ZTRKI,YTRKI,XTRKI)
        ELSE
          CALL PXLINE(0,2,ZTRKI,YTRKI,XTRKI)
        ENDIF
        IF (MUBANK.LE.1) CALL PXLINE(0,2,ZTRKO,YTRKO,XTRKO)
      ELSEIF (IVIEW .EQ. 8) THEN
        IF (MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)) THEN
          CALL PXLINE(2,2,XTRKI,YTRKI,ZTRKI)
        ELSE
          CALL PXLINE(0,2,XTRKI,YTRKI,ZTRKI)
        ENDIF
        IF (MUBANK.LE.1) CALL PXLINE(0,2,XTRKO,YTRKO,ZTRKO)
      ELSEIF (IVIEW .EQ. 9) THEN
        IF (MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)) THEN
          CALL PXLINE(2,2,XTRKI,ZTRKI,YTRKI)
        ELSE
          CALL PXLINE(0,2,XTRKI,ZTRKI,YTRKI)
        ENDIF
        IF (MUBANK.EQ.1) CALL PXLINE(0,2,XTRKO,ZTRKO,YTRKO)
      ELSEIF (IVIEW .LE. 10) THEN
        IF (MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)) THEN
          CALL PXLINE(2,2,YTRKI,ZTRKI,XTRKI)
        ELSE
          CALL PXLINE(0,2,YTRKI,ZTRKI,XTRKI)
        ENDIF
        IF (MUBANK.LE.1) CALL PXLINE(0,2,YTRKO,ZTRKO,XTRKO)
      ELSEIF (IVIEW .EQ. 11) THEN
        IF (MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)) THEN
          CALL PXLINE(2,2,YTRKI,XTRKI,ZTRKI)
        ELSE
          CALL PXLINE(0,2,YTRKI,XTRKI,ZTRKI)
        ENDIF
        IF (MUBANK.LE.1) CALL PXLINE(0,2,YTRKO,XTRKO,ZTRKO)
      ELSEIF (IVIEW .EQ. 12) THEN
        IF (MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)) THEN
          CALL PXLINE(2,2,ZTRKI,XTRKI,YTRKI)
        ELSE
          CALL PXLINE(0,2,ZTRKI,XTRKI,YTRKI)
        ENDIF
        IF (MUBANK.LE.1) CALL PXLINE(0,2,ZTRKO,XTRKO,YTRKO)
      ENDIF
   45 CONTINUE
      K=K+1
      IF (MUBANK.EQ.1) THEN
        IF (K.LE.NTRACK) GOTO 800
      ELSEIF (MUBANK.EQ.2) THEN
        LMUON=GZMUON(K)
        IF(LMUON.NE.0) GOTO 800
      ELSEIF (MUBANK.EQ.3) THEN
        LPMUO=GZPMUO(K)
        IF(LPMUO.NE.0) GOTO 800
      ENDIF
C
   50 CALL JRCLOS        !(SEGMENT ISEGEV+IVIEW)
C
C ****  Reset RCP bank
C
  900 CONTINUE
  990 CALL EZRSET                       ! Reset RCP bank
C
  999 RETURN
C#######################################################################
      ENTRY PMLEGEND
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the legends of the muon detector.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created   4-NOV-1991   Lupe Howell
C-
C----------------------------------------------------------------------
C
C ****  Display legends when in picking mode
C
      IF(MUONLY)THEN
        CALL LEGENDPT
      ELSE
        IF ( PU_PICK_ACTIVE() ) THEN
          IF (IPOS .GT. 0) THEN
            CALL J4RGET(1, XMN, XMX, YMN, YMX)
            AORG(1) = XMN + (XMX-XMN)*0.01
            AORG(2) = YMN + (YMX-YMN)*0.86
            AORG(3) = 0.
            CALL PUTEXT_CREATE(IDBOX)
            CALL PUTEXT_SET(IDBOX,'P',AORG)
            CALL PUTEXT(IDBOX,LINE,IPOS)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
