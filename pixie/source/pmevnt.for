      SUBROUTINE PMEVNT(IVIEW,VIEW)
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
C  VIEW - 1 is bend view
C         2 is nonbend view
C
C  Revision History:
C  =================
C  February 2, 1986 - Original Creation
C  April 15, 1986 - Added call to TRGCNT
C  4-30   CHANGE ZEBRA SLIGHTLY,FIX TRACK HIT BUG, PUT IN MAGNET BEND DH
C  October 14,1986 - extracted "info from ZEBRA" code & made new subroutines
C                    to take care of this - TK
C dh 9/89 change gtmuot call
C cf,dh  11/89 add third view
C  DH 4/90 ADD IVIEW 7-12, LABEL TRACKS
C  DH 1/91 CHANGE GTMUOT
C-   Updated   5-APR-1991   Harrison B. Prosper
C-      Fix PUOPEN -- JRCLOS logic
C- CY 10/15/91 Use IFW2 in MUOT to throw away hopeless tracks.  Also, include
C-             "MUON TK FIT" to draw only "good" fits in MUOT.
C- SH 10/18/91 added view=12 for PMMUD1 call 
C  DH 4/92 change 'bad' track designation
C  S.H.       3-AUG-92 Check quality flag of track
C- SH    4/20/93 - added HITFLG,DTIMFLG
C- T. McKibben 3-NOV-1993 Changed call to GTMUHT for run 1B compatibility
C- BH    15-NOV-1993 Added variable NMSCT to call GTMUHT
C- TMcK  04-DEC-1993 Run 1B modifications made to correct version of PMEVNT
C========================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C    Local Declarations:
C    ===================
      INTEGER QUAD,LMUHT,GZMUHT,NSAMUS,IFW3,ISPARE
      INTEGER DTIMFLG
C
C      INTEGER ISEGEV                        ! Retained segment index
      INTEGER I,J,K,L                       ! Loop variables
      INTEGER NTRACK                        ! No. tracks
      INTEGER NPTRK                         ! No. of points on track
      INTEGER NHCTR                         ! No. of hit trigger counters
      INTEGER LMUD1,GZMUD1,NMUD1,MAXMUD1,MAXMUOH
      INTEGER NWRAW,NWPRO,NWMOD,NSRAW,NSPRO,NSMOD !Run 1B GTMUHT
      INTEGER NMUHP,NMUOF,NVERS,LPMUOF(460),NMSCT !Run 1B GTMUHT
      INTEGER ICUT,IFLAG
      INTEGER IVIEW,MODK,NTEMP,IPHITS
      INTEGER NPTRAK
      INTEGER VIEW,KVIEW
      INTEGER IFW1,IFW2,IER
      INTEGER DRCELLS,DRHITS,DRTRACKS,DRTKHITS
      INTEGER LMUOT,GZMUOT
      INTEGER MUTKQUAL
      REAL DPT ! If PT below DPT track is blue, otherwise green
      REAL XTRK(3),YTRK(3)                  ! 3 points on track
      REAL ZTRK(3)
      REAL XMAGC,YMAGC,ZMAGC,ELCAL,ELFE,SPARE1,SPARE2
      REAL XCOSOM,YCOSOM,ZCOSOM
      REAL XCOSIM,YCOSIM,ZCOSIM
      REAL CHSQBV,CHSNBV,MOM,MOMER
      REAL SIGN,PX,PY,PZ,PT
      REAL RMAX,RMIN,YPMAX,YPMIN
      REAL XI,YI,ZI
      REAL XHTRAK(40),YHTRAK(40),ZHTRAK(40),IHWADD(40)
      CHARACTER*3 ICOL(6)
      CHARACTER*2 CCTRK
      LOGICAL EZERROR,MUFIT
C
      DATA ICOL/'GRE','CYA','MAG',       ! Array of colors for tracks
     X          'GRE','CYA','MAG'/       !  and processed hits
      DATA YPMAX,YPMIN/510.,0./
      DATA IFLAG,ICUT/0,0/
C
C    Include Statements:
C    ===================
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:PXMHTK.INC'
C========================================================================
C
C    Executable Code:
C    ================
      NWRAW=0
      NTRACK=0
      NWPRO=0
C
C ****  Picking the right RCP bank
C
      CALL EZPICK('PX_MUODIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PMEVNTC',
     &         'Bank PX_MUODIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PUGETV('MUON TK FIT',MUFIT)
      CALL PUGETV('MUON TK QUAL',MUTKQUAL)
      CALL PUGETV('MUON MAX MUD1',MAXMUD1)
      IF(MAXMUD1.LE.0)MAXMUD1=9999
      CALL PUGETV('MUON MAX MUOH',MAXMUOH)
      IF(MAXMUOH.LE.0)MAXMUOH=9999
      CALL PUGETV('MUON DRAW CELLS', DRCELLS)
      CALL PUGETV('MUON DRAW HITS', DRHITS)
      CALL PUGETV('MUON DRAW TRACKS',DRTRACKS)
      CALL PUGETV('MUON HITS ON TKS',DRTKHITS)
      CALL PUGETV('MUON DRIFT TIME',DTIMFLG)
      CALL PUGETV('TRACK DPT', DPT)
C
C    Get processed hit info from ZEBRA bank
C    ======================================
      CALL GTMUHT(NWRAW,NWPRO,NWMOD,NSRAW,NSPRO,NSMOD,
     &            NMUHP,NMUOF,NMSCT,NVERS,LPMUOF)
C
C ****  Open segment; branch to 900 to close segment
C
      CALL PUOPEN
C draw cells
      KVIEW=IVIEW
      IF(IVIEW.EQ.3)KVIEW=12
      IF (DRCELLS.GT.0) THEN
        IF(NWRAW.LT.MAXMUD1) THEN
           CALL PMMUD1(KVIEW,IFLAG,ICUT)
        ELSE
           CALL INTMSG(' !!!!WARNING -too many MUD1 hits to display')
        ENDIF
      ENDIF
C    Display raw hit cells in the bend view, if not already displayed
C    ======================================
      IF(VIEW.EQ.1.AND.DRCELLS.EQ.0.AND.NWRAW.LE.MAXMUD1)THEN
        CALL PMCELL(NWRAW,IVIEW)
      ENDIF 
C CHECK NUMBER OF PROCESSED HITS
      IF(NWPRO.LE.0)GO TO 15
C
      IF(DRHITS.LE.0)GO TO 15
      IF(NWPRO.GT.MAXMUOH)THEN
        CALL INTMSG(' !!!!WARNING -too many MUOH hits to display')
        GO TO 15
      ENDIF
C    Loop over and mark processed hits
C    =================================
    5 DO 10 IPHITS=1,NWPRO
        CALL PMHITS(IPHITS,IVIEW,DRHITS,DTIMFLG)
   10 CONTINUE
C
C    Get number of tracks from ZEBRA banks
C    =====================================
   15 IF (DRTRACKS .EQ. 1) CALL GTMTRH(NTRACK)
      IF(NTRACK.LE.0)GO TO 900
C CHECK THAT MUOT BANK EXISTS
        LMUOT=GZMUOT(0)
        IF(LMUOT.LE.0)GO TO 900
C
C    Loop over hit on tracks and tracks and draw them
C    ================================================
      DO 45 K = 1,NTRACK
C
C    Plot vernier pad hits and circle processed hits in
C    the nonbend and bend views, respectively
C    ==================================================
        IF (DRTKHITS .EQ. 1) THEN
CCCC    BELOW CALL TO GTMHOT SHOULD BE REPLACED BY GTMUOT CALL
          CALL GTMHOT(1,K,NPTRAK,XHTRAK,YHTRAK,ZHTRAK,IHWADD)
          IF (VIEW.EQ.2) CALL PMVERN(IVIEW,K,NPTRAK)
          IF (VIEW.EQ.1) CALL PMCIRC(IVIEW,K,NPTRAK)
        ENDIF
C
C    Draw the track
C    ==============
        IF (DRTRACKS .EQ. 1) THEN
          CALL GTMUOT(K,NPTRAK,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,
     X     XI,YI,ZI,XMAGC,
     X      YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,ZCOSOM,
     X     CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,SPARE1,SPARE2)
C-
C--- Reset IFW1 flag
C-
        IFW1 = MOD(IFW1,10)
C-
C    DRAW GOOD FIT TRACKS ONLY(MUFIT : T), OR ALL(MUFIT : F)
C    =======================================================
          IF (MUFIT .AND. (CHSQBV.GT.900.))   GO TO 45
C CHECK TRACK QUALITY (IFLGW4.EQ.0)
        IF(ISPARE.GT.0.AND.MUTKQUAL.GT.0)GO TO 45
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
C Choose color based on PT of track
        IF(PT.LT.DPT)THEN
          CALL PXCOLR('BLU')
        ELSE
          CALL PXCOLR('GRE')
        ENDIF
C
C    Point 2 is in the center of the magnet
C    ======================================
          XTRK(2) = XMAGC
          YTRK(2) = YMAGC
          ZTRK(2) = ZMAGC
C
C    Point 3 is the interaction diamond
          IF(IVIEW.EQ.1.OR.IVIEW.EQ.2.OR.IVIEW.EQ.7.OR.IVIEW.EQ.8) THEN
            RMIN = (-YMAGC)/YCOSIM
            YTRK(3) = 0
            XTRK(3) = (RMIN*XCOSIM) + XMAGC
            ZTRK(3) = (RMIN*ZCOSIM) + ZMAGC
          ELSEIF(IVIEW.EQ.5.OR.IVIEW.EQ.6.OR.IVIEW.EQ.11.OR.
     A                                   IVIEW.EQ.12) THEN
            RMIN = (-XMAGC)/XCOSIM
            XTRK(3) = 0
            YTRK(3) = (RMIN*YCOSIM) + YMAGC
            ZTRK(3) = (RMIN*ZCOSIM) + ZMAGC
          ELSE
            RMIN = (-ZMAGC)/ZCOSIM
            ZTRK(3) = 0
            XTRK(3) = (RMIN*XCOSIM) + XMAGC
            YTRK(3) = (RMIN*YCOSIM) + YMAGC
          ENDIF
C    Point 2 is YPMAX 'outside' of the iron
C    ======================================
          RMAX = YPMAX
          XTRK(1) = (RMAX*XCOSOM) + XMAGC
          YTRK(1) = (RMAX*YCOSOM) + YMAGC
          ZTRK(1) = (RMAX*ZCOSOM) + ZMAGC
          IF(IVIEW.EQ.7.OR.IVIEW.EQ.8) THEN
            YTRK(1)=-YTRK(1)
            YTRK(2)=-YTRK(2)
            YTRK(3)=-YTRK(3)
          ELSE IF(IVIEW.EQ.9.OR.IVIEW.EQ.10) THEN
            ZTRK(1)=-ZTRK(1)
            ZTRK(2)=-ZTRK(2)
            ZTRK(3)=-ZTRK(3)
          ELSE IF(IVIEW.EQ.11.OR.IVIEW.EQ.12) THEN
            XTRK(1)=-XTRK(1)
            XTRK(2)=-XTRK(2)
            XTRK(3)=-XTRK(3)
          ENDIF
C
          CALL PXITOC(K,2,CCTRK)
          CALL JSIZE(50.,50.)
          IF(IVIEW.EQ.1) THEN
            CALL JMOVE(ZTRK(1),YTRK(1)+5.)
            CALL J3STRG(CCTRK)
          ELSE IF(IVIEW.EQ.2) THEN
            CALL JMOVE(XTRK(1),YTRK(1)+5.)
            CALL J3STRG(CCTRK)
          ELSE IF(IVIEW.EQ.3) THEN
            CALL JMOVE(XTRK(1),ZTRK(1)+5.)
            CALL J3STRG(CCTRK)
          ENDIF
          IF (IVIEW .EQ. 1) CALL PXLINE(0,3,ZTRK,YTRK,XTRK)
          IF (IVIEW .EQ. 2) CALL PXLINE(0,3,XTRK,YTRK,ZTRK)
          IF (IVIEW .EQ. 3) CALL PXLINE(0,3,XTRK,ZTRK,YTRK)
          IF (IVIEW .EQ. 4) CALL PXLINE(0,3,YTRK,ZTRK,XTRK)
          IF (IVIEW .EQ. 5) CALL PXLINE(0,3,YTRK,XTRK,ZTRK)
          IF (IVIEW .EQ. 6) CALL PXLINE(0,3,ZTRK,XTRK,YTRK)
          IF (IVIEW .EQ. 7) CALL PXLINE(0,3,ZTRK,YTRK,XTRK)
          IF (IVIEW .EQ. 8) CALL PXLINE(0,3,XTRK,YTRK,ZTRK)
          IF (IVIEW .EQ. 9) CALL PXLINE(0,3,XTRK,ZTRK,YTRK)
          IF (IVIEW .EQ. 10) CALL PXLINE(0,3,YTRK,ZTRK,XTRK)
          IF (IVIEW .EQ. 11) CALL PXLINE(0,3,YTRK,XTRK,ZTRK)
          IF (IVIEW .EQ. 12) CALL PXLINE(0,3,ZTRK,XTRK,YTRK)
        ENDIF
   45 CONTINUE
C
C ****  Close segment
C
  900 CONTINUE
      CALL PUCLOSE        !(SEGMENT ISEGEV+IVIEW)
C
  990 CALL EZRSET                       ! Reset RCP bank
  999 RETURN
      END
