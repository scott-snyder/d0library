      SUBROUTINE PM3DTK
C----------------------------------------------------------------------
C-
C-  Purpose and Methods : Draws a 3-D reconstructed muon track up to
C-                        A-layer using MUOT.
C-
C-  Created  12-OCT-1990   CARY YOSHIKAWA
C- T. McKibben 3-NOV-1993 Changed call to GTMUHT for run 1B compatibility
C-   Updated  15-NOV-1993   BH Added variable NMSCT to call GTMUHT
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C    Local Declarations:
C    ===================
      INTEGER QUAD,XQUAD,LMUHT,GZMUHT,NSAMUS,IFW3,ISPARE
C
C      INTEGER ISEGEV                        ! Retained segment index
      INTEGER I,J,K,L,M                     ! Loop variables
      INTEGER NTRACK                        ! No. tracks
      INTEGER TRKNUM                        ! TRACK LABEL NUMBER
      INTEGER NPTRK                         ! No. of points on track
      INTEGER NHCTR                         ! No. of hit trigger counters
      INTEGER NWRAW,NWPRO,NWMOD,NSRAW,NSPRO,NSMOD !Run 1B GTMUHT
      INTEGER NMUHP,NMUOF,NVERS,LPMUOF(460),NMSCT !Run 1B GTMUHT
      INTEGER IVIEW,MODK,NTEMP,IPHITS
      INTEGER IFLAG
      INTEGER IFW1,IFW2
      REAL XTRKI(2),YTRKI(2),ZTRKI(2),NM,MR            ! INNER TRACK
      REAL XTRKO(2),YTRKO(2),ZTRKO(2)                  ! OUTTER TRACK
      REAL XATRK(2),YATRK(2),ZATRK(2)
      REAL ELCAL,ELFE,SPARE1,SPARE2
      REAL XMAGC,YMAGC,ZMAGC
      REAL XCOSOM,YCOSOM,ZCOSOM
      REAL XCOSIM,YCOSIM,ZCOSIM
      REAL CHSQBV,CHSNBV,MOM,MOMER
      REAL RMAX,RMIN,YPMAX,YPMIN
      REAL XI,YI,ZI,PX,PY,PT,PTMIN,DPT,XCHARSIZE,YCHARSIZE
      REAL XHTRAK(40),YHTRAK(40),ZHTRAK(40),IHWADD(40),PI
      REAL DISTALYR,DISTAMC,XNUMDIR,YNUMDIR,PHINUM,RNUM,XNUM,YNUM
      CHARACTER*3 ICOL(6),JCOL,CNUM,CTRKNUM,MC
      CHARACTER*30 STR1
      REAL VWSAVE(85)
      LOGICAL ALAYER
C
C
C    Include Statements:
C    ===================
      INCLUDE 'D0$INC:GRAPHF77.INC/NOLIST'
      INCLUDE 'D0$INC:PXMHTK.INC/NOLIST'
      DATA RNUM/245/
C
C    Executable Code:
C    ================
      PI=3.1415927
      CALL PUOPEN
      NWRAW=0
      NTRACK=0
      NWPRO=0
C
C    Get processed hit info from ZEBRA bank
C    ======================================
      CALL GTMUHT(NWRAW,NWPRO,NWMOD,NSRAW,NSPRO,NSMOD,
     &            NMUHP,NMUOF,NMSCT,NVERS,LPMUOF)
C
C    Get number of tracks from ZEBRA banks
C    =====================================
      CALL GTMTRH(NTRACK)
C
C    Loop over hit on tracks and tracks and draw them
C    ================================================
      DO 45 K = 1,NTRACK
C
C    Draw the track
C    ==============
          CALL GTMUOT(K,NPTRK,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,
     X  XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     X  YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,SPARE1,
     X  SPARE2)
C
C     SET COLOR OF TRACKS BY THE PT VALUE
C     ===================================
        PX=MOM*XCOSIM
        PY=MOM*YCOSIM
        PT=SQRT(PX**2+PY**2)
        CALL PUGETV('TRACK PTMIN',PTMIN)
        CALL PUGETV('TRACK DPT',DPT)
        IF (PT.LT.PTMIN) THEN
          GOTO 45
        ELSEIF (PT.LT.PTMIN+DPT) THEN
          JCOL='BLU'
        ELSEIF (PT.LT.PTMIN+2*DPT) THEN
          JCOL='CYA'
        ELSEIF (PT.LT.PTMIN+3*DPT) THEN
          JCOL='GRE'
        ELSEIF (PT.LT.PTMIN+4*DPT) THEN
          JCOL='MAG'
        ELSE
          JCOL='RED'
        ENDIF
        CALL PXCOLR(JCOL)
C
C       CALCULATE ENDPOINTS OF TRACK(S) TO BE DRAWN
C       ===========================================
        DISTALYR=SQRT(XI**2+YI**2+ZI**2)    !
        XATRK(1)=XI-DISTALYR*XCOSIM         !
        YATRK(1)=YI-DISTALYR*YCOSIM         !  A_LAYER ONLY
        ZATRK(1)=ZI-DISTALYR*ZCOSIM         !
        XATRK(2)=XI                         !
        YATRK(2)=YI                         !
        ZATRK(2)=ZI                         !
C       DRAW THE TRACK
C       ==============
        CALL PXLINE(0,2,XATRK,YATRK,ZATRK)
C
   45 CONTINUE
C
      CALL JRCLOS                       ! (SEGMENT ISEGEV+IVIEW)
      WRITE (STR1,55) PTMIN
   55 FORMAT('TRK PTMIN = ',F6.2)
      CALL LEGENDPT
      CALL PUMESS(STR1)
C
  999 RETURN
      END
