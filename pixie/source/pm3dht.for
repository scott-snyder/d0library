      SUBROUTINE PM3DHT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the muon HITS in 3-D using the
C-     -MUOH zebra bank.
C-     -Assumes being called into NO open segment.
C-     -This version does NOT draw the drift ambiguities for those hits
C-        NOT used for tracks.  It draws the drift coord. on the wire.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-  NOTE : This routine will be updated to be able to use global fitted
C-         muon tracks in MUON to flag the hits on tracks from those
C-         not on the tracks.
C-
C-   Created  26-SEP-1990   Cary Y. Yoshikawa
C-   Updated  18-JUN-1991   C.Y.
C-                            -Killed drift ambiguities.
C-                            -Replaced J3POLY w/ J3PLGN for E&S emulator.
C-   Modified 30-OCT-1991   Nobu Oshima ( Fix the stinking bug. )
C-   Modified 30-JAN-1993   Lupe Howell Add new label to skip over hits on
C-          on tracks if no tracking bank exist
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C    Local Declarations:
C    ===================
C
      INTEGER FIRST,NMUSRT,NWD,L
      INTEGER NTRK,NPHITS,NMODHT,IMODHT,NMOD,FIRSTMODLOC,IPHITS
      INTEGER FIRSTMODNUMLOC,IPOINTNUM
      INTEGER LMUHT,LMUOH,LMUOF,LMTRH,LMUOT,LMHTT
      INTEGER ITRK,NHITS_ON_TRK,IHIT,IPOINT,MODNUM(500)
      INTEGER NHITS_TOTAL,GZMUHT,GZMTRH,IFLAG,IHWADD(40)
      INTEGER IDRIFTF,ABSIDRIFTF,ITDIVF,ORENT,ICELLHIT,NCELLHITS,J,SIGN
      INTEGER ALLHITS,HTONTK
      REAL DRIFTF,TDIVF
      REAL X,Y,Z,XFT(40),YFT(40),ZFT(40)
C      REAL X1(7),Y1(7),Z1(7),X2(5),Y2(5),Z2(5)
      REAL X1(4),Y1(4),Z1(4),X2(4),Y2(4),Z2(4)
      REAL MARKSIZE
      CHARACTER*3 JCOL
      REAL VWSAVE(85)
      LOGICAL ONTKHT,OFFTKHT,FITTED_HIT(1000,2)
C
      DATA FIRST,NWD/0,28/
      DATA MARKSIZE/5.0/
C
C----------------------------------------------------------------------
C    Executable Code:
C    ================
      CALL PUOPEN
C
C    SETUP ADDRESSES
C    ==============
      LMUHT=GZMUHT(0)
      IF (LMUHT.EQ.0) THEN
        CALL JRCLOS
        CALL PUMESS('MUHT BANK DOES NOT EXIST')
        GOTO 999
      ENDIF
      LMUOH=LQ(LMUHT-1)
      IF (LMUOH.EQ.0) THEN
        CALL JRCLOS
        CALL PUMESS('MUOH BANK DOES NOT EXIST')
        GOTO 999
      ENDIF
C Fix size so can read old MC
      IF(FIRST.EQ.0) THEN
        NMUSRT=IQ(LMUHT+2)
        IF(LMUHT.NE.0.AND.NMUSRT.NE.0.AND.LMUOH.NE.0) THEN
          NWD=28
          L=IQ(LMUOH-1)/NMUSRT
          IF(L.EQ.25) NWD=25
          FIRST=1
        ENDIF
      ENDIF
      LMUOF=LQ(LMUHT-2)
      IF (LMUOF.EQ.0) THEN
        CALL JRCLOS
        CALL PUMESS('MUOF BANK DOES NOT EXIST')
        GOTO 999
      ENDIF
      LMTRH=GZMTRH(0)
      IF (LMTRH.EQ.0) GOTO 5
      LMUOT=LQ(LMTRH-1)
      IF (LMUOT.EQ.0) GOTO 5
      LMHTT=LQ(LMUOT-1)
      IF (LMHTT.EQ.0) GOTO 5
C
    5 NHITS_TOTAL=IQ(LMUHT+2)
      IF (NHITS_TOTAL.GT.500) THEN
        CALL JRCLOS
        CALL PUMESS('ERROR IN PM3DHT-NUMBER OF HITS TOO LARGE')
        GOTO 999
      ENDIF
      DO 10 IHIT=1,NHITS_TOTAL
        DO 15 ITDIVF=1,2
          FITTED_HIT(IHIT,ITDIVF)=.FALSE.
   15   CONTINUE
   10 CONTINUE
C
C    GET MODULE NUMBER FOR EACH PROCESSED HIT
C    ========================================
      NMODHT=IQ(LMUHT+3)
      DO 20 IMODHT=1,NMODHT
        NMOD=IQ(LMUOF+1+10*(IMODHT-1))
        NPHITS=IQ(LMUOF+4+10*(IMODHT-1))
        FIRSTMODLOC=IQ(LMUOF+5+10*(IMODHT-1))
        FIRSTMODNUMLOC=INT(FIRSTMODLOC/NWD)+1
        DO 30 IPHITS=1,NPHITS
          MODNUM(FIRSTMODNUMLOC+IPHITS-1)=NMOD
   30   CONTINUE
   20 CONTINUE
C
C    GET SWITCHES
C    ============
      CALL PUGETV('MUON DRAW HITS',ALLHITS)
      CALL PUGETV('MUON HITS ON TKS',HTONTK)
      IF (ALLHITS.EQ.1) THEN
        ONTKHT=.TRUE.
        OFFTKHT=.TRUE.
      ELSEIF (ALLHITS.EQ.1.AND.HTONTK.EQ.0) THEN
        ONTKHT=.FALSE.
        OFFTKHT=.TRUE.
      ELSEIF (ALLHITS.EQ.0.AND.HTONTK.EQ.1) THEN
        ONTKHT=.TRUE.
        OFFTKHT=.FALSE.
      ELSEIF (ALLHITS.EQ.0.AND.HTONTK.EQ.0) THEN
        ONTKHT=.FALSE.
        OFFTKHT=.FALSE.
      ENDIF
C
C    SKIP OVER HITS ON TRACKS IF TRACKING BANKS DON'T EXIST
C    ======================================================
      IF (LMTRH.EQ.0.OR.LMUOT.EQ.0.OR.LMHTT.EQ.0) GOTO 45
C
C    FLAG WHICH HITS WERE USED BY TRACKS
C    ===================================
      NTRK=IQ(LMTRH+1)
      DO 40 ITRK=1,NTRK
        NHITS_ON_TRK=IQ(LMUOT+1)
        DO 50 IHIT=1,NHITS_ON_TRK
          IPOINT=IQ(LMHTT+2+5*(IHIT-1))
          ITDIVF=IQ(LMHTT+4+5*(IHIT-1))
          FITTED_HIT(IPOINT,ITDIVF)=.TRUE.
   50   CONTINUE
C
C    DRAW HITS WHICH ARE USED IN THE FIT
C    ===================================
        IF (ONTKHT) THEN
          IFLAG=1
          CALL GTMHOT(IFLAG,ITRK,NHITS_ON_TRK,XFT,YFT,ZFT,IHWADD)
          IF (NHITS_ON_TRK .GT. 40) THEN
            CALL JRCLOS
            CALL PUMESS('ERROR IN PM3DHT-NHITS_ON_TRK TOO LARGE')
            GOTO 999
          ENDIF
          JCOL='RED'
          CALL PXCOLR(JCOL)
          DO 65 IHIT=1,NHITS_ON_TRK
            IPOINT=IQ(LMHTT+2+5*(IHIT-1))
C.N.O.      IF (MODNUM(IPOINT).GE.100) GOTO 65 ! DON'T DRAW IF NOT A-LAYER
            CALL J3MOVE(XFT(IHIT)-.5*MARKSIZE,YFT(IHIT),ZFT(IHIT))
            CALL J3DRAW(XFT(IHIT)+.5*MARKSIZE,YFT(IHIT),ZFT(IHIT))
            CALL J3MOVE(XFT(IHIT),YFT(IHIT)-.5*MARKSIZE,ZFT(IHIT))
            CALL J3DRAW(XFT(IHIT),YFT(IHIT)+.5*MARKSIZE,ZFT(IHIT))
            CALL J3MOVE(XFT(IHIT),YFT(IHIT),ZFT(IHIT)-.5*MARKSIZE)
            CALL J3DRAW(XFT(IHIT),YFT(IHIT),ZFT(IHIT)+.5*MARKSIZE)
   65     CONTINUE
        ENDIF
        IF (ITRK.LT.NTRK) THEN
          LMUOT=LQ(LMUOT)
          LMHTT=LQ(LMUOT-1)
          IF (LMUOT.EQ.0) THEN
            CALL JRCLOS
            CALL PUMESS('NEXT MUOT BANK DOES NOT EXIST')
            GOTO 999
          ENDIF
          IF (LMHTT.EQ.0) THEN
            CALL JRCLOS
            CALL PUMESS('NEXT MHTT BANK DOES NOT EXIST')
            GOTO 999
          ENDIF
        ENDIF
   40 CONTINUE
C
C    DRAW HITS WHICH ARE NOT USED IN THE FIT
C    =======================================
   45 IF (OFFTKHT) THEN
        NHITS_TOTAL=IQ(LMUHT+2)
        DO 70 IHIT=1,NHITS_TOTAL
C.N.O.    IF (MODNUM(IHIT).GE.100) GOTO 70 ! DON'T DRAW IF NOT IN A-LAYER
          NCELLHITS=IQ(LMUOH+6+NWD*(IHIT-1))
          SIGN=0
          DO 80 ICELLHIT=1,NCELLHITS
            IF (FITTED_HIT(IHIT,ICELLHIT)) GOTO 70   ! DON'T DRAW IF

            CALL GTMPHT(IHIT,ICELLHIT,SIGN,X,Y,Z)
            JCOL='GRE'
            CALL PXCOLR(JCOL)
            CALL J3MOVE(X-.5*MARKSIZE,Y,Z)
            CALL J3DRAW(X+.5*MARKSIZE,Y,Z)
            CALL J3MOVE(X,Y-.5*MARKSIZE,Z)
            CALL J3DRAW(X,Y+.5*MARKSIZE,Z)
            CALL J3MOVE(X,Y,Z-.5*MARKSIZE)
            CALL J3DRAW(X,Y,Z+.5*MARKSIZE)
C
C     GET POINTS FOR J3PLGN
C
            X1(1)=X+.5*MARKSIZE
            Y1(1)=Y+.5*MARKSIZE
            Z1(1)=Z+.5*MARKSIZE
            X1(2)=X+.5*MARKSIZE
            Y1(2)=Y-.5*MARKSIZE
            Z1(2)=Z+.5*MARKSIZE
            X1(3)=X-.5*MARKSIZE
            Y1(3)=Y-.5*MARKSIZE
            Z1(3)=Z+.5*MARKSIZE
            X1(4)=X-.5*MARKSIZE
            Y1(4)=Y+.5*MARKSIZE
            Z1(4)=Z+.5*MARKSIZE
C
            X2(1)=X+.5*MARKSIZE
            Y2(1)=Y+.5*MARKSIZE
            Z2(1)=Z-.5*MARKSIZE
            X2(2)=X+.5*MARKSIZE
            Y2(2)=Y-.5*MARKSIZE
            Z2(2)=Z-.5*MARKSIZE
            X2(3)=X-.5*MARKSIZE
            Y2(3)=Y-.5*MARKSIZE
            Z2(3)=Z-.5*MARKSIZE
            X2(4)=X-.5*MARKSIZE
            Y2(4)=Y+.5*MARKSIZE
            Z2(4)=Z-.5*MARKSIZE
C
C     DRAW THE CUBES USING J3PLGN & CONNECTING LINES
C
            CALL J3PLGN(X1,Y1,Z1,4)
            CALL J3PLGN(X2,Y2,Z2,4)
            CALL J3MOVE(X1(1),Y1(1),Z1(1))
            CALL J3DRAW(X2(1),Y2(1),Z2(1))
            CALL J3MOVE(X1(2),Y1(2),Z1(2))
            CALL J3DRAW(X2(2),Y2(2),Z2(2))
            CALL J3MOVE(X1(3),Y1(3),Z1(3))
            CALL J3DRAW(X2(3),Y2(3),Z2(3))
            CALL J3MOVE(X1(4),Y1(4),Z1(4))
            CALL J3DRAW(X2(4),Y2(4),Z2(4))
C
   80     CONTINUE
   70   CONTINUE
      ENDIF
      CALL JRCLOS
C
  999 RETURN
      END
