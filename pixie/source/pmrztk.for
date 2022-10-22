      SUBROUTINE PMRZTK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the side view (R vs. Z) of muon tracks
C-      using the reconstructed MUOT zebra bank.  It collapses the phi
C-      value either upward or downward, depending upon which selected
C-      region the track lies.  The phi cut is determined by the
C-      calorimeter.  The track number will be negative if there is
C-      no A-layer hit in MUOT.
C-
C-      The 3-D reconstructed muon track is drawn using information
C-      from either MUOT, MUON, or PMUO.  The switch is:
C-              -- "MUON TK BANK"  1=MUOT / 2=MUON / 3=PMUO / 4=MUCD
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-SEP-1990   Cary Y. Yoshikawa
C-   Updated  26-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK
C-   Updated  01-MAY-1991   Cary Y. Yoshikawa   MUON, PMUO updated.
C-   Updated  16-MAY-1991   N. Oshima (Global PHI handling by PX_SYSTEM_RCP)
C-   Updated  21-JUN-1991   C.Y. added rectangle to simulate cut of A-layer
C-                              with picked phi value set from endview.
C-   Updated  21-JUL-1992   P.Q. added PT to display near track number
C-   Updated  19-JAN-1993   N.O.,C.Y. Draw dashed lines for tracks inside
C-                          magnet w/ no A-layer hits.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C    Include Statements:
C    ===================
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PXMHTK.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
C    Local Declarations:
C    ===================
      INTEGER QUAD,XQUAD,LMUHT,GZMUHT,NSAMUS,IFW3,XIFW3,ISPARE
      INTEGER K,I                       ! Loop variable
      INTEGER NTRACK                        ! No. tracks
      INTEGER NMMOD,LPMUHT(310)   !GTMUHT
      INTEGER NPTRAK,XNPTRAK,TRKNUM
      INTEGER IFW1,XIFW1,IFW2,XIFW2,IFLAG,IVIEW
      INTEGER LMUON,GZMUON,LPMUO,GZPMUO,LMUCD,GZMUCD
      INTEGER MUBANK,DRAWTRK,IER
      INTEGER TKLAB
      REAL ZTRKI(2),RTRKI(2),ZTRKO(2),RTRKO(2),ZERO(2)
      REAL ELCAL,ELFE,SPARE1,SPARE2
      REAL XMAGC,YMAGC,ZMAGC,RMAGC,RMAX,XXMAGC,XYMAGC,XZMAGC
      REAL XCOSOM,YCOSOM,ZCOSOM,XXCOSOM,XYCOSOM,XZCOSOM
      REAL XCOSIM,YCOSIM,ZCOSIM,XXCOSIM,XYCOSIM,XZCOSIM
      REAL CHSQBV,CHSNBV,MOM,MOMER,XCHSQBV,XCHSNBV,XMOM,XMOMER
      REAL XA,YA,ZA,XO,YO,ZO,RO,XXA,XYA,XZA
      REAL PX,PY,PZ,PT,PTMIN,DPT,ORIGIN
      REAL PHIBIN,DISTALYR,RALYR,DISTAMC,DISTMC,ZCHARSIZE,RCHARSIZE
      REAL THETAB,THETA,PHI,ZNUM,RNUM,PHIPK,DPHIPK
      REAL XBOX_BDR,YBOX_BDR,RBOX,ZBOX,PHIBOX_BDR
      CHARACTER*3 JCOL,CNUM0,CTRKNUM0
      CHARACTER*8 CNUM,CTRKNUM
      CHARACTER*30 STR1
      REAL VWSAVE(85)
      LOGICAL ALYR,LPHITYP,EZERROR,MUONLY
C
      DATA ZERO/0,0/
      DATA RMAX/500/
      DATA ZCHARSIZE,RCHARSIZE/10.,10./
      DATA XBOX_BDR,YBOX_BDR,ZBOX/320.88,292.94,392./
C
C----------------------------------------------------------------------
C    Executable Code:
C    ================
      NTRACK=0
C
C ****  Select PIXIE RCP bank and getting all PIXIE parameters
C ****  RCP file
C
C--- CHECK LOCAL OR GLOBAL
      CALL EZPICK('PX_SYSTEM_RCP')          ! Selecting SYSTEM bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PMRZTK','Bank PX_SYSTEM_RCP NOT FOUND',
     &     'W')
        GOTO 999
      ENDIF
      CALL PUGET_l('PHI TYPE',LPHITYP)
      IF(LPHITYP) THEN           ! GLOBAL Mode
        CALL PUGETV('PHI CENTER',PHIPK)
        CALL PUGETV('PHI WIDTH',DPHIPK)
        IF (DPHIPK .GE. 87.1875) THEN
          PHIPK = 90.
          DPHIPK= 90.
        ENDIF
        PHIPK=RADIAN*PHIPK
        DPHIPK=RADIAN*DPHIPK
      ELSE                            ! DEFAULT Mode
        PHIPK  =PI/2.
        DPHIPK =PI/2.
      ENDIF
      CALL EZRSET
C---
      CALL EZPICK('PX_MUODIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PMRZTK',
     &    'Unable to pick RCP bank PX_MUODIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGET_i ('MUON DRAW TRACKS',DRAWTRK)
      CALL PUGET_i ('MUON TK BANK',MUBANK)
      CALL PUGETV('TRACK PTMIN',PTMIN)
      CALL PUGETV('TRACK DPT',DPT)
      CALL PUGET_i('MUON LABEL TKS',TKLAB)
C    Get number of tracks from ZEBRA banks
C    =====================================
      CALL GTMTRH(NTRACK)
      IF(NTRACK.LE.0)GO TO 900
C
C    IF DON'T DESIRE TRACK, SKIP DRAWING TRACK
C    =========================================
      IF (DRAWTRK.EQ.0) GOTO 900           ! 1=DRAW / 0=DON'T DRAW
      CALL PUOPEN
C
C
C    Loop over hits on tracks
C    ========================
      DO 45 K = 1,NTRACK
C
C    GET A-LAYER INFORMATION FROM MUOT
C    =================================
        CALL GTMUOT(K,XNPTRAK,XQUAD,NSAMUS,XIFW1,XIFW2,XIFW3,ISPARE,
     X                XXA,XYA,XZA,XXMAGC,
     X                XYMAGC,XZMAGC,XXCOSIM,XYCOSIM,XZCOSIM,XXCOSOM,
     X                XYCOSOM,XZCOSOM,XCHSQBV,XCHSNBV,XMOM,XMOMER,
     X                ELCAL,ELFE,SPARE1,SPARE2)
C-
C--- Reset XIFW1 here
C-
       XIFW1 = MOD(XIFW1,10)
C
C    FIND WHICH MUON BANK TO USE
C    ===========================
        IF (MUBANK.LE.1) THEN
C
C    READ INFORMATION FROM RECONSTRUCTED MUOT BANK
C    =============================================
          CALL GTMUOT(K,NPTRAK,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,
     X                  XA,YA,ZA,XMAGC,
     X                  YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     X                  YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,
     X                  ELCAL,ELFE,SPARE1,SPARE2)
          MOM=ABS(MOM)
        ELSEIF (MUBANK.EQ.2) THEN
C
C    GET MOMENTUM INFORMATION FROM MUON
C    ==================================
          LMUON=GZMUON(K)
          IF (LMUON.EQ.0) THEN
            CALL JRCLOS
            CALL PUMESS('MUON BANK DOES NOT EXIST')
            GOTO 900
          ENDIF
          PX=Q(LMUON+11)
          PY=Q(LMUON+12)
          PZ=Q(LMUON+13)
          MOM=ABS(Q(LMUON+14))
          PT=Q(LMUON+15)
          ORIGIN=IQ(LMUON+8)
          XO=Q(LMUON+37)
          YO=Q(LMUON+38)
          ZO=Q(LMUON+39)
          IF (MOM.EQ.0) THEN
            CALL INTMSG('MOM=0 IN MUON BANK')
            GOTO 45
          ENDIF
          XCOSIM=PX/MOM
          YCOSIM=PY/MOM
          ZCOSIM=PZ/MOM
        ELSEIF (MUBANK.EQ.3) THEN
C
C    GET MOMENTUM INFORMATION FROM PMUO
C    ==================================
          LPMUO=GZPMUO(K)
          IF (LPMUO.EQ.0) THEN
            CALL JRCLOS
            CALL INTMSG('PMUO BANK DOES NOT EXIST')
            GOTO 900
          ENDIF
          PX=Q(LPMUO+10)
          PY=Q(LPMUO+11)
          PZ=Q(LPMUO+12)
          MOM=ABS(Q(LPMUO+13))
          PT=Q(LPMUO+14)
          ORIGIN=IQ(LPMUO+5)
          XO=Q(LPMUO+25)
          YO=Q(LPMUO+26)
          ZO=Q(LPMUO+27)
          IF (MOM.EQ.0) THEN
            CALL INTMSG('MOM=0 IN PMUO BANK')
            GOTO 45
          ENDIF
          XCOSIM=PX/MOM
          YCOSIM=PY/MOM
          ZCOSIM=PZ/MOM
        ELSEIF (MUBANK.EQ.4) THEN
C
C    GET MOMENTUM INFORMATION FROM MUCD (NO MOMENTUM INFORMATION)
C    ==================================
          LMUCD=GZMUCD(K)
          IF (LMUCD.EQ.0) THEN
            CALL JRCLOS
            CALL INTMSG('MUCD BANK DOES NOT EXIST')
            GOTO 900
          ENDIF
          RO=Q(LMUCD+7)
          ZO=Q(LMUCD+8)
          PHI=Q(LMUCD+3)
        ELSE
          CALL JRCLOS
          CALL INTMSG('NO MUON BANK CHOSEN')
          GOTO 900
        ENDIF
C
C     SET COLOR OF TRACKS BY THE PT VALUE IF MUON ONLY=.TRUE.
C     ===================================
        CALL PUGET_l('MUON ONLY',MUONLY)
        IF (MUBANK.EQ.4) THEN           ! MUCD HAS NO MOMENTUM INFO.
          JCOL='YEL'
          GOTO 15
        ENDIF
        PX=MOM*XCOSIM
        PY=MOM*YCOSIM
        PT=SQRT(PX**2+PY**2)
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
        IF( .NOT. MUONLY)THEN
          JCOL='GRE'
        ENDIF
   15   CONTINUE
        CALL PXCOLR(JCOL)
C
        CALL JSIZE(10.,10.)
        THETAB=0.6400882             ! CC & EC BORDER IN GRAPHICS
                                     ! ONLY
        IF (MUBANK.NE.4) THEN
          IF (PX.EQ.0.AND.PY.EQ.0) THEN
            CALL INTMSG('PX=PY=0')
            GOTO 45
          ENDIF
          PHI=ATAN2(PY,PX)
        ENDIF
C
C       DETERMINE WHETHER MUON IS IN UPPER, LOWER, OR NEITHER PICKED
C       REGIONS
C       ============================================================
C
        IF (-PI.LE.PHIPK.AND.PHIPK.LT.-PI+DPHIPK) THEN
          IF (-PI.LE.PHI.AND.PHI.LT.PHIPK+DPHIPK.OR.PHIPK-DPHIPK+2*PI.
     &        LE.PHI.AND.PHI.LE.PI) GOTO 20             ! UPPER RANGE
          IF (PHIPK-DPHIPK+PI.LE.PHI.AND.PHI.LT.PHIPK+DPHIPK+PI)
     &        GOTO 30                                   ! LOWER RANGE
          GOTO 45                                     ! OUT OF RANGE
        ENDIF
        IF (-PI+DPHIPK.LE.PHIPK.AND.PHIPK.LT.-DPHIPK) THEN
          IF (PHIPK-DPHIPK.LE.PHI.AND.PHI.LT.PHIPK+DPHIPK)
     &        GOTO 20                                   ! UPPER RANGE
          IF (PHIPK-DPHIPK+PI.LE.PHI.AND.PHI.LT.PHIPK+DPHIPK+PI)
     &        GOTO 30                                   ! LOWER RANGE
          GOTO 45                                     ! OUT OF RANGE
        ENDIF
        IF (-DPHIPK.LE.PHIPK.AND.PHIPK.LT.DPHIPK) THEN
          IF (PHIPK-DPHIPK.LE.PHI.AND.PHI.LT.PHIPK+DPHIPK)
     &        GOTO 20                                   ! UPPER RANGE
          IF (PHIPK-DPHIPK+PI.LE.PHI.AND.PHI.LE.PI
     &        .OR.-PI.LE.PHI.AND.PHI.LT.PHIPK+DPHIPK-PI)
     &        GOTO 30                                   ! LOWER RANGE
          GOTO 45                                     ! OUT OF RANGE
        ENDIF
        IF (DPHIPK.LE.PHIPK.AND.PHIPK.LT.PI-DPHIPK) THEN
          IF (PHIPK-DPHIPK.LE.PHI.AND.PHI.LT.PHIPK+DPHIPK)
     &        GOTO 20                                   ! UPPER RANGE
          IF (PHIPK-DPHIPK-PI.LE.PHI.AND.PHI.LT.PHIPK+DPHIPK-PI)
     &        GOTO 30                                   ! LOWER RANGE
          GOTO 45                                     ! OUT OF RANGE
        ENDIF
        IF (PI-DPHIPK.LE.PHIPK.AND.PHIPK.LE.PI) THEN
          IF (PHIPK-DPHIPK.LE.PHI.AND.PHI.LE.PI
     &        .OR.-PI.LE.PHI.AND.PHI.LE.PHIPK+DPHIPK-2*PI)
     &        GOTO 20                                   ! UPPER RANGE
          IF (PHIPK-DPHIPK-PI.LE.PHI.AND.PHI.LE.PHIPK+DPHIPK-PI)
     &        GOTO 30                                   ! LOWER RANGE
          GOTO 45                                     ! OUT OF RANGE
        ENDIF
C
C       FOR MUON IN UPPER PICKED REGION, INTERPOLATE TRACK & CALCULATE
C       POSITION OF NUMBER LABEL
C       ==============================================================
   20   CONTINUE                      ! MUON IS IN UPPER PICKED REGION
C
C       CALCULATE ENDPOINTS OF TRACK(S) TO BE DRAWN
C       ===========================================
        DISTMC=SQRT(XXMAGC**2+XYMAGC**2+XZMAGC**2)
        DISTALYR=SQRT(XXA**2+XYA**2+XZA**2)
        IF (MUBANK.LE.1) THEN
C
C       POINT TRKO(1) IS AT THE MAGNET CENTER
          IF (IFW1 .NE. 5) THEN
            RMAGC=SQRT(XMAGC**2+YMAGC**2)
            RTRKO(1)=RMAGC
            ZTRKO(1)=ZMAGC
C       POINT TRKO(2) IS EXTRAPOLATED OUTWARD FROM MAGNET CENTER
            RTRKO(2)=RMAX
            ZTRKO(2)=ZMAGC+(RMAX-RMAGC)*ZCOSOM/SQRT(XCOSOM**2+YCOSOM**2)
          ELSE
            DO I=1,2
              RTRKO(I)=0.
              ZTRKO(I)=0.
            ENDDO
          ENDIF
C
C       IF THERE'S A HIT IN THE A-LAYER, INNER TRACK (TRKI) IS EXTRAPOLATED
C       FROM A-LAYER HIT OUTWARD TO MAGNET CENTER (TRKI(2)) & INWARD TO
C       INTERACTION REGION (TRKI(1)).  IF NO HIT IN A-LAYER, INNER TRACK
C       IS A LINE EXTENDING THE MAGNETIC CENTER HIT INWARD.
C       ===================================================================
          RALYR=SQRT(XA**2+YA**2)
          DISTAMC=SQRT((XMAGC-XA)**2+(YMAGC-YA)**2+(ZMAGC-ZA)**2)
          IF (IFW1.EQ.1 .OR. IFW1.EQ.4) THEN           ! NO A-LAYER HIT
            RTRKI(2)=RMAGC
            ZTRKI(2)=ZMAGC
            RTRKI(1)=RMAGC-DISTMC*SQRT(XCOSOM**2+YCOSOM**2)
            ZTRKI(1)=ZMAGC-DISTMC*ZCOSOM
          ELSE                          ! HAVE A-LAYER HIT
            RTRKI(2)=RALYR+DISTAMC*SQRT(XCOSIM**2+YCOSIM**2)
            ZTRKI(2)=ZA+DISTAMC*ZCOSIM
            RTRKI(1)=RALYR-DISTALYR*SQRT(XCOSIM**2+YCOSIM**2)
            ZTRKI(1)=ZA-DISTALYR*ZCOSIM
          ENDIF
        ELSEIF (MUBANK.EQ.2.OR.MUBANK.EQ.3) THEN
          IF (ORIGIN.EQ.1) THEN                 ! ORIGIN=VERTEX
            RTRKI(1)=SQRT(XO**2+YO**2)
            ZTRKI(1)=ZO
            RTRKI(2)=RTRKI(1)+DISTMC*SQRT(XCOSIM**2+YCOSIM**2)
            ZTRKI(2)=ZO+DISTMC*ZCOSIM
          ELSEIF (ORIGIN.EQ.2) THEN             ! ORIGIN= MUON SYSTEM
            RTRKI(2)=SQRT(XO**2+YO**2)
            ZTRKI(2)=ZO
            RTRKI(1)=RTRKI(2)-DISTALYR*SQRT(XCOSIM**2+YCOSIM**2)
            ZTRKI(1)=ZO-DISTALYR*ZCOSIM
          ELSE
            CALL JRCLOS
            CALL INTMSG(
     &          'ORIGIN OF MUON TRACK IN MUON OR PMUO NOT DEFINED')
            GOTO 900
          ENDIF
        ELSEIF (MUBANK.EQ.4) THEN
          RTRKI(1)=0
          ZTRKI(1)=0
          RTRKI(2)=RO
          ZTRKI(2)=ZO
        ELSE
          CALL JRCLOS
          CALL PUMESS('NO MUON BANK CHOSEN')
          GOTO 900
        ENDIF
C
        THETA=ATAN2(RTRKI(2),ZTRKI(2))
C
        IF (0.LE.THETA.AND.THETA.LT.THETAB) THEN
          ZNUM=440.
          RNUM=(ZNUM/ZTRKI(2))*RTRKI(2)-RCHARSIZE
          GOTO 35
        ENDIF
        IF (THETAB.LE.THETA.AND.THETA.LT.(PI-THETAB)) THEN
          RNUM=340.
          ZNUM=(RNUM/RTRKI(2))*ZTRKI(2)
          IF (ZNUM.GE.0) ZNUM=ZNUM-3*ZCHARSIZE
          GOTO 35
        ENDIF
        IF ((PI-THETAB).LE.THETA.AND.THETA.LE.PI) THEN
          ZNUM=-470.
          RNUM=(RTRKI(2)/ZTRKI(2))*ZNUM
          GOTO 35
        ENDIF
C
C       FOR MUON IN LOWER PICKED REGION, INTERPOLATE TRACK & CALCULATE
C       POSITION OF NUMBER LABEL
C       ==============================================================
   30   CONTINUE                      ! MUON IS IN LOWER PICKED REGION
C
C       CALCULATE ENDPOINTS OF TRACK(S) TO BE DRAWN
C       ===========================================
        DISTMC=SQRT(XXMAGC**2+XYMAGC**2+XZMAGC**2)
        DISTALYR=SQRT(XXA**2+XYA**2+XZA**2)
        IF (MUBANK.LE.1) THEN
C
C       POINT TRKO(1) IS AT THE MAGNET CENTER
          IF (IFW1 .NE. 5) THEN
            RMAGC=SQRT(XMAGC**2+YMAGC**2)
            RTRKO(1)=-RMAGC
            ZTRKO(1)=ZMAGC
C       POINT TRKO(2) IS EXTRAPOLATED OUTWARD FROM MAGNET CENTER
            RTRKO(2)=-RMAX
            ZTRKO(2)=ZMAGC+(RMAX-RMAGC)*ZCOSOM/SQRT(XCOSOM**2+YCOSOM**2)
          ELSE
            DO I=1,2
              RTRKO(I)=0.
              ZTRKO(I)=0.
            ENDDO
          ENDIF
C
C       IF THERE'S A HIT IN THE A-LAYER, INNER TRACK (TRKI) IS EXTRAPOLATED
C       FROM A-LAYER HIT OUTWARD TO MAGNET CENTER (TRKI(2)) & INWARD TO
C       INTERACTION REGION (TRKI(1)).  IF NO HIT IN A-LAYER, INNER TRACK
C       IS A LINE EXTENDING THE MAGNETIC CENTER HIT INWARD.
C       ===================================================================
          RALYR=SQRT(XA**2+YA**2)
          DISTAMC=SQRT((XMAGC-XA)**2+(YMAGC-YA)**2+(ZMAGC-ZA)**2)
          IF (IFW1.EQ.1 .OR. IFW1.EQ.4) THEN           ! NO A-LAYER HIT
            RTRKI(2)=-RMAGC
            ZTRKI(2)=ZMAGC
            RTRKI(1)=-RMAGC+DISTMC*SQRT(XCOSOM**2+YCOSOM**2)
            ZTRKI(1)=ZMAGC-DISTMC*ZCOSOM
          ELSE                          ! HAVE A-LAYER HIT
            RTRKI(2)=-RALYR-DISTAMC*SQRT(XCOSIM**2+YCOSIM**2)
            ZTRKI(2)=ZA+DISTAMC*ZCOSIM
            RTRKI(1)=-RALYR+DISTALYR*SQRT(XCOSIM**2+YCOSIM**2)
            ZTRKI(1)=ZA-DISTALYR*ZCOSIM
          ENDIF
        ELSEIF (MUBANK.EQ.2.OR.MUBANK.EQ.3) THEN
          IF (ORIGIN.EQ.1) THEN                 ! ORIGIN=VERTEX
            RTRKI(1)=-SQRT(XO**2+YO**2)
            ZTRKI(1)=ZO
            RTRKI(2)=-(RTRKI(1)+DISTMC*SQRT(XCOSIM**2+YCOSIM**2))
            ZTRKI(2)=ZO+DISTMC*ZCOSIM
          ELSEIF (ORIGIN.EQ.2) THEN             ! ORIGIN= MUON SYSTEM
            RTRKI(2)=-SQRT(XO**2+YO**2)
            ZTRKI(2)=ZO
            RTRKI(1)=-(RTRKI(2)-DISTALYR*SQRT(XCOSIM**2+YCOSIM**2))
            ZTRKI(1)=ZO-DISTALYR*ZCOSIM
          ELSE
            CALL JRCLOS
            CALL INTMSG('ORIGIN NOT DEFINED IN MUON OR PMUO')
            GOTO 900
          ENDIF
        ELSEIF (MUBANK.EQ.4) THEN
          RTRKI(1)=0
          ZTRKI(1)=0
          RTRKI(2)=-RO
          ZTRKI(2)=ZO
        ELSE
          CALL JRCLOS
          CALL PUMESS('NO MUON BANK CHOSEN')
          GOTO 900
        ENDIF
C
        THETA=ATAN2(RTRKI(2),ZTRKI(2))
C
        IF (-PI.LE.THETA.AND.THETA.LT.(-PI+THETAB)) THEN
          ZNUM=-470.
          RNUM=(RTRKI(2)/ZTRKI(2))*ZNUM-ZCHARSIZE
          GOTO 35
        ENDIF
        IF ((-PI+THETAB).LE.THETA.AND.THETA.LT.-THETAB) THEN
          RNUM=-350.
          ZNUM=(RNUM/RTRKI(2))*ZTRKI(2)
          IF (ZNUM.LT.0) ZNUM=ZNUM-3*ZCHARSIZE
          GOTO 35
        ENDIF
        IF (-THETAB.LE.THETA.AND.THETA.LT.0) THEN
          ZNUM=440.
          RNUM=(RTRKI(2)/ZTRKI(2))*ZNUM
          GOTO 35
        ENDIF
C
C       DRAW TRACK AND LABEL NUMBER AND PT
C       ==================================
   35   CONTINUE
        CALL JMOVE(ZNUM,RNUM)
C       TRACK NUMBER WILL BE NEGATIVE IF IT HAS NO A-LAYER HIT
        IF (XIFW1.EQ.1 .OR. XIFW1.EQ.4) THEN
          TRKNUM=-K                   ! NO A-LAYER HIT
        ELSE
          TRKNUM=K                    ! HAVE A-LAYER HIT
        ENDIF
        IF(TKLAB.EQ.2.AND.PT.GT.0.AND.PT.LT.100.)THEN
          WRITE(CNUM,38) TRKNUM, PT
          READ(CNUM,42) CTRKNUM
   38     FORMAT(I3,1X,F4.0)
   42     FORMAT(A8)
          CALL J3STRG(CTRKNUM)
        ELSE IF(TKLAB.GT.0)THEN
          WRITE(CNUM0,43) TRKNUM
          READ(CNUM0,44) CTRKNUM0
   43     FORMAT(I3)
   44     FORMAT(A3)
          CALL J3STRG(CTRKNUM0)
        ENDIF
C
        IF (IFW1.EQ.1 .OR. IFW1.EQ.4) THEN  !NO A-LAYER
          CALL PXLINE(2,2,ZTRKI,RTRKI,ZERO)
        ELSE
          CALL PXLINE(0,2,ZTRKI,RTRKI,ZERO)
        ENDIF
        IF (MUBANK.LE.1) CALL PXLINE(0,2,ZTRKO,RTRKO,ZERO)
   45 CONTINUE
C
C       DRAW THE BOX WHICH REPRESENTS THE CUT VIEW OF THE A-LAYER
C
      IF( MUONLY ) THEN
        PHIBOX_BDR=ATAN2(YBOX_BDR,XBOX_BDR)
        IF (0.LE.PHIPK.AND.PHIPK.LE.PHIBOX_BDR .OR. (PI-PHIBOX_BDR).LE.
     &    PHIPK.AND.PHIPK.LE.PI) THEN
          RBOX=XBOX_BDR/(ABS(COS(PHIPK)))
        ELSEIF(PHIBOX_BDR.LT.PHIPK.AND.PHIPK.LT.(PI-PHIBOX_BDR)) THEN
          RBOX=YBOX_BDR/SIN(PHIPK)
        ELSE
          CALL INTMSG('ERROR IN PMRZRK--PHIPK NOT IN 0 TO PI RANGE')
        ENDIF
        JCOL='RED'
        CALL PXCOLR(JCOL)
        CALL JMOVE(ZBOX,RBOX)
        CALL JDRAW(ZBOX,-RBOX)
        CALL JDRAW(-ZBOX,-RBOX)
        CALL JDRAW(-ZBOX,RBOX)
        CALL JDRAW(ZBOX,RBOX)
      ENDIF
C
      CALL JRCLOS                       !(SEGMENT ISEGEV+IVIEW)
      IF (MUBANK.EQ.4) GOTO 900         ! NO PTMIN FOR MUCD
      IF (.NOT. MUONLY) GOTO 900
C
C       DRAW PT LEGEND
C       ===============================
      CALL LEGENDPT
C
  900 CONTINUE
      CALL EZRSET
  999 RETURN
      END
