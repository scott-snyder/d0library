      SUBROUTINE PM3DTH
C----------------------------------------------------------------------
C-
C-  Purpose and Methods :
C-      1) Draws 3-D reconstructed muon tracks up to the A-layer.
C-         The switch to draw the track is:
C-              -- "MUON DRAW TRACKS"   1=draw / 0=don't draw
C-      2) Draws the hits from MUOH which are used and/or not used in
C-         determining the track.  The call is to PM3DHT. Switches are:
C-              a) "MUON DRAW HITS"
C-              b) "MUON HITS ON TKS"
C-      3) Draws the 3-D reconstructed muon track using information
C-         from either MUOT, MUON, or PMUO.  The switch is:
C-              -- "MUON TK BANK"  1=MUOT / 2=MUON / 3=PMUO / 4=MUCD
C-
C-
C-   Created  07-NOV-1990   CARY YOSHIKAWA
C-   Updated  26-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK
C-   Updated  18-SEP-1991   Nobu Oshima( No LEGENDPT in Combined Menus )
C-   Updated  ??-OCT-1991   Nobu Oshima/C.Y.( Add muon track list. )
C-   Updated  25-OCT-1991   C.Y.  Killed PMUO & MUON dependence on MUOT.
C-                                Eliminate use of ORIGIN in PMUO & MUON.
C-   Updated  23-MAR-2004   compile with g77.
C
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC'
C
C    Local Declarations:
C    ===================
      INTEGER QUAD,LMUHT,GZMUHT,NSAMUS,IFW3,ISPARE
      INTEGER K                             ! Loop variables
      INTEGER NTRACK                        ! No. tracks
      INTEGER NPTRK                         ! No. of points on track
      INTEGER IFW1,IFW2,D3TRK,IER
      INTEGER LMUON,GZMUON,LPMUO,GZPMUO,LMUCD,GZMUCD
      INTEGER MUBANK                        ! 1=MUOT,2=MUON,3=PMUO
      INTEGER IDBOX,IPOS,TRKNUM, I,TKLAB
      REAL XTRK(2),YTRK(2),ZTRK(2),XTRKO(2),YTRKO(2),ZTRKO(2)
      REAL XMAGC,YMAGC,ZMAGC,ELCAL,ELFE,SPARE1
      REAL XCOSOM,YCOSOM,ZCOSOM,SPARE2
      REAL XCOSIM,YCOSIM,ZCOSIM
      REAL CHSQBV,CHSNBV,MOM,MOMER
      REAL XI,YI,ZI,PX,PY,PZ,PT,PTMIN,DPT
      REAL DISTALYR,DISTMC,DISTAMC
      REAL MARKSIZE,XCHARSIZE,YCHARSIZE
      REAL XO,YO,ZO,AORG(3),XMN,XMX,YMN,YMX,SIGN
      REAL XA_PMUO,YA_PMUO,ZA_PMUO,PHITK
      REAL XMAX_MUOT,YMAX_MUOT,ZMAX_MUOT,RMAX
      CHARACTER*3 JCOL,CNUM0,CTRKNUM0
      CHARACTER*8 CNUM,CTRKNUM
      CHARACTER*30 STR1
      CHARACTER*22 LINE(40)
      CHARACTER*4 BKNAME
      LOGICAL ALYR,EZERROR,MUONLY,MUFIT,PU_PICK_ACTIVE
C
      DATA MARKSIZE/5./
      DATA XCHARSIZE,YCHARSIZE/10,10/
      DATA XMAX_MUOT,YMAX_MUOT,ZMAX_MUOT/581.66,581.66,965.2/
      DATA XA_PMUO,YA_PMUO,ZA_PMUO/309.88,281.94,467.36/
      DATA LINE(2) /' TRKID     P      PT  '/
C----------------------------------------------------------------------
C-
C--- Check do picking...
C-
      IF ( PU_PICK_ACTIVE() ) THEN
        GO TO 999
      ENDIF
C---
C
C ****  Select PIXIE RCP bank
C
      CALL EZPICK('PX_MUODIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PM3DTH',
     &    'Unable to pick RCP bank PX_MUODIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('TRACK PTMIN',PTMIN)
      CALL PUGETV('TRACK DPT',DPT)
      CALL PUGETV('MUON ONLY',MUONLY)
      CALL PUGETV ('MUON TK BANK',MUBANK)
      CALL PUGETV('MUON TK FIT',MUFIT)
      CALL PUGETV('MUON LABEL TKS',TKLAB)
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
C
C    DRAW HITS IF DESIRED
C    ====================
      CALL PM3DHT
C
C    Executable Code:
C    ================

C
C    IF DON'T DESIRE TRACK, SKIP DRAWING TRACK
C    =========================================
      CALL PUGETV ('MUON DRAW TRACKS',D3TRK)
      IF (D3TRK.EQ.0) GOTO 900           ! 1=DRAW / 0=DON'T DRAW
C
C    Get number of tracks from ZEBRA banks
C    =====================================
      IF (MUBANK.EQ.1) THEN
        CALL GTMTRH(NTRACK)
        IF(NTRACK.LE.0)GO TO 900
      ENDIF
      CALL PUOPEN
      IPOS=2
C
C    Loop over hit on tracks and tracks and draw them
C    ================================================
      K = 1
C
C
C    FIND WHICH MUON BANK TO USE
C    ===========================
      lmucd = 0
      lpmuo = 0
      lmuon = 0
 800  continue
      IF (MUBANK.LE.1) THEN
C
C    GET MOMENTUM INFORMATION FROM MUOT
C    ==================================
   60   CONTINUE
        CALL GTMUOT(K,NPTRK,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,
     X    XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     X    YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,SPARE1,
     X    SPARE2)
C-
C--- Reset IFW1 flag
C-
        IFW1 = MOD(IFW1,10)
C-
C    DRAW GOOD FIT TRACKS ONLY
C    =======================================================
        IF (MUFIT .AND. (IFW2.GT.0))   GO TO 45
        IF (CHSQBV.GT.900.)              GO TO 45
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
        PHITK=ATAN2(PY,PX)
      ELSEIF (MUBANK.EQ.2) THEN
C
C    GET MOMENTUM INFORMATION FROM MUON
C    ==================================
        if (lmuon .eq. 0) LMUON=GZMUON(1)
        IF (LMUON.EQ.0) THEN
          CALL JRCLOS
          CALL PUMESS('MUON BANK DOES NOT EXIST')
          GOTO 900
        ENDIF
   70   CONTINUE
        PX=Q(LMUON+11)
        PY=Q(LMUON+12)
        PZ=Q(LMUON+13)
        MOM=Q(LMUON+14)
        IF (MOM.EQ.0) THEN
          CALL ERRMSG('PIXIE','PMEVNTC','MOM=0 IN MUON BANK','W')
          GOTO 45
        ENDIF
        SIGN=MOM/ABS(MOM)
        MOM=ABS(MOM)
        PT=Q(LMUON+15)
C          ORIGIN=IQ(LMUON+8)
C          ORIGIN=1                      ! TEMPORARY TILL MURECO FIXED
        XO=Q(LMUON+37)
        YO=Q(LMUON+38)
        ZO=Q(LMUON+39)
        XCOSIM=PX/MOM
        YCOSIM=PY/MOM
        ZCOSIM=PZ/MOM
        PHITK=ATAN2(PY,PX)
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
   80   CONTINUE
        PX=Q(LPMUO+10)
        PY=Q(LPMUO+11)
        PZ=Q(LPMUO+12)
        MOM=Q(LPMUO+13)
        IF (MOM.EQ.0) THEN
          CALL ERRMSG('PIXIE','PMEVNTC','MOM=0 IN PMUO BANK','W')
          GOTO 45
        ENDIF
        SIGN=MOM/ABS(MOM)
        MOM=ABS(MOM)
        PT=Q(LPMUO+14)
C          ORIGIN=IQ(LPMUO+5)
C          ORIGIN=1                      ! TEMPORARY TILL MURECO FIXED
        XO=Q(LPMUO+25)
        YO=Q(LPMUO+26)
        ZO=Q(LPMUO+27)
        XCOSIM=PX/MOM
        YCOSIM=PY/MOM
        ZCOSIM=PZ/MOM
        PHITK=ATAN2(PY,PX)
      ELSEIF (MUBANK.EQ.4) THEN
C
C    GET MOMENTUM INFORMATION FROM MUCD (NO MOMENTUM INFORMATION)
C    ==================================
        if (lmucd .eq. 0) LMUCD=GZMUCD(1)
        IF (LMUCD.EQ.0) THEN
          CALL JRCLOS
          CALL PUMESS('MUCD BANK DOES NOT EXIST')
          GOTO 900
        ENDIF
   90   CONTINUE
        XO=Q(LMUCD+4)
        YO=Q(LMUCD+5)
        ZO=Q(LMUCD+8)
        PHITK=ATAN2(YO,XO)
      ELSE
        CALL JRCLOS
        CALL PUMESS('NO MUON BANK CHOSEN')
        GOTO 900
      ENDIF
C     SET COLOR OF TRACKS BY THE PT VALUE IF MUON ONLY, OTHERWISE GREEN
C     ===================================
      IF (MUBANK.EQ.4) THEN           ! MUCD HAS NO MOMENTUM INFO.
        JCOL='GRE'
        GOTO 20
      ENDIF
      IF (.NOT. MUONLY) THEN
        IPOS = IPOS + 1
        WRITE(LINE(IPOS),110) K,SIGN*MOM,PT
  110   FORMAT(I4,2X,2F8.2)
      ENDIF
      IF (PT.LT.PTMIN) GO TO 45
      IF(MUONLY)THEN
        IF (PT.LT.PTMIN+DPT) THEN
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
      ELSE
        JCOL='GRE'
      ENDIF
   20 CONTINUE
C        JCOL='GRE'                      ! TEMPORARY FOR B/W SCREEN
      CALL PXCOLR(JCOL)
C
C       CALCULATE ENDPOINTS OF TRACK(S) TO BE DRAWN
C       ===========================================
      IF (MUBANK.LE.1) THEN
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
          IF (ABS(PT).GT.ABS(PZ)) THEN           ! CENTRAL TRACK
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
C       IF THERE'S A HIT IN A-LAYER, INNER TRACK (TRK) IS EXTRAPOLATED
C       OUTWARD TO MAGNET CENTER (TRK(2)) & INWARD TO INTERACTION REGION
C       (TRK(1)).
C       =================================================================
        IF (IFW1.EQ.1 .OR. IFW1.EQ.4) THEN    ! NO A-LAYER HIT
          XTRK(2)=XMAGC               ! ONLY FOR NUMBER DIRECTION
          YTRK(2)=YMAGC               ! ONLY FOR NUMBER DIRECTION
          ZTRK(2)=ZMAGC               ! ONLY FOR NUMBER DIRECTION
        ELSE                                  ! HAVE A-LAYER HIT
          XTRK(2)=XI+DISTAMC*XCOSIM
          YTRK(2)=YI+DISTAMC*YCOSIM
          ZTRK(2)=ZI+DISTAMC*ZCOSIM
          XTRK(1)=XI-DISTALYR*XCOSIM
          YTRK(1)=YI-DISTALYR*YCOSIM
          ZTRK(1)=ZI-DISTALYR*ZCOSIM
        ENDIF
      ELSEIF (MUBANK.EQ.2.OR.MUBANK.EQ.3) THEN
C          IF (ORIGIN.EQ.1) THEN         ! ORIGIN=VERTEX
        IF (ABS(PZ).GT.ABS(PT)) THEN        ! END TRACK
          DISTALYR=ABS(ZA_PMUO/ZCOSIM)
        ELSE                                ! CENTRAL TRACK
          IF ((1./4.)*PI.LE.PHITK.AND.PHITK.LE.(3./4.)*PI .OR.
     &           (-3./4.)*PI.LT.PHITK.AND.PHITK.LT.(-1./4.)*PI ) THEN
            DISTALYR=ABS(YA_PMUO/YCOSIM)
          ELSE
            DISTALYR=ABS(XA_PMUO/XCOSIM)
          ENDIF
        ENDIF
        XTRK(1)=XO
        YTRK(1)=YO
        ZTRK(1)=ZO
        XTRK(2)=XO+DISTALYR*XCOSIM
        YTRK(2)=YO+DISTALYR*YCOSIM
        ZTRK(2)=ZO+DISTALYR*ZCOSIM
C          ELSEIF (ORIGIN.EQ.2) THEN     ! ORIGIN=A-LAYER HIT
C            DISTALYR=SQRT(XO**2+YO**2+ZO**2)
C            XTRK(1)=XO-DISTALYR*XCOSIM
C            YTRK(1)=YO-DISTALYR*YCOSIM
C            ZTRK(1)=ZO-DISTALYR*ZCOSIM
C            XTRK(2)=XO
C            YTRK(2)=YO
C            ZTRK(2)=ZO
C          ELSE
C            CALL JRCLOS
C            CALL PUMESS(
C     &        'ORIGIN OF MUON TRACK IN MUON OR PMUO NOT DEFINED')
C            GOTO 900
C          ENDIF
      ELSEIF (MUBANK.EQ.4) THEN
        XTRK(1)=0
        YTRK(1)=0
        ZTRK(1)=0
        XTRK(2)=XO
        YTRK(2)=YO
        ZTRK(2)=ZO
      ELSE
        CALL JRCLOS
        CALL PUMESS('NO MUON BANK CHOSEN')
        GOTO 900
      ENDIF
C       DRAW THE TRACK
C       ==============
      IF (.NOT.(MUBANK.EQ.1.AND.(IFW1.EQ.1.OR.IFW1.EQ.4)))
     &    CALL PXLINE(0,2,XTRK,YTRK,ZTRK)
      IF (MUBANK.LE.1) CALL PXLINE(0,2,XTRKO,YTRKO,ZTRKO)
C
C       DRAW THE TRACK NUMBER AND MOM
C       =====================
      CALL J3MOVE(XTRK(2),YTRK(2),ZTRK(2))
      TRKNUM=K
      CALL JSIZE(XCHARSIZE,YCHARSIZE)
      IF(TKLAB.EQ.2.AND.PT.GT.0.AND.PT.LT.100.)THEN
        WRITE(CNUM,25) TRKNUM, PT
        READ(CNUM,35) CTRKNUM
   25   FORMAT(I3,1X,F4.0)
   35   FORMAT(A8)
        CALL J3STRG(CTRKNUM)
      ELSEIF (TKLAB.GT.0)THEN
        WRITE(CNUM0,26)TRKNUM
        READ(CNUM0,36)CTRKNUM0
   26   FORMAT(I3)
   36   FORMAT(A3)
        CALL J3STRG(CTRKNUM0)
      ENDIF
C
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
      ELSEIF (MUBANK.EQ.4) THEN
        LMUCD=GZMUCD(K)
        IF(LMUCD.NE.0) GOTO 800
      ENDIF
C
   50 CALL JRCLOS
      IF (MUBANK.EQ.4) GOTO 900         ! NO PTMIN FOR MUCD
      IF(D3TRK.NE.0)THEN
        IF (MUONLY) THEN
          CALL LEGENDPT
        ELSE
C.N.O.          CALL J4RGET(1, XMN, XMX, YMN, YMX)
C.N.O.          AORG(1) = XMN + (XMX-XMN)*0.01
C.N.O.          AORG(2) = YMN + (YMX-YMN)*0.93
C.N.O.          AORG(3) = 0.
C.N.O.          CALL PUTEXT_CREATE(IDBOX)
C.N.O.          CALL PUTEXT_SET(IDBOX,'P',AORG)
C.N.O.          CALL PUTEXT(IDBOX,LINE,IPOS)
          DO I = 1,IPOS
            CALL INTMSG(LINE(I))
          ENDDO
        ENDIF
      ENDIF
C
C
C ****  Reset RCP bank
C
  900 CONTINUE
      CALL EZRSET
  999 RETURN
      END
