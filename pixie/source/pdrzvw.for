      SUBROUTINE PDRZVW
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : display R-Z view of the CDC
C-                       If draw CDC only (no other detectors), the track
C-                       is extrapolated to the beam line
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  27-APR-1989   Qizhong Li-Demarteau
C-   Updated  01-AUG-1989   Qizhong Li-Demarteau  change parameters to integer
C-   Updated  10-OCT-1989   Qizhong Li-Demarteau  R-Z view split into two
C-                          hemisperes and select a phi region is available
C-   Updated  ??-???-1990   C. Yoshikawa add global PHI handling + ALLPHIPK
C-   Updated  25-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK
C-   Updated  16-MAY-1991   N. Oshima (Global PHI handling by PX_SYSTEM_RCP)
C-   Updated  09-AUG-1991   An-Dien Nguyen : change sizes and positions of
C-                          track numbers in RZ-view
C-       parallel version   S. Hagopian  roads, colors
C-   Updated  16-AUG-1991   T. Trippe  combine versions
C-   Updated  20-AUG-1991   Q.L./C.Y. (Fixed bug in Global PHI handling)
C-   Updated   3-SEP-1991   Qizhong Li-Demarteau  fix the bug introduced
C-                                                when adding roads
C-   Updated   9-SEP-1991   N. Oshima
C-      Remove ALLPHIPK, Check 'CDC ONLY' before using PX_SYSTEM and add
C-      global PHI handling of tracks.
C-   Updated  17-SEP-1991   Qizhong Li-Demarteau  fix bug in PHI handling
C-   Updated 18-SEP-1991   S. Hagopian, changed from ZTRAKS.RCP to  
C-      ZTRAKS.PARAMS. Added counter NROAD. If NROAD.NE.0, calls LEGEND_LINE
C-   Updated   4-OCT-1991   Lupe Howell Definition of tracks color based in
C-             device.  If in color device use colors otherwise line styles
C-   Updated   5-NOV-1991   Qizhong Li-Demarteau  use VEE bit from IQ(LDTRK)
C-   Updated   9-DEC-1991   Qizhong Li-Demarteau  draw hits from compressed
C-                                  hits bank if the DSEC banks do not exit
C-   Updated  29-JAN-1992   for TOP/SIDE VIEWS(By Sharon?)
C-   Updated   6-MAR-1992   Qizhong Li-Demarteau  removed machine block 
C-   Updated   5-AUG-1992   N. Oshima  Remove 'CALL PXCOLR('FOR')' at just
C-                                     after the label 250.
C-   Updated   9-FEB-1993   Jeffrey Bantly  add Level 0 slow vertex marker,
C-                                          values are multiple interaction
C-                                          flag of Slow Z (1-4) 
C-   Updated   8-FEB-1995   N. Oshima - fixed a typo on C(LDRFT+26+WIR[E]).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS/LIST'
C
      INTEGER KCDCH,NCDCHIT,MAXCHITS
      INTEGER LDRFT, LAYER, SECTOR, WIRE, IHIT, NHITS, IPT, KPDSEC
      INTEGER LAY, SEC, WIR, NUMHIT, KP, PLDTTH, NSWHIT, I, RZFLAG
      INTEGER DLNUM, HLABEL, LHIT, NFADC, IPHIT, LDALS, IPWIR, ITRK
      INTEGER IFDHIT, IFDTRK, IFDWIR, IFDLBL, IFDVER, IPATH, ERR, ISIDE
      INTEGER GZCDCH, GZDTRH, GZDSEC, GZDGEH, GZDRFT, GZDALS
      INTEGER DSECUL, DSECUH, DSECDL, DSECDH, DRUPRZ, DRDWRZ
      INTEGER LISV1, GZISV1, LVERH, GZVERH, NV, IV
      INTEGER IPHIPK,IDPHIPK, IER,NROAD,ICALL,KCOL,KINT,KFIL,KSTY
      INTEGER LDHIT, GZDHIT
      INTEGER MI_FLAG
C
      REAL    XMAX, YMAX, XMIN, YMIN, SIZSCAL, XSIZ, YSIZ
      REAL    PHIPK,DPHIPK
      REAL    Z1, Z2, R1, R2, ZPOS, RPOS, ZERROR, RHIT, ZHIT, ZCHISQ
      REAL    Z11, Z22, R11, R22, ZTRKNM, RTRKNM
      REAL    CPHIW, SPHIW, DDIS, XHPOS, YHPOS, HITPHI, RHPOS1, ZHPOS1
      REAL    RHPOS, ZHPOS, ZPOS1, ZPOS2, XWIR, YWIR, RWIR
      REAL    THETA, PHI1, PHI2, PHI3, PHI4, TRKPHI, CHKTRK, CHKHIT
      REAL    TLTRK, RTRKHI, RTRKLO
      REAL    SCALX, SCALY, THELMT
      REAL    XPOS, YPOS, ZVER(10), DZVER(10), ZISAJV, PRCENT, YXRATI
      REAL    RNMOFF, ZNMOFF
      PARAMETER( RNMOFF = 3.5 )
      PARAMETER( ZNMOFF = 0.0 )
      REAL    RCDCLO
      PARAMETER( RCDCLO = 0.0 )
      REAL    FASTZ, SLOWZ
      LOGICAL IFDCDC,LPHITYP,EZERROR,SETFLG(32)
      LOGICAL INEL,INMU,INTAU,INVEE
      LOGICAL FGOOD, SGOOD
C
      CHARACTER*80 TEXT
      CHARACTER*4  TRKNUM, DPATH
      CHARACTER*1  MIFC(4)
C
      EQUIVALENCE (IPATH,DPATH)
      SAVE ICALL
      DATA ICALL/0/
      DATA MIFC / '1','2','3','4'/
C----------------------------------------------------------------------
C
C ****  Get parameters from currently selected bank
C
      CALL PUGETV('CDC ONLY',IFDCDC )
      CALL PUGETV('CDC DRAW WIRES', IFDWIR )
      CALL PUGETV('CDC DRAW HITS',  IFDHIT )
      CALL PUGETV('CDC DRAW TRACK', IFDTRK )
      CALL PUGETV('CDC DRAW LABEL', IFDLBL )
      CALL PUGETV('CDC DRAW VERTEX',IFDVER )
      CALL PUGETV('CDC MAX HITS',MAXCHITS)
C Check number of hits in CDC
      KCDCH=GZCDCH(0)
      NCDCHIT=IQ(KCDCH+1)
      IF(NCDCHIT.GT.MAXCHITS)THEN
        CALL INTMSG(' !!!! WARNING - TOO MANY CDC HITS')
        GO TO 999
      ENDIF  
      NROAD=0
C
      IF (ICALL.EQ.0) THEN
        ICALL=1
        CALL EZPICK('DTRAKS_RCP')
        IF (EZERROR(IER)) THEN
          DPATH = 'RECO'
        ELSE
          CALL EZGET('DPATH',IPATH,ERR)
        ENDIF
        CALL EZRSET
      ENDIF
C-
C--- SET DEFAULT PHI1..4, THEN CHECK LOCAL OR GLOBAL
C-
      PHI1=0.
      PHI2=PI
      PHI3=PI
      PHI4=2.*PI
C-
      IF ( IFDCDC ) THEN
        CALL PDGPHI(PHI1,PHI2,PHI3,PHI4)
      ELSE
        CALL EZPICK('PX_SYSTEM_RCP')          ! Selecting SYSTEM bank
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PDRZVW','Bank PX_SYSTEM_RCP NOT FOUND',
     &     'W')
          GOTO 999
        ENDIF
        CALL PUGETV('PHI TYPE',LPHITYP)
        IF (LPHITYP) THEN                  ! GLOBAL Mode
          CALL PUGETV('PHI CENTER',PHIPK)
          CALL PUGETV('PHI WIDTH',DPHIPK)
          PHIPK = PHIPK + 2.8125
          IF (DPHIPK .GE. 87.1875) THEN
            DPHIPK= 90.
          ENDIF
C- DEFINE PHI1..4 BY PX_SYSTEM_RCP
          PHI1=RADIAN*(PHIPK-DPHIPK)
          PHI2=RADIAN*(PHIPK+DPHIPK)
          PHI3=PHI1+PI
          PHI4=PHI2+PI
        ELSE
          CALL PDGPHI(PHI1,PHI2,PHI3,PHI4)
        ENDIF
        CALL EZRSET
      ENDIF
C-
C--- SET PHI VALUES AND LOGICAL 'CDC' TO .TRUE.
C-
      CALL PDSTRD(PHI1,PHI2,PHI3,PHI4)
C
C   draw the beam line and collision point
C
      CALL PUOPEN
      CALL JLSTYL(0)         ! Setting the line style back to normal
      CALL JLWIDE(16383)     ! Setting line thickness to normal
      CALL PXCOLR('FOR')
      CALL JMOVE(-100.,-0.25)
      CALL JDRAW(100.0,-0.25)
      CALL JMOVE(-100.,0.25)
      CALL JDRAW(100.0,0.25)
      CALL JMOVE(0.0,3.0)
      CALL JDRAW(0.0,-3.0)
C
C    draw chamber in the R-Z view
C
      IF (LDGEH .LE. 0) LDGEH = GZDGEH()
      Z1 = C(LDGEH + 14)
      Z2 = -C(LDGEH + 14)
      R1 = C(LDGEH + 10)
      R2 = C(LDGEH + 12)
      IF (LDRFT .LE. 0) LDRFT = GZDRFT()
      CALL PXCOLR('BLU')
      CALL JRECT(Z1,R1,Z2,R2)
      Z11 = C(LDGEH + 15)
      Z22 = -C(LDGEH + 15)
      R11 = C(LDGEH + 9)
      R22 = C(LDGEH + 13)
      CALL JRECT(Z11,R11,Z22,R22)
      CALL JRECT(Z1,-R1,Z2,-R2)
      CALL JRECT(Z11,-R11,Z22,-R22)
C
C   draw the outer sense wires
C
      IF (IFDWIR .NE. 0) THEN
        DO 101 LAY = 0, 3
          LDALS = GZDALS(LAY,0)
          DO 102 WIR= 0,6
            IF (WIR .NE. 0 .AND. WIR .NE. 6) GOTO 102
            IPWIR = LDALS + 6 + IC(LDALS+6) * WIR
            XWIR = C(IPWIR+1)
            YWIR = C(IPWIR+2)
            RWIR = SQRT(XWIR ** 2 + YWIR **2)
            CALL JMOVE(Z2,RWIR)
            CALL JDRAW(Z1,RWIR)
            CALL JMOVE(Z2,-RWIR)
            CALL JDRAW(Z1,-RWIR)
  102     CONTINUE
  101   CONTINUE
      ENDIF
C
C   draw all Z hits
C
      IF (IFDHIT .NE. 0) THEN
        IF (LCDCH .EQ. 0) THEN
          CALL PATHST(DPATH)
          LCDCH = GZCDCH()
        ENDIF
        IF (LCDCH .EQ. 0) GOTO 301
C        IF (IQ(LCDCH+1) .EQ. 0) GOTO 301         ! No hit in the chamber...
        IF (PHI1.GE.0) THEN
          DSECUL = INT(PHI1 * 32 / TWOPI) - 1
        ELSE
          DSECUL = INT(PHI1 * 32 / TWOPI - 1.0) - 1
        ENDIF
        DSECUH = INT(PHI2 * 32 / TWOPI) + 1
        DSECDL = DSECUL + 16
        DSECDH = DSECUH + 16
C
        CALL PXCOLR('MAG')
C
        CALL VZERO(SETFLG,32)
        DO 600 SECTOR=0,31
          IF (SETFLG(SECTOR+1)) GOTO 600
          SETFLG(SECTOR+1)=.TRUE.
          IF ( LPHITYP ) THEN
            IF (DSECUL.LT.0) THEN
              DSECUL=DSECUL+32
              IF (DSECUH.LT.SECTOR.AND.SECTOR.LT.DSECDL)
     &            SETFLG(SECTOR+1)=.FALSE.
              IF (DSECDH.LT.SECTOR.AND.SECTOR.LT.DSECUL)
     &            SETFLG(SECTOR+1)=.FALSE.
              DSECUL=DSECUL-32
            ELSEIF (DSECDH.GT.31) THEN
              DSECDH=DSECDH-32
              IF (DSECDH.LT.SECTOR.AND.SECTOR.LT.DSECUL)
     &            SETFLG(SECTOR+1)=.FALSE.
              IF (DSECUH.LT.SECTOR.AND.SECTOR.LT.DSECDL)
     &            SETFLG(SECTOR+1)=.FALSE.
              DSECDH=DSECDH+32
            ELSE
              IF (DSECUH.LT.SECTOR.AND.SECTOR.LT.DSECDL)
     &            SETFLG(SECTOR+1)=.FALSE.
              IF (SECTOR.LT.DSECUL.OR.SECTOR.GT.DSECDH)
     &            SETFLG(SECTOR+1)=.FALSE.
            ENDIF
            LPHITYP = .FALSE.
          ELSE
            IF (DSECUH.LT.SECTOR.AND.SECTOR.LT.DSECDL)
     &          SETFLG(SECTOR+1)=.FALSE.
            IF (SECTOR.LT.DSECUL.OR.SECTOR.GT.DSECDH)
     &          SETFLG(SECTOR+1)=.FALSE.
          ENDIF
  600 ENDDO
          IF (SETFLG(1).OR.SETFLG(32)) THEN     ! SECTOR = 0,31
            SETFLG(1)=.TRUE.
            SETFLG(32)=.TRUE.
          ENDIF
          IF (SETFLG(16).OR.SETFLG(17)) THEN    ! SECTOR = 15,16
            SETFLG(16)=.TRUE.
            SETFLG(17)=.TRUE.
          ENDIF
C
      DO 1 LAYER = 0, 3
        DO 2 SECTOR = 0, 31
          IF (.NOT.SETFLG(SECTOR+1)) GOTO 2
          IF(LDSEC(SECTOR,LAYER).EQ.0)
     &        LDSEC(SECTOR,LAYER) = GZDSEC(SECTOR,LAYER)
          KPDSEC = LDSEC( SECTOR, LAYER )
          IF (KPDSEC .EQ. 0) THEN
            LDHIT = GZDHIT()
            IF (LDHIT .EQ. 0) GOTO 2
            CALL PDZHIT_CMPRS(LAYER,SECTOR,IFDWIR,PHI1,PHI2,PHI3,PHI4)
            GOTO 2
          ENDIF
          LDALS = GZDALS(LAYER,SECTOR)
          CPHIW = C( LDALS+3 )
          SPHIW = C( LDALS+4 )
          DO 3 WIRE = 0, 6
            IF (WIRE .NE. 0 .AND. WIRE .NE. 6) GOTO 3
            NHITS = IQ(KPDSEC+4+WIRE)       ! get number of hits
            IF (NHITS .LE. 0) GO TO 3
            IPWIR = LDALS + 6 + IC(LDALS+6) * WIRE
            XWIR = C(IPWIR+1)
            YWIR = C(IPWIR+2)
            RPOS = SQRT(XWIR ** 2 + YWIR **2)
            DO 4 IHIT = 1, NHITS
              IPT = KPDSEC + IQ(KPDSEC+4+7+WIRE) +
     &              IQ(KPDSEC+3) * (IHIT-1)
              ZERROR = Q(IPT+6)
              IF (ZERROR .GT. 9990.) GOTO 4
              ZPOS = Q(IPT+4)
              IF (ABS(ZPOS) .GE. Z1) GOTO 4
              DO 5 ISIDE = 0, 1
                DDIS = Q(IPT+2+ISIDE) - C(LDRFT+26+WIRE)
                XHPOS = XWIR + DDIS * CPHIW
                YHPOS = YWIR + DDIS * SPHIW
                HITPHI = ATAN2(YHPOS, XHPOS)
                IF(HITPHI .LT. 0.) HITPHI = HITPHI + TWOPI
                IF (HITPHI .GE. PHI1 .AND. HITPHI .LE. PHI2) THEN
                  CALL PDZHIT(ZPOS, RPOS, IFDWIR)
                  GOTO 4
                ENDIF
                IF (HITPHI .GE. PHI3 .AND. HITPHI .LE. PHI4) THEN
                  CALL PDZHIT(ZPOS, -RPOS, IFDWIR)
                  GOTO 4
                ENDIF
    5         CONTINUE
    4       CONTINUE
    3     CONTINUE
    2   CONTINUE
    1 CONTINUE
      ENDIF
  301 CONTINUE
C
C   draw R-Z tracks
C
      IF (IFDTRK .NE. 0) THEN
        IF (IFDCDC) THEN
          RTRKHI = R2
          RTRKLO = RCDCLO
        ELSE
          RTRKHI = R2
          RTRKLO = R1
        ENDIF
        IF (LDGEH .LE. 0) LDGEH = GZDGEH()
        IF (LDRFT .LE. 0) LDRFT = GZDRFT()
        THELMT = ATAN2(C(LDRFT+15)+C(LDRFT+25), 2*C(LDGEH+19))
        CALL PATHRS
        LDTRH = GZDTRH()
        IF (LDTRH .EQ. 0) GOTO 201
        KP = LQ(LDTRH-1)               ! pointer to bank 'DTRK'
        IF (KP.LE.0) GOTO 201          ! no track
  211   IF (IQ(KP+5) .EQ. 0) GOTO 213
        IF ((Q(KP+9) .LE. THELMT) .OR. ((PI-Q(KP+9)) .LE. THELMT))
     &    GOTO 213
        PLDTTH = LQ(KP-1)             ! pointer to bank 'DTTH'
        DRUPRZ = 0
        DRDWRZ = 0
        CALL JLSTYL(0)         ! Setting the line style back to normal
        CALL JLWIDE(16383)     ! Setting line thickness to normal
        CALL PXCOLR('FOR')
C
C ****  set color for tracks in roads depending on particle type
C 
        INMU=(IBITS(IQ(KP),MUROAD,1).EQ.1)
        INEL=(IBITS(IQ(KP),ELROAD,1).EQ.1)
        INTAU=(IBITS(IQ(KP),TAUROAD,1).EQ.1)
        INVEE=(IBITS(IQ(KP),9,1).EQ.1)
        IF(INVEE)THEN
          CALL PXCOLN('CDC',16,4,.TRUE.,KCOL,KINT,KFIL,KSTY) ! Yellow
          NROAD=NROAD+1
        ENDIF
        IF(INTAU)THEN
          CALL PXCOLN('CDC',7,4,.TRUE.,KCOL,KINT,KFIL,KSTY) ! Cyan
          NROAD=NROAD+1
        ENDIF
        IF(INMU)THEN
          CALL PXCOLN('CDC',9,4,.TRUE.,KCOL,KINT,KFIL,KSTY) ! Green
          NROAD=NROAD+1
        ENDIF
        IF(INEL)THEN
          CALL PXCOLN('CDC',13,4,.TRUE.,KCOL,KINT,KFIL,KSTY) ! Red
          NROAD=NROAD+1
        ENDIF
        IF (PLDTTH .LE. 0) GOTO 250     ! no hits information
        NSWHIT = IQ(KP+2)               ! # of hits on SW
        PLDTTH = PLDTTH + 2 * NSWHIT
        DO 200 I = 0,27
          RZFLAG = IBITS(IQ(KP+4),I,1)  ! RZflag=0 no hit on this wire;
          IF (RZFLAG.NE.0) THEN
            HLABEL = IQ(PLDTTH+1)                ! get hit label
            PLDTTH = PLDTTH+2
            LAY = IBITS(HLABEL,16,2)
            SEC = IBITS(HLABEL,11,5)
            WIR = IBITS(HLABEL,8,3)
            IF (WIR .NE. 0 .AND. WIR .NE. 6) GOTO 200
            NUMHIT = IBITS(HLABEL,1,7)
            ISIDE  = IBITS(HLABEL,0,1)
            LDALS = GZDALS(LAY,SEC)
            CPHIW = C(LDALS+3)
            SPHIW = C(LDALS+4)
            IPWIR = LDALS + 6 + IC(LDALS+6) * WIR
            XWIR = C(IPWIR+1)
            YWIR = C(IPWIR+2)
            IF (LDSEC(SEC,LAY) .EQ. 0)
     &            LDSEC(SEC,LAY) = GZDSEC(SEC,LAY)
            KPDSEC = LDSEC(SEC, LAY)
            LHIT   = IQ(KPDSEC + 3)
            NFADC  = IQ(KPDSEC + 2)
            IPHIT  = IQ(KPDSEC + NFADC + 4 + WIR) +
     &                     (NUMHIT-1) * LHIT + KPDSEC
            ZERROR = Q(IPHIT + 6)
            IF (ZERROR .GT. 9990.) GOTO 200
            DDIS = Q(IPHIT+2+ISIDE) - C(LDRFT+26+WIR)
            XHPOS = XWIR + DDIS * CPHIW
            YHPOS = YWIR + DDIS * SPHIW
            HITPHI = ATAN2(YHPOS, XHPOS)
            IF (HITPHI .LT. 0) HITPHI = HITPHI + TWOPI
            IF (HITPHI .GE. PHI1 .AND. HITPHI .LE. PHI2) THEN
              DRUPRZ = DRUPRZ + 1
            ELSE
              IF (HITPHI .GE. PHI3 .AND. HITPHI .LE. PHI4)
     &            DRDWRZ = DRDWRZ + 1
            ENDIF
C-
C--- Adapts the global PHI picking. ( Nobu. 09-SEP-1991 )
C-
            IF (HITPHI.GT.1.5*PI .AND. PHI1.LT.0.) THEN
              CHKHIT = HITPHI - TWOPI
              IF (CHKHIT .GE. PHI1 .AND. CHKHIT .LE. PHI2) THEN
                DRUPRZ = DRUPRZ + 1
              ENDIF
            ELSEIF (HITPHI.GT.0. .AND. PHI4.GT.TWOPI) THEN
              CHKHIT = HITPHI + TWOPI
              IF (CHKHIT .GE. PHI3 .AND. CHKHIT .LE. PHI4) THEN
                DRDWRZ = DRDWRZ + 1
              ENDIF
            ENDIF
C---
C-
            IF (DRUPRZ .GE. 2 .OR. DRDWRZ .GE. 2) GOTO 220
          ENDIF
  200   CONTINUE
        GOTO 220
C
C  When there is no hits information for the tracks, we can only use the
C  phi of the track to determine if this track is in the phi road. Although
C  this may include some tracks not belong to this phi region (these tracks
C  are not pointing to the vertex), but what we can do when the hits bank
C  is dropped? too bad...
C
  250   TRKPHI = Q(KP + 6)
        IF (TRKPHI .EQ. 0) THEN
          DRUPRZ = 2
        ELSE
          IF (TRKPHI .EQ. PI .OR. TRKPHI .EQ. TWOPI) THEN
            DRDWRZ = 2
          ELSE
            IF (TRKPHI .GE. PHI1 .AND. TRKPHI .LE. PHI2) THEN
              DRUPRZ = 2
            ELSE
              IF (TRKPHI .GE. PHI3 .AND. TRKPHI .LE. PHI4)
     &            DRDWRZ = 2
            ENDIF
          ENDIF
        ENDIF
C-
C--- Adapts the global PHI picking. ( Nobu. 09-SEP-1991 )
C-
        IF (TRKPHI.GT.1.5*PI .AND. PHI1.LT.0.) THEN
          CHKTRK = TRKPHI - TWOPI
          IF (CHKTRK .GE. PHI1 .AND. CHKTRK .LE. PHI2) THEN
            DRUPRZ = 2
          ENDIF
        ELSEIF (TRKPHI.GT.0. .AND. PHI4.GT.TWOPI) THEN
          CHKTRK = TRKPHI + TWOPI
          IF (CHKTRK .GE. PHI3 .AND. CHKTRK .LE. PHI4) THEN
            DRDWRZ = 2
          ENDIF
        ENDIF
C-
  220   IF ((DRUPRZ .GE. 2) .OR. (DRDWRZ .GE. 2)) THEN
          TLTRK = (RTRKHI - Q(KP + 10)) / SIN(Q(KP + 9))
          ZHPOS = Q(KP+11) + TLTRK * COS(Q(KP+9))
          RHPOS = RTRKHI
          ZTRKNM = ZHPOS + ZNMOFF
          RTRKNM = RHPOS + RNMOFF
          ITRK = IQ(KP-5)
          TLTRK = (RTRKLO - Q(KP + 10)) / SIN(Q(KP + 9))
          ZHPOS1 = Q(KP+11) + TLTRK * COS(Q(KP+9))
          RHPOS1 = RTRKLO
          IF (ITRK .LT. 10) THEN
            WRITE (TRKNUM, 1001) ITRK
 1001       FORMAT (1X,I1,2X)
          ELSE
            IF (ITRK .LT. 100) THEN
              WRITE (TRKNUM, 1002) ITRK

 1002         FORMAT (1X,I2,1X)
            ELSE
              WRITE (TRKNUM, 1003) ITRK
 1003         FORMAT (1X,I3)
            ENDIF
          ENDIF
C
C    if a track has at least two hits lying in the requested phi region,
C    we will draw this track. When the phi region is 0.0 to 180.0 degree,
C    if a track acrosses the x axias, the track will be drawn on both
C    up part and down part, but some hits on this track are drawn on the up
C    part and some hits on this track are drawn at the down part, depending
C    on the hit position.
C
          CALL J4RGET(1, XMIN, XMAX, YMIN, YMAX)
          SIZSCAL = .015
          XSIZ = (XMAX-XMIN)*SIZSCAL
          YSIZ = (YMAX-YMIN)*SIZSCAL
C
C *** draw tracks and track numbers
C
          IF (DRUPRZ .GE. 2) THEN
            CALL JMOVE(ZHPOS, RHPOS)
            CALL JDRAW(ZHPOS1, RHPOS1)
            IF (MOD(IFDLBL,2) .NE. 0) THEN
              CALL JJUST(2,2)
              CALL JMOVE(ZTRKNM, RTRKNM)
              CALL JSIZE(XSIZ, YSIZ)
              CALL J1STRG(TRKNUM)
            ENDIF
          ENDIF
          IF (DRDWRZ .GE. 2) THEN
            CALL JMOVE( ZHPOS, -RHPOS )
            CALL JDRAW( ZHPOS1, -RHPOS1 )
            IF (MOD(IFDLBL,2) .NE. 0) THEN
              CALL JJUST (2,2)
              CALL JMOVE( ZTRKNM, -RTRKNM )
              CALL JSIZE(XSIZ, YSIZ)
              CALL J1STRG(TRKNUM)
            ENDIF
          ENDIF
        ENDIF
C
C
        IF (IFDCDC .AND. IFDTRK .GT. 2) THEN
          THETA = Q(KP+9) * 180.0 / PI
          WRITE(TEXT,1004) ITRK, THETA
 1004     FORMAT ('   TRACK',I3,':   THETA = ',F7.1)
          CALL PUMESS(TEXT)
        ENDIF
C
  213   KP = LQ(KP)                    ! pointer to next bank 'DTRK'
        IF (KP.NE.0) GOTO 211          ! if another track
      ENDIF
  201 CONTINUE
C
C  draw vertex
C
      IF (IFDVER .GT. 0) THEN
C MARK LEVEL 0 SLOW Z VEREX with number 1 to 4 
C where number is level 0 multiple interaction flag
        CALL L0_VERTICES(FASTZ,FGOOD,SLOWZ,SGOOD,MI_FLAG)
        IF ( SGOOD ) THEN
          IF ( MI_FLAG.GE.1 .AND. MI_FLAG.LE.4 ) THEN
            CALL JJUST(2,2)
            CALL PXCOLR('CYAN')
            CALL JMOVE(SLOWZ,0.0)
            CALL J3STRG(MIFC(MI_FLAG))
          ENDIF
        ENDIF
C DRAW isajet vertex
        IF (IFDVER .GT. 1) THEN
          LISV1 = GZISV1()
          IF (LISV1 .LE. 0) GOTO 151
          ZISAJV = Q(LISV1+9)
          CALL PXCOLR('RED')
          CALL JCIRCL(ZISAJV,0.,0.,1.,30)
        ENDIF
  151   LVERH = GZVERH()
        IF ( LVERH .LE. 0 ) GO TO 160
        CALL ZVERTE(NV, ZVER, DZVER)
        IF ( NV .EQ. 0 ) GO TO 160
        CALL PXCOLR('RED')
        DO 150 IV = 1, NV
          CALL JMOVE(ZVER(IV)-2.,-2.)
          CALL JDRAW(ZVER(IV)+2.,2.)
          CALL JMOVE(ZVER(IV)-2.,2.)
          CALL JDRAW(ZVER(IV)+2.,-2.)
          CALL JRECT(ZVER(IV)-DZVER(IV),0.08,ZVER(IV)+DZVER(IV),-0.08)
  150   CONTINUE
      ENDIF
C
C  write phi road information and draw a scal
C
  160 IF (IFDLBL .GT. 1) THEN
        IF (IFDHIT .NE. 0 .OR. IFDTRK .NE. 0) THEN
          WRITE (TEXT,10) PHI1*180./PI, PHI2*180./PI
   10     FORMAT (' PHI ROAD: ',F5.1,' to ',F5.1,' DEGREES')
          XPOS = Z22 - 10.0
          YPOS = R22 + 40.0
          PRCENT = 1.0
          YXRATI = 2.0
          CALL JJUST(1,2)
          CALL PUVSTR(XPOS, YPOS, PRCENT, YXRATI, TEXT)
        ENDIF
        SCALX = Z11 - 5.0
        SCALY = -R22 - 32.0
        CALL PDSCAL(SCALX,SCALY,20)
      ENDIF
  900 CALL PUCLOSE
C
      IF(NROAD.GT.0) CALL LEGEND_ROAD
  999 RETURN
      END
