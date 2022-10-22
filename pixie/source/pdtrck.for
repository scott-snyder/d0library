      SUBROUTINE PDTRCK(SECMIN,SECMAX,EXTRAP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display full tracks in the sectors from
C-                         SECMIN to SECMAX of CDC
C-
C-   Inputs  : SECMIN, SECMAX, EXTRAP
C-
C-   Outputs : None
C-
C-   Created  26-APR-1989   Qizhong Li-Demarteau
C-   Updated  01-AUG-1989   Qizhong Li-Demarteau  change parameters to integer
C-   Updated   5-APR-1991   Harrison B. Prosper   Fix some EZ problems
C-   Updated  15-APR-1991   Qizhong Li-Demarteau  use alignment information
C-                               for sector drawing and added SEC # display
C-   Updated  10-AUG-1991   An-Dien Nguyen  reposition Rphi track numbers
C-       parallel version   S. Hagopian  roads, colors
C-   Updated  17-AUG-1991   T. Trippe  combine versions
C-   Updated   3-SEP-1991   Qizhong Li-Demarteau  fix the bug introduced
C-                                                when adding roads
C-   Updated  10-SEP-1991   Lupe Howell  Error checking for EZPICK
C-   Updated  16-SEP-1991 S. Hagopian, added call to LEGEND_ROAD; counter NROAD
C-                         used ZTRAKS.PARAMS instead of ZTRAKS.RCP
C-   Updated   3-OCT-1991   Lupe Howell Definition of tracks color based in
C-             device.  If in color device use colors otherwise line styles
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau  use VEE bit from IQ(LDTRK)
C-   Updated   9-DEC-1991   Qizhong Li-Demarteau  draw hits from compressed
C-                                  hits bank if the DSEC banks do not exit
C-   Updated  15-JUL-1992   Qizhong Li-Demarteau  don't draw mirror hits if
C-                                                the hits are on tracks
C-   Updated  30-JAN-1993   Lupe Howell Moved the label 210 outside IF block
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
C
      INTEGER SECMIN, SECMAX
      REAL    EXTRAP
C
      INTEGER IS,I,L
      INTEGER NHITS(0:NBSENS-1), NWIR, LHIT
      INTEGER KPWIRE(0:NBSENS-1), KP
      INTEGER LAY, SEC, IWIR, ISEG, GZDTRH, LDTRH, GZDSEC, GZDALS
      INTEGER NTRK, IPHIT, WRFLAG, ITRK
      INTEGER WIR, NUMHIT, ISIDE, HLABEL,PLDTTH, KPDSEC, NFADC
      INTEGER IFDSEC, IFDWIR, IFDHIT, IFDTRK, IFDLBL, IFISTR
      INTEGER IPATH, ERR, PLDALS
      INTEGER NROAD,KCOL,KINT,KFIL,KSTY
      INTEGER LDRFT, LDALS, IPWIR
      INTEGER NBWIR
      INTEGER LDHIT, GZDHIT
      LOGICAL EZERROR
      LOGICAL INEL,INMU,INTAU,INVEE
      LOGICAL TRKFLG, FIRST, ALGNMT, IFDCDC
C
      CHARACTER*4 TRKNUM, DPATH
      EQUIVALENCE (IPATH, DPATH)
      CHARACTER*2 SECNUM
C
      REAL    YR,DISTAN, SLOP, VTXCUT
      REAL    PHIW, CPHIW, SPHIW, PCPHIW
      REAL    DDIS1, DDIS
      REAL    DRANG1, DRANG2
      REAL    SIZDIS, XHPOS, YHPOS, XTRKNM, YTRKNM
      REAL    RAYCEN, DELRAY, PHICEN, DELPHI
      REAL    X1, X2, Y1, Y2, R1, R2, PHI1, PHI2, ZZ
      REAL    R3, X3, Y3, NUMPHI
      REAL    XWIR, YWIR, TLSEG, XCENT, YCENT, XORI, YORI
      REAL    XMAX, YMAX, XMIN, YMIN, SIZSCAL, XSIZ, YSIZ
C
      REAL    XNMOFF, YNMOFF
      PARAMETER( XNMOFF = 2.0 )
      PARAMETER( YNMOFF = 2.0 )
C
      REAL    DEGRAD
      PARAMETER( DEGRAD = 3.1415926535/180.)
C --------------------------------------------------------------------
      DATA TLSEG / 13.0 /
      DATA SIZDIS / 0.2 /
      SAVE FIRST
      DATA FIRST/.TRUE./, ALGNMT/.FALSE./
C --------------------------------------------------------------------
C
      CALL PUGET_l('CDC ONLY',IFDCDC )
      NROAD=0
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( .NOT. EZERROR(ERR) ) THEN
          CALL EZGET('DPATH',IPATH,ERR)
          CALL EZRSET
        ENDIF
        PLDALS = GZDALS(0,0)
        PCPHIW = C(PLDALS+3)
        IF (ABS(PCPHIW) .LE. 0.001) ALGNMT = .TRUE.
      ENDIF
C
      CALL PUGET_i('CDC DRAW SECTORS',   IFDSEC )
      CALL PUGET_i('CDC DRAW WIRES',     IFDWIR )
      CALL PUGET_i('CDC DRAW HITS',      IFDHIT )
      CALL PUGET_i('CDC DRAW TRACK',     IFDTRK )
      CALL PUGET_i('CDC DRAW ISATRK ',   IFISTR )
      CALL PUGET_i('CDC DRAW LABEL',     IFDLBL )
C OPEN SEGMENT
      CALL PUOPEN
C
      DO 88 LAY = 0,3
C
C ****  Access DRFT bank for nominal geometry
C
        LDRFT = LC( LDGEH - 3 )
        RAYCEN = C( LDRFT+11+2*LAY )
        PHICEN = C( LDRFT+12+2*LAY ) * DEGRAD
        DELRAY = C( LDRFT+8 )
        DELPHI = C( LDRFT+9 ) * DEGRAD
        R1 = ( RAYCEN - DELRAY ) / COS( DELPHI )
        R2 = ( RAYCEN + DELRAY ) / COS( DELPHI )
        IF (IFDSEC .EQ. -1) THEN
          CALL PXCOLR('BLU')
          IF(LAY.EQ.0)THEN
            CALL JCIRCL(0.,0.,0.,R1,0)
          ENDIF
          CALL JCIRCL(0.,0.,0.,R2,0)
        ENDIF
        DO 89 IS = SECMIN, SECMAX
          SEC = IS
          IF( SEC .LT. 0  ) SEC = SEC + 32
          IF( SEC .GT. 31 ) SEC = SEC - 32
          CALL PXCOLR('BLU')
          IF (IFDSEC .GT. 0) THEN
C
C ****  Draw the cell limits.
C
            PHI1 = PHICEN + (2*SEC-1) * DELPHI
            PHI2 = PHICEN + (2*SEC+1) * DELPHI
            X1 =  R1 * COS( PHI1 )
            Y1 =  R1 * SIN( PHI1 )
            IF (ALGNMT) CALL PDALGN(X1,Y1,ZZ)
            CALL JMOVE( X1, Y1 )
            X2 =  R1 * COS( PHI2 )
            Y2 =  R1 * SIN( PHI2 )
            IF (ALGNMT) CALL PDALGN(X2,Y2,ZZ)
            CALL JDRAW( X2, Y2 )
            X2 =  R2 * COS( PHI2 )
            Y2 =  R2 * SIN( PHI2 )
            IF (ALGNMT) CALL PDALGN(X2,Y2,ZZ)
            IF (IFDSEC .LE. 1) THEN
              CALL JMOVE(X2,Y2)
            ELSE
              CALL JDRAW(X2,Y2)
            ENDIF
            X2 =  R2 * COS( PHI1 )
            Y2 =  R2 * SIN( PHI1 )
            IF (ALGNMT) CALL PDALGN(X2,Y2,ZZ)
            CALL JDRAW( X2, Y2 )
            IF (IFDSEC .GT. 1) CALL JDRAW(X1,Y1)
            IF (IFDSEC .EQ. 3) THEN
              IF (LAY .EQ. 3) THEN
                IF (SEC .LE. 15) R3 = R2 + 0.05*(R2-R1)*SEC
                IF (SEC .GT. 15) R3 = R2 + 0.05*(R2-R1)*(31-SEC) + 1.0
                PHI1 = ATAN2(Y2,X2)
                NUMPHI = PHI1 + DELPHI / 2
                X3 =  R3 * COS(NUMPHI)
                Y3 =  R3 * SIN(NUMPHI)
                IF (SEC .LT. 10) THEN
                  WRITE (SECNUM, 2001) SEC
                ELSE
                  WRITE (SECNUM, 2002) SEC
                ENDIF
 2001           FORMAT (I1)
 2002           FORMAT (I2)
                CALL JMOVE(X3, Y3)
                CALL J1STRG(SECNUM)
              ENDIF
            ENDIF
          ENDIF
          CALL PXCOLR( 'MAG' )
          CALL PATHST(DPATH)
          IF (IFDHIT .NE. 0) THEN
            LDHIT = GZDHIT()
            IF (LDHIT .NE. 0) THEN
              CALL PDXYHT_CMPRS(LAY,SEC)
              LHIT = 0
            ELSE
              CALL ZGDSEC(LAY, SEC, NBWIR, KPWIRE(0), NHITS(0), LHIT)
            ENDIF
          ENDIF
          LDALS = GZDALS(LAY, SEC)
          CPHIW = C( LDALS+3 )
          SPHIW = C( LDALS+4 )
          DO 99 IWIR= 0,NBWIR-1
            IPWIR = LDALS + 6 + IC(LDALS+6) * IWIR
            XWIR = C( IPWIR+1 )
            YWIR = C( IPWIR+2 )
C
C  Mark wire positions with an "+"
C
            IF (IFDWIR .NE. 0) THEN
              CALL JCMARK(2)
              CALL JMARK( XWIR, YWIR )
            ENDIF
            IF (LHIT .EQ. 0) GOTO 99
            IF (IFDHIT .NE. 0) THEN
              DO 66 L = 1,NHITS(IWIR)
C
C  Get drift distances from ZEBRA banks and mark raw hit positions
C  with a short segment
C
                KP = KPWIRE( IWIR ) + (L-1)*LHIT + 2
                DDIS = Q(KP) - C(LDRFT+26+IWIR) - .5*SIZDIS
                XHPOS = XWIR + DDIS * CPHIW
                YHPOS = YWIR + DDIS * SPHIW
                CALL JMOVE( XHPOS, YHPOS )
                XHPOS = XHPOS + SIZDIS * CPHIW
                YHPOS = YHPOS + SIZDIS * SPHIW
                CALL JDRAW( XHPOS, YHPOS )
                DDIS = Q(KP+1) - C(LDRFT+26+IWIR) - .5*SIZDIS
                XHPOS = XWIR + DDIS * CPHIW
                YHPOS = YWIR + DDIS * SPHIW
                CALL JMOVE( XHPOS, YHPOS )
                XHPOS = XHPOS + SIZDIS * CPHIW
                YHPOS = YHPOS + SIZDIS * SPHIW
                CALL JDRAW( XHPOS, YHPOS )
   66         CONTINUE
            ENDIF
   99     CONTINUE
   89   CONTINUE
   88 CONTINUE
      CALL PUCLOSE
C
C ****  Now, draw the full tracks
C
      IF (IFDTRK .NE. 0) THEN
        CALL PATHRS
        LDTRH = GZDTRH()
        IF (LDTRH.EQ.0) RETURN
        CALL PUOPEN
        KP = LQ(LDTRH-1)               ! pointer to bank 'DTRK'
        IF (KP.EQ.0) GOTO 212
  211   TRKFLG = .FALSE.
C
C ****  Set color for tracks in roads depending on particle type
C
        INMU=(IBITS(IQ(KP),MUROAD,1).EQ.1)
        INEL=(IBITS(IQ(KP),ELROAD,1).EQ.1)
        INTAU=(IBITS(IQ(KP),TAUROAD,1).EQ.1)
        INVEE=(IBITS(IQ(KP),9,1).EQ.1)
        CALL JLSTYL(0)
        CALL JLWIDE(16383)
        CALL PXCOLR('FOR')
        IF(INVEE)THEN
          CALL PXCOLN('CDC',16,4,.TRUE.,KCOL,KINT,KFIL,KSTY) ! Yellow
          NROAD=NROAD+1
        ENDIF
        IF(INTAU)THEN
          CALL PXCOLN('CDC',7,4,.TRUE.,KCOL,KINT,KFIL,KSTY)  ! Cyan
          NROAD=NROAD+1
        ENDIF
        IF(INMU)THEN
          CALL PXCOLN('CDC',9,4,.TRUE.,KCOL,KINT,KFIL,KSTY)  ! Green
          NROAD=NROAD+1
        ENDIF
        IF(INEL)THEN
          CALL PXCOLN('CDC',13,4,.TRUE.,KCOL,KINT,KFIL,KSTY) ! Red
          NROAD=NROAD+1
        ENDIF
C
C *** First, bold the hits on the track
C
        PLDTTH = LQ(KP-1)             ! pointer to bank 'DTTH'
        IF (PLDTTH .LE. 0)   THEN
          TRKFLG = .TRUE.
          GOTO 210
        ENDIF
        DO 200 I = 0,27
          WRFLAG = IBITS(IQ(KP+3),I,1)
          IF (WRFLAG.NE.0) THEN
            HLABEL = IQ(PLDTTH+1)
            PLDTTH = PLDTTH+2
            LAY = IBITS(HLABEL, 16, 2)
            SEC = IBITS(HLABEL, 11, 5)
            IF (SEC .LE. SECMAX .AND. SEC .GE. SECMIN) THEN
              TRKFLG = .TRUE.
              WIR = IBITS(HLABEL,  8, 3)
              NUMHIT = IBITS(HLABEL,  1, 7)
              ISIDE  = IBITS(HLABEL,  0, 1)
              KPDSEC = GZDSEC(SEC, LAY)
              IF (KPDSEC .LE. 0) GOTO 200
              LHIT   = IQ(KPDSEC + 3)
              NFADC  = IQ(KPDSEC + 2)
              IPHIT  = IQ(KPDSEC + NFADC + 4 + WIR) +
     &                     (NUMHIT-1) * LHIT + KPDSEC
              YR = Q(IPHIT + ISIDE + 2) - C(LC(LDGEH-3) +26+WIR)
C
              LDALS = LC( LC( LC( LSCDC-5 ) -(LAY+1) ) -(SEC+1) )
              CPHIW = C( LDALS+3 )
              SPHIW = C( LDALS+4 )
              IPWIR = LDALS + 6 + IC(LDALS+6) * WIR
              XWIR = C( IPWIR+1 )
              YWIR = C( IPWIR+2 )
              XHPOS = XWIR + YR * CPHIW
              YHPOS = YWIR + YR * SPHIW
              CALL JCMARK(1)
              CALL JJUST(2,2)
              CALL JMARK( XHPOS, YHPOS )
            ENDIF
          ENDIF
  200   CONTINUE
C
C ****  Then draw the full tracks
C
  210   CONTINUE
        IF (TRKFLG) THEN
          XHPOS = Q( KP+7 ) + TLSEG * COS( Q(KP+6) )
          YHPOS = Q( KP+8 ) + TLSEG * SIN( Q(KP+6) )
          CALL JMOVE( XHPOS, YHPOS )
          XTRKNM = XHPOS + XNMOFF * COS( Q(KP+6) )
          YTRKNM = YHPOS + YNMOFF * SIN( Q(KP+6) )
          ITRK = IQ( KP-5 )
          XHPOS = Q( KP+7 ) - (TLSEG + EXTRAP) * COS( Q(KP+6) )
          YHPOS = Q( KP+8 ) - (TLSEG + EXTRAP) * SIN( Q(KP+6) )
          CALL JDRAW( XHPOS, YHPOS )
          IF (MOD(IFDLBL,2) .NE. 0) THEN
            CALL J4RGET(1, XMIN, XMAX, YMIN, YMAX)
            SIZSCAL = 0.015
            XSIZ = (XMAX - XMIN)*SIZSCAL
            YSIZ = (YMAX - YMIN)*SIZSCAL
            CALL JJUST(2,2)
            CALL JMOVE( XTRKNM, YTRKNM )
            CALL JSIZE (XSIZ, YSIZ)
            IF (ITRK .LT. 10) THEN
              WRITE (TRKNUM, 1001) ITRK
 1001         FORMAT (1X,I1,2X)
            ELSE
              IF (ITRK .LT. 100) THEN
                WRITE (TRKNUM, 1002) ITRK
 1002           FORMAT (1X,I2,1X)
              ELSE
                WRITE (TRKNUM, 1003) ITRK
 1003           FORMAT (1X,I3)
              ENDIF
            ENDIF
            CALL J1STRG(TRKNUM)
          ENDIF
        ENDIF
C
  213   KP = LQ(KP)                    ! pointer to next bank 'DTRK'
        IF (KP.NE.0) GOTO 211
C
  212   CALL PUCLOSE
      ENDIF
C
      IF (IFISTR .GT. 1) CALL PDISTR
C
C ****  Draw the Legend
C
      IF(NROAD.GT.0) CALL LEGEND_ROAD
C
      RETURN
      END
