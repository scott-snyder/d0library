      SUBROUTINE PISATRACK
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays ISAJET tracks in 3D.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Updated  07-MAY-1992   N. Oshima( draw X,Y,Z axes and ISV1 vertex )
C-   Updated  10-OCT-1991   N. Oshima( adapt system picking and PHYDIS )
C-   Updated  29-JAN-1991   Lupe Howell  Setting the correct RCP file
C-   Modified 18-DEC-1990   Nobu.(to get new track parameters.)
C-   Modified 14-NOV-1990   Nobu.(can be an action routine.)
C-   Modified 05-APR-1990   Nobu.(can handle particle IDENT code.)
C-   Created  12-SEP-1989   Nobuaki Oshima
C-   Modified 05-MAR-1993   V.Bhatnagar
C-     Displays parton jets,elec.,muons,photons AND taus only
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISP2.LINK'
      INCLUDE 'D0$LINKS:IZISP3.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZISV2.LINK'
      INCLUDE 'D0$LINKS:IZPJHD.LINK'
      INCLUDE 'D0$LINKS:IZPJET.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
C----------------------------------------------------------------------
      INTEGER  IDD,IDABS,LEVSV2, ITKCHO,IER,PID,NJET
      REAL     PXYZ(3),P, XV(3),XC(3), PTMIN,DPT,LTRK
      REAL     XVSV1(3),XVSV2(3), VWSAVE(85),ETJ,EJ
      REAL     YBGN,ZBGN,YEND,ZEND
      REAL     RLEVEL
      INTEGER  LISAE,LISV1,LISP1,LISV2,LISP2,LISP3
      INTEGER  LPARE,LISPN,LISPNX(99),LPJHD,LPJET
      LOGICAL  PU_PICK_ACTIVE,EZERROR,FLGVAL
      CHARACTER*22 STR1
C
C----------------------------------------------------------------------
C-
      CALL JIQDIL(RLEVEL)
C
C ****  Make sure we pick correct bank
C
      CALL EZPICK('PX_PHYDIS_RCP')          ! Selecting Caldis bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PISATRACK','Bank PX_PHYDIS_RCP NOT FOUND',
     &    'W')
        GOTO 999
      ENDIF
C-
      IF ( LHEAD .LE. 0 ) THEN
        CALL ERRMSG('PIXIE','PISATRACK','LHEAD is ZERO!','W')
        GOTO 999
      ENDIF
C-
C--- Check do picking...
C-
      IF ( PU_PICK_ACTIVE() ) THEN
        CALL PIPICK
        IF( RLEVEL .EQ. -2. )   CALL PX_PICK_QUIT
        GO TO 999
      ENDIF
C-
C--- Get parameters for track display from currently set RCP bank
      CALL PUGETV ('TRACK PTMIN',PTMIN)
      CALL PUGETV ('TRACK DPT',DPT)
      CALL PUGETV ('TRACK LENGTH',LTRK)
      CALL PUGET_i ('ISAJET TRK CHOICE',ITKCHO)
C-
      LISAE = LQ(LHEAD-IZISAE)
      IF (LISAE .EQ. 0)             GO TO 950
      LISV1  = LQ(LISAE-IZISV1)
C-
      CALL PUOPEN
C-
C--- Draw X, Y and Z axes
      CALL PXLWID(2)
      CALL PXCOLR('YEL')
      CALL J3MOVE(0., 0., 0.)
      CALL JR3DRA(240., 0., 0.)
      CALL JSIZE(10.,10.)
      CALL J3STRG('X')
      CALL J3MOVE(0., 0., 0.)
      CALL JR3DRA(0., 240., 0.)
      CALL JSIZE(10.,10.)
      CALL J3STRG('Y')
      CALL J3MOVE(0., 0., 0.)
      CALL JR3DRA(0., 0., 380.)
      CALL JSIZE(10.,10.)
      CALL JBASE( 0., 0., 1.)
      CALL JPLANE( 0., 1., 0.)
      CALL J3STRG('Z')
C-
C--- Start ISV1 loop...
C-
      IF (LISV1 .EQ. 0)             GO TO 900
      XVSV1(1) = Q(LISV1+7)
      XVSV1(2) = Q(LISV1+8)
      XVSV1(3) = Q(LISV1+9)
C-
C---Start PJET loop...
C-
      LPJHD = LQ(LISAE-IZPJHD)
      IF (LPJHD .EQ. 0)              GO TO 100
      NJET  = IQ(LPJHD+3)
      LPJET = LQ(LPJHD-IZPJET)
  150 IF (LPJET .EQ. 0)              GO TO 100
      ETJ     =  Q(LPJET+2)
      IF (ETJ .LT. PTMIN)            GO TO 175
      PXYZ(1) =  Q(LPJET+3)
      PXYZ(2) =  Q(LPJET+4)
      PXYZ(3) =  Q(LPJET+5)
      EJ      =  Q(LPJET+6)
      PID     =  0
      CALL VZERO(XC,3)
      CALL PLISTK(PTMIN,PID,DPT,LTRK,PXYZ, EJ, XVSV1, XC)
  175 LPJET   =  LQ(LPJET)
      GO TO 150
C-
  100 IF (LISV1 .EQ. 0)             GO TO 900
      XVSV1(1) = Q(LISV1+7)
      XVSV1(2) = Q(LISV1+8)
      XVSV1(3) = Q(LISV1+9)
C-
C--- Draw Vertex(ISV1)
      CALL PXLWID(4)
      CALL PXCOLR('RED')
      YBGN = XVSV1(2) + 5.
      ZBGN = XVSV1(3) + 5.
      YEND = XVSV1(2) - 5.
      ZEND = XVSV1(3) - 5.
      CALL J3MOVE(XVSV1(1),YBGN,ZBGN)
      CALL J3DRAW(XVSV1,YEND,ZEND)
      ZBGN = XVSV1(3) - 5.
      ZEND = XVSV1(3) + 5.
      CALL J3MOVE(XVSV1(1),YBGN,ZBGN)
      CALL J3DRAW(XVSV1,YEND,ZEND)

C-
C--- Start ISP1 loop...
C-

      LISP1 = LQ(LISV1-IZISP1)
  250 IF (LISP1 .EQ. 0)              GO TO 800
      IDD = IQ(LISP1+1)
      PXYZ(1) =  Q(LISP1+2)
      PXYZ(2) =  Q(LISP1+3)
      PXYZ(3) =  Q(LISP1+4)
      P       =  Q(LISP1+5)
      IDABS = ABS(IDD)
      IF(IDABS.EQ.11 .OR. IDABS.EQ.13 .OR. IDABS.EQ.15) GO TO 700
      IF (ITKCHO .GE. 10) THEN
        IF (IDABS .NE. ITKCHO)   GO TO 700
      ENDIF
C-
      IF (LQ(LISP1-4) .NE. 0)   THEN
        LEVSV2 = 0
        LISV2  = LQ(LISP1-4)
        LPARE  = LQ(LISV2-3)
        IF (LPARE .NE. LISP1)        GO TO 650
        XC(1) = Q(LISV2+7)
        XC(2) = Q(LISV2+8)
        XC(3) = Q(LISV2+9)
        PID   = IDABS
C-
C--- Draw ISP1 ended at ISV2
        CALL PLISTK(PTMIN,PID,DPT,LTRK,PXYZ, P, XVSV1, XC)
C-
        LISP2 = LQ(LISV2-IZISP2)
        CALL UCOPY(XC,XV,3)
C--- Start   ISP2 loop...
  300   IF (LISP2 .EQ. 0)              GO TO 600
        IDD = IQ(LISP2+1)
        PXYZ(1) =  Q(LISP2+2)
        PXYZ(2) =  Q(LISP2+3)
        PXYZ(3) =  Q(LISP2+4)
        P       =  Q(LISP2+5)
        IDABS = ABS(IDD)
        IF(IDABS.EQ.11 .OR. IDABS.EQ.13 .OR. IDABS.EQ.15) GO TO 500
        IF (ITKCHO .GE. 10) THEN
          IF (IDABS .NE. ITKCHO)   GO TO 500
        ENDIF
        IF (LQ(LISP2-4) .NE. 0)   THEN
          LISV2  = LQ(LISP2-4)
          LPARE  = LQ(LISV2-3)
          IF (LPARE .NE. LISP2)        GO TO 450
          XC(1)  = Q(LISV2+7)
          XC(2)  = Q(LISV2+8)
          XC(3)  = Q(LISV2+9)
          PID    = IDABS
C-
C--- Draw ISP2 ended at ISV2
          CALL PLISTK(PTMIN,PID,DPT,LTRK,PXYZ, P, XV, XC)
C-
          LISPN = LQ(LISV2-IZISP2)
          IF (LISPN .GT. 0) THEN
            LEVSV2 = LEVSV2 + 1
            LISPNX(LEVSV2) = LISP2
            CALL UCOPY(XC,XV,3)
            LISP2 = LISPN
          ELSE
            GO TO   500
          ENDIF
  450     CONTINUE
        ELSE
C--- Draw ISP2 tracks
          CALL VZERO(XC,3)
          PID = IDABS
          CALL PLISTK(PTMIN,PID,DPT,LTRK,PXYZ, P, XV, XC)
        ENDIF
  500   LISP2 = LQ(LISP2)
        GO TO 300
  600   CONTINUE
        IF (LEVSV2 .GE. 1) THEN
          LISP2  = LISPNX(LEVSV2)
          LISV2  = LQ(LISP2+1)
          XV(1)  = Q(LISV2+7)
          XV(2)  = Q(LISV2+8)
          XV(3)  = Q(LISV2+9)
          LISP2  = LQ(LISP2)
          LEVSV2 = LEVSV2 - 1
          GO TO 300
        ENDIF
  650   CONTINUE
      ELSE
C--- Draw ISP1 tracks
        CALL VZERO(XC,3)
        PID = IDABS
        CALL PLISTK(PTMIN,PID,DPT,LTRK,PXYZ, P, XVSV1, XC)
      ENDIF
  700 LISP1 = LQ(LISP1)
      GO TO 250
  800 LISV1 = LQ(LISV1)
      GO TO 100
  900 CONTINUE

C-
      CALL JRCLOS
C-
C *** Drawing Legend (setting viewing parameters to X Y viwe)
C-
      WRITE(STR1,201) PTMIN
  201 FORMAT('ET(PT)-MIN:',F5.1,' GeV')
      CALL PCTEXT(1,STR1)
      CALL LEGENDTK
C
C ****  Reset RCP bank
C
  950 CALL EZRSET
  999 RETURN
      END
