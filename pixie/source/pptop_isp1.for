      SUBROUTINE PPTOP_ISP1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays ISAJET tracks with PC3Dxx's which
C-                         displays Calorimeter Cells
C-                         Colors track based on ISAJ - PX_TOPDIS.RCP
C-                         PXPARAMS
C-
C-   Inputs  : None
C-   Outputs : none
C-   Controls: None
C-
C-   Created  25-OCT-1990 Chip Stewart (from Nobuaki Oshima)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*3 COLORS(6)*3,LABELS(6)*5
      CHARACTER LABEL*30
      INTEGER  IDD,NLAB,ICOL(14)
      INTEGER  LISAE,LISV1,LISP1,LISAJ,ISAJ_ID,LISAQ,ISAQ_ID,I
      REAL     P(3),PTOT, X(3),X1(3), PTMIN,LTRK,PLEN,PT
      DATA NLAB/6/,LABELS/' W+',' B',' W-',' BB',' IR',' UNDL'/
      DATA ICOL/9,7,16,12,14,11,13,15,10,8,6,5,4,3/ !from P3LEGO scrambled
C----------------------------------------------------------------------
C
C ****  open retained segment
C
      LCONNR = .TRUE.
      CALL PUOPEN
C-
C--- Get parameters for track display
      CALL PUGETV('TOPDIS ETMIN',PTMIN)
      CALL PUGETV('TOPDIS WCOL',I)
      CALL PXCOLITOC(ICOL(I),COLORS(1))
      CALL PUGETV('TOPDIS BCOL',I)
      CALL PXCOLITOC(ICOL(I),COLORS(2))
      CALL PUGETV('TOPDIS WBCOL',I)
      CALL PXCOLITOC(ICOL(I),COLORS(3))
      CALL PUGETV('TOPDIS BBCOL',I)
      CALL PXCOLITOC(ICOL(I),COLORS(4))
      CALL PUGETV('TOPDIS IRCOL',I)
      CALL PXCOLITOC(ICOL(I),COLORS(5))
      CALL PUGETV('TOPDIS UNCOL',I)
      CALL PXCOLITOC(ICOL(I),COLORS(6))
      CALL PUGETV('TRACK LENGTH',LTRK)
      LTRK = MAX(LTRK,400.)
C-
      LISAE = LQ(LHEAD-IZISAE)
      IF (LISAE .EQ. 0)             GO TO 999
      LISV1  = LQ(LISAE-IZISV1)
C-
C--- Start ISV1 loop...
C-
  100 IF (LISV1 .EQ. 0)             GO TO 900
      LISP1 = LQ(LISV1-IZISP1)
      X(1) = Q(LISV1+7)
      X(2) = Q(LISV1+8)
      X(3) = Q(LISV1+9)
C-
C--- Start ISP1 loop...
  200 IF (LISP1 .EQ. 0)              GO TO 800
      IDD = IQ(LISP1+1)
      P(1) =  Q(LISP1+2)
      P(2) =  Q(LISP1+3)
      P(3) =  Q(LISP1+4)
      PTOT = SQRT ( P(1)**2 + P(2)**2 + P(3)**2 )
      PT   = SQRT ( P(1)**2 + P(2)**2 )
      IF (PT.LT. PTMIN) GOTO 700
C-   Top colors
      LISAJ = LQ(LISP1-3)
      LISAQ = LQ(LISP1-2)
      ISAJ_ID = 0
      ISAQ_ID = 0
      IF(LISAJ.GT.0) ISAJ_ID = IQ(LISAJ+1)
      IF(LISAQ.GT.0) ISAQ_ID = IQ(LISAQ+1)
      IF(ISAJ_ID.EQ.80) THEN
        CALL PXCOLR(COLORS(1)) ! W+ from t quark
      ELSE IF(ISAJ_ID.EQ.6) THEN
        CALL PXCOLR(COLORS(2)) ! b quark from t
      ELSE IF(ISAJ_ID.EQ. -80) THEN
        CALL PXCOLR(COLORS(3)) ! W- from t bar
      ELSE IF(ISAJ_ID.EQ. -6) THEN
        CALL PXCOLR(COLORS(4)) ! b bar quark from t bar
      ELSE IF(ISAQ_ID.NE. 0) THEN
        CALL PXCOLR(COLORS(5)) ! something else (IR)
      ELSE
        CALL PXCOLR(COLORS(6)) ! underlying event
      END IF
C- momenta lengths
      PLEN = LTRK * MIN (1., PT/50.)
C - plot track
      CALL J3MOVE(X(1), X(2), X(3))
      X1(1) = X(1) + (PLEN*P(1)/PTOT)
      X1(2) = X(2) + (PLEN*P(2)/PTOT)
      X1(3) = X(3) + (PLEN*P(3)/PTOT)
      CALL JR3DRA(X1(1)-X(1),X1(2)-X(2),X1(3)-X(3))
C
  700 LISP1 = LQ(LISP1)
      GO TO 200
  800 LISV1 = LQ(LISV1)
      GO TO 100
  900 CONTINUE
C-
  999 CONTINUE
      CALL JRCLOS
      WRITE(LABEL,201) PTMIN
  201 FORMAT('ISP1 PTMIN= ',F6.2)
      CALL PUMESS(LABEL)
      CALL LEGEND(COLORS,LABELS,NLAB)
      RETURN
      END
