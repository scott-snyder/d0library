      SUBROUTINE PPTOP_ISAQ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays ISAJET partons with PC3Dxx's which
C-                         displays Calorimeter Cells
C-                         Colors track based on ISAJ -
C                             RED        W+ from t quark
C                             DARK RED   b quark from t
C                             GREEN      W- from t bar
C                             DARK GREEN     b bar quark from t bar
C                             BLUE       something else in ISAJ
C                             DARK BLUE  underlying event
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
      INCLUDE 'D0$LINKS:IZISAQ.LINK'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*3 COLORS(6)*3,LABELS(6)*5
      CHARACTER LABEL*30
      INTEGER  IDD,IDABS,LEVSV2, NLAB,ICOL(14),I
      INTEGER  LISAE,LISAQ,LISAJ,ISAJ_ID
      REAL     P(3),PTOT, X(3),X1(3), PTMIN,LTRK,PLEN,PT
      DATA NLAB/6/,LABELS/' W+',' B',' W-',' BB',' IR',' UNDL'/
      DATA ICOL/9,7,16,12,14,11,13,15,10,8,6,5,4,3/ !from P3LEGO
C----------------------------------------------------------------------
C
C ****  open retained segment
C
      LCONNR = .TRUE.
      CALL PUOPEN
C-
C--- Get parameters for track display
      CALL PUGETV('TOPDIS ETMIN',PTMIN)
      CALL PUGETV('TRACK LENGTH',LTRK)
      CALL PUGET_i('TOPDIS WCOL',I)
      CALL PXCOLITOC(ICOL(I),COLORS(1))
      CALL PUGET_i('TOPDIS BCOL',I)
      CALL PXCOLITOC(ICOL(I),COLORS(2))
      CALL PUGET_i('TOPDIS WBCOL',I)
      CALL PXCOLITOC(ICOL(I),COLORS(3))
      CALL PUGET_i('TOPDIS BBCOL',I)
      CALL PXCOLITOC(ICOL(I),COLORS(4))
      CALL PUGET_i('TOPDIS IRCOL',I)
      CALL PXCOLITOC(ICOL(I),COLORS(5))
      CALL PUGET_i('TOPDIS UNCOL',I)
      CALL PXCOLITOC(ICOL(I),COLORS(6))
      LTRK = MAX(LTRK,400.)
C-
      LISAE = LQ(LHEAD-IZISAE)
      IF (LISAE .EQ. 0)             GO TO 999
C-
C--- Start ISAQ loop...
C-
      LISAQ = LQ(LISAE-IZISAQ)
C-
C--- Start ISP1 loop...
  200 IF (LISAQ .EQ. 0)              GO TO 900
      IDD = IQ(LISAQ+1)
      P(1) =  Q(LISAQ+2)
      P(2) =  Q(LISAQ+3)
      P(3) =  Q(LISAQ+4)
      PTOT = SQRT ( P(1)**2 + P(2)**2 + P(3)**2 )
      PT   = SQRT ( P(1)**2 + P(2)**2 )
      IF (PT.LT. PTMIN) GOTO 700
C     IF (ITKCHO .GE. 10) THEN IDABS = ABS(IDD)
C     IF(IDABS.EQ.11 .OR. IDABS.EQ.13 .OR. IDABS.EQ.15) GO TO 700
C     IF (IDABS .NE. ITKCHO)   GO TO 700 -
C-   Top colors
      LISAJ = LQ(LISAQ-1)
      ISAJ_ID = 0
      IF(LISAJ.GT.0) ISAJ_ID = IQ(LISAJ+1)
      IF(ISAJ_ID.EQ.80) THEN
        CALL PXCOLR(COLORS(1)) ! W+ from t quark
      ELSE IF(ISAJ_ID.EQ.6) THEN
        CALL PXCOLR(COLORS(2)) ! b quark from t
      ELSE IF(ISAJ_ID.EQ. -80) THEN
        CALL PXCOLR(COLORS(3)) ! W- from t bar
      ELSE IF(ISAJ_ID.EQ. -6) THEN
        CALL PXCOLR(COLORS(4)) ! b bar quark from t bar
      ELSE
        CALL PXCOLR(COLORS(5)) ! something else (IR)
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
  700 LISAQ = LQ(LISAQ)
      GO TO 200
  900 CONTINUE
C-
  999 CONTINUE
      CALL JRCLOS
      WRITE(LABEL,201) PTMIN
  201 FORMAT('TRK PTMIN= ',F6.2)
      CALL PUMESS(LABEL)
      CALL LEGEND(COLORS,LABELS,NLAB)
      RETURN
      END
