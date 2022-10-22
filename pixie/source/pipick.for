      SUBROUTINE PIPICK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Allows user to pick an ISAJET track and
C-                         displays P, Pt and ID of the track
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  12-JUN-1990   Michael W. Peters
C-   Updated  19-SEP-1991   Nobuaki Oshima( Adapt to System Picking. )
C-   Modified 05-MAR-1993   V.Bhatnagar (Picks parton jets also)
C-     Enabled to display phi and eta of the picked objects
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISP2.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZISV2.LINK'
      INCLUDE 'D0$LINKS:IZPJHD.LINK'
      INCLUDE 'D0$LINKS:IZPJET.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER  IDD,IDABS,LEVSV2, ITKCHO,PID
      REAL     PXYZ(3),P, XV(3),XC(3), PTMIN,LTRK
      REAL     XVSV1(3),XVSV2(3),ETJ,EJ,SETA,SPHI
      REAL     XPV(2),DIST,DMIN,PSV,PTSV,ETASV,PHISV
      INTEGER  LISAE,LISV1,LISP1,LISV2,LISP2
      INTEGER  LPARE,LISPN,LISPNX(99),LPJHD,LPJET
      INTEGER  IDSV,IDBOX,NJET,IL,IK
      CHARACTER*4  ID,IDC,PJET,PART
      CHARACTER*44 LINE(2)
C-
C----------------------------------------------------------------------
C
C ****  Get window coordinates of picked point
C
      CALL PU_GET_PICKV(XPV)
C
C   10 CONTINUE
C      CALL PUMESS('Click to select ISAJET track')
C      CALL JIENAB( 1, 2, 1 )
C      CALL JLOCAT( 1, 1, 1, ICHAR, XPV(1), XPV(2) )
C      CALL JIDISA( 1, 2, 1 )
C      CALL PUSLVP( XPV(1) , XPV(2) )
      DMIN=1.E6
C-
C--- Get parameters for track display
      CALL PUGET_i('ISAJET TRK CHOICE',ITKCHO)
      CALL PUGETV('TRACK PTMIN',PTMIN)
      CALL PUGETV('TRACK LENGTH',LTRK)
C-
      LISAE = LQ(LHEAD-IZISAE)
      IF (LISAE .EQ. 0)             GO TO 999

C-
C--- Start ISV1 loop...
C-
      LISV1  = LQ(LISAE-IZISV1)
C-
  100 IF (LISV1 .EQ. 0)             GO TO 998
      XVSV1(1) = Q(LISV1+7)
      XVSV1(2) = Q(LISV1+8)
      XVSV1(3) = Q(LISV1+9)
C-
      LISP1 = LQ(LISV1-IZISP1)
      XVSV1(1) = Q(LISV1+7)
      XVSV1(2) = Q(LISV1+8)
      XVSV1(3) = Q(LISV1+9)
C-
C--- Start ISP1 loop...
  200 IF (LISP1 .EQ. 0)              GO TO 800
      IDD = IQ(LISP1+1)
      PXYZ(1) =  Q(LISP1+2)
      PXYZ(2) =  Q(LISP1+3)
      PXYZ(3) =  Q(LISP1+4)
      P       =  Q(LISP1+5)
      IDABS   =  ABS(IDD)
      SPHI    =  Q(LISP1+7)
      SETA    =  Q(LISP1+9)
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
C-
C--- Draw ISP1 ended at ISV2
        CALL PIDIST(PTMIN,LTRK,PXYZ, P, XVSV1, XC, XPV, DIST)
        IF(DIST.LT.DMIN) THEN
          DMIN=DIST
          PSV=P
          PTSV=SQRT(PXYZ(1)**2+PXYZ(2)**2)
          ETASV=SETA
          PHISV=SPHI
          IDSV=IDD
        ENDIF
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
        SPHI    =  Q(LISP2+7)
        SETA    =  Q(LISP2+9)
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
C-
C--- Draw ISP2 ended at ISV2
          CALL PIDIST(PTMIN,LTRK,PXYZ, P, XV, XC, XPV, DIST)
          IF(DIST.LT.DMIN) THEN
            DMIN=DIST
            PSV=P
            PTSV=SQRT(PXYZ(1)**2+PXYZ(2)**2)
            ETASV=SETA
            PHISV=SPHI
            IDSV=IDD
          ENDIF
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
          CALL PIDIST(PTMIN,LTRK,PXYZ, P, XV, XC, XPV, DIST)
          IF(DIST.LT.DMIN) THEN
            DMIN=DIST
            PSV=P
            PTSV=SQRT(PXYZ(1)**2+PXYZ(2)**2)
            ETASV=SETA
            PHISV=SPHI
            IDSV=IDD
          ENDIF
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
        CALL PIDIST(PTMIN,LTRK,PXYZ, P, XVSV1, XC, XPV, DIST)
        IF(DIST.LT.DMIN) THEN
          DMIN=DIST
          PSV=P
          PTSV=SQRT(PXYZ(1)**2+PXYZ(2)**2)
          ETASV=SETA
          PHISV=SPHI
          IDSV=IDD
        ENDIF
      ENDIF
  700 LISP1 = LQ(LISP1)
      GO TO 200
  800 LISV1 = LQ(LISV1)
      GO TO 100
C-
  998 CONTINUE
C-
C.N.O.(IT WAS .05)
      IF(DMIN .LT. .02) THEN
        IF (IDSV.EQ.12) THEN
          PART = 'E-'
        ELSEIF (IDSV.EQ.-12) THEN
          PART = 'E+'
        ELSEIF (IDSV.EQ.14) THEN
          PART = 'MU-'
        ELSEIF (IDSV.EQ.-14) THEN
          PART = 'MU+'
        ELSEIF (IDSV.EQ.16) THEN
          PART = 'TAU-'
        ELSEIF (IDSV.EQ.-16) THEN
          PART = 'TAU+'
        ELSEIF (IDSV.EQ.10) THEN
          PART = 'PHO'
        ELSE
          GO TO 888
        ENDIF
        WRITE(LINE(1),1003)
 1003   FORMAT('   PID     P       PT       PHI      ETA ')
        WRITE(LINE(2),1004)PART,PSV,PTSV,PHISV,ETASV
 1004   FORMAT(2X,A4,2F8.2,2(1X,F8.2))
        DO IL =1,2
          CALL INTMSG(LINE(IL))
        ENDDO
        DMIN = 1.E6
      ENDIF
C-
C---Start PJET loop...
  888 LISV1  = LQ(LISAE-IZISV1)
C-
      IF (LISV1 .EQ. 0)             GO TO 999
      XVSV1(1) = Q(LISV1+7)
      XVSV1(2) = Q(LISV1+8)
      XVSV1(3) = Q(LISV1+9)
C-
      LPJHD = LQ(LISAE-IZPJHD)
      IF (LPJHD .EQ. 0)              GO TO 999
      NJET  = IQ(LPJHD+3)
      LPJET = LQ(LPJHD-IZPJET)
  150 IF (LPJET .EQ. 0)              GO TO 999
      ETJ     =  Q(LPJET+2)
      IF (ETJ .LT. PTMIN)            GO TO 175
      PXYZ(1) =  Q(LPJET+3)
      PXYZ(2) =  Q(LPJET+4)
      PXYZ(3) =  Q(LPJET+5)
      EJ      =  Q(LPJET+6)
      PID     =  0
      CALL VZERO(XC,3)
      CALL PIDIST(PTMIN,LTRK,PXYZ,EJ, XVSV1, XC, XPV, DIST)
      IF(DIST.LT.DMIN) THEN
        DMIN=DIST
        PSV=EJ
        PTSV=ETJ
        SPHI    =  Q(LPJET+8)
        SETA    =  Q(LPJET+10)
        IF(DMIN .LT. .02) THEN
          WRITE(LINE(1),1000)
 1000     FORMAT('    ID     P       PT       PHI      ETA ')
          WRITE(LINE(2),1001)'PJET',PSV,PTSV,SPHI,SETA
 1001     FORMAT(2X,A4,2F8.2,2(1X,F8.2))
          DO IK =1,2
            CALL INTMSG(LINE(IK))
          ENDDO
          DMIN = 1.E6
        ENDIF
      ENDIF
C-
  175 LPJET=LQ(LPJET)
      GO TO 150
C-
  999 RETURN
      END
