      SUBROUTINE PSAMDETC(IVIEW,IFLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO DRAW THE SAMUS STATIONS
C-
C-   Inputs  : IVIEW    1 = Z Y     (HORIZONTAL-VERTICAL)
C-                      2 = X Y
C-                      3 = X Z
C-             IFLAG=1 for +Z,, -1 for -Z, 0 for both
C-             IFLAG= 2 for STATION A, -Z
C-             IFLAG= 3 for STATION B, -Z
C-             IFLAG= 4 for STATION C, -Z
C-             IFLAG= 5 for STATION A, +Z
C-             IFLAG= 6 for STATION B, +Z
C-             IFLAG= 7 for STATION C, +Z
C-                    (Individual stations used for X-Y END view)   
C-   Outputs :
C-   Controls:
C-
C-   Created  14-MAY-1991   Cary Y. Yoshikawa
C-   Updated  27-JAN-1993   Vladimir Glebov  ! Correct SAMUS Station 
C-                                           ! Coordinates 
C-   Updated  29-JAN-1993   Vladimir Glebov  ! Skip GTSAM1 call 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IVIEW,IFLAG,I
      INTEGER NHIT,IERR,NST,NSC,IWIR,ITIME
      REAL DXDET,DYDET,DZDET,DXHOLE,DYHOLE,DZHOLE,XCEN,YCEN,ZCEN
      REAL XOMIN,XOMAX,YOMIN,YOMAX,ZOMIN,ZOMAX
      REAL XIMIN,XIMAX,YIMIN,YIMAX,ZIMIN,ZIMAX
      REAL XDOMIN,XDOMAX,YDOMIN,YDOMAX,XDIMIN,XDIMAX,YDIMIN,YDIMAX
      REAL STA(9,6)
      INTEGER DRAWCHAM,IFIRST,ICH(6)
C
      DATA STA/0.0,0.0,-424.026,! STATION 1: COORDINATES
     &         165.0,165.0,15.0,!            HALF SIZES OF STATION
     &         25.5,25.5,15.0,  !            HALF SIZES OF HOLES
     &         0.0,0.0,-670.29, ! STATION 2: COORDINATES
     &         165.0,165.0,15.0,!            HALF SIZES OF STATION
     &         25.5,25.5,15.0,  !            HALF SIZES OF HOLES
     &         0.0,0.0,-962.92, ! STATION 3: COORDINATES
     &         165.0,165.0,15.0,!            HALF SIZES OF STATION
     &         38.1,38.1,15.0,  !            HALF SIZES OF HOLES
     &         0.0,0.0,+423.30, ! STATION 4: COORDINATES
     &         165.0,165.0,15.0,!            HALF SIZES OF STATION
     &         25.5,25.5,15.0,  !            HALF SIZES OF HOLES
     &         0.0,0.0,+669.79, ! STATION 5: COORDINATES
     &         165.0,165.0,15.0,!            HALF SIZES OF STATION
     &         25.5,25.5,15.0,  !            HALF SIZES OF HOLES
     &         0.0,0.0,+964.71, ! STATION 6: COORDINATES
     &         165.0,165.0,15.0,!            HALF SIZES OF STATION
     &         38.1,38.1,15.0/  !            HALF SIZES OF HOLES
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Check DRAW CHAMBER PARAMETER
      CALL PUGETV('SAMUS DRAW CHAM',DRAWCHAM)
      IF(DRAWCHAM.EQ.0)GO TO 999
C Initialize ICH array
      IFIRST=0
      IF(DRAWCHAM.EQ.2)IFIRST=1
      DO 10 I=1,6
   10 ICH(I)=IFIRST
      IF(DRAWCHAM.EQ.2)GO TO 40
      IF(IFLAG.GT.1.AND.IFLAG.LT.8)THEN
        ICH(IFLAG-1)=1
        GO TO 40
      ENDIF      
C
C mark which stations are hit      
C
C      NHIT=0
C   20 NHIT=NHIT+1 
C      CALL GTSAM1(NHIT,NST,NSC,IWIR,ITIME,IERR)
C      IF(IERR.NE.0)GO TO 50
C      IF(NST.GT.0.AND.NST.LT.7)THEN
C        ICH(NST)=1      
C      ENDIF
C      GO TO 20
C
C ****  MAKE ALL STATIONS "HIT"  VG 01/29/93
C
      do i = 1,6
        ich(i) = 1
      end do  
c
C zero side not flagged
c
   40 IF(IFLAG.EQ.1)THEN
        ICH(1)=0
        ICH(2)=0
        ICH(3)=0
      ELSE IF (IFLAG.EQ.-1)THEN
        ICH(4)=0
        ICH(5)=0
        ICH(6)=0
      ENDIF
   50 CALL PUOPEN
      CALL PXCOLR('BLU')
C      IF(FIRST)THEN
C        CALL INRCP('SAM_STPFILE',IER)
C        FIRST=.FALSE.
C        IF(IER.NE.0)THEN
C          WRITE(6,*)' INRCP ERROR SAM_STPFILE'
C          GO TO 900
C        ENDIF
C      ENDIF
C      CALL EZPICK('SAM_STPFILE')
C      IF (EZERROR(IER)) THEN
C        CALL ERRMSG('PIXIE','PSAMDETC',
C     &    'BANK SAMUS_STATION_RCP NOT FOUND','W')
C        GOTO 900
C      ENDIF
      DO 500 I=1,6                      ! 6 = NUMBER OF SAMUS STATIONS
      IF(ICH(I).EQ.0)GO TO 500
C        CALL EZGETA(SAMUS_STATION(I),START,END,STEP,VAL,IER)
        XCEN=STA(1,I)
        YCEN=STA(2,I)
        ZCEN=STA(3,I)
        DXDET=STA(4,I)
        DYDET=STA(5,I)
        DZDET=STA(6,I)
        DXHOLE=STA(7,I)
        DYHOLE=STA(8,I)
        DZHOLE=STA(9,I)
C
C    CALCULATE INNER & OUTER CORNERS OF MAGNET
C
        XOMIN=XCEN-DXDET
        XOMAX=XCEN+DXDET
        YOMIN=YCEN-DYDET
        YOMAX=YCEN+DYDET
        ZOMIN=ZCEN-DZDET
        ZOMAX=ZCEN+DZDET
        XIMIN=XCEN-DXHOLE
        XIMAX=XCEN+DXHOLE
        YIMIN=YCEN-DYHOLE
        YIMAX=YCEN+DYHOLE
        ZIMIN=ZCEN-DZHOLE
        ZIMAX=ZCEN+DZHOLE
C
C    TAKE CARE OF IVIEW
C
        IF (IVIEW.EQ.1) THEN
          XDOMIN=ZOMIN
          XDOMAX=ZOMAX
          YDOMIN=YOMIN
          YDOMAX=YOMAX
          XDIMIN=ZIMIN
          XDIMAX=ZIMAX
          YDIMIN=YIMIN
          YDIMAX=YIMAX
        ELSEIF (IVIEW.EQ.2) THEN
          XDOMIN=XOMIN
          XDOMAX=XOMAX
          YDOMIN=YOMIN
          YDOMAX=YOMAX
          XDIMIN=XIMIN
          XDIMAX=XIMAX
          YDIMIN=YIMIN
          YDIMAX=YIMAX
        ELSEIF (IVIEW.EQ.3) THEN
          XDOMIN=ZOMIN
          XDOMAX=ZOMAX
          YDOMIN=XOMIN
          YDOMAX=XOMAX
          XDIMIN=ZIMIN
          XDIMAX=ZIMAX
          YDIMIN=XIMIN
          YDIMAX=XIMAX
        ELSE                              ! ERROR
          CALL ERRMSG('PIXIE','PSAMDETC','IVIEW OUT OF RANGE','W')
          GOTO 900
        ENDIF
C
C    DRAW THE SAMUS STATIONS
C
        IF (IVIEW.EQ.1 .OR. IVIEW.EQ.3) THEN
          CALL JLSTYL(0)
          CALL JMOVE(XDIMIN,YDIMAX)
          CALL JDRAW(XDOMIN,YDOMAX)
          CALL JDRAW(XDOMAX,YDOMAX)
          CALL JDRAW(XDIMAX,YDIMAX)
          CALL JDRAW(XDIMIN,YDIMAX)
          CALL JMOVE(XDOMIN,YDOMIN)
          CALL JDRAW(XDIMIN,YDIMIN)
          CALL JDRAW(XDIMAX,YDIMIN)
          CALL JDRAW(XDOMAX,YDOMIN)
          CALL JDRAW(XDOMIN,YDOMIN)
C Draw part of detector across beam line as dashed
          CALL JLSTYL(1)
          CALL JMOVE(XDIMIN,YDIMAX)
          CALL JDRAW(XDIMIN,YDIMIN) 
          CALL JMOVE(XDIMAX,YDIMAX)
          CALL JDRAW(XDIMAX,YDIMIN) 
        ELSEIF (IVIEW.EQ.2) THEN
          CALL JLSTYL(0)
          CALL JMOVE(XDOMIN,YDOMIN)
          CALL JDRAW(XDOMIN,YDOMAX)
          CALL JDRAW(XDOMAX,YDOMAX)
          CALL JDRAW(XDOMAX,YDOMIN)
          CALL JDRAW(XDOMIN,YDOMIN)
          CALL JMOVE(XDIMIN,YDIMIN)
          CALL JDRAW(XDIMIN,YDIMAX)
          CALL JDRAW(XDIMAX,YDIMAX)
          CALL JDRAW(XDIMAX,YDIMIN)
          CALL JDRAW(XDIMIN,YDIMIN)
        ELSE
          WRITE(6,*)'IVIEW ERROR IN PSAMAG'
        ENDIF
  500 CONTINUE
  900 CALL JRCLOS
  999 RETURN
      END
