      SUBROUTINE PFTKDR(PHI1,PHI2,PHI3,PHI4,IVIEW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws FDC tracks in R - Z view.
C-
C-   Inputs  : PHI1,PHI2,PHI3,PHI4 = phi limit of plot
C-             IVIEW = type of view, 1=xz, 2=yz, 3=rz, 4=xy(half 0)
C-                                   5=xy(half 1)
C-
C-   Created  xx-MAY-1989   Daria Zieminska
C-   Updated  10-JAN-1990   Lupe Howell  Implemented Color table
C-   Updated   7-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   6-NOV-1990   Jeffrey Bantly  add phi angle limits
C-   Updated  30-APR-1991   Jeffrey Bantly  general cleanup 
C-   Updated  30-AUG-1991   Robert Avery  Get MUROAD, etc, from ZTRAKS.PARAMS
C-   Updated   9-SEP-1991   Robert E. Avery  Correction in use of 
C-                              status word of FDCT track (bit 0 = half). 
C-   Updated  11-OCT-1991   Robert E. Avery  Add track labels. 
C-   Updated   8-NOV-1991   Robert E. Avery  Check phi bounds with call to
C-                              new function, PFPHICHK.
C-   Updated  21-NOV-1991   Robert E. Avery  Upper half of plot is PHI1-PHI2 
C-   Updated  25-JAN-1992   Robert E. Avery  Delete error message for no
C-      tracks in FDC.
C-   Updated   7-FEB-1992   Robert E. Avery  Change colour of FDC tracks. 
C-   Updated  26-MAY-1992   Robert E. Avery  Option to extend track to beam 
C-   Updated   2-APR-1993   Robert E. Avery  Don't assume tracks are
C-                              consequtively numbered.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZFDCT.LINK'
      INCLUDE 'D0$LINKS:IZFDTH.LINK'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
C
      INTEGER GZFTRH,GZFDCT
      INTEGER LFDCT,LFTRH,LFDTH
      INTEGER NTRK,IVIEW      
      INTEGER UNITF,UNITL,WIREF,WIREL,IXV(32),NXV
      INTEGER HALF,HALFTK,QUADF,QUADL
      INTEGER RUNTYPE,IER
      INTEGER DRAWLABEL,TRACK 
      INTEGER DRAW_EXT
      INTEGER PFPHICHK,RSIGN
      INTEGER STATUS 
C
      REAL    XG,YG, DXDZ,DYDZ,XC,YC,Z0(2)
      REAL    PHI1,PHI2,PHI3,PHI4,ANGLE,PHI
      REAL    XHIT1,YHIT1,ZHIT1,RHIT1
      REAL    XHIT2,YHIT2,ZHIT2,RHIT2
      REAL    XIMP, YIMP, ZIMP, RIMP, RIMP_CUT         
C
      LOGICAL FDC_ONLY 
      LOGICAL FIRST
C
      CHARACTER*4 TRKCLR
C  
      SAVE FIRST,Z0
C
      DATA RIMP_CUT /5.0/
      DATA TRKCLR /'FOR '/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      LFTRH=GZFTRH()
      IF(LFTRH.GT.0) THEN
        Z0(1)=Q(LFTRH+3)
        Z0(2)=Q(LFTRH+4)
        NTRK=IQ(LFTRH+2)
      ELSE
        GOTO 999
      ENDIF
C
      IF(FIRST)THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('RUNTYPE',RUNTYPE,IER)
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF 
      CALL EZPICK('PX_FDCDIS_RCP')
      CALL PUGET_l('FDC ONLY        ', FDC_ONLY)
      CALL PUGET_i('FDC DRAW TRACK LABEL', DRAWLABEL)
      CALL PUGET_i('FDC DRAW EXTENDED TRACK',DRAW_EXT)
      CALL PUGETV('FDC IMPACT CUT',RIMP_CUT)
      CALL PUGETA('FDC COLR TRACK',TRKCLR)
      CALL EZRSET
C
      HALF=0
      IF(IVIEW.GE.4) HALF=IVIEW-4
C
      LFDCT = LQ(LFTRH-IZFDCT)
      DO WHILE ( LFDCT.NE.0 ) 
        TRACK = IQ(LFDCT-5)
C
C set color for tracks in roads depending on particle type
C   (make Tracks default if particle ID not set)
        STATUS = IQ(LFDCT)
        CALL PXCOLR(TRKCLR)
        IF (BTEST(STATUS,MUROAD))  CALL PXCOLR('GRE')
        IF (BTEST(STATUS,ELROAD))  CALL PXCOLR('RED')
        IF (BTEST(STATUS,TAUROAD)) CALL PXCOLR('CYA')
        IF (BTEST(STATUS,9))       CALL PXCOLR('YEL')
C
C  Loop over tracks
C
        HALFTK = IAND(1,IQ(LFDCT+1))
        IF((IVIEW.GE.4).AND.(HALF.NE.HALFTK)) GOTO 90

C
C  Find first and last point on track
C
        CALL UBITS ( IQ(LFDCT+3), 32, IXV, NXV)
        WIREF = IXV(1) - 1   ! first wire on track (counting 0 through 31)
        WIREL = IXV(NXV) - 1 ! last wire on track
        IF (WIREF.LE.7) THEN
          UNITF=0
          QUADF=0
          WIREF=7-WIREF
        ELSE IF (WIREF.GE.24) THEN
          UNITF=0
          QUADF=4
          WIREF=WIREF-24
        ELSE
          UNITF=1
          QUADF=0
          WIREF=WIREF-8
        END IF
        IF (WIREL.LE.7) THEN
          UNITL=0
          QUADL=0
          WIREL=7-WIREL
        ELSE IF (WIREL.GE.24) THEN
          UNITL=0
          QUADL=4
          WIREL=WIREL-24
        ELSE
          UNITL=1
          QUADL=0
          WIREL=WIREL-8
        END IF
C
C  Find x,y,z of first hit
C
        CALL GTFALH(HALFTK,UNITF,QUADF,0,WIREF,XC,YC,ZHIT1)
        IF(ZHIT1.EQ. 0.0) GOTO 90
        XG=Q(LFDCT+4)
        YG=Q(LFDCT+5)
        RSIGN = PFPHICHK(XG,YG,PHI1,PHI2,PHI3,PHI4) 
        IF ( RSIGN .EQ. 0 ) THEN
          GOTO 90
        ENDIF
        PHI=Q(LFDCT+6)
        DXDZ=Q(LFDCT+7)
        DYDZ=Q(LFDCT+8)
        XHIT1=XG+(ZHIT1-Z0(HALFTK+1))*DXDZ
        YHIT1=YG+(ZHIT1-Z0(HALFTK+1))*DYDZ
        RHIT1 = RSIGN * SQRT(XHIT1**2 + YHIT1**2)
        IF(IVIEW.EQ.1) CALL JMOVE( ZHIT1, XHIT1 )
        IF(IVIEW.EQ.2) CALL JMOVE( ZHIT1, YHIT1 )
        IF(IVIEW.EQ.3) CALL JMOVE( ZHIT1, RHIT1 )
        IF(IVIEW.GE.4) CALL JMOVE( XHIT1, YHIT1 )
C
C  Find x,y,z of last hit
C
        CALL GTFALH(HALFTK,UNITL,QUADL,0,WIREL,XC,YC,ZHIT2)
        IF (ZHIT2.EQ. 0.0) GOTO 90
        XHIT2 = XG+(ZHIT2-Z0(HALFTK+1))*DXDZ
        YHIT2 = YG+(ZHIT2-Z0(HALFTK+1))*DYDZ
        RHIT2 = RSIGN * SQRT(XHIT2**2 + YHIT2**2)
        IF(IVIEW.EQ.1) CALL JDRAW( ZHIT2, XHIT2 )
        IF(IVIEW.EQ.2) CALL JDRAW( ZHIT2, YHIT2 )
        IF(IVIEW.EQ.3) CALL JDRAW( ZHIT2, RHIT2 )
        IF(IVIEW.GE.4) CALL JDRAW( XHIT2, YHIT2 )
C
C  On request, extend track to beam
C
        IF ( (DRAW_EXT.GE.1) .AND. FDC_ONLY ) THEN
          CALL FGET_CLOSE(TRACK,XIMP,YIMP,ZIMP,RIMP)
          IF ( RIMP.LT.RIMP_CUT ) THEN
            CALL JLSTYL(2)
            RIMP = RSIGN * SQRT(XIMP**2+YIMP**2)
            IF(IVIEW.EQ.1) CALL JMOVE( ZHIT1, XHIT1 )
            IF(IVIEW.EQ.2) CALL JMOVE( ZHIT1, YHIT1 )
            IF(IVIEW.EQ.3) CALL JMOVE( ZHIT1, RHIT1 )
            IF(IVIEW.GE.4) CALL JMOVE( XHIT1, YHIT1 )
            IF(IVIEW.EQ.1) CALL JDRAW( ZIMP, XIMP )
            IF(IVIEW.EQ.2) CALL JDRAW( ZIMP, YIMP )
            IF(IVIEW.EQ.3) CALL JDRAW( ZIMP, RIMP )
            IF(IVIEW.GE.4) CALL JDRAW( XIMP, YIMP )
            CALL JLSTYL(0)
          ENDIF
        ENDIF
C
        IF ( DRAWLABEL .GE. 1 ) THEN
          CALL PXCOLR(TRKCLR)
          CALL PFTKDRL(TRACK, HALFTK, XHIT2, YHIT2, RHIT2, PHI, IVIEW)
        ENDIF
C
   90   CONTINUE
        LFDCT = LQ(LFDCT)
      ENDDO
C---------------------------------------------------------------------
  999 RETURN
      END
