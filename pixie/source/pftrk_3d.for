      SUBROUTINE PFTRK_3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make 3D display for FDC tracks (FDCT banks)
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created  23-JUN-1992   Robert E. Avery (from pftkdr).
C-   Updated   2-APR-1993   Robert E. Avery  Don't assume tracks are
C-                              consequtively numbered.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INCLUDE 'D0$LINKS:IZFDTH.LINK'
      INCLUDE 'D0$LINKS:IZFDCT.LINK'
C
      INTEGER GZFTRH,GZFDCT
      INTEGER LFTRH,LFDCT,LFDTH
      INTEGER NTRK
      INTEGER UNITF,UNITL,WIREF,WIREL,IXV(32),NXV
      INTEGER HALFTK,QUADF,QUADL
      INTEGER TRACK 
      INTEGER STATUS 
      INTEGER DRAW_TRACKS
      INTEGER DRAW_EXT
C
      REAL    XG,YG, DXDZ,DYDZ,XC,YC,Z0
      REAL    XPOS1,YPOS1,ZPOS1
      REAL    XPOS2,YPOS2,ZPOS2
      REAL    XIMP, YIMP, ZIMP, RIMP, RIMP_CUT         
C
      LOGICAL FDC_ONLY 
      LOGICAL DRAW_LEGEND 
C
      CHARACTER*4 TRKCLR
C  
      DATA TRKCLR /'CYA'/
      DATA RIMP_CUT /5.0/
C----------------------------------------------------------------------
C
      LFTRH=GZFTRH()
      IF(LFTRH.GT.0) THEN
        NTRK=IQ(LFTRH+2)
      ELSE
        GOTO 999
      ENDIF
C
      CALL EZPICK('PX_FDCDIS_RCP')
      CALL PUGETA('FDC COLR TRACK',TRKCLR)
      CALL PUGETV('FDC DRAW 3DTRKS',DRAW_TRACKS)
      CALL PUGETV('FDC ONLY', FDC_ONLY)
      CALL PUGETV('FDC DRAW EXTENDED TRACK',DRAW_EXT)
      CALL PUGETV('FDC IMPACT CUT',RIMP_CUT)
      CALL EZRSET
      IF (DRAW_TRACKS .LE. 0) GOTO 999
C
      CALL PUOPEN
      DRAW_LEGEND = .FALSE.
      LFDCT = LQ(LFTRH-IZFDCT)
      DO WHILE ( LFDCT.NE.0 ) 
        TRACK = IQ(LFDCT-5)
C
C set color for tracks in roads depending on particle type
C   (make Tracks default if particle ID not set)
        STATUS = IQ(LFDCT)
        CALL PXCOLR(TRKCLR)
        IF (BTEST(STATUS,MUROAD))  THEN
          CALL PXCOLR('GRE')
          DRAW_LEGEND = .TRUE.
        ENDIF
        IF (BTEST(STATUS,ELROAD))  THEN
          CALL PXCOLR('RED')
          DRAW_LEGEND = .TRUE.
        ENDIF
        IF (BTEST(STATUS,TAUROAD)) THEN
          CALL PXCOLR('CYA')
          DRAW_LEGEND = .TRUE.
        ENDIF
        IF (BTEST(STATUS,9)) THEN
          CALL PXCOLR('YEL')
          DRAW_LEGEND = .TRUE.
        ENDIF
C
C  Loop over tracks
C
        HALFTK = IAND(1,IQ(LFDCT+1))
        LFDTH = LQ( LFDCT - IZFDTH )
        IF ( LFDTH .LE. 5 ) THEN
          UNITF=0
          QUADF=0
          WIREF=7
          UNITL=0
          QUADL=4
          WIREL=7
        ELSE
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
        END IF
C
C  Find x,y,z of first hit
C
        CALL GTFALH(HALFTK,UNITF,QUADF,0,WIREF,XC,YC,ZPOS1)
        XG=Q(LFDCT+4)
        YG=Q(LFDCT+5)
        DXDZ=Q(LFDCT+7)
        DYDZ=Q(LFDCT+8)
        CALL FGETZ0(TRACK,Z0)
        XPOS1=XG+(ZPOS1-Z0)*DXDZ
        YPOS1=YG+(ZPOS1-Z0)*DYDZ
        CALL J3MOVE(XPOS1, YPOS1, ZPOS1)
C
C  Find x,y,z of last hit
C
        CALL GTFALH(HALFTK,UNITL,QUADL,0,WIREL,XC,YC,ZPOS2)
        XPOS2 = XG+(ZPOS2-Z0)*DXDZ
        YPOS2 = YG+(ZPOS2-Z0)*DYDZ
        CALL J3DRAW(XPOS2, YPOS2, ZPOS2)
C
C  On request, extend track to beam
C
        IF ( (DRAW_EXT.GE.1) .AND. FDC_ONLY ) THEN
          CALL FGET_CLOSE(TRACK,XIMP,YIMP,ZIMP,RIMP)
          IF ( RIMP.LT.RIMP_CUT ) THEN
            CALL JLSTYL(2)
            CALL J3MOVE(XPOS1, YPOS1, ZPOS1)
            CALL J3DRAW(XIMP, YIMP, ZIMP)
            CALL JLSTYL(0)
          ENDIF
        ENDIF
C
        LFDCT = LQ(LFDCT)
      ENDDO
C
      CALL JRCLOS
      IF (DRAW_LEGEND) CALL LEGEND_ROAD
C---------------------------------------------------------------------
  999 RETURN
      END
