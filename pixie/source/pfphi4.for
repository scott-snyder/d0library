      SUBROUTINE PFPHI4( HALF, QUART, XMIN, XMAX, YMIN, YMAX )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw one quarter of the phi vertical display
C-
C-   Inputs  : HALF,SECTOR - FDC Half, Sector being displayed
C-             QUART       - quarter of phi being displayed
C-             XMIN,XMAX,YMIN,YMAX - Window size
C-   Outputs : draws one quarter of phi vertical display
C-   Controls:
C-
C-   Created  24-OCT-1988   Jeffrey Bantly
C-   Updated   7-FEB-1990   Jeffrey Bantly  general cleanup
C-   Updated  30-MAR-1992   Robert E. Avery  (on his birthday) 
C-                              Change  color name.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER HALF, SECTOR, SECTR, QUART, DUM
      INTEGER LKFDPH, GZFDPH
      CHARACTER*3 TMPCOL
      REAL    XMIN, XMAX, YMIN, YMAX
      REAL    XLEN, YLEN, XC, YC, DX, DY
      REAL    XSCALE, YSCALE
      INTEGER DRAWIR, DRAHIT, DRATRK
      CHARACTER*4 CVAL, REM
      INTEGER TYP,IVAL,IER
      CHARACTER*2 LAB(0:35)
      LOGICAL EZERROR
      DATA LAB /' 0',' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9','10',
     &          '11','12','13','14','15','16','17','18','19','20','21',
     &          '22','23','24','25','26','27','28','29','30','31','32',
     &          '33','34','35'/
C----------------------------------------------------------------------
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFPHI4','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZ_GET_ARRAY('PXPARAMS','FDC DRAW WIRE',1,DRAWIR,CVAL,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','FDC DRAW HITS',1,DRAHIT,CVAL,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','FDC DRAW TSEG',1,DRATRK,CVAL,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','FDC COLR SECTOR',1,IVAL,
     &       TMPCOL,TYP,REM,IER)
      XLEN = (XMAX - XMIN)/2
      YLEN = (YMAX - YMIN)/9
      LKFDPH = GZFDPH()
      IF(LKFDPH.LE.0) GOTO 990
      XSCALE = (XLEN/2) / C( LKFDPH + 69 + 4 )             !  world/cm
      YSCALE = (YLEN/2) / (C( LKFDPH + 69 + 3 ) * PI /36)
      DO 10 SECTOR = 0, 8
        XC = XMIN + XLEN
        YC = YMIN + (8.5-SECTOR)*YLEN
        DX = XLEN/2
        DY = YLEN/2
        CALL PXRECT( TMPCOL, XC, YC, 0., DX, DY )
        CALL JSIZE(.65,1.)
        CALL JJUST(2,2)
        CALL JMOVE(XC,YC+.15*DY)
        SECTR = SECTOR + QUART*9
        CALL J3STRG(LAB(SECTR))
C
C    Draw hits and tracks if requested
C
        IF( DRAWIR .GE. 1 ) CALL PFPWIR( HALF, SECTR,
     &                XC, YC, DX, DY, XSCALE, YSCALE )
        IF( DRAHIT .GE. 1 ) CALL PFPHIT( HALF, SECTR,
     &                XC, YC, DX, DY, XSCALE, YSCALE )
        IF( DRATRK .GE. 1 ) CALL PFPTRK( HALF, SECTR,
     &                XC, YC, DX, DY, XSCALE, YSCALE )
   10 CONTINUE
C----------------------------------------------------------------------
  990 CONTINUE
C
C ****  Reset RCP bank
C
      CALL EZRSET
  999 RETURN
      END
