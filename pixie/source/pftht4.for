      SUBROUTINE PFTHT4( HALF, QUAD, XMIN, XMAX, YMIN, YMAX )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw one theta vertical display
C-
C-   Inputs  : HALF,QUAD - FDC quadrant identifier
C-             XMIN,XMAX,YMIN,YMAX - window sizes
C-   Outputs : draws theta vertical display
C-   Controls:
C-
C-   Created  24-OCT-1988   Jeffrey Bantly
C-   Updated   7-FEB-1990   Jeffrey Bantly  general cleanup
C-   Updated  14-MAY-1991   Susan K. Blessing  Remove EZPICK call from loop.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER HALF, QUAD, SECTOR, LAYER
      INTEGER LFDTA
      INTEGER GZFDRT, GZFDTA
      INTEGER DRAWIR, DRAHIT, DRATRK
      INTEGER TYP,IVAL,IER
C
      REAL    XMIN, XMAX, YMIN, YMAX
      REAL    XLEN, YLEN, XC, YC, DX, DY
      REAL    XSCALE, YSCALE, OFFSET
C
      CHARACTER*3 TMPCOL
      CHARACTER*1 LAB(0:7)
      CHARACTER*50 FTEXT
      CHARACTER*4 CVAL, REM
C
      LOGICAL EZERROR
C
      DATA LAB /'0','1','2','3','4','5','6','7'/
C
C----------------------------------------------------------------------
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFTHT4','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
C
      XLEN = (XMAX - XMIN)/4.
      YLEN = (YMAX - YMIN)/9.
      LFDTA = GZFDTA()
      IF( LFDTA .LE. 0 ) GOTO 990
      IF( C(LFDTA+47).EQ.0. .OR. C(LFDTA+46).EQ.0. ) GOTO 995
      XSCALE = (XLEN/2.) / C( LFDTA + 5 + 38 + 4 )   !  world/cm
      YSCALE = (YLEN/2.) / C( LFDTA + 5 + 38 + 3 )   !  world/cm
      LAYER = 0
      IF(QUAD.GE.4) LAYER = 1
      DO 10 SECTOR = 0, 5
        XC = XMIN + (LAYER*6.+1.)*(XLEN/2.)
        DX = XLEN/2.
        IF( SECTOR .LE. 2) THEN
          OFFSET = 8.5
          YC = YMIN + (OFFSET-SECTOR)*YLEN
          DY = YLEN/2.
        ELSE
          OFFSET = 1.
          YC = YMIN + ((5.-SECTOR)*2. + OFFSET) * YLEN
          DY = YLEN
        ENDIF
        CALL EZ_GET_ARRAY('PXPARAMS','FDC COLR SECTOR',1,IVAL,
     &       TMPCOL,TYP,REM,IER)
        CALL PXRECT( TMPCOL, XC, YC, 0., DX, DY )
        CALL JSIZE(.65,1.)
        CALL JJUST(2,2)
        CALL JMOVE(XC-.15*DX,YC+.15*DY)
        CALL J3STRG(LAB(QUAD))
        CALL JMOVE(XC+.15*DX,YC+.15*DY)
        CALL J3STRG(LAB(SECTOR))
C
C    draw wires, hits, and tracks if requested
C
        CALL EZ_GET_ARRAY('PXPARAMS','FDC DRAW WIRE',1,DRAWIR,
     &       CVAL,TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','FDC DRAW HITS',1,DRAHIT,
     &       CVAL,TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','FDC DRAW TSEG',1,DRATRK,
     &       CVAL,TYP,REM,IER)
        IF( DRAWIR .GE. 1 ) CALL PFTWIR( HALF, QUAD, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
        IF( DRAHIT .GE. 1 ) CALL PFTHIT( HALF, QUAD, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
        IF( DRATRK .GE. 1 ) CALL PFTTRK( HALF, QUAD, SECTOR,
     &            XC, YC, DX, DY, XSCALE, YSCALE )
   10 CONTINUE
      GOTO 998
C
C   Link value to FDTA in STP bad.
C
  990 WRITE(FTEXT,100) LFDTA
  100 FORMAT(' Link value to FDTA bank bad = ',I8)
      CALL PFUMES(FTEXT)
      GOTO 998
C
C   Size values of Theta in FDTA in STP bad.
C
  995 WRITE(FTEXT,101) C(LFDTA+47),C(LFDTA+46)
  101 FORMAT(' Size values bad = ',2F12.4)
      CALL PFUMES(FTEXT)
C
C----------------------------------------------------------------------
  998 CONTINUE
C
C ****  Reset RCP bank
C
      CALL EZRSET
  999 RETURN
      END
