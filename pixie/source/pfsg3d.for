      SUBROUTINE PFSG3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine that will display one set of
C-                         FDC segments in 3-D.  Options for hits, 
C-                         segments, and calculating a track are 
C-                         available in FDC.PXPARA
C-
C-   Created  22-MAY-1990   Jeffrey Bantly
C-   Updated   7-AUG-1990   Jeffrey Bantly  put in 3 views 
C-   Updated  23-JAN-1991   Jeffrey Bantly  add bank checks 
C-   Updated  11-FEB-1991   Robert Avery    up string quality to 3 to 
C-                                          avoid XDW driver long string 
C-                                          bomb AND skip to end if no
C-                                          segments to make a track
C-   Updated   4-MAR-1991   Lupe Howell  Implementing PIXIE using COMPACK
C-   Updated  30-APR-1991   Jeffrey Bantly  cleanup using new Compack 
C-   Updated  14-MAY-1991   Susan K. Blessing  Use North and South halves 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER HALF, LAYER, LADDER(0:2), STRQ, IVIEW
      INTEGER LKFTRH,GZFTRH,LKFDCH,GZFDCH,IER
      INTEGER LEN,II,JJ
      LOGICAL PFPICK_SEGMT,EZERROR
      REAL XMIN,XMAX,YMIN,YMAX,XC,YC,ZC,X0,Y0,Z0(2)
      CHARACTER*60 PROM
      CHARACTER*80 STRING
      DATA PROM/ ' Enter view type (default Prev or 1)>'/
      DATA HALF/0/
      DATA IVIEW/1/
C----------------------------------------------------------------------
C
      LKFDCH=GZFDCH()
      IF(LKFDCH.LE.5) THEN
        CALL INTMSG(' No FDC Hit banks present')
        GOTO 999
      ENDIF
      LKFTRH=GZFTRH()
      IF(LKFTRH.GT.0) THEN
        Z0(1)=Q(LKFTRH+3)
        Z0(2)=Q(LKFTRH+4)
      ELSE
        CALL INTMSG(' No FDC track or segment banks present')
        GOTO 999
      ENDIF
C
C Optimize string quality initially to prevent bombs in XDW driver
C   for long strings:
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFSG3D','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGET_i('STRING QUALITY',STRQ)        
      CALL PUSETV('STRING QUALITY',3)
C
C  Make choice of how to view display
C
      CALL PUGET_i('FDC 3D VIEW',IVIEW)
      CALL OUTMSG('1')
      CALL OUTMSG('    Choose a viewpoint to see FDC Half in 3-D')
      CALL OUTMSG(' ( 1=above center, 2=along segments, 3=along beam )')
      STRING=' '
      LEN=0
      CALL GETPAR(1,PROM,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LEN)
      IF(LEN.NE.0) READ(STRING(1:LEN),*,ERR=980) IVIEW
      IF(IVIEW.LT.1 .OR. IVIEW.GT.3) THEN
        IVIEW=1
        CALL OUTMSG(' Bad choice of view, set to default value of 1.')
      ENDIF
      CALL PUSETV('FDC 3D VIEW',IVIEW)
C
C  Get segment ladder from user.
C
      IF ( PFPICK_SEGMT(LADDER,HALF) ) THEN
C
C  Calculate and set 3-D viewing parameters.
C
        IF(IVIEW.EQ. 2) THEN
          CALL PUGETV('FDC 3D X0',X0)
          CALL PUGETV('FDC 3D Y0',Y0)
        ENDIF
        CALL JVUPNT(0.,0.,Z0(HALF+1))
        IF(IVIEW.EQ.1) CALL JNORML(-100.,-100.,Z0(HALF+1) )
        IF(IVIEW.EQ.2) THEN
          CALL JVUPNT(X0,Y0,Z0(HALF+1))
          CALL JNORML(-100.,-100.,0.)
        ENDIF
        IF(IVIEW.EQ.3) CALL JNORML(0.,0.,Z0(HALF+1) )
C
C  Draw 3-D track display
C
        CALL PUOPEN
        CALL PF3MES(' ')
        CALL PF3MES(' FDC SEGMENTS W 3-D HITS, SEGS, AND TRACK')
        CALL PF3DSEGS(HALF,LADDER)             ! draw segments 
        CALL JRCLOS
      ENDIF
      GOTO 990
C
C----------------------------------------------------------------------
  980 CONTINUE
      CALL INTMSG(' Error reading input.')
C
  990 CONTINUE
      CALL PUSETV('STRING QUALITY',STRQ)     ! reset string quality
      CALL EZRSET
  999 RETURN
      END
