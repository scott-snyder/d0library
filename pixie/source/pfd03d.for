      SUBROUTINE PFD03D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine that will display the FDC
C-                         in the D0 configuration in 3-D.
C-                         Options for hits, segments, and tracks are
C-                         available in PX_FDCDIS.RCP
C-
C-   Created  26-OCT-1990   Jeffrey Bantly
C-   Updated  23-JAN-1991   Jeffrey Bantly  add bank checks
C-   Updated  11-FEB-1991   Robert Avery    up string quality to 3 to
C-                                          avoid XDW driver long string
C-                                          bomb
C-   Updated  21-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER HALF, LAYER, TRKNUM, STRQ, IVIEW
      INTEGER LKFTRH,GZFTRH,LKFDCH,GZFDCH,IER
C
      REAL XMIN,XMAX,YMIN,YMAX,XC,YC,ZC,X0,Y0,Z0(2)
C
      LOGICAL EZERROR
C
      DATA HALF/0/
C----------------------------------------------------------------------
C
      LKFDCH=GZFDCH()
      CALL OUTMSG('1')
      IF(LKFDCH.LE.5) THEN
        CALL OUTMSG(' No FDC Hit banks present')
        GOTO 999
      ENDIF
      LKFTRH=GZFTRH()
      IF(LKFTRH.GT.0) THEN
        Z0(1)=Q(LKFTRH+3)
        Z0(2)=Q(LKFTRH+4)
      ELSE
        CALL OUTMSG(' No FDC track banks present')
        GOTO 999
      ENDIF
C
C Optimize string quality initially to prevent bombs in XDW driver
C   for long strings:
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFD03D','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('STRING QUALITY',STRQ)
      CALL PUSETV('STRING QUALITY',3)
C
C  Select track to view.
C
      CALL PFPICK_TRACK(TRKNUM,HALF,1)
      CALL PUGETV('FDC 3D VIEW',IVIEW)
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
        CALL JVUPNT(X0,Y0,0.5*Z0(HALF+1))
        CALL JNORML(-100.,-100.,0.)
      ENDIF
      IF(IVIEW.EQ.3) CALL JNORML(0.,0.,Z0(HALF+1) )
C
C  Draw 3-D track display
C
      CALL PUOPEN
      CALL PF3MES(' ')
      CALL PF3MES(' FDC D0 EXPT 3-D HITS, SEGS, AND TRACKS')
      IF(TRKNUM.LE.0) TRKNUM=1
      CALL PF3DDRAW(HALF,TRKNUM)        ! draw track
      CALL PFPTRAK(TRKNUM)
      CALL JRCLOS
      CALL PUSETV('STRING QUALITY',STRQ)        ! reset string quality
      CALL EZRSET
C
C----------------------------------------------------------------------
  999 RETURN
      END
