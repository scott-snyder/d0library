      SUBROUTINE PFTB3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine that will display the FDC
C-                         in the Testbeam '90 configuration in 3-D.
C-                         Options for hits, segments, and tracks are
C-                         available in FDC.PXPARA
C-
C-   Created  22-MAY-1990   Jeffrey Bantly
C-   Updated   7-AUG-1990   Jeffrey Bantly  put in 3 views
C-   Updated  23-JAN-1991   Jeffrey Bantly  add bank checks
C-   Updated  11-FEB-1991   Robert Avery    up string quality to 3 to
C-                                          avoid XDW driver long string
C-                                          bomb
C-   Updated  20-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK
C-   Updated  30-APR-1991   Jeffrey Bantly  cleanup using new Compack 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER ICONT(10),NTRACK
      INTEGER HALF, LAYER, TRKNUM, STRQ, IVIEW
      INTEGER LKFTRH,GZFTRH,LKFDCH,GZFDCH, IER
      REAL XMIN,XMAX,YMIN,YMAX,XC,YC,ZC,X0,Y0,Z0(2)
      LOGICAL EZERROR
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
C ****  Pick PIXIE RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFTB3D','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
C
C Optimize string quality initially to prevent bombs in XDW driver
C   for long strings:
C
      CALL PUGET_i('STRING QUALITY',STRQ)
      CALL PUSETV('STRING QUALITY',3)
C
C  Select track to use for viewing parameters.
C
      CALL PFPICK_TRACK(TRKNUM,HALF,1)
      CALL PUGET_i('FDC 3D VIEW',IVIEW)
      IF(IVIEW.EQ. 2) THEN
        CALL PUGETV('FDC 3D X0',X0)
        CALL PUGETV('FDC 3D Y0',Y0)
      ENDIF
C
C  Calculate and set 3-D viewing parameters
C
      CALL JVUPNT(0.,0.,Z0(HALF+1))
      IF(IVIEW.EQ.1) CALL JNORML(-100.,-100.,Z0(HALF+1) )
      IF(IVIEW.EQ.2) THEN
        CALL JVUPNT(X0,Y0,0.5*Z0(HALF+1))
        CALL JNORML(-100.,-100.,0. )
      ENDIF
      IF(IVIEW.EQ.3) CALL JNORML(0.,0.,Z0(HALF+1) )
C
C  Draw 3-D track display for all TB tracks
C
      CALL PUOPEN
      CALL PF3MES(' ')
      CALL PF3MES(' FDC TESTBEAM 3-D HITS, SEGS, AND TRACKS')
      CALL GTFTRH(ICONT)
      NTRACK=ICONT(2)
      DO 10 TRKNUM=1,NTRACK             ! loop over all tracks with hits.
        CALL PF3DDRAW(HALF,TRKNUM)
        CALL PFPTRAK(TRKNUM)
   10 CONTINUE
      CALL JRCLOS
      CALL PUSETV('STRING QUALITY',STRQ)        ! reset string quality
C
C----------------------------------------------------------------------
  900 CALL EZRSET
  999 RETURN
      END
