      SUBROUTINE MTC_GETMUVTX(POINT,DIRCOS, zvertex,hadfrac,enersum)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Point the input candidate track, specified
C-    by the input point(3) and direction cosines dircos(3) through
C-    the calorimeter to determine a vertex for the track.
C-
C-   Inputs  : point(3) and dircos(3) specify the position and direction
C-             of the input candidate track in the muon system A-layer
C-             or mid-toroid.
C-   Outputs :
C-      zvertex - the position of the track origin at the beamline
C-                obtained by tracking the candidate through the
C-                calorimeter
C-      hadfrac - the fraction of hadronic layers hit in the longest
C-                set of contiguously hit cal cells in the traj path
C-      enersum - the sum of the energies seen in the cal cells
C-                associated with the track
C-
C-   Created  24-JUL-1995   Elizabeth Gallas
C-   Notes:  part of the D0FIX muon vertexing package -
C-    The original version of this program assumes the z-vertex
C-    position is the z location of the dca between the beamline and a
C-    a line drawn from the input muon point and the center of gravity
C-    of the fit through the calorimeter cells associated with the track.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input ...
      REAL    POINT(3),DIRCOS(3)
C- output ...
      REAL    zvertex,hadfrac,enersum
C- local stuff ...
      REAL    cdist,hfrac,esum,cosmtc(3),pntmtc(3),tresid
      INTEGER nhad,np,iflg
      REAL    zvtx(3),zdca
C----------------------------------------------------------------------
C- find the cal cells associated with the track, fit a line through them
      CALL MTC_HADTRACKS( POINT,DIRCOS,
     &  cdist,hfrac,esum,cosmtc,pntmtc,tresid,nhad,np,iflg)
C- if input track doesn't project to the beamline (iflg<0), the candidate
C- trajectory is assumed to go through z=0.
C-    IF(iflg.LT.0) go to 666
C- no good calorimeter track was found if nhad<2 or hfrac<.75
      IF(nhad.LE.1) go to 666
      IF(hfrac.LE.0.65) go to 666
C- fit a line from the point in the a layer through the point found by
C- hadtracks ... determine a 'vertex' from this line
      CALL MTC_PNTTOCOS(PNTMTC,POINT, dircos)
      CALL MTCL2_ZIMPACT(PNTMTC,DIRCOS, zdca,zvtx,iflg)

      zvertex = zvtx(3)
      hadfrac = hfrac
      enersum = esum
      go to 999
C----------------------------------------------------------------------
  666 CONTINUE
      zvertex = -200.
      hadfrac = 0.
      enersum = 0.

  999 RETURN
      END
