      SUBROUTINE PFHALF( HALF )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display one half of the Forward Drift Chambers
C-                         with hits and tracks, if requested
C-
C-   Inputs  : HALF 
C-   Outputs : puts display on window
C-   Controls:
C-
C-   Created  27-JAN-1989   Jeffrey Bantly
C-   Updated  23-JAN-1991   Jeffrey Bantly  remove segment creation 
C-   Updated  20-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK 
C-   Updated  30-APR-1991   Jeffrey Bantly  make better use of Compack 
C-   Updated  14-MAY-1991   Susan K. Blessing  Make the user use N or S 
C-                                             for half value
C-   Updated   8-AUG-1991   Robert E. Avery   Only prompt user if HALF ne -1
C-                              Other cleanups.
C-   Updated   9-SEP-1991   Robert E. Avery  Use previous half as default.
C-   Updated   8-NOV-1991   Robert E. Avery  Clean up, draw real tracks here
C-                              (not in PFTHET).
C-   Updated  25-JAN-1992   Robert E. Avery  Check existance of banks.
C-    Add call to FDCISA, so that isajet tracks in FDC can be drawn.
C-   Updated  14-JAN-1993   Robert E. Avery  Use REAL version of PI.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      REAL    PI, TWOPI, HALFPI, RAD
      INCLUDE 'D0$INC:PI.INC'
C
      INTEGER HALF, LAYER, IVIEW
      INTEGER DRDELH, DRATRK, DRAISA, IER
      INTEGER LISAE, GZISAE
      INTEGER LFTRH, GZFTRH
C
      LOGICAL EZERROR
C
      CHARACTER*11 TEXT
C
C----------------------------------------------------------------------
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFHALF','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('FDC DRAW TRACK',DRATRK)
      CALL PUGETV('FDC DRAW ISATRK',DRAISA)
      CALL EZRSET
C
C   draw label
C
      IF(HALF.EQ.0) THEN
        TEXT = ' North FDC '
      ELSEIF(HALF.EQ.1) THEN
        TEXT = ' South FDC '
      ELSE
        HALF = 0 
        TEXT = ' North FDC '
      ENDIF
      CALL JJUST(2,2)
      CALL PUVSTR( 0.0, 75.0, 1.5, 1.5,TEXT)
C
C   draw inner theta
C
      LAYER = 0
      CALL PFTHET(HALF,LAYER)

C
C   draw outer theta
C
      LAYER = 1
      CALL PFTHET(HALF,LAYER)
C
C   draw real tracks if requested
C
      IVIEW=4+HALF
      LFTRH=GZFTRH()
      IF ( (LFTRH.GT.0) .AND. ( DRATRK .GT. 0 ) ) THEN 
        CALL PFTKDR(0,PI,PI,TWOPI,IVIEW)
      ENDIF
C
C  draw isajet tracks if requested
C
      LISAE = GZISAE()
      IF ( (DRAISA.GE.1) .AND. (LISAE .GT. 0) ) THEN
        CALL FDCISA
        CALL PFISTR(HALF)
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
