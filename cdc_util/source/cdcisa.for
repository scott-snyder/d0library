      SUBROUTINE CDCISA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Builds the bank DITR which contains the ISAJET
C-   tracks in a fiducial volume inside the CDC
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-AUG-1988   Ghita Rahal-Callot
C-   Updated  22-MAR-1989   Qizhong Li-Demarteau  use DDEBUG.INC
C-   Updated  14-AUG-1991   Tom Trippe  put CDCISA name in ABORT msg.
C-   Updated  11-MAR-1992   Qizhong Li-Demarteau  use ERRMSG
C-   Updated   3-MAY-1994   Norman A. Graf  Call DISA_MARK to associate 
C-                                      DTRK banks with ISAJET particles
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$LINKS:IZDRFT.LINK'
      INCLUDE 'D0$INC:CDLTRK.INC'
C
      REAL RMIN, ZMIN, RMAX, ZMAX
      INTEGER NISA, LDRFT
      REAL FIDU
C----------------------------------------------------------------------
C
C ****  Find the limits of the CDC RMIN, ZMIN, RMAX, ZMAX in the geometry
C ****  banks
C
      IF (LDGEH .LE. 0) THEN
        CALL ERRMSG('DTRAKS','CDCISA','Geometry not defined','F')
      ENDIF
      LDRFT = LC ( LDGEH - IZDRFT )
      IF (LDRFT .LE. 0) THEN
        CALL ERRMSG('DTRAKS','CDCISA','Geometry not defined','F')
      ENDIF
C
C ****  Creates a fiducial volume in the CDC
C
      FIDU = .6
      RMIN = C ( LDRFT + 11 ) - C ( LDRFT + 8 ) + FIDU
      RMAX = C ( LDRFT + 17 ) + C ( LDRFT + 8 ) - FIDU
      ZMIN = C ( LDGEH + 14 )
      ZMAX = ZMIN
C
C ****  Create the Bank containing the ISAJET tracks crossing all the CDC
C
      NISA = 0
      CALL CDISTR ( ZMIN, RMIN, ZMAX, RMAX, NISA )
C
C ****  Clean up DITR bank
C
      CALL PUDITR
C
C ****  Match to CDC tracks
C
      CALL DISA_MARK
C
C ****  Print the ISAJET bank if asked
C
      IF ( LVLDBG(10) .GT. 0 ) CALL PRDITR( LUNDBG, LDITR, 0, ' ', 0 )
  999 RETURN
      END
