      SUBROUTINE VTXISA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Builds the bank VITR which contains the ISAJET
C-   tracks in a fiducial volume inside the VTX
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-AUG-1988   Ghita Rahal-Callot
C-   Updated  24-OCT-1988   Ghita Rahal-Callot  : adapted for VTX
C-   Updated  14-AUG-1991   Tom Trippe  put debug control in VTRAKS.RCP
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVRFT.LINK'
C
      REAL RMIN, ZMIN, RMAX, ZMAX
      INTEGER NISA, LVRFT
      INTEGER USUNIT, PRUNIT, ICALL, IER
      LOGICAL DBG_VTXISA
      REAL FIDU
      SAVE ICALL,PRUNIT,DBG_VTXISA
      DATA ICALL /0/
C----------------------------------------------------------------------
      IF(ICALL .EQ. 0) THEN
        ICALL = 1
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('DBG_VTXISA',DBG_VTXISA,IER)
        CALL EZRSET
        PRUNIT=USUNIT()
      ENDIF
C
C ****  Find the limits of the VTX RMIN, ZMIN, RMAX, ZMAX in the geometry
C ****  banks
C
      IF ( LVGEH .LE. 0 ) CALL D0_ABORT(
     &                    ' **** VTXISA: Geometry not defined')
      LVRFT = LC ( LVGEH - IZVRFT )
      IF ( LVRFT .LE. 0 ) CALL D0_ABORT(
     &                    ' **** VTXISA: Geometry not defined')
C
C ****  Creates a fiducial volume in the VTX
C
      FIDU = .6
      RMIN = C ( LVRFT +  7 ) - C ( LVRFT + 5 ) + FIDU
      RMAX = C ( LVRFT + 21 ) + C ( LVRFT + 19 ) - FIDU
      ZMIN = C ( LVGEH + 17 )
      ZMAX = C ( LVGEH + 21 )
C
C ****  Create the Bank containing the ISAJET tracks crossing all the VTX
C
      NISA = 0
      CALL VTISTR ( ZMIN, RMIN, ZMAX, RMAX, NISA )
C
C ****  Print the ISAJET bank if asked
C
      IF ( DBG_VTXISA ) THEN
        CALL PRVITR( PRUNIT, 0, 0, ' ', 0 )
      ENDIF
C
  999 RETURN
      END
