      FUNCTION FADCDL (WID, X1, X2, XMIN, XMAX)
C======================================================================
C
C   Purpose and Methods : Integrate a Breit and Wigner function
C                         between X1 and X2, its center being 0
C
C   Inputs  :      WID  width of the signal in the FADC ( FWHM )
C                  X1,X2  Bin limits
C                  XMIN, XMAX, limits for the truncation of the BW
C   Outputs :      None
C
C-   Created  12-MAR-1987   Ghita Rahal-Callot
C-   Updated  25-OCT-1988  Tom Trippe : correct normalisation
C
C======================================================================
      IMPLICIT NONE
C
      REAL FADCDL
      REAL X1, X2, WID, HWID, XMIN, XMAX, ANORM
      REAL SAVMIN, SAVMAX, SAVWID
      DATA SAVMIN, SAVMAX, SAVWID / 3*0./
C
C======================================================================
C
      HWID = WID / 2.
C
C ****  NORMALISATION
      IF ( (SAVMIN.NE.XMIN) .OR. (SAVMAX.NE.XMAX) .OR.
     &                           (SAVWID.NE.WID)) THEN
        SAVMIN = XMIN
        SAVMAX = XMAX
        SAVWID = WID
        ANORM = ATAN (XMAX/HWID ) - ATAN ( XMIN/HWID )
      END IF
C
C
      FADCDL = ( ATAN(X2/HWID) - ATAN(X1/HWID) ) / ANORM
C
  999 CONTINUE
      RETURN
      END
