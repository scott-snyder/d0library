      SUBROUTINE MNGTCN(NMOD,NCEL,NPMT,XYZ,DXYZ,TZER,TSLP,JERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get constants for scintillator
C-
C-   Inputs  : NMOD    Module ID
C-             NCEL    Cell number
C-
C-   Outputs : NPMT    Number of phototubes
C-             XYZ(3)  Center of scintillator
C-             DXYZ(3) Half widths of scintillator
C-             TZER(2) T0 constants
C-             TSLP(2) ADC to Time constants
C-             JERR    Error flag (0 if OK)
C-
C-   Controls: None
C-
C-   Created   4-NOV-1992   B.S.Acharya
C_   Updated   7-FEB-1994   Atsushi Taketani  use MUSCNT
C----------------------------------------------------------------------
      IMPLICIT NONE
C-Arguements--
      INTEGER NMOD,NCEL,NPMT,JERR
      REAL TZER(2), TSLP(2), XYZ(3), DXYZ(3)
C--INcludes--
C--Local Variables---
      INTEGER MUSCNT_ADR, NSCI, NWIR
      CHARACTER*4 HSHAPE
      INTEGER     NSPAR, NBUF, IBUF(8)
      REAL        ROTM(3,3), BUF(8), SPAR(3)
      EQUIVALENCE (BUF(1), IBUF(1))
      INTEGER     K1, K2
C-----------------------------------------------------------------------
C      CALL GTMSGE(NMOD,NCEL,NPMT,XYZ,DXYZ,JERR)
      JERR = 1
      CALL GTMSTC(NMOD,NCEL,TZER,TSLP)
C
      NWIR = NCEL/4
      NSCI = NWIR/2 + 1 
      MUSCNT_ADR = NMOD + NSCI*1000
      CALL MUSCNT( MUSCNT_ADR, HSHAPE, NSPAR, SPAR, XYZ, ROTM, NBUF,
     &  IBUF )
      IF ( NSPAR.EQ.0 ) GOTO 999
      NPMT = IBUF(1)
      DO 110 K1=1,3
        DXYZ(K1) = 0.0
      DO 100 K2=1,3
  100   DXYZ(K1) = DXYZ(K1) + ROTM(K1,K2)*SPAR(K2)
  110 DXYZ(K1) = ABS(DXYZ(K1))
C
      JERR = 0
C
  999 RETURN
      END
