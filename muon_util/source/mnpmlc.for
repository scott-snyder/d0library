      SUBROUTINE MNPMLC( NMOD, NSCINT, NPM, POS, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Muon scintillator Photo tube position
C-                         on local cordinate of scintillator. 
C-                         This routine calls MUSCINT routine and get
C-                         geometry information.
C-
C-   Inputs  : NMOD  : muon module number
C-             NSCINT: scintillator number
C-             NPM   : photo tube number ( must be 1  or 2 )
C-   Outputs : POS   : 3 dimensional position in local cordinate of 
C-                     Scintillator
C-             IER   : error code, 0 - no error
C-   Controls: 
C-
C-   Created  29-MAY-1992   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMOD, NSCINT, NPM, IER
      REAL    POS(3)
C
      REAL    SPAR(3),XPAR(3),ROTM(3,3), BUF(9), ROT(3,3)
      CHARACTER*4 HSHAPE
      INTEGER NBUF,IBUF(9), NSPAR
      EQUIVALENCE ( BUF(1), IBUF(1))
      REAL    POSD(3)
      INTEGER I
C
      INTEGER INDEX,NERR
      REAL DET
C----------------------------------------------------------------------
      CALL MUSCNT( NMOD+NSCINT*1000, HSHAPE,NSPAR,SPAR,XPAR,ROTM
     +                 ,NBUF,IBUF)
C
      IF ( ROTM(1,1).GE.100.0 ) THEN     ! not existence scintillator
        IER = 1
        GOTO 999
      END IF
C
      IF ( NPM.GT.IBUF(1) ) THEN         ! not existence photo tube
        IER = 1
        GOTO 999
      END IF
C
      IER = 0
C
      DO 100 I=1,3
  100   POSD(I) = BUF( I + (NPM-1)*3 + 1  ) - XPAR(I)
C
      CALL MATIN1( ROTM, 3,3,3,0, INDEX, NERR, DET )
      CALL VMATR( POSD, ROTM, POS, 3, 3 )
C
  999 RETURN
      END
