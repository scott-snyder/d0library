      LOGICAL FUNCTION B1M_MUNEARJET(LPMUO,NJTS,ETAJET,PHIJET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search for a jet within DR from input Muon
C-
C-   Returned value  : TRUE if such Jet exists,  FALSE otherwise
C-   Inputs  : LPMUO and  DR = max Mu-Jet distance for "near-ness"
C-   Outputs : None
C-   Controls: None
C-
C-   Created   14-SET-1995   Arthur Maciel
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'

      INTEGER LPMUO,NJTS,II
      REAL    ETAMU,PHIMU,DRMJ
      REAL    DRMIN,DETA,DPHI
      REAL    ETAJET(*),PHIJET(*)

C.. Init.
      B1M_MUNEARJET = .FALSE.
      IF(NJTS.LE.0) GOTO 999

      ETAMU = Q(LPMUO+16)             ! Eta
      PHIMU = Q(LPMUO+17)             ! Phi (rad.)

C.. Find Closest jet to muon
      DRMIN = 9.

C.. Loop over test objects looking for closest one
      DO II=1,NJTS
        DETA=ETAMU-ETAJET(II)
        DPHI=ABS(PHIMU-PHIJET(II))
c-      fix dphi at 0-2pi boundary.
        IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
        DRMJ=SQRT( DETA**2+DPHI**2 )
        IF(DRMJ.LT.DRMIN) DRMIN=DRMJ
      ENDDO

      IF(DRMIN.LT.1.0) B1M_MUNEARJET = .TRUE.

  999 RETURN
      END
