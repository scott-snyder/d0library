      SUBROUTINE CATALOGUE_EM_CORR(LBANK,BANK,ECACL,ECLUS,PHI,THETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Transmits results of all corrections to 
C-                         EM clusters to VCOR, which is used by CAFIX 
C-                         to recalculate missing Et for the event.
C-
C-   Inputs  :  LBANK (I) Link to PELC/PPHO
C-              BANK  (C) 'PELC' or 'PPHO'
C-              ECACL (R)  Uncorrected Energy of cluster
C-              ECLUS (R)  Corrected Energy of cluster
C-              PHI   (R)  Phi of cluster
C-              THETA (R)  Theta of cluster
C-
C-   Outputs : to VCOR bank
C-             
C-             ECORR (R)    EX,EY,EZ,E plus variances (as well as known)
C-
C-   Controls: NONE
C-
C-   Created  16-DEC-1992   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LBANK,LVCOR
      REAL ECACL,ECLUS,PHI,THETA
      REAL EDIFF,ECORR(8)
      CHARACTER*4 BANK
C
C----------------------------------------------------------------------
C
      EDIFF    = ECLUS - ECACL
      ECORR(1) = EDIFF*COS(PHI)*SIN(THETA)  ! Ex
      ECORR(2) = EDIFF*SIN(PHI)*SIN(THETA)  ! Ey
      ECORR(3) = EDIFF*COS(THETA)           ! Ez
      ECORR(4) = EDIFF
C
      CALL VCORFL(LBANK,BANK,4,ECORR,LVCOR)
      LQ(LBANK-4) = LVCOR
  999 RETURN
      END
