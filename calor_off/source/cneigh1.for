      SUBROUTINE CNEIGH1(IETAC,IPHIC,IETA,IPHI,DELETA,DELPHI,CSPREAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given IETAC, IPHIC of the center of a
C-                         Cluster and IETA,IPHI of a cell,
C-                         routine works out DELETA,DELPHI
C-                         the difference in ETA and PHI
C-                         For H Matrix usage
C-
C-   Inputs  : IETAC,IPHIC,IETA,IPHI
C-             CSPREAD = WINDOW AROUND HIGH CELL THAT IS ALLOWED
C-   Outputs : DELETA,DELPHI. difference in ETA and PHI with maximum
C-             limits set by HMATRIX dimensions
C-   Controls:
C-
C-   Created   4-JUN-1989   Rajendran Raja
C-   REMOVED BRINGING ETA AND PHI WITHIN BOUNDARIES. THIS WILL MEAN THAT
C-   ONLY THE NEAREST CELL IS COUNTED IN THE H MATRIX . REDUCES EFFECT 
C-   OF NOISE.
C-   Updated  12-AUG-1992   Meenakshi Narain  take of PHI above ETA > 32 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETAC,IPHIC,IETA,IPHI,DELETA,DELPHI,ISIGN
      INCLUDE 'D0$INC:CHMATR_NEW.INC'
      REAL    ETACM,ETAIM,SIGN
      REAL    CSPREAD
C----------------------------------------------------------------------
      ETACM=IETAC-SIGN(0.5,FLOAT(IETAC))         ! Solve the displaced zero
      ETAIM=IETA-SIGN(0.5,FLOAT(IETA))         ! Solve the displaced zero
      DELETA = ETAIM-ETACM              ! difference in ETA
      IF (IABS(DELETA).LE.CSPREAD) THEN
        DELETA = MAX(DELETA,-NET)
        DELETA = MIN(DELETA,NET)          ! Bringing within bounds
      END IF
C
C ****  NOW THE PHI PART
C
      DELPHI = IPHI - IPHIC
      IF(IABS(DELPHI).GT.31)THEN
        DELPHI = DELPHI - ISIGN(64,DELPHI)
      ENDIF
      IF (IETA.GE.33) THEN
        DELPHI = DELPHI/2
      END IF
      IF (IABS(DELPHI).LE.CSPREAD) THEN
        DELPHI = MAX(DELPHI,-NPH)
        DELPHI = MIN(DELPHI,NPH)
      ENDIF
  999 RETURN
      END
