      SUBROUTINE GAP_CAL_GAP(ETA1,ETA2,DR,IETAL,IETAH,Z)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert jet etas +/- cone to detector IETA
C-                         Finds nearest IETA edge
C-
C-
C-   Inputs  : ETA1, ETA2 = eta of jets (no order)
C-   Outputs : IETAL = IETA of upper edge of smaller eta jet cone
C-             IETAH = IETA of lower edge of bigger eta jet cone
C-
C-   Created  23-MAR-1993   Brent J. May
C-   Fixed    29-APR-1993   BJM  - convert to calor ieta using vertex
C-   Modified 27-MAY-1993   BJM  - add jet radius parameter DR
C-   Modified 22-OCT-1993   BJM  - subtract .05 from each jet to fix
C-                                 1st detac bin
C-   Renamed  23-JAN-1993   BJM  - renamed, no IER
C-   Modified 03-OCT-1995   AGB  - Use nominal vertex
C-   Modified 25-MAR-1996   AGB  - CALETA_INV_QCD
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC'
C----------------------------------------------------------------------
      INTEGER IETAH, IETAL, IIER
      REAL ETA1, ETA2, ETAL, ETAH, EL, EH, Z, DR
C----------------------------------------------------------------------
C get cone edges
      EL = MIN(ETA1,ETA2) + DR - 0.05
      EH = MAX(ETA1,ETA2) - DR + 0.05
C convert physics eta to detector real eta using vertex
      IF (Z.GT.150.) Z = ZOFF(IRUN)   !Nominal position
      CALL DET_CAL_ETA(Z,EL,ETAL)
      CALL DET_CAL_ETA(Z,EH,ETAH)
C convert real detector eta to IETA
      CALL CALETA_INV_QCD(ETAL,IETAL,IIER)
      CALL CALETA_INV_QCD(ETAH,IETAH,IIER)
C
  999 RETURN
      END
