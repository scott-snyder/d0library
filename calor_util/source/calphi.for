      SUBROUTINE CALPHI( IPHI, IETA, CENPHI, DELPHI, IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : THIS SUBROUTINE RETURNS THE CENTRAL TOWER
C-                         VALUE AND RANGE OF THE PHI 
C-                         FOR THE NOMINAL "PHYSICS" PHI.
C-
C-   Inputs  :    IPHI     "PHYSICS" PHI
C-                IETA     "PHYSICS" ETA
C-   Outputs :    CENPHI   PHI at the center of the tower
C-                DELPHI   range of PHI spanned by the tower
C-                IERR     error code  -- 0: OK 
C-                                        1: invalid eta, phi combination
C-   Controls:    NONE
C-
C-   Created  24-OCT-1988   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER  IPHI, IPH, IETA, IE, IERR
      REAL CENPHI, DELPHI
C
      IE = ABS(IETA)
      IERR = 0
C
      IF ( IE .GE. 1 .AND. IE .LE. 32) THEN         ! nominal region
         CENPHI = (IPHI-0.5)*TWOPI/64
         DELPHI = TWOPI/64 
      ELSE IF ( IE .GE. 33 .AND. IE .LE. 37 .AND. MOD(IPHI,2) .EQ. 1)
     +     THEN
         CENPHI = IPHI * TWOPI/64     ! eta 3.2 => 5.0 
         DELPHI = TWOPI/32
      ELSE
         CENPHI = 999.0             ! error situation
         DELPHI = 999.0
         IERR = 1
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
