C+
      SUBROUTINE SATRPL (LD, PL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : LD - hit address.
C-   Outputs : PL - number of tracking plane.
C-   Controls: none.
C-
C-   Created  25-OCT-1993   Alexander Efimov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LD, PL
      INTEGER N_PLANES, N_LAYS
      PARAMETER (N_PLANES=3, N_LAYS=4)
      INTEGER NST, NSC, IST, NPL, TUBE, LAY, NL1, M
      INTEGER LSSEC, GZSSEC
C
      M = IQ(LD+2)
      NST  = IBITS (M,  0,  5)     ! station number
      NSC  = IBITS (M,  5,  5)     ! section number
      NPL  = IBITS (M, 10,  5)     ! plane number
      TUBE = IBITS (M, 16, 16)     ! tube number
      IF (NST .EQ. 3 .OR. NST .EQ. 6) THEN
        IST = 2
      ELSE
        IST = 1
      END IF
      LSSEC = GZSSEC (NST, NSC)        ! 'SSEC' bank address
      NL1 = IC(LSSEC+10)            ! first tube layer number
      LAY = MOD (TUBE-1, N_LAYS)         ! layer number
      IF (IC(LSSEC+9) .GT. 0) THEN
        LAY = NL1 + LAY
        IF (LAY .GT. N_LAYS) LAY = LAY - N_LAYS
      ELSE
        LAY = NL1 - LAY
        IF (LAY .LT.      1) LAY = LAY + N_LAYS
      END IF
      PL = (IST - 1) * N_PLANES * N_LAYS +
     &     (NPL - 1) * N_LAYS + LAY
C
      RETURN
      END
