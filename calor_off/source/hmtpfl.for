      SUBROUTINE HMTPFL(LHMTP,LEVEL,PHI_LO,PHI_HI,THETA_LO,THETA_HI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank HMTP
C-
C-   Inputs  :LHMTP = link of bank to be filled.
C-            LHMTP < 0, routine will get link using GZHMTP
C-            LHMTP = 0, routine will book bank.
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  25-SEP-1990 12:23:38.33  Norman A. Graf
C-   Updated   6-JUL-1992   Rajendran Raja  CHANGED TO NEW FORMAT
C-   Updated  20-FEB-1994   Meenakshi Narain  all call to HITSINFO
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:CHMATR_NEW.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER LHMTP, GZHMTP
      REAL PHI_LO,PHI_HI,THETA_LO,THETA_HI
      INTEGER LEVEL
      REAL    CHSQ_ALL,PROB_ALL,CHSQ_TRUNC,PROB_TRUNC
      INTEGER NDIM_ALL,NDIM_TRUNC
      INTEGER PACKED_HITS(3)
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF
C
      IF(LHMTP.LT.0)LHMTP = GZHMTP()    ! GET LINK.
C
      IF(LHMTP.EQ.0)CALL BKHMTP(LHMTP)
C
C Book the bank if argument = 0.
C
      IQ(LHMTP+1)  = 4    !VERSION
      IQ(LHMTP+2)  = LEVEL
      CHSQ_ALL = C(LHMTR+3)
      NDIM_ALL = C(LHMTR+4)
      PROB_ALL  = C(LHMTR+5)
      CHSQ_TRUNC = C(LHMTR+6)
      NDIM_TRUNC = C(LHMTR+7)
      PROB_TRUNC = C(LHMTR+8)
      IQ(LHMTP+3)  = NDIM_ALL
      IQ(LHMTP+4)  = NDIM_TRUNC
      Q(LHMTP+5)   = CHSQ_ALL
      Q(LHMTP+6)   = PROB_ALL
      Q(LHMTP+7)   = CHSQ_TRUNC
      Q(LHMTP+8)   = PROB_TRUNC
      Q(LHMTP+9)   = PHI_LO
      Q(LHMTP+10)  = PHI_HI
      Q(LHMTP+11)  = THETA_LO
      Q(LHMTP+12)  = THETA_HI
      CALL UCOPY(PRED_CENTER,Q(LHMTP+13),3)  !HMATRIX POSITION PREDICTED.
C----------------------------------------------------------------------
C
C ****  fill packed hits in photon road
C
      CALL HITSINFO(LHMTP,PACKED_HITS)
      CALL UCOPY(PACKED_HITS,Q(LHMTP+16),3)
  999 RETURN
      END
