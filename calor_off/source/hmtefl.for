      SUBROUTINE HMTEFL(LHMTE,LEVEL,PHI_LO,PHI_HI,THETA_LO,THETA_HI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank HMTE
C-
C-   Inputs  :LHMTE = link of bank to be filled.
C-            LHMTE < 0, routine will get link using GZHMTE
C-            LHMTE = 0, routine will book bank.
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  25-SEP-1990 12:17:52.73  Norman A. Graf
C-   Updated   6-JUL-1992   Rajendran Raja  CHANGED TO NEW H MATRIX FORMAT
C-   Updated  20-FEB-1994   Meenakshi Narain  Add call to HITSinfo
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
      INTEGER LHMTE, GZHMTE
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
      IF(LHMTE.LT.0)LHMTE = GZHMTE()    ! GET LINK.
C
      IF(LHMTE.EQ.0)CALL BKHMTE(LHMTE)
C
C Book the bank if argument = 0.
C
      IQ(LHMTE+1)  = 4    !VERSION 3
      IQ(LHMTE+2)  = LEVEL
      CHSQ_ALL = C(LHMTR+3)
      NDIM_ALL = C(LHMTR+4)
      PROB_ALL  = C(LHMTR+5)
      CHSQ_TRUNC = C(LHMTR+6)
      NDIM_TRUNC = C(LHMTR+7)
      PROB_TRUNC = C(LHMTR+8)
      IQ(LHMTE+3)  = NDIM_ALL
      IQ(LHMTE+4)  = NDIM_TRUNC
      Q(LHMTE+5)   = CHSQ_ALL
      Q(LHMTE+6)   = PROB_ALL
      Q(LHMTE+7)   = CHSQ_TRUNC
      Q(LHMTE+8)   = PROB_TRUNC
      Q(LHMTE+9)   = PHI_LO
      Q(LHMTE+10)  = PHI_HI
      Q(LHMTE+11)  = THETA_LO
      Q(LHMTE+12)  = THETA_HI
      CALL UCOPY(PRED_CENTER,Q(LHMTE+13),3)  !HMATRIX POSITION PREDICTED.
C
C ****  fill packed hits in photon road
C
      CALL HITSINFO(LHMTE,PACKED_HITS)
      CALL UCOPY(PACKED_HITS,Q(LHMTE+16),3)
C fill in the rest of the bank here.
C----------------------------------------------------------------------
  999 RETURN
      END
