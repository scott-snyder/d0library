      SUBROUTINE TND1FL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank TND1.
C-
C-   Inputs  :
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  16-APR-1996 22:18:24.71  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTND1,NBINS
      REAL HMIN,HMAX
      PARAMETER (NBINS = 76)
      PARAMETER (HMIN = 0.01)
      PARAMETER (HMAX = 30.0)
      REAL TSUM_TO_EPS_1LYR(NBINS),NORM_RUNS_1LYR(3), NORM_HITS_1LYR
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      INTEGER GZTND1,I
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST

      DATA TSUM_TO_EPS_1LYR/0.99981,0.98589,0.97181,0.95710,0.94104,
     &0.92250,0.89982,0.87115,0.83579,0.79519,
     &0.75121,0.70480,0.65639,0.60629,0.55498,
     &0.50328,0.45241,0.40399,0.35956,0.31997,
     &0.28513,0.25424,0.22620,0.20024,0.17627,
     &0.15513,0.13743,0.12260,0.10983,0.09864,
     &0.08875,0.07996,0.07212,0.06512,0.05885,
     &0.05323,0.04819,0.04366,0.03959,0.03593,
     &0.03264,0.02967,0.02699,0.02458,0.02240,
     &0.02044,0.01866,0.01706,0.01560,0.01428,
     &0.01309,0.01200,0.01101,0.01011,0.00928,
     &0.00852,0.00783,0.00719,0.00660,0.00606,
     &0.00555,0.00509,0.00465,0.00425,0.00388,
     &0.00353,0.00321,0.00291,0.00263,0.00238,
     &0.00213,0.00191,0.00170,0.00151,0.00132,0.00116/
      DATA NORM_RUNS_1LYR/1.16346,0.87243,0.87243/    ! 1a, 1b, 1c
      DATA NORM_HITS_1LYR/1.03134/  ! trunc. sum scale for single hits
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF
C
      LTND1 = GZTND1()    ! GET LINK.
C
      IF ( LTND1 .EQ. 0 ) THEN
        CALL BKTND1(LTND1)
      ENDIF
C
C Book the bank if argument = 0.
C
      IC(LTND1+1)  = NBINS               ! nbins
      C  (LTND1+2) = HMIN
      C  (LTND1+3) = HMAX
      C  (LTND1+4)=NORM_RUNS_1LYR(1)  !1a
      C  (LTND1+5)=NORM_RUNS_1LYR(2)  !1b
      C  (LTND1+6)=NORM_RUNS_1LYR(3)  !1c
      C  (LTND1+7)=NORM_HITS_1LYR     !trunc. sum scale for '11' tracks
      DO I=1,NBINS
        C(LTND1+7+I)=TSUM_TO_EPS_1LYR(I)
      END DO
C
C fill in the rest of the bank here.
  999 RETURN
      END
