      SUBROUTINE TND2FL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank TND2.
C-
C-   Inputs  : 
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  16-APR-1996 22:49:45.89  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEbstp.INC'
C----------------------------------------------------------------------
      INTEGER GZTND2,I
      INTEGER LTND2,NBINS
      REAL HMIN,HMAX
      PARAMETER (NBINS = 76)
      PARAMETER (HMIN = 0.01)
      PARAMETER (HMAX = 30.0)
      REAL TSUM_TO_EPS_2LYR(NBINS),norm_runs_2lyr(3), norm_hits_2lyr
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA TSUM_TO_EPS_2LYR/0.99962,0.99677,0.99236,0.98563,0.97559,
     &0.96131,0.94221,0.91784,0.88755,0.85047,
     &0.80563,0.75225,0.69045,0.62248,0.55251,
     &0.48390,0.41829,0.35681,0.30086,0.25210,
     &0.21169,0.17939,0.15350,0.13198,0.11356,
     &0.09761,0.08382,0.07191,0.06169,0.05299,
     &0.04570,0.03969,0.03477,0.03071,0.02726,
     &0.02421,0.02142,0.01882,0.01640,0.01412,
     &0.01199,0.01004,0.00831,0.00686,0.00569,
     &0.00475,0.00399,0.00339,0.00289,0.00248,
     &0.00213,0.00185,0.00160,0.00139,0.00121,
     &0.00105,0.00092,0.00080,0.00069,0.00060,
     &0.00052,0.00045,0.00039,0.00033,0.00028,
     &0.00024,0.00020,0.00017,0.00014,0.00011,
     &0.00008,0.00006,0.00004,0.00003,0.00001,0.00000/
      DATA norm_runs_2lyr/1.06346,0.92607,0.92607/    ! 1a, 1b, 1c
      DATA norm_hits_2lyr/1.05984/  ! trunc. sum scale for '11' tracks
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF
C
        LTND2 = GZTND2()    ! GET LINK.
C
      IF ( LTND2 .EQ. 0 ) THEN
        CALL BKTND2(LTND2)
      ENDIF
C
C Book the bank if argument = 0.
C
C
C fill in the rest of the bank here.
      IC (LTND2+1) = NBINS               ! nbins
      C  (LTND2+2) = HMIN
      C  (LTND2+3) = HMAX
      c  (LTND2+4)=norm_runs_2lyr(1)  !1a
      c  (LTND2+5)=norm_runs_2lyr(2)  !1b
      c  (LTND2+6)=norm_runs_2lyr(3)  !1c
      c  (LTND2+7)=norm_hits_2lyr     !trunc. sum scale for '11' tracks
      DO I=1,NBINS
        C(LTND2+7+I)=TSUM_TO_EPS_2LYR(I)
      END DO
  999 RETURN
      END
