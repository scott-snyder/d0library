      REAL FUNCTION ETRUNC_REF_MC(ETRUNC,NHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute espt for MC events
C-     tables have been computed using mc W-e,nu and Z-->ee. There were 149 bins
C-     for 0<etrunc<25. The Etrunc distributions have been smoothed and
C-     integrated. Under/over flows have been taken into account. each value in
C-     the tables represent the integral of the distribution up to the right
C-     edge of the bin. For instance, the first bin value contains the
C-     probability for Etrunc>xmi, the second bin contains the probability for
C-     Etrunc>xmi+step. We get the current value by interpolating between the 2
C-     values at the edges of the bin

C-
C-   Returned value  :
C-   Inputs  :ETRUNC: truncated <Efired> 
C-            NHIT: number of hit cells in each layer
C-   Outputs :
C-   Controls:
C-
C-   Created   5-OCT-1995   A. Zylberstejn
C-   Updated   5-JAN-1996   A. ZYLBERSTEJN  Calibration constants: 680,619,641
C-                                           set such that < Efired>=4
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL DX,ENERGT,IGEN,SL
C      INCLUDE 'D0$INC:PAWIDN_TRD.INC'
      REAL ETRUNC
      INTEGER I,ICAS,NHIT(3),IFOIS
      integer nbin
      PARAMETER( nbin =  150)
      REAL EPST1(nbin),EPST2(nbin),EPSTR(nbin,2),STEP,XMA,XMI
      EQUIVALENCE (EPST1(1),EPSTR(1,1)),(EPST2(1),EPSTR(1,2))
      INTEGER NX,IB,IBI,IBS
      REAL X(nbin)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA XMI/0./,XMA/25./
c etrunc one cell
c nx 149 xmi 0.000 xma 25.000 mean value  6.617
centries:      4655 underflows:       0.0 overflows:       4.0
      data epst1/
     +1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,
     +1.000,0.999,0.998,0.998,0.997,0.994,0.993,0.989,0.984,0.981,
     +0.973,0.960,0.949,0.937,0.918,0.896,0.874,0.850,0.822,0.791,
     +0.758,0.724,0.691,0.654,0.612,0.577,0.540,0.503,0.469,0.436,
     +0.396,0.363,0.335,0.308,0.282,0.258,0.235,0.214,0.195,0.180,
     +0.168,0.150,0.139,0.128,0.118,0.111,0.100,0.091,0.084,0.078,
     +0.071,0.065,0.060,0.056,0.054,0.051,0.047,0.045,0.043,0.038,
     +0.036,0.034,0.030,0.028,0.027,0.026,0.025,0.023,0.023,0.021,
     +0.020,0.020,0.020,0.018,0.017,0.017,0.016,0.015,0.015,0.014,
     +0.014,0.013,0.012,0.012,0.011,0.011,0.010,0.009,0.009,0.009,
     +0.008,0.008,0.007,0.007,0.006,0.006,0.006,0.005,0.005,0.005,
     +0.005,0.005,0.004,0.004,0.004,0.004,0.003,0.003,0.003,0.003,
     +0.003,0.003,0.003,0.002,0.002,0.002,0.002,0.002,0.002,0.002,
     +0.002,0.002,0.002,0.002,0.002,0.002,0.002,0.002,0.002,0.001,
     +0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001,0.001/
c etrunc 2 cells
c nx 149 xmi 0.000 xma 25.000 mean value  6.920
centries:      5315 underflows:       0.0 overflows:       7.0
      data epst2/
     +1.000,1.000,1.000,1.000,0.999,0.999,0.998,0.998,0.997,0.996,
     +0.994,0.991,0.990,0.986,0.983,0.979,0.975,0.970,0.964,0.959,
     +0.949,0.938,0.925,0.911,0.899,0.882,0.863,0.839,0.816,0.788,
     +0.765,0.736,0.701,0.671,0.636,0.607,0.574,0.545,0.513,0.484,
     +0.449,0.422,0.394,0.371,0.345,0.323,0.306,0.288,0.267,0.251,
     +0.233,0.214,0.199,0.184,0.172,0.160,0.150,0.140,0.127,0.118,
     +0.111,0.103,0.096,0.091,0.084,0.080,0.076,0.071,0.066,0.061,
     +0.057,0.054,0.051,0.046,0.043,0.039,0.037,0.034,0.033,0.031,
     +0.030,0.029,0.027,0.026,0.025,0.024,0.023,0.022,0.021,0.020,
     +0.019,0.018,0.017,0.016,0.016,0.015,0.015,0.015,0.014,0.013,
     +0.012,0.012,0.012,0.011,0.011,0.011,0.011,0.011,0.010,0.010,
     +0.009,0.009,0.009,0.008,0.008,0.008,0.007,0.007,0.007,0.006,
     +0.006,0.006,0.006,0.005,0.005,0.004,0.004,0.004,0.004,0.003,
     +0.003,0.003,0.003,0.003,0.003,0.003,0.003,0.003,0.002,0.002,
     +0.002,0.002,0.002,0.002,0.002,0.002,0.002,0.002,0.002,0.001/
      IF(FIRST)THEN
        IFOIS=0
        nx=nbin-1
        STEP=(XMA-XMI)/FLOAT(NX)
        DO I=1,NX
          X(I)=XMI+FLOAT(I-1)*STEP ! bin right edge
        END DO
C        print*,x
        FIRST=.FALSE.
      END IF
      IFOIS=IFOIS+1
      ETRUNC_REF_MC=1.
      IF(NHIT(1).LE.0 .OR.NHIT(2).LE.0 .OR.NHIT(3).EQ.0)GO TO 999
      ICAS=1
      IF(NHIT(1).GT.1 .OR.NHIT(2).GT.1 .OR.NHIT(3).GT.1)ICAS=2
      IF(ETRUNC.LE.XMI)THEN
        ETRUNC_REF_MC=EPSTR(1,ICAS)
        GO TO 999
      END IF
      IF(ETRUNC.GE.XMA)THEN
        ETRUNC_REF_MC=EPSTR(Nbin,ICAS)
        GO TO 999
      END IF
      IB=(ETRUNC-XMI)/STEP+1
      IBI=IB+1
      DX=ETRUNC-X(IB)
      SL=(EPSTR(IBI,ICAS)-EPSTR(IB,ICAS))/step
      ETRUNC_REF_MC=EPSTR(IB,ICAS)+DX*SL
  999 RETURN
      END
