      REAL FUNCTION LIKE_REF_MC(LIKE,NHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute espt for MC events
C-     tables have been computed using mc W-e,nu and Z-->ee. There were 99 bins
C-     for -2<like<8. The Like distributions have been smoothed and
C-     integrated. Under/over flows have been taken into account. each vakue in
C-     the tables represent the integral of the distribution up to the right
C-     edge of the bin. For instance, the first bin value contains the
C-     probability for Like>xmi, the second bin contains the probability for
C-     Like>xmi+step. We get the current value by interpolating between the 2
C-     values at the edges of the bin

C-
C-   Returned value  :
C-   Inputs  :
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
      REAL LIKE
      INTEGER I,ICAS,NHIT(3),IFOIS
      integer nbin
      PARAMETER( nbin =  100)
      REAL LIK1(nbin),LIK2(nbin),LIKR(nbin,2),STEP,XMA,XMI
      EQUIVALENCE (LIK1(1),LIKR(1,1)),(LIK2(1),LIKR(1,2))
      INTEGER NX,IB,IBI,IBS
      REAL X(nbin)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA XMI/-2./,XMA/6./
c  lik2 one cell
c   nx  99 xmi-2.000 xma  6.000 mean value  3.276
c  entries:      4655 underflows:      47.0 overflows:      54.0
      DATA LIK1/
     +0.990,0.989,0.989,0.988,0.988,0.987,0.987,0.987,0.987,0.986,
     +0.985,0.984,0.983,0.982,0.981,0.980,0.980,0.978,0.978,0.977,
     +0.975,0.973,0.972,0.970,0.968,0.966,0.962,0.960,0.958,0.954,
     +0.952,0.950,0.947,0.943,0.937,0.929,0.925,0.920,0.914,0.907,
     +0.900,0.894,0.888,0.881,0.874,0.867,0.860,0.847,0.837,0.823,
     +0.811,0.798,0.785,0.773,0.758,0.742,0.728,0.713,0.700,0.683,
     +0.667,0.650,0.635,0.610,0.587,0.563,0.541,0.518,0.492,0.470,
     +0.446,0.416,0.391,0.370,0.344,0.320,0.295,0.275,0.252,0.230,
     +0.212,0.191,0.173,0.156,0.141,0.122,0.104,0.090,0.077,0.067,
     +0.057,0.048,0.040,0.033,0.027,0.024,0.019,0.016,0.014,0.012/
clik2 2 cells
c nx  99 xmi-2.000 xma  6.000 mean value  2.424
c  entries:      5078 underflows:      93.0 overflows:       4.0
      DATA LIK2/
     +0.982,0.980,0.978,0.977,0.976,0.974,0.973,0.971,0.970,0.969,
     +0.967,0.964,0.960,0.957,0.956,0.953,0.948,0.944,0.942,0.937,
     +0.935,0.932,0.928,0.924,0.918,0.911,0.907,0.902,0.897,0.890,
     +0.880,0.873,0.865,0.858,0.848,0.840,0.830,0.818,0.808,0.800,
     +0.783,0.772,0.759,0.746,0.730,0.716,0.702,0.682,0.665,0.649,
     +0.634,0.617,0.600,0.581,0.556,0.531,0.512,0.485,0.460,0.438,
     +0.417,0.396,0.372,0.349,0.325,0.305,0.286,0.266,0.247,0.224,
     +0.203,0.184,0.165,0.146,0.129,0.113,0.098,0.087,0.077,0.065,
     +0.057,0.048,0.044,0.039,0.032,0.027,0.022,0.018,0.016,0.011,
     +0.010,0.008,0.008,0.006,0.004,0.004,0.003,0.002,0.001,0.001/
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
      LIKE_REF_MC=1.
      IF(NHIT(1).LE.0 .OR.NHIT(2).LE.0 .OR.NHIT(3).EQ.0)GO TO 999
      ICAS=1
      IF(NHIT(1).GT.1 .OR.NHIT(2).GT.1 .OR.NHIT(3).GT.1)ICAS=2
      IF(LIKE.LE.XMI)THEN
        LIKE_REF_MC=LIKR(1,ICAS)
        GO TO 999
      END IF
      IF(LIKE.GE.XMA)THEN
        LIKE_REF_MC=LIKR(Nbin,ICAS)
        GO TO 999
      END IF
      IB=(LIKE-XMI)/STEP+1
      IBI=IB+1
      DX=LIKE-X(IB)
      SL=(LIKR(IBI,ICAS)-LIKR(IB,ICAS))/step
      LIKE_REF_MC=LIKR(IB,ICAS)+DX*SL
  999 RETURN
      END
