      SUBROUTINE LJTOP_HMATRIX_CALC_SPHERICITY(USE_W)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CALCULATES SPHERICIT AND Y
C-
C-   Inputs  : USE_W if true, will use the W 4vectron in calculations
C-   Outputs : 
C-   Controls: 
C-
C-   Created  25-SEP-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:EVENT_QUAN.INC'
      INTEGER NMAX
      PARAMETER( NMAX = 101 )
      REAL    P4VECS(4,NMAX),P3VECS(3,NMAX)
      REAL    P3_EIGVEC(3,3),P3_EIGVAL(3)
      REAL    P3_EIGVEC1(3,3),P3_EIGVAL1(3)
      INTEGER NOBJ,CMFRAME
      INTEGER I
      REAL    TOT_E
      REAL    PARTON(4),APARTON(4)
      REAL    FUDGE
      DATA FUDGE/1.0/
      SAVE FUDGE
      REAL    XF,TAU,HROOT_ES
      DATA HROOT_ES /1800.0/
      SAVE HROOT_ES
      REAL    X1,X2,ROOT
      REAL    PTOT(4),MOBJ
      LOGICAL USE_W
C----------------------------------------------------------------------
      NOBJ = 0
      DO I = 1 , NJETS
        NOBJ = NOBJ + 1
        CALL UCOPY(P25_JETS(1,I),P4VECS(1,NOBJ),4)
      ENDDO
C
      IF ( USE_W ) THEN
        NOBJ = NOBJ + 1
        CALL UCOPY(W4_VEC(1),P4VECS(1,NOBJ),4)
      ENDIF
C
      IF ( NOBJ.EQ.0 ) THEN
C PROTECT AGAINST 0 JET MUON CASE
        SPHERIC = 0.0
        YPLANAR = 0.0
        APLANAR = 0.0
C
        SPHERIC1 = 0.0
        YPLANAR1 = 0.0
        APLANAR1 = 0.0
        RETURN
      ENDIF
C
      CMFRAME = 0
      CALL SPHERICITY0(NOBJ,P4VECS,CMFRAME,SPHERIC,YPLANAR,APLANAR,
     &  P3_EIGVAL,P3_EIGVEC,PTOT,MOBJ)
C
C ****  NOW INCLUDE BEAM PARTONS IN PLANARITY CALCULATIONS
C
      CALL UZERO(PARTON,1,4)
      CALL UZERO(APARTON,1,4)
C
      XF = 0.0
      TAU = 0.0
      DO I = 1 , NOBJ
        XF = XF + P4VECS(3,I)  !Z COMPONENT OF FINAL STATE MOMENTA
        TAU = TAU + P4VECS(4,I) !FINAL STATE ENERGY
      ENDDO
C
      XF = XF/HROOT_ES         !FEYNMAN X
      TAU = (TAU/(2.0*HROOT_ES))**2  !SHAT/S
C
      ROOT = SQRT(XF*XF + 4.0*TAU)
      X1 = 0.5*(XF+ROOT)
      X2 = 0.5*(-XF+ROOT)
C
C BOTH X1 AND X2 POSITIVE.
C
      PARTON(3) = -X1*HROOT_ES  !INTO FINAL STATE
      APARTON(3) = X2*HROOT_ES
      PARTON(4) = X1*HROOT_ES
      APARTON(4) = X2*HROOT_ES
C
      NOBJ = NOBJ + 1
      CALL UCOPY(PARTON,P4VECS(1,NOBJ),4)
      NOBJ = NOBJ + 1
      CALL UCOPY(APARTON,P4VECS(1,NOBJ),4)
C
      CALL SPHERICITY0(NOBJ,P4VECS,CMFRAME,SPHERIC1,YPLANAR1,
     &  APLANAR1,P3_EIGVAL1,P3_EIGVEC1,PTOT,MOBJ)
C
  999 RETURN
      END
