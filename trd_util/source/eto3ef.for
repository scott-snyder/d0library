      REAL FUNCTION ETO3EF(ENERGT,ANG,IGEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute electron efficiency with TOTAL ENERGY
C-                         deposited in the 3 TRD layers when 3 TEC are
C-                         hit
C-
C-   Returned value  : Value of the electron efficiency
C-   Inputs  : ENERGT   = summ of the energy deposit in 3 layers
C-             ANG      = polar angle of the track in degrees
C-             IGEN     = 1 for electron efficiency calculation
C-                      = 0 for 5 GeV/c pion rejection
C-   Outputs :
C-   Controls:
C-
C-   Created   7-SEP-1989   A. Zylberstejn
C-   Updated   8-MAR-1990   J.Fr. Glicenstein  5 GeV/c hadron rejection 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCONST.INC'
      INTEGER IGEN
      REAL ENERGT,ANG,ANG1,ANG2,FINT
      INTEGER LTPE31,LTPE32,GZTPE31,GZTPE32
      INTEGER I,IB,J,JB,LTPP,LTPE,NA(2),NBIN,NST
      PARAMETER (NBIN=50)
      REAL X(2),A(NBIN+2),FE(2,NBIN),FP(2,NBIN)
      REAL CANG,C1,C2,DA,DANG,ORI,S,STP
      DATA ANG1,ANG2/90.,130./
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE A,NA,FP,FE,ORI,NST
      IF(FIRST)THEN
        LTPE31=GZTPE31()
        LTPE32=GZTPE32()
        NST=IC(LTPE31+11)
        ORI=(C(LTPE31+12))
        STP=(C(LTPE31+13))
        DANG=ABS((ANG2-ANG1))
        FIRST=.FALSE.
        A(1)=0.
        A(2)=abs(ANG2-ang1)
        NA(1)=2
        NA(2)=NBIN
        DO 10 J =  1,  NST
          A(2+J)=ORI+(J-1)*STP
   10   CONTINUE
         LTPE=LTPE32+13
         LTPP=LTPE32+NST+13
        DO 20 I =  1,  2
          DO 15 J=1,NST
            FE(I,J)=C(LTPE+J)
            FP(I,J)=C(LTPP+J)
   15     CONTINUE
         LTPE=LTPE31+13
         LTPP=LTPE31+NST+13
   20   CONTINUE
      END IF
      X(1)=ABS(ANG-ANG1)
      X(2)=AMAX1(ENERGT,ORI)
      X(2)=AMIN1(ENERGT,A(2+NST))
      IF (IGEN.EQ.0) THEN
       ETO3EF=FINT(2,X,NA,A,FP)
       IF (ETO3EF.GT.0.) THEN
        ETO3EF = 1./ETO3EF
       ENDIF
      ELSE
       ETO3EF=FINT(2,X,NA,A,FE)
      ENDIF
   40 CONTINUE
  999 RETURN
      END
