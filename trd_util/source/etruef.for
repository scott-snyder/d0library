      REAL FUNCTION ETRUEF(ENERGT,ANG,IGEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute electron efficiency with TRUNCATED MEAN
C-                          of total energy deposited in the 3 TRD layers
C-
C-   Returned value  : Value of the electron efficiency
C-   Inputs  : ENERGT   = truncated summ of the energy deposit in 3 layers
C-             ANG      = polar angle of the track in degrees
C-             IGEN     = 1 for electron efficiency
C-                      =0 for 5 GeV/c hadron rejection
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-SEP-1989   A. Zylberstejn
C-   Updated   8-MAR-1990   J.Fr. Glicenstein  5 GeV/c Hadron rejection 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCONST.INC'
      REAL ENERGT,ANG,ANG1,ANG2,FINT
      INTEGER LTPTR1,LTPTR2,GZTPTR1,GZTPTR2
      INTEGER I,IB,J,JB,LTPE,LTPP,NA(2),NBIN,NST
      INTEGER IGEN
      PARAMETER (NBIN=50)
      REAL X(2),A(NBIN+2),FE(2,NBIN),FP(2,NBIN)
      REAL CANG,C1,C2,DA,DANG,ORI,S,STP
      DATA ANG1,ANG2/90.,130./
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE A,NA,FE,FP,ORI,NST
      IF(FIRST)THEN
        LTPTR1=GZTPTR1()
        LTPTR2=GZTPTR2()
        FIRST=.FALSE.
        NST=IC(LTPTR1+11)
        ORI=(C(LTPTR1+12))
        STP=(C(LTPTR1+13))
        DANG=ABS((ANG2-ANG1))
        A(1)=0.
        A(2)=abs(ANG2-ang1)
        NA(1)=2
        NA(2)=NBIN
        DO 10 J =  1,  NST
          A(2+J)=ORI+(J-1)*STP
   10   CONTINUE
         LTPE=LTPTR2+13
         LTPP=LTPTR2+NST+13
        DO 20 I =  1,  2
          DO 15 J=1,NST
            FE(I,J)=C(LTPE+J)
            FP(I,J)=C(LTPP+J)
   15     CONTINUE
         LTPE=LTPTR1+13
         LTPP=LTPTR1+NST+13
   20   CONTINUE
      END IF
      X(1)=ABS(ANG-ANG1)
      X(2)=AMAX1(ENERGT,ORI)
      X(2)=AMIN1(ENERGT,A(2+NST))
      IF (IGEN.EQ.0) THEN
       ETRUEF=FINT(2,X,NA,A,FP)
       IF (ETRUEF.GT.0.) ETRUEF = 1./ETRUEF     ! Computes rejection
      ELSE
       ETRUEF=FINT(2,X,NA,A,FE)
      ENDIF
   40 CONTINUE
  999 RETURN
      END
