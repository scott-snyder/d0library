      REAL FUNCTION ETR3EF(ENERGT,ANG,IGEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute electron efficiency with TRUNCATED MEAN
C-                          of total energy deposited in the 3 TRD layers
C-                          when the 3 TEC are hit
C-
C-   Returned value  : Value of the electron efficiency
C-   Inputs  : ENERGT   = truncated summ of the energy deposit in 3 layers
C-             ANG      = polar angle of the track in degrees
C-             IGEN     = 1 for electron efficiency
C-                      = 0 for 5 GeV/c hadron rejection
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-SEP-1989   A. Zylberstejn
C-   Updated   8-MAR-1990   J.Fr. Glicenstein: 5 GeV/c pion rejection 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCONST.INC'
      REAL ENERGT,ANG,ANG1,ANG2,FINT
      INTEGER LTPT31,LTPT32,GZTPT31,GZTPT32
      INTEGER I,IB,J,JB,LTPE,LTPP,NA(2),NBIN,NST
      PARAMETER (NBIN=50)
      INTEGER IGEN
      REAL X(2),A(NBIN+2),FE(2,NBIN),FP(2,NBIN)
      REAL CANG,C1,C2,DA,DANG,ORI,S,STP
      DATA ANG1,ANG2/90.,130./
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE A,NA,FE,FP,ORI,NST
      IF(FIRST)THEN
        LTPT31=GZTPT31()
        LTPT32=GZTPT32()
        FIRST=.FALSE.
        NST=IC(LTPT31+11)
        ORI=(C(LTPT31+12))
        STP=(C(LTPT31+13))
        DANG=ABS((ANG2-ANG1))
        A(1)=0.
        A(2)=abs(ANG2-ang1)
        NA(1)=2
        NA(2)=NBIN
        DO 10 J =  1,  NST
          A(2+J)=ORI+(J-1)*STP
   10   CONTINUE
         LTPE=LTPT32+13
         LTPP=LTPT32+13+NST
        DO 20 I =  1,  2
          DO 15 J=1,NST
            FE(I,J)=C(LTPE+J)
            FP(I,J)=C(LTPP+J)
   15     CONTINUE
         LTPE=LTPT31+13
         LTPP=LTPT31+13+NST
   20   CONTINUE
      END IF
      X(1)=ABS(ANG-ANG1)
      X(2)=AMAX1(ENERGT,ORI)
      X(2)=AMIN1(ENERGT,A(2+NST))
C      PRINT*,' X',X(1),X(2)
      IF (IGEN.EQ.0) THEN               ! 5 GeV/c pion rejection
       ETR3EF=FINT(2,X,NA,A,FP)
       IF (ETR3EF.GT.0.) THEN 
        ETR3EF=1./ETR3EF
       ENDIF
      ELSE
       ETR3EF=FINT(2,X,NA,A,FE)
      ENDIF
   40 CONTINUE
  999 RETURN
      END
