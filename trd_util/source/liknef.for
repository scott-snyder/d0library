      REAL FUNCTION LIKNEF(LIKENT,CASE,ANG,IGEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute electron efficiency with LIKELIHOOD
C-                          of total energy deposited in the 3 TRD layers
C-                          and total number of clusters.
C-
C-   Returned value  : Value of the electron efficiency
C-   Inputs  : LIKENT   = likelihood  of the energy deposit in 3 layers
C-             CASE     = 1,2,3 as in CLIKEN
C-             ANG      = polar angle of the track in degrees
C-             IGEN     = 1 electron efficiency
C-                      = 0 5 Gev/c pion rejection
C-   Outputs :
C-   Controls:
C-
C-   Created   1-MAR-1990   J.Fr. Glicenstein, copied on LIKEEF   
C-   Updated   8-MAR-1990   J.Fr. Glicenstein  5 GeV/c pion rejection 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCONST.INC'
      REAL LIKENT,ANG,ANG1,ANG2,FINT
      INTEGER LTPCX1,LTPCX2,GZTPCA1,GZTPCA2
      INTEGER GZTPCB1,GZTPCB2,GZTPCC1,GZTPCC2
      INTEGER I,IB,J,JB,LTPE,LTPP,NA(2),NBIN,NST
      INTEGER LOUT,TRUNIT,CASE,IGEN
      PARAMETER (NBIN=50)
      REAL X(2),A(NBIN+2),FE(2,NBIN),FP(2,NBIN)
      REAL CANG,C1,C2,DA,DANG,ORI,S,STP
      DATA ANG1,ANG2/90.,130./
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE A,NA,FE,FP,ORI,NST
      IF(FIRST)THEN
        FIRST=.FALSE.
      IF (CASE.EQ.1) THEN      
       LTPCX1=GZTPCA1()
       LTPCX2=GZTPCA2()
      ELSE IF (CASE.EQ.2) THEN      
       LTPCX1=GZTPCB1()
       LTPCX2=GZTPCB2()
      ELSE IF (CASE.EQ.3) THEN      
       LTPCX1=GZTPCC1()
       LTPCX2=GZTPCC2()
      ENDIF
C        IF(LTPCX1.LE.0 .OR. LTPCX2.LE.0) CALL TDEFLI ! Define links if necessary
        NST=IC(LTPCX1+11)
        ORI=(C(LTPCX1+12))
        STP=(C(LTPCX1+13))
        LOUT=TRUNIT()
        DANG=ABS((ANG2-ANG1))
        A(1)=0.
        A(2)=ABS(ANG2-ANG1)
        NA(1)=2
        NA(2)=NBIN
        DO 10 J =  1,  NST
          A(2+J)=ORI+(J-1)*STP
   10   CONTINUE
         LTPE = LTPCX2+13
         LTPP = LTPCX2+13+NST
        DO 20 I =  1,  2
          DO 15 J=1,NST
            FE(I,J)=C(LTPE+J)
            FP(I,J)=C(LTPP+J)
   15     CONTINUE
         LTPE = LTPCX1+13
         LTPP = LTPCX1+13+NST
   20   CONTINUE
      END IF
      X(1)=ABS(ANG-ANG1)
      X(2)=AMAX1(LIKENT,ORI)
      X(2)=AMIN1(LIKENT,A(2+NST))
      IF (IGEN.EQ.0) THEN               ! 5 GeV/c hadron rejection
       LIKNEF=FINT(2,X,NA,A,FP)
       IF (LIKNEF.GT.0) THEN
        LIKNEF = 1./LIKNEF
       ELSE
        LIKNEF = 170000.               
C see comment in LIKEEF
       ENDIF
      ELSE
       LIKNEF=FINT(2,X,NA,A,FE)
      ENDIF
   40 CONTINUE
  999 RETURN
      END
