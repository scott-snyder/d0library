      REAL FUNCTION LIKEEF(LIKENT,ANG,IGEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute electron efficiency with LIKELIHOOD
C-                          of total energy deposited in the 3 TRD layers
C-
C-   Returned value  : Value of the electron efficiency
C-   Inputs  : LIKENT   = likelihood  of the energy deposit in 3 layers
C-             ANG      = polar angle of the track in degrees
C-             IGEN     = 1 for electron efficiency calculation
C-                      = 0 for 5 GeV/c hadron rejection
C-   Outputs :
C-   Controls:
C-
C-   Created  26-SEP-1989   A. Zylberstejn
C-   Updated   8-MAR-1990   J.Fr. Glicenstein  5 GeV/c hadron rejection 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCONST.INC'
C      INCLUDE 'D0$INC:LTRD.INC'
      REAL LIKENT,ANG,ANG1,ANG2,FINT
      INTEGER LTPLE1,LTPLE2,GZTPLE1,GZTPLE2
      INTEGER I,IB,J,JB,LTPE,LTPP,NA(2),NBIN,NST
      INTEGER LOUT,TRUNIT,IGEN
      PARAMETER (NBIN=50)
      REAL X(2),A(NBIN+2),FE(2,NBIN),FP(2,NBIN)
      REAL CANG,C1,C2,DA,DANG,ORI,S,STP
      DATA ANG1,ANG2/90.,130./
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE A,NA,FE,FP,ORI,NST
      IF(FIRST)THEN
        LTPLE1=GZTPLE1()
        LTPLE2=GZTPLE2()
C        IF(LTPLE1.LE.0 .OR. LTPLE2.LE.0) CALL TDEFLI ! Define links if necessary
        NST=IC(LTPLE1+11)
        ORI=(C(LTPLE1+12))
        STP=(C(LTPLE1+13))
        LOUT=TRUNIT()
        DANG=ABS((ANG2-ANG1))
        FIRST=.FALSE.
        A(1)=0.
        A(2)=ABS(ANG2-ANG1)
        NA(1)=2
        NA(2)=NBIN
        DO 10 J =  1,  NST
          A(2+J)=ORI+(J-1)*STP
   10   CONTINUE
         LTPE=LTPLE2+13
         LTPP=LTPLE2+NST+13
        DO 20 I =  1,  2
          DO 15 J=1,NST
            FE(I,J)=C(LTPE+J)
            FP(I,J)=C(LTPP+J)
   15     CONTINUE
         LTPE=LTPLE1+13
         LTPP=LTPLE1+NST+13
   20   CONTINUE
      END IF
      X(1)=ABS(ANG-ANG1)
      X(2)=AMAX1(LIKENT,ORI)
      X(2)=AMIN1(LIKENT,A(2+NST))
      IF (IGEN.EQ.0) THEN               ! Computes 5 GeV/c hadron
                                        ! rejection
       LIKEEF=FINT(2,X,NA,A,FP)
       IF (LIKEEF.GT.0.) THEN
        LIKEEF = 1./LIKEEF
       ELSE
        LIKEEF = 170000.                ! Likeef < 1 in one bin --> rejection
                                        ! >= Number of entries in PRLIKETOT10
                                        ! /2.3 (90% C.L.)
       ENDIF
      ELSE
       LIKEEF=FINT(2,X,NA,A,FE)
      ENDIF
   40 CONTINUE
  999 RETURN
      END
