      REAL FUNCTION CLIKEN(ENERGT,NCLUST,CASE,ANG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute likelihood with TOTAL ENERGY deposited
C-                         AND nb. of clusters in the 3 TRD layers
C-
C-   Returned value: Likelihood total energy/nb of clusters
C-   Inputs  : ENERGT(I)= energy deposit in layer I
C-             NCLUST(I)= number of clusters in layer I
C-             CASE     =1,2 OR 3 depending on the clusters energy
C-                                treshold
C-             ANG      = polar angle of the track in degrees
C-   Controls:
C-
C-   Created    9-SEP-1989   A.Zylberstejn
C-   Updated    8-MAR-1990   J.Fr. Glicenstein : Correction and completion
C-   Corrected 31-DEC-1991   A. Zylberstejn
C-   Updated  23-MAR-1992   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      REAL ENERGT(3),ANG,ANG1,ANG2
      INTEGER CASE,I,ICH,IB,LOUT,NCLUST(3),NST,TRUNIT
      INTEGER LTMXX1,LTMXX2,GZTMAA1,GZTMAA2
      INTEGER GZTMAB1,GZTMAB2,GZTMAC1,GZTMAC2
      INTEGER NSTERG,NSTCLU,ORICLU,IBCLU,IBERG
      REAL ORIERG
      REAL CANG,C1,C2,DA,DANG,S,STP
      LOGICAL FIRST
      DATA ANG1,ANG2/50.,90./
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (CASE.EQ.1) THEN
        LTMXX1=GZTMAA1()
        LTMXX2=GZTMAA2()
      ELSE IF (CASE.EQ.2) THEN
        LTMXX1=GZTMAB1()
        LTMXX2=GZTMAB2()
      ELSE IF (CASE.EQ.3) THEN
        LTMXX1=GZTMAC1()
        LTMXX2=GZTMAC2()
      ELSE
        CALL ERRMSG(' Unavailable option ','CLIKEN',' ','W')
      ENDIF
      IF(FIRST)THEN
        NST    = IC(LTMXX1+11)
        NSTERG = MOD(NST,1000)
        NSTCLU = (NST-NSTERG)/100
        I      = ABS(C(LTMXX1+12))
        ORICLU = -I/100
        ORIERG = -MOD(I,100)
        STP    = (C(LTMXX1+13))
        DANG   =ANG2-ANG1
        LOUT   =TRUNIT()
        FIRST  =.FALSE.
      END IF
      DA=ABS(ANG-ANG1)/DANG
      CLIKEN = 0.
      DO 40 ICH=1,3
        IBERG=1
        IF(ENERGT(ICH).GT.0.)IBERG=(ENERGT(ICH)-ORIERG)/STP +1
        IBERG=MIN0(IBERG,NSTERG)
        IBCLU=MIN0(NCLUST(ICH)-ORICLU,NSTCLU)
        IB = IBERG+(IBCLU-1)*NSTERG+(ICH-1)*1000
        C1=C(LTMXX1+13+IB)
        C2=C(LTMXX2+13+IB)
        CANG=C1+(C2-C1)*DA
        IF(CANG.LE.0.)THEN
C          CALL ERRMSG(' Problem_TRD: cang=0 ','CLIKEN',' ','W')
        ELSE
          CLIKEN =CLIKEN+ALOG(CANG)
        END IF
   40 CONTINUE
  999 RETURN
      END
