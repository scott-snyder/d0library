      REAL FUNCTION CLIKET(ENERGT,ANG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute likelihood with TOTAL ENERGY deposited
C-                         in the 3 TRD layers
C-
C-   Returned value  : Value of the likelihood
C-   Inputs  : ENERGT(I)= energy deposit in layer I
C-             ANG      = polar angle of the track in degrees
C-   Outputs :
C-   Controls:
C-
C-   Created   7-SEP-1989   A. Zylberstejn
C-   Updated  23-MAR-1992   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C      INCLUDE 'D0$INC:LTRD.INC'
      REAL ENERGT(3),ANG,ANG1,ANG2
      INTEGER I,ICH,IB,NST
      INTEGER LTMXE1,LTMXE2,GZTMXE1,GZTMXE2
      REAL CANG,C1,C2,DA,DANG,LOUT,ORI,S,STP
      LOGICAL FIRST
      DATA ANG1,ANG2/90.,130./
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      LTMXE1=GZTMXE1()
      LTMXE2=GZTMXE2()
      IF(FIRST)THEN
        NST=IC(LTMXE1+11)
        ORI=(C(LTMXE1+12))
        STP=(C(LTMXE1+13))
        DANG=(ANG2-ANG1)
        LOUT=LUDEBG
        FIRST=.FALSE.
      END IF
      CLIKET=0.
      DA=ABS(ANG-ANG1)/DANG
      DO 40 ICH=1,3
        IB=(ENERGT(ICH)-ORI)/STP +1
        IB=MAX0(1,IB)
        IB=MIN0(IB,NST)+(ICH-1)*NST
        C1=C(LTMXE2+13+IB)
        C2=C(LTMXE1+13+IB)
        CANG=C1+(C2-C1)*DA
        IF(CANG.LE.0.)THEN
C          CALL ERRMSG(' Problem_TRD: cang=0 ','CLIKET',' ','W')
        ELSE
          CLIKET=CLIKET+ALOG(CANG)
        END IF
   40 CONTINUE
  999 RETURN
      END
