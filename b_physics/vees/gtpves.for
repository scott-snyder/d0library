      SUBROUTINE GTPVES(IVEE,VEE) 
C----------------------------------------------------------------------
C
C    Get contents of PVES banks
C 
C    Inputs  : 
C       IVEE = particle number
C    Outputs : 
C       contents of PVES  in VEE array
C 
C    21-JUL-1990   Daria Zieminska
C
C-   Updated 1-sep-1993  Oscar Ramírez  Also save integers values in vee array 
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NDATA
      PARAMETER (NDATA=61)
      REAL VEE(NDATA)
      INTEGER IVEE,LPVES,GZPVES
      CALL VZERO(VEE,NDATA) 
      LPVES=GZPVES(IVEE)
      IF(LPVES.GT.0) THEN
        CALL UCOPY(Q(LPVES+1),VEE,NDATA) 
        VEE(1)=FLOAT(IQ(LPVES+1))
        VEE(3)=FLOAT(IQ(LPVES+3))
        VEE(4)=FLOAT(IQ(LPVES+4))
        VEE(5)=FLOAT(IQ(LPVES+5))
        VEE(11)=FLOAT(IQ(LPVES+11))
        VEE(12)=FLOAT(IQ(LPVES+12))
        VEE(28)=FLOAT(IQ(LPVES+28))
        VEE(29)=FLOAT(IQ(LPVES+29))
        VEE(45)=FLOAT(IQ(LPVES+45))
        VEE(46)=FLOAT(IQ(LPVES+46))
      ENDIF
 1000 RETURN
      END
