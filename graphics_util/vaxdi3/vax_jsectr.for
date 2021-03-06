      SUBROUTINE JSECTR(X0,Y0,Z0,R,NSEG,A0,A1)
      INCLUDE 'D0$INC:DI3INC.INC'
C  SET THIS UP AS A POLYGON IN SCRATCH SO THAT
C  IT FILLS ON CLOSE.
      IF(NSEG.GE.1)THEN
        NS=NSEG
      ELSE
C  DEFAULT NUMBER OF SIDES
        NS=100
      ENDIF
      DPHI=(3.14159/180.)*(A1-A0)/NS
      PHI=(3.14159/180.)*A0
      CSD=COS(DPHI)
      SND=SIN(DPHI)
      CS=COS(PHI)
      SN=SIN(PHI)
      XPOLY(1)=X0+R*CS
      YPOLY(1)=Y0+R*SN
      ZPOLY(1)=Z0
      DO 111 I=2,NS
        CSPR=CS*CSD-SN*SND
        SNPR=SN*CSD+CS*SND
        XPOLY(I)=X0+R*CSPR
        YPOLY(I)=Y0+R*SNPR
        ZPOLY(I)=Z0
        CS=CSPR
        SN=SNPR
  111 CONTINUE
      NNS=NS+1
      XPOLY(NNS)=X0
      YPOLY(NNS)=Y0
      ZPOLY(NNS)=Z0
      CALL J3PLGN(XPOLY,YPOLY,ZPOLY,NNS)
      END
