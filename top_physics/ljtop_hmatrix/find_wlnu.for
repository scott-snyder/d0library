      SUBROUTINE FIND_WLNU(WMASS,LEP4VEC,NU2VEC,W4VEC,WPZ2,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-          calculate W 4-vector given lepton and neutrino 2-vector
C-   Inputs  : 
C-        LEP4VEC= lepton 4-vector
C-        NU2VEC = neutrino px,py
C-   Outputs : 
C-       WMASS= MASS OF W
C-       W4VEC= W 4-vector
C-       WPZ2 = 2nd solution Pz for W
C-       OK   = false -> no solution
C-
C-   Created  26-NOV-1990   Serban D. Protopopescu
C-   Updated   3-JAN-1991   Rajendran Raja  ADDED W MASS AS AN ARGUMENT 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    LEP4VEC(4),NU2VEC(2),W4VEC(4),WZ2ND
      REAL    B,C,MWSQ,A1,A2,PZ1,PZ2,WPZ1,WPZ2,ROOT
      LOGICAL OK
      REAL    WMASS
C----------------------------------------------------------------------
C
C             solve for Pz of neutrino
C
      MWSQ = WMASS*WMASS
      A1=(MWSQ+2.*LEP4VEC(1)*NU2VEC(1)+2.*LEP4VEC(2)*NU2VEC(2))
     &  /LEP4VEC(4)/2.
      A2=LEP4VEC(3)/LEP4VEC(4)
      B=A2*A1/(A2*A2-1.)
      C=(A1*A1-NU2VEC(1)**2-NU2VEC(2)**2)/(A2*A2-1.)
      ROOT=B**2-C
      OK=.TRUE.
      IF ( ROOT.GT.0 ) THEN
        ROOT=SQRT(ROOT)
        PZ1=-B+ROOT
        PZ2=-B-ROOT
      ELSE
        PZ1=-B
        PZ2=-B
        IF(-ROOT.GT.100.) OK=.FALSE.
      ENDIF
C
C      fill W 4-vector
      W4VEC(1)=LEP4VEC(1)+NU2VEC(1)
      W4VEC(2)=LEP4VEC(2)+NU2VEC(2)
      WPZ1=LEP4VEC(3)+PZ1
      WPZ2=LEP4VEC(3)+PZ2
      IF(ABS(WPZ1).LT.ABS(WPZ2)) THEN
        W4VEC(3)=WPZ1
        W4VEC(4)=LEP4VEC(4)+SQRT(NU2VEC(1)**2+NU2VEC(2)**2+PZ1**2)
      ELSE
        W4VEC(3)=WPZ2
        W4VEC(4)=LEP4VEC(4)+SQRT(NU2VEC(1)**2+NU2VEC(2)**2+PZ2**2)
        WPZ2=WPZ1
      ENDIF
  999 RETURN
      END
