      REAL FUNCTION GJN(IND,X,N)
      REAL X,ARG
      INTEGER IND,N
C...JetNet function G
C...Gives sigmoid function N with argument X
C...The derivative GPrime is also calculated and stored in GPJN.

      IF(N.EQ.1) THEN
C...        1 -> g(x)=1/(1+exp(-2x))
        ARG=TANH(X)
        GJN=0.5*(1.0+ARG)
      ELSEIF(N.EQ.2) THEN
C...        2 -> g(x)=tanh(x)
        ARG=TANH(X)
        GJN=ARG
      ENDIF
      RETURN
      END
