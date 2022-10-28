      REAL FUNCTION KT2_FUN(X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gives double exponential function
C-   to describe kt2 distribution. Fitted using 140 Gev ttbar production
C-
C-   Inputs  : X= KT**2
C-   Outputs : PROBABILITY OF KT**2
C-   Controls: 
C-
C-   Created  14-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    X
      REAL    EXP1(2),EXP2(2),ANORM,BIN
      INTEGER EVENT
      INTEGER IER
      REAL    A,B,C,D,RENORM
C
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET_rarr('KT2_EXPONENTIAL1',EXP1,IER)
        CALL EZGET_rarr('KT2_EXPONENTIAL2',EXP2,IER)
        CALL EZGET('KT2_BINSIZE',BIN,IER)
        CALL EZGET_i('KT2_EVENTS',EVENT,IER)
        CALL EZRSET
      ENDIF
C
      ANORM = BIN*EVENT
      A= EXP(EXP1(1))/ANORM
      B= -EXP1(2)
      C= EXP(EXP2(1))/ANORM
      D= -EXP2(2)
C
      KT2_FUN = A*EXP(-B*X) + C*EXP(-D*X) 
C
C ****  this is so as to make the average Kt2 weight be equal to 1.
C
      RENORM = A*A/(2.*B) + (C*C)/(2.*D) + (2.*A*C)/(B+D)
      KT2_FUN = KT2_FUN/RENORM
C
  999 RETURN
      END
