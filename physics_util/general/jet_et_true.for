      REAL FUNCTION JET_ET_TRUE(ET_MEAS,ALGNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given ET_measured and the cone size,
C-   will return the true ET for MC data
C-
C-   Inputs  : ET_MEAS in Gev
C-             Algorithm names (Cone7,cone5,Neighbor etc')
C-   Outputs : JET_ET_TRUE = true ET in GeV
C-   Controls: NONE
C-
C-   Created   7-MAR-1994   Rajendran Raja   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NC
      PARAMETER( NC = 3 )  
      INTEGER NALG
      PARAMETER( NALG = 3 )
C
C IALG=1 CONE.7
C IALG=2 CONE.5
C IALG=3 NN
C
      REAL MC_COEFF(NC,NALG)
      CHARACTER*(*) ALGNAME
      REAL    ET_MEAS
C
      DATA MC_COEFF/ -3.2081      ,   1.3063      , -0.34415E-03  ,
     &               -1.2911      ,   1.3788      , -0.83428E-03  ,
     &               1.6368      ,   1.5164      , -0.21183E-02  /
C

      SAVE MC_COEFF
      INTEGER IALG
      REAL    PAR(3)
      REAL    X
C
      LOGICAL MONTE_CARLO_DATA
C----------------------------------------------------------------------
      IF ( .NOT.MONTE_CARLO_DATA() ) THEN
        JET_ET_TRUE = ET_MEAS
        RETURN
      ENDIF
C
      IF ( ALGNAME.EQ.'CONE7' ) THEN
        IALG = 1
      ELSEIF ( ALGNAME.EQ.'CONE5' ) THEN
        IALG = 2
      ELSEIF ( ALGNAME.EQ.'NEIGHBOR' ) THEN
        IALG = 3
      ELSE
        CALL ERRMSG('TOP_PHYSICS','JET_ET_TRUE',
     &    'UNKOWN ALGORITHM NAME ','W')
        JET_ET_TRUE = ET_MEAS
        RETURN
      ENDIF
C
      X = ET_MEAS
      CALL UCOPY(MC_COEFF(1,IALG),PAR,3)
      JET_ET_TRUE = PAR(1)+PAR(2)*X+PAR(3)*X*X
C
C      WRITE(6,1)IALG,ET_MEAS,JET-ET_TRUE
C    1 FORMAT(' IALG,ET_MEAS,JET_ET_TRUE ',I5,2F15.5)
C
C
  999 RETURN
      END

