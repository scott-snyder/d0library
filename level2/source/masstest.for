      LOGICAL FUNCTION MASSTEST(ET1,ET2,ETA1,ETA2,PHI1,PHI2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates the invariant mass of two objects
C-                         and then makes cuts on ETA and ETA BOOST and on
C-                         the invariant mass threshold.
C-
C-   Returned value  : MASSTEST returns TRUE if 2 objects pass all cuts
C-   Inputs  : ET1,ET2,ETA1,ETA2,PHI1,PHI2 values for each object
C-   Outputs : none
C-   Controls:
C-
C-   Created  20-AUG-1993   Kathy Fatyga
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:L2_MASSCUT_CUTS.INC'
      INCLUDE 'D0$INC:RMAS_VALUES.INC'
      REAL ETA1,ETA2,INVMASS_SQUARED
      REAL ETABOOST,INVMASS,ET1,ET2,PHI1,PHI2
C
      LOGICAL ETA_RANGE_PASS,ETA_BOOST_PASS,MASSPASS
C
C----------------------------------------------------------------------
C
C    initialize the logicals
      ETA_RANGE_PASS=.FALSE.
      ETA_BOOST_PASS=.FALSE.
      MASSPASS=.FALSE.
      MASSTEST=.FALSE.
C
C cut on eta range of each object and etaboost
c
C        Cut on ETA range of each object
c
      IF((ETA1.GT.ETA1_MIN).AND.(ETA1.LT.ETA1_MAX).AND.
     &  (ETA2.GT.ETA2_MIN).AND.(ETA2.LT.ETA2_MAX))ETA_RANGE_PASS=.TRUE.
c test both combinations of 2 objects to avoid bias
      IF((ETA2.GT.ETA1_MIN).AND.(ETA2.LT.ETA1_MAX).AND.
     &  (ETA1.GT.ETA2_MIN).AND.(ETA1.LT.ETA2_MAX))ETA_RANGE_PASS=.TRUE.
c
c          Cut on ABSOLUTE eta boost range
c
      ETABOOST=ABS(ETA1+ETA2)/2
      IF((ETABOOST.GT.ETABOOST_MIN).AND.(ETABOOST.LT.ETABOOST_MAX))
     &    ETA_BOOST_PASS=.TRUE.
c
C cut on invariant mass threshold or range
C
      INVMASS_SQUARED=2*ET1*ET2*(COSH(ETA1-ETA2)-COS(PHI1-PHI2))
      IF(INVMASS_SQUARED.LT.0.0) THEN
         INVMASS_SQUARED=0.0
         CALL ERRMSG('NEG_INVMASS_SQUARED','MASSTEST',
     &     'Invariant mass squared is negative!','W')
      ENDIF
      INVMASS=SQRT(INVMASS_SQUARED)
C
      IF(MASS_MAX.LT.0.0)THEN !if MASS_MAX is neg. only use threshold cut
        IF(INVMASS.GT.MASS_MIN)MASSPASS=.TRUE.
C
      ELSE
        IF(
     &  (INVMASS.GT.MASS_MIN).AND.(INVMASS.LT.MASS_MAX))MASSPASS=.TRUE.
      ENDIF
C
      IF(ETA_RANGE_PASS.AND.ETA_BOOST_PASS.AND.MASSPASS)MASSTEST=.TRUE.
C
      IF(LEADING)THEN !only leading pair looked at
         PAIR_MASS=INVMASS
         PAIR_BOOST=ETABOOST
         PAIR_VAL(1,1)=ET1
         PAIR_VAL(1,2)=ETA1
         PAIR_VAL(1,3)=PHI1
         PAIR_VAL(2,1)=ET2
         PAIR_VAL(2,2)=ETA2
         PAIR_VAL(2,3)=PHI2
         IF(MASSTEST)THEN
           PAIR_STATUS=-1
         ELSE
           IF(.NOT.ETA_RANGE_PASS)PAIR_STATUS=PAIR_STATUS+1
           IF(.NOT.ETA_BOOST_PASS)PAIR_STATUS=PAIR_STATUS+100
           IF(.NOT.MASSPASS)PAIR_STATUS=PAIR_STATUS+10000        
         ENDIF
      ELSE !all pairs looked through
         IF(MASSTEST.AND.PAIR_PASS)THEN
           PAIR_PASS=.FALSE.
           PAIR_STATUS=-1
           PAIR_MASS=INVMASS
           PAIR_BOOST=ETABOOST
           PAIR_VAL(1,1)=ET1
           PAIR_VAL(1,2)=ETA1
           PAIR_VAL(1,3)=PHI1
           PAIR_VAL(2,1)=ET2
           PAIR_VAL(2,2)=ETA2
           PAIR_VAL(2,3)=PHI2
         ELSEIF(PAIR_PASS) THEN
C     *** only 99 pairs can be stored in the status word ***
           N_STAT=N_STAT+1
           IF(N_STAT.LE.99)THEN
             IF(.NOT.ETA_RANGE_PASS)PAIR_STATUS=PAIR_STATUS+1
             IF(.NOT.ETA_BOOST_PASS)PAIR_STATUS=PAIR_STATUS+100
             IF(.NOT.MASSPASS)PAIR_STATUS=PAIR_STATUS+10000
           ENDIF
           IF(INVMASS.GT.PAIR_MASS)THEN
              PAIR_MASS=INVMASS
              PAIR_BOOST=ETABOOST
              PAIR_VAL(1,1)=ET1
              PAIR_VAL(1,2)=ETA1
              PAIR_VAL(1,3)=PHI1
              PAIR_VAL(2,1)=ET2
              PAIR_VAL(2,2)=ETA2
              PAIR_VAL(2,3)=PHI2              
           ENDIF
         ENDIF
      ENDIF
  999 RETURN
      END
