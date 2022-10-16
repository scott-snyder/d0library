      LOGICAL FUNCTION TOP_JETS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter TOP--> ALLJETS events from according to 
C-                         the following criteria 
C-          ETJET > ETMIN
C-          NJET = NJETS_MIN
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-DEC-1992   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    ETMIN,ET,TEMPLATE(5,4)
      INTEGER NJETS,NJETS_MIN,IER,ICHOICE
      INTEGER LJETS,GZJETS
      LOGICAL FIRST,PASSED
      DATA    FIRST/.TRUE./
      DATA TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7 
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2

C----------------------------------------------------------------------
C
C ****  initialization 
C
      PASSED   = .FALSE.
      TOP_JETS = .FALSE.
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('TOP_JETS_RCP',IER)       
        IF(IER.EQ.0) THEN
          CALL EZPICK('TOP_JETS_RCP')  
          IF (IER.EQ.0) CALL EZGET_i('JETS_ALGORITHM',ICHOICE,IER)
          IF (IER.EQ.0) CALL EZGET('JETS_MIN_ET',ETMIN,IER)
          IF (IER.EQ.0) CALL EZGET_i('NJETS_MIN',NJETS_MIN,IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('TOP_JETS','TOP_JETS_RCP',
     &        'ERROR GETTING TOP_JETS RCP VALUES','W')
          ENDIF
          CALL EZRSET
        ENDIF
      ENDIF
C
      NJETS = 0
C
      IF(ICHOICE.EQ.1) CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
      IF(ICHOICE.EQ.2) CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
      IF(ICHOICE.EQ.3) CALL SET_CAPH('CONE_JET',TEMPLATE(1,3),IER)
      IF(ICHOICE.EQ.4) CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
C
      LJETS=GZJETS()
      IF(LJETS.NE.0) THEN
        CALL ZSORT(IXCOM,LJETS,6)
        LJETS=GZJETS()
        CALL ZTOPSY(IXCOM,LJETS)
        LJETS=GZJETS()
        DO WHILE (LJETS.GT.0)
          ET =SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
          IF (ET.GE.ETMIN) NJETS=NJETS+1
          LJETS=LQ(LJETS)          ! pointer to next jet
        ENDDO
      ENDIF
      IF (NJETS.GE.NJETS_MIN) PASSED = .TRUE.
      CALL RESET_CAPH
C
      TOP_JETS = PASSED
C
  999 RETURN
      END
