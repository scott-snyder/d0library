      SUBROUTINE TOP_LEPTONS_JET_ET_DEF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Redine Jet Et value in Jets Banks to be
C-                         Sqrt(Px**2+Py**2)
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-NOV-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER GZJETS,LJETS,IER
C
      REAL CONE_TEMPLATE_7(3),CONE_TEMPLATE_5(3)
      REAL CONE_TEMPLATE_3(3),NN_TEMPLATE(5)
C
      DATA CONE_TEMPLATE_7/ 1., 6., 0.7/
      DATA CONE_TEMPLATE_5/ 1., 6., 0.5/
      DATA CONE_TEMPLATE_3/ 1., 6., 0.3/
      DATA NN_TEMPLATE    / 2., 7., 2., 8., 2./
C
C *** Loop over JETS Banks and re-set Et values
C *** Start with 0.7 cone jets
C
      CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_7,IER)
      LJETS=GZJETS()
      IF(LJETS.NE.0) THEN
        DO WHILE (LJETS.GT.0)
          Q(LJETS+6)=SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
          LJETS=LQ(LJETS)
        ENDDO
      ENDIF
      CALL RESET_CAPH
C
C *** Next the 0.5 cone jets
C
      CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_5,IER)
            LJETS=GZJETS()
      IF(LJETS.NE.0) THEN
        DO WHILE (LJETS.GT.0)
          Q(LJETS+6)=SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
          LJETS=LQ(LJETS)
        ENDDO
      ENDIF
      CALL RESET_CAPH
C
C *** Next the 0.3 cone jets
C
      CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_3,IER)
            LJETS=GZJETS()
      IF(LJETS.NE.0) THEN
        DO WHILE (LJETS.GT.0)
          Q(LJETS+6)=SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
          LJETS=LQ(LJETS)
        ENDDO
      ENDIF
      CALL RESET_CAPH
C
C *** Lastly the NN jets
C
      CALL SET_CAPH('CONE_JET',NN_TEMPLATE,IER)
            LJETS=GZJETS()
      IF(LJETS.NE.0) THEN
        DO WHILE (LJETS.GT.0)
          Q(LJETS+6)=SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
          LJETS=LQ(LJETS)
        ENDDO
      ENDIF
      CALL RESET_CAPH
C----------------------------------------------------------------------
  999 RETURN
      END
