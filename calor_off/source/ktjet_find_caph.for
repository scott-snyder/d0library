C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_FIND_CAPH.FOR
C *1     3-FEB-1994 14:37:48 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_FIND_CAPH.FOR
      FUNCTION  KTJET_FIND_CAPH(Y_TARGET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the CAPH pointer to the KT algorithm
C-                         which was run with a ycut value as close as
C-                         possible or equal to Y_TARGET
C-
C-   Inputs  : [R]  Y_TARGET :Y cut value of target algorithm
C-   Outputs : [I]  KTJET_FIND_CAPH : Pointer to appropriate CAPH bank
C-   Controls:
C-
C-   Created  24-MAR-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LCAPH, GZCAPH, KTJET_FIND_CAPH, IER, LCAPH_MAX
      REAL TEMPLATE(8), Y_MAX, Y_TARGET
C----------------------------------------------------------------------
      TEMPLATE(1)=0
      CALL SET_CAPH('NN_JET', TEMPLATE, IER )
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('No KT JETS','KTJET_FIND_CAPH',
     &      'No previous Kt jets', 'W')
        KTJET_FIND_CAPH = 0
        RETURN
      ELSE
        LCAPH = GZCAPH()
        Y_MAX = -1.
        LCAPH_MAX = 0
        DO WHILE ( LCAPH .GT. 0 )
          IF ( Q( LCAPH + 6 ) .GT. Y_MAX .AND. Q( LCAPH + 6) .LE.
     &        Y_TARGET  ) THEN
            Y_MAX = Q( LCAPH + 6 )
            LCAPH_MAX  = LCAPH
          ENDIF
          LCAPH      = LQ( LCAPH )
        ENDDO
      ENDIF
      KTJET_FIND_CAPH = LCAPH_MAX
      CALL RESET_CAPH
  999 RETURN
      END
