      FUNCTION EMAG_JETS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return TRUE if this event passes the following:
C-                
C-          JT(2,Et>15.,EM>0.85,R=0.3) (No rapidity cut off-line)      
C-          for the first level trigger bit EM_2_MED
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-MAR-1994   Iain A. Bertram
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'          ! Event zebra store
      LOGICAL EMAG_JETS 
      LOGICAL L1NAME_PASSED                     ! See if event has passed
                                                ! trigger bit required.
      CHARACTER*32 TRIGGER_NAME_1,TRIGGER_NAME_2
      INTEGER N_JETS                            ! Number of Jets
      REAL MAX_JET_ET(2), MAX_FOR_JET_ET(2)     ! Et of 2 maximum jets
C
      INTEGER IER, LJETS, GZJETS, I, J
      REAL TEMPLATE(20), ET, ETA, EMFR
C----------------------------------------------------------------------
C
C   Assume that this event will be dropped.
C
      EMAG_JETS = .FALSE.
C
C   Set CAPH to conesize = 0.3
C
      TEMPLATE(1) = 1             ! Look at one word only
      TEMPLATE(2) = 6             ! Look at the 6th word (cone size)
      TEMPLATE(3) = .3            ! Require that word be .3 (r=.3)
      CALL SET_CAPH( 'CONE_JET', TEMPLATE, IER )
      IF ( IER .NE. 0 ) GOTO 999  ! No jets found?!      
C
C   Check to see if EM_2_MED trigger bit is set. If not skip event.
C
      TRIGGER_NAME_1 = 'EM_2_MED'
      TRIGGER_NAME_2 = 'EM_2_HIGH'
      IF(.NOT.(L1NAME_PASSED(TRIGGER_NAME_1) .OR. 
     &  L1NAME_PASSED(TRIGGER_NAME_2)) ) GOTO 999
C
C   Find 2 maximum Et jets with EM fraction > 0.85.
C
      CALL VZERO( MAX_JET_ET, 2 )
      N_JETS = 0
      LJETS = GZJETS()
      DO WHILE ( LJETS .GT. 0 )
        ET    = Q( LJETS + 6)
        ETA   = Q( LJETS + 9)
        EMFR  = Q( LJETS + 14)
C
        IF ( EMFR .GT. 0.85) THEN
          N_JETS = MIN( 2, N_JETS + 1 )
          DO I = 1, N_JETS
            IF ( ET .GT. MAX_JET_ET(I) ) THEN
              DO J = N_JETS, I+1, -1
                MAX_JET_ET(J) = MAX_JET_ET(J-1)
              ENDDO
              MAX_JET_ET( I ) = ET
            ENDIF
          ENDDO
        ENDIF
        LJETS = LQ( LJETS )
      ENDDO      
C
C     Require  at least two jets to have EMFR > 0.85
C
      IF ( N_JETS .LT. 2) GOTO 999
C
C     Require that Et of two leading jets is greater than 15 GeV.
C
      IF ( (MAX_JET_ET(1) .LT. 15.) .OR. (MAX_JET_ET(2) .LT. 15.) ) GOTO
     &  999
C
      EMAG_JETS = .TRUE.
C
C----------------------------------------------------------------------
  999 CALL RESET_CAPH
      RETURN
C
      END
