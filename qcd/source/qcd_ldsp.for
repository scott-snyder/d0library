      SUBROUTINE QCD_LDSP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simulate the local DSP's in Cal L1.5
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-JUN-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:PSL1.INC'
      INCLUDE 'D0$INC:L2J_UTIL.INC'
      INTEGER IETA, IPHI, IETA_LOOK(40), I, IETAC, IPHIC, IETALOW
      INTEGER IETAHI, IETA2, IPHI2
      REAL ET, THRESH(0:3)
      INTEGER  COUNT
      LOGICAL FIRST
      DATA  FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
        THRESH_1X1 = 3.
        THRESH_3X3 = 5.
        THRESH_5X5 = 8.
        THRESH(0) = -1.
        THRESH(1) = THRESH_1X1
        THRESH(2) = THRESH_3X3
        THRESH(3) = THRESH_5X5
        CALL L2J_UTIL_INIT              ! Init L2J lookups
        DO IETA = 1, 40
          IF ( IETA .GT. 20 ) THEN
            IETA_LOOK( IETA ) = IETA - 20
          ELSE
            IETA_LOOK( IETA ) = IETA - 21
          ENDIF
        ENDDO
      ENDIF
C
      CALL VZERO( NENTRY, 5 )
C
      DO IETA = 1, 40
        DO IPHI = 1, 32
          IETAC = IETA_LOOK( IETA )
          IPHIC = IPHI
C
          ET = 0.
          COUNT = 0
          DO WHILE ( ET .GT. THRESH(COUNT) .AND. COUNT .LT. 3 )
            ET = 0.
            NENTRY( COUNT ) = NENTRY( COUNT ) + 1
            COUNT = COUNT + 1
            IETALOW = IETA - COUNT + 1
            IETALOW = MAX( IETALOW, 1 )
            
            IETAHI  = IETA + COUNT - 1
            IETAHI  = MIN( IETAHI, 40 )

             DO IETA2 = IETALOW, IETAHI
              DO IPHI2 = IPHI - COUNT + 1, IPHI + COUNT - 1
                ET = ET + PSL1TOW( IETA2, L1J_PHI(IPHI2) )
              ENDDO
            ENDDO
          ENDDO
C
C: A final candidate made it till COUNT = 4 
C
          IF ( COUNT .EQ. 3 .AND. ET .GT. THRESH(COUNT) ) THEN
            NENTRY(3) = MIN( 88, NENTRY(3) + 1 )
            PSLDSP_ET( NENTRY(3) ) = ET
            PSLDSP_ETA( NENTRY(3) ) = IETA
            PSLDSP_PHI( NENTRY(3) ) = IPHIC
          ENDIF
        ENDDO
      ENDDO
C
C: Histogram number of final candidates
C
      CALL HFILL( 904, FLOAT( NENTRY(1) ), 0., 1. )
      CALL HFILL( 905, FLOAT( NENTRY(2) ), 0., 1. )
      CALL HFILL( 906, FLOAT( NENTRY(3) ), 0., 1. )
  999 RETURN
      END
