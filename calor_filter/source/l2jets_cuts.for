      SUBROUTINE L2JETS_CUTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make cuts on this jet according to the parameter
C-                         set we were given. Set flag on jet if it may
C-                         contribute.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: change NOWRESULT to .TRUE. if jets make cut.
C-
C-   Created   3-JUN-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:L2LINK.INC'               ! L2 link common
      INCLUDE 'D0$INC:L2JETS_PAR.INC'
      INCLUDE 'D0$INC:L2JETS_CONT.INC'
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
      INCLUDE 'D0$INC:L2JETS_HOT.INC'
      INTEGER JET_COUNT                         ! count # that make cuts.
      INTEGER ICAND,IP, I, IWORD, IBIT
      INTEGER IAND, IBSET
C&IF VAXVMS,VAXELN
C&ELSE
C&      EXTERNAL IAND, IBSET
C&ENDIF

C----------------------------------------------------------------------
      JET_COUNT = 0                     ! init jet count
C---Cycle through the candidates: choose those with correct bit set and
C---that have flag = L2J_DONE
      DO 500, ICAND = 1 , NJTHOT
      IP = LJAUX + (NOW_IND_PARAM - 1)*NREP_JAUX*NJTHOT + (ICAND - 1)*
     &  NREP_JAUX
      IF ( IQ( IP + PJEVT) .NE. L2J_DONE) GOTO 500
      IF (IAND(JET_MASK(ICAND) ,NOWL1BIT) .EQ. 0) GOTO 500
C---Start applying cuts:
      IF ( Q( IP + PJET ) .LT. ETMIN( NOWPARAM ) ) GOTO 500
      IF ( Q( IP + PJEMFR) .LT. ABS(Q(IP+PJET))*EMFRACT_MIN( NOWPARAM))
     &    GOTO 500
      IF ( Q( IP + PJEMFR) .GT. ABS(Q(IP+PJET))*EMFRACT_MAX( NOWPARAM))
     &  GOTO 500
      IF ( SQRT( Q(IP+PJETASIZ)**2 + Q(IP+PJPHISIZ)**2)   .LT.
     &  ABS(Q(IP+PJET))* MINRAD(NOWPARAM)) GOTO 500
      IF ( SQRT( Q(IP+PJETASIZ)**2 + Q(IP+PJPHISIZ)**2) .GT.
     &  ABS(Q(IP+PJET))* MAXRAD( NOWPARAM)) GOTO  500
C---Record that this jet passed the individual jet cuts
      IWORD = (NOWPARAM-1)/32
      IBIT  = NOWPARAM - 32*IWORD - 1
      IQ( IP + PJMASK + IWORD ) = IBSET( IQ( IP + PJMASK +IWORD), IBIT)
C---Count those that pass all cuts:
      JET_COUNT = JET_COUNT + 1
  500 CONTINUE

C---Final test: Do we have enough passing jets to make the count?
      IF (JET_COUNT .GE. NJET_COUNT( NOWPARAM )) NOWRESULT = .TRUE.
  999 RETURN
      END
