      SUBROUTINE CJTANL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CAJETS Analysis Package for D0RECO
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   27-FEB-1990   Boaz Klima
C-   Updated   9-OCT-1990   Boaz Klima  - Set CAPH to DEFAULT algorithm
C-   Updated  20-NOV-1991   Boaz Klima  - Version 2 of JETS ( no JTSH )
C-   Updated  20-NOV-1991   Nick Hadley, Boaz Klima
C-                            Add VERIFY and SUMMARY stuff
C-   Modified 27-FEB-1995   R. Astur "Prevent array overflow"
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER IER,II
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
      INCLUDE 'D0$LINKS:IZJTSH.LINK'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER I, J, K
      REAL    EMFRAC
      REAL    PJJ, EJJ, MJJ
      INTEGER NCJ, LJETS, GZJETS, LJTSH, LPNUT, GZPNUT
      REAL    CJ_ETA(25), CJ_PHI(25), CJ_PT(25), CJ_E(4,25)
      REAL ETOT,ETRANS,ETMISS,RWIDTH
      INTEGER ITEMP, IFL
      REAL    TEMPLATE(20)
      INTEGER LCAPH, GZCAPH, NUMBER_CAPH
      LOGICAL FLGVAL, VERIFY
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C----------------------------------------------------------------------
C
      CALL DHDIR('CAJETS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','CJTANL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF(FIRST)THEN
        FIRST = .FALSE.
C
C ****  Loop over all CAPH only if VERIFY is TRUE
C
        IF ( FLGVAL('VERIFY') ) THEN
          NUMBER_CAPH = 4
        ELSE
          NUMBER_CAPH = 1
        ENDIF
C
C $$$$$$$$$$$$$$$$$$$$$$$$$$
C $$$$  BOOK HISTOGRAMS HERE
C $$$$$$$$$$$$$$$$$$$$$$$$$$
C
        DO II = 1, NUMBER_CAPH
          CALL HBOOK1(100+II,' No. of Jets(NCJ)',60,0.,12.,0.)
          CALL HBOOK1(200+II,' Et  of Jets',60,0.,120.,0.)
          CALL HBOOK2(300+II,' Phi vs Eta of Jets',40,-4.,4.,
     &                                             32,0.,6.4,0.)
          CALL HBOOK1(400+II,' Jet-Jet Mass ; Et(jet) > 15 GeV',
     &                                             60,0.,240.,0.)
          CALL HBOOK2(500+II,' Et(PNUT) vs Et(EMjet) ',40,0.,120.,
     &                                            40,0.,120.,0.)
        END DO
C
        CALL HBARX(0)
C
      ENDIF
C
C $$$$$$$$$$$$$$$$$$$$$$$$$$
C $$$$  FILL HISTOGRAMS HERE
C $$$$$$$$$$$$$$$$$$$$$$$$$$
C
C ****  SET PATH TO THE DEFAULT JET FINDING ALGORITHM
C ****       ( CONE WITH R=0.7, ET=8GEV )
C
      II = 1
      TEMPLATE(1)=0
      CALL SET_CAPH('CONE_JET',TEMPLATE,IER)
      IF ( IER.NE.0 ) GOTO 998
      LCAPH = GZCAPH( )
      IF( LCAPH .EQ. 0 ) GOTO 998
C
C ****  LOOP OVER RECO JETS
C
  100 LJETS = LQ(LCAPH-IZJETS)
      NCJ = 0
      IF( LJETS .EQ. 0 ) GOTO 250
  200 NCJ = NCJ + 1
      IF ( NCJ .LE. 25 ) THEN
        CJ_ETA(NCJ) = Q(LJETS+ 9)
        CJ_PHI(NCJ) = Q(LJETS+ 8)
        CJ_PT(NCJ) = Q(LJETS+ 6)
        CALL HFILL(200+II,CJ_PT(NCJ),0.,1.)
        CALL HFILL(300+II,CJ_ETA(NCJ),CJ_PHI(NCJ),1.)
        DO I = 1, 4
          CJ_E(I,NCJ) = Q(LJETS+1+I)
        END DO
      ENDIF
      LJETS = LQ( LJETS )
      IF( LJETS .NE. 0 ) GOTO 200
  250 CONTINUE
C
C ****  MULTIPLICITY OF JETS
C
      CALL HFILL(100+II,FLOAT(NCJ),0.,1.)
C
C ****  CALCULATE TWO JET INVARIANT MASS FOR EVERY PAIR OF JETS
C
      J = 1
      DO WHILE ( (CJ_PT(J).GT.15.).AND.(J .LE. MIN(24,NCJ-1) ) )
        K = J + 1
        DO WHILE ( (CJ_PT(K).GT.15.).AND.(K.LE.MIN(NCJ,25)) )
          PJJ = 0.0
          DO I = 1, 3
            PJJ =  PJJ + ( CJ_E(I,J)+CJ_E(I,K) )**2
          END DO
          PJJ = SQRT (PJJ)
          EJJ = CJ_E(4,J) + CJ_E(4,K)
          MJJ = SQRT (ABS( (EJJ+PJJ)*(EJJ-PJJ) ) )
          CALL HFILL(400+II,MJJ,0.0,1.0)
          K = K + 1
        END DO
        J = J + 1
      END DO
C
C ****  PLOT ET(EM JET) VS ET(PNUT) - LOOKING FOR W-->E+NU
C
C ****  GET MISSING ET
C
C
      LPNUT = GZPNUT(1)
      IF(LPNUT.GT.0) THEN
        ETMISS = Q(LPNUT+7)
      END IF
C
C ****  LOOP OVER RECO JETS
C
      LJETS = LQ(LCAPH-IZJETS)
      IF (LJETS.LE.0) GOTO 997
  600 ETOT = Q(LJETS+5)
      ETRANS = Q(LJETS+6)
      IF ( IQ(LJETS+1).GT.1 ) THEN
        EMFRAC = Q(LJETS+14)
        RWIDTH = SQRT(Q(LJETS+12)**2 + Q(LJETS+13)**2)
      ELSE
        LJTSH = LQ(LJETS-IZJTSH)
        EMFRAC = Q(LJTSH+5)
        RWIDTH = SQRT(Q(LJTSH+3)**2 + Q(LJTSH+4)**2)
      ENDIF
      IF (EMFRAC.GE.0.9) THEN
        IF (RWIDTH.LE.0.15) THEN
          CALL HFILL(500+II,ETRANS,ETMISS,1.)
        END IF
      END IF
      LJETS = LQ(LJETS)
      IF (LJETS.GT.0) GOTO 600
C
C ****  Loop over all CAPH only if VERIFY is TRUE
C
  997 IF ( NUMBER_CAPH.EQ.1 ) GOTO 998
      II = II + 1
      LCAPH = LQ( LCAPH )
      IF( LCAPH .NE. 0 ) GOTO 100
  998 CALL RESET_CAPH
  999 RETURN
      END
