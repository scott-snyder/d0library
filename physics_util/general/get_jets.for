      SUBROUTINE GET_JETS(NJETS_MAX,ET_CUT,JSIZ,P23,NJETS,JET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  gets kinematic quantities of jets from
C-   Jets bank. The output is ordered according to decreasing
C-   ET = Q(LJETS+6), highest ET first.
C-
C-   Use entry point GET_JETS_SELECT(ALGORITHM,TEMPLATE) to select
C-   Jet algorithm. If GET_JETS_SELECT is NEVER called then the
C-   default is ALGORITHM='CONE_JET' and TEMPLATE(1)=0.0.
C-
C-   Use entry point GET_JETS_CONE(MAXCONE,CONE,NCONE) to return the
C-   cone sizes of the jets in the current event.
C-
C-   Inputs  : NJETS_MAX [I]   Maximum number of jets to output in P23(23,*)
C-             ET_CUT    [R]   ET cut for jets
C-             JSIZ            number of words buffered for each jet
C-   Outputs : P23(31,*) [R]
C-
C-              P(1..9)
C-                            PX, PY, PZ, E, ET, ETA, PHI, DET_ETA, QUALITY
C-                    I = 9
C-              P(I+1)    Sig**2(Ex)
C-              P(I+2)    Sig**2(Ey)
C-              P(I+3)    RMS Eta width
C-              P(I+4)    RMS Phi width
C-              P(I+5)    Fraction of EM Et
C-              P(I+6)    Flag for merging/splitting
C-              P(I+7)    Number of cells above threshold (1GeV def)
C-              P(I+8)    Fraction of ICD/MG Et(EtICD+EtMG/TOTAL_ET)
C-              P(I+9)    Fraction of CH Et (Et CH/TOTAL_ET)
C-              P(I+10)   Ratio of hottest to next-hottest cell
C-              P(I+11)   Number of CDC tracks in jet cone
C-              P(I+12)   Number of TOWERS comprising 90% of jet Et
C-              P(I+13)   ConeSize (-1.0 for Nearest Neighbor)
C-              P(I+14)   Phi Difference between MET and JET (PNUT(2))
C-              P(I+15)   Spare
C-              P(I+16)   Energy Correction Flag (I)
C-              P(I+17)   Sig**2(Ez)
C-              P(I+18)   dExdEy
C-              P(I+19)   dExdEz
C-              P(I+20)   dEydEz
C-              P(I+21)   Change in missing ET X component due to Jet energy
C-                        scale re-calibration.
C-              P(I+22)   Change in missing ET Y component due to Jet energy
C-                        scale re-calibration.
C-
C-
C-             NJETS     [I]   Total number of jets surviving ET cut,
C-                             but not exceeding MAXJETS
C-             JET       [L]   TRUE IF NJETS > 0
C-   Controls:
C-
C-   Created   5-JAN-1991   Rajendran Raja
C-   Updated  12-NOV-1991   Stan M. Krzywdzinski, Harrison B. Prosper
C-      Make in a general utility
C-   Updated  14-AUG-1992   Stan M. Krzywdzinski
C-      Make sure P8 is not overwritten beyond its bounds
C-      Make sure the 'usual' ET is used
C-   Updated  16-OCT-1992   Stan M. krzywdzinski
C       Increased P8(8,*) --> P11(11,*)
C-   Updated   9-APR-1993   Rajendran Raja  Calls OBJECT_JET
C-                          CALLS JET_CAL and gets corrected energies
C-                          ALSO DOES JET_ET_MCCORR AUTOMATICALLY IF MC DATA
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NJETS_MAX,NJETS
      INTEGER JSIZ,NJETSP
      REAL    ET_CUT,P23(JSIZ,*)
      LOGICAL JET
      INTEGER MAXCONE,NCONE
      REAL    CONE(*)
      INTEGER NSIZE
      REAL    TEMP1
      REAL    RAT
C----------------------------------------------------------------------
      REAL    TEMP(*)
      CHARACTER*(*) ALGORITHM
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
C
      CHARACTER*32 ALG_NAME
      INTEGER IER, N, I ,J ,IP , ISYS
      INTEGER GZJETS,GZCAPH
      LOGICAL MONTE_CARLO_DATA
      REAL    PHI,PHI_CALC,EX,EY,TEMPLATE(50), ET_JET
      REAL    JET_CAL
      REAL    ZVERT,ECORR,ETA
      REAL    VERT(14)
      REAL    CONE_SIZE
      REAL    OLDE(5),OLDETA,OLDPHI,OLDEMF,OLDCONE
      REAL    NEWE(5),NEWETA,NEWPHI
      REAL    DE(5)
      INTEGER ICHOICE
C
      LOGICAL DO_CORRS(3)
      LOGICAL DO_ZSP_CORR,DO_UNDEVT_CORR,DO_OUTOFCONE_CORR
      INTEGER SYSI
      LOGICAL FOUND_MATCH
      SAVE DO_ZSP_CORR,DO_UNDEVT_CORR,DO_OUTOFCONE_CORR
      SAVE SYSI
      SAVE FOUND_MATCH
C----------------------------------------------------------------------
      DATA DO_ZSP_CORR /.TRUE./,DO_UNDEVT_CORR/.TRUE./,
     &  DO_OUTOFCONE_CORR/.TRUE./
      DATA SYSI /0/
      DATA FOUND_MATCH/.TRUE./
C----------------------------------------------------------------------
      LOGICAL ZSP_CORR,UNDEVT_CORR,OUTOFCONE_CORR
      INTEGER SYS
C----------------------------------------------------------------------
      INTEGER LUN, NEV_IN, NEV_SEL
CCC      COMMON /TOTALS/ LUN, NEV_IN, NEV_SEL
C----------------------------------------------------------------------
      SAVE ALG_NAME,TEMPLATE
      DATA ALG_NAME /'CONE_JET'/
      DATA TEMPLATE /50*0.0/
C----------------------------------------------------------------------
      NJETSP = JSIZ - 2  !TWO WORDS FOR MISSING ET CORRECTION
      CALL VZERO(P23,JSIZ*NJETS_MAX)
      NJETS = 0
      JET = .FALSE.
      IER = 0
C
C ****  Select Jet algorithm
C

      CALL SET_CAPH(ALG_NAME,TEMPLATE,IER)
C
      IF ( IER .LT. 0 ) THEN
        CALL ERRMSG('BADJETS','GET_JETS',
     &    'Problem selecting jets','W')
        GOTO 999
      ELSE
        CONE_SIZE = 0.0
        IF ( ALG_NAME.EQ.'CONE_JET' ) THEN
          CONE_SIZE=TEMPLATE(3)
          IF ( TEMPLATE(3).EQ.0.0 ) THEN
            CONE_SIZE = 0.7  !DEFAULT
          ENDIF
        ENDIF
      ENDIF
C
      LJETS = GZJETS()
      IF (LJETS .LE. 0) THEN
        GO TO 999
      ENDIF
C
      CALL NOBJ_JETS(NJETS,NSIZE)
      IF ( NSIZE.GT.(JSIZ-2) ) THEN
        CALL ERRMSG('CALORIMETER','GET_JETS',
     &    ' Space allocation too small in GET_JETS','W')
      ENDIF
      NJETS = MIN(NJETS,NJETS_MAX)
      IP = 0
      DO I = 1 , NJETS
        IP = IP + 1
        CALL OBJECT_JET(I,(JSIZ-2),P23(1,IP))
        IF ( P23(5,IP).LT.ET_CUT ) THEN
          IP = IP -1   !JET BELOW ET
        ENDIF
      ENDDO
      NJETS = IP
      IF (NJETS .GT. 0) JET = .TRUE.
      CALL GTVERT(1,VERT)
C
C      ZVERT = VERT(5)
c      SYSI = 0  !NOMINAL CORRECTION
c      DO_CORRS(1) = DO_ZSP_CORR
c      DO_CORRS(2) = DO_UNDEVT_CORR
c      DO_CORRS(3) = DO_OUTOFCONE_CORR
C
      DO I = 1 , NJETS
C
C ****  now get the corrections done in CAFIX
C
        CALL UCOPY(P23(1,I),OLDE(1),5)
        OLDETA = P23(6,I)
        OLDPHI = P23(7,I)
        OLDEMF = P23(14,I)
        OLDCONE = P23(22,I)
C
        IF ( OLDCONE.EQ.0.7 ) THEN
          ICHOICE = 1
        ELSE
          ICHOICE = 2
        ENDIF
C
        IF ( MONTE_CARLO_DATA() ) THEN
C
          CALL MC_ET_CORR(OLDE,OLDETA,OLDPHI,OLDEMF,ICHOICE,
     &     NEWE,NEWETA,NEWPHI,DE,FOUND_MATCH)
C
          CALL UCOPY(NEWE(1),P23(1,I),5)
          P23(6,I) = NEWETA
          P23(7,I) = NEWPHI
          ECORR = NEWE(4)
C
C ****  RATIO DEBUG
C
          RAT = NEWE(5)/OLDE(5)
          CALL DO_HF1(711,RAT,1.0)
          CALL DO_HF2(712,RAT,OLDE(5),1.0)
C
        ELSE
          ECORR = P23(4,I)
        ENDIF
        DO J = 1 , 4
          TEMP1 = P23(J,I)*ECORR/P23(4,I)
          IF ( J.LE.2 ) THEN
C
C change in missing Et component
C this is for the sake of consistency. it will not be used . results from
C missing_et correction will be used instead.
C
            P23(NJETSP+J,I) = P23(NJETSP+J,I) - (TEMP1-P23(J,I))
          ENDIF
          P23(J,I) = TEMP1
        ENDDO
        P23(5,I) = SQRT(P23(1,I)**2+P23(2,I)**2) !ET
      ENDDO
C
      CALL RESET_CAPH
      RETURN
C
C ****  Entry point to select jet finding algorithm
C
      ENTRY GET_JETS_SELECT(ALGORITHM,TEMP)
      ALG_NAME = ALGORITHM
      N = ABS(TEMP(1))
      IF ( N .LE. 0 ) THEN
        TEMPLATE(1) = 0.0
      ELSE
        DO I =  1, 2*N+1
          TEMPLATE(I) = TEMP(I)
        ENDDO
      ENDIF
      RETURN
C
C ****  Return cone sizes of all jets in current event
C
      ENTRY GET_JETS_CONE(MAXCONE,CONE,NCONE)
      NCONE = 0
      LCAPH = GZCAPH()  ! Get address of first bank in linear chain
      DO WHILE ( LCAPH .GT. 0 )
        IF ( IQ(LCAPH+K_ALGORITHM) .EQ. A_CONE_JET ) THEN
          NCONE = NCONE + 1
          CONE(NCONE) = Q(LCAPH+6) ! Cone size
        ENDIF
        LCAPH = LQ(LCAPH)
      ENDDO
C
      RETURN
C
      ENTRY GET_JETS_SET_CORR(ZSP_CORR,UNDEVT_CORR,OUTOFCONE_CORR,
     &  SYS)
      DO_ZSP_CORR = ZSP_CORR
      DO_UNDEVT_CORR = UNDEVT_CORR
      DO_OUTOFCONE_CORR = OUTOFCONE_CORR
      SYSI = SYS
C
  999 RETURN
      END
