      SUBROUTINE TOP_FIT_MATCH_JETS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MATCH PJETS TO .5 AND .7 CONE JETS AND DO BTAG
C-                         FOR E+JETS
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-FEB-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:EVENT_QUAN_2C.INC'
      CHARACTER*32 ALG
C
      REAL JET_TEMPLATE(3)
C
      SAVE JET_TEMPLATE
      LOGICAL MONTE_CARLO_DATA
C
      INTEGER NALG
      PARAMETER( NALG = 3 )
      INTEGER NPJET,NJETS(NALG), NMATCH(NALG)
      INTEGER GZPJET,GZJETS,IALG,IER
C
      INTEGER NHEAD
      PARAMETER( NHEAD = 9 ) !NUMBER OF HEADER WORDS
      INTEGER NPJTMX
      PARAMETER( NPJTMX = 7 )
      INTEGER NSAV
      PARAMETER( NSAV = 9 ) !QUANTITIES PER PJET
      INTEGER NREP          !QUANTITIES PER JET
      PARAMETER( NREP = NSAV*(NALG+1) )
      INTEGER NBUF
      PARAMETER( NBUF = NHEAD+NREP*NPJTMX )
      REAL    BUFFER(NBUF)
C
      REAL    ET_PJET
C
      INTEGER IND
      INTEGER LPJPT
      EQUIVALENCE (LPJPT,CSTLNK(LNKMX))
      INTEGER LISAQ,LISAQ1
      EQUIVALENCE (LISAQ,CSTLNK(LNKMX-1))
      EQUIVALENCE (LISAQ1,CSTLNK(LNKMX-2))
C
      INTEGER J,NL
      INTEGER GZ_MATCH_JETS
      LOGICAL first
      SAVE first
      DATA first / .true. /

C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_FIT_RCP')
        CALL DO_HBOOK('TOP_JET_MATCH')
        CALL EZRSET
      ENDIF
C
      IF ( .NOT.MONTE_CARLO_DATA() ) THEN
        RETURN
      ENDIF
C
      CALL UZERO(BUFFER,1,NBUF)
C
      BUFFER(1) = RUNC
      BUFFER(2) = EVENTC
C
      CALL MATCH_JETS_BEGIN(10,8,9,8)  !1ST SET PJETS
C
      LPJET = GZPJET()
      CALL ZSORT(IXCOM,LPJET,2)  !SORT ON ET
      CALL ZTOPSY(IXCOM,LPJET)  !DECREASING ORDER IN ET
      DO IALG = 1 , NALG
        IF ( IALG.EQ.1 ) THEN
          ALG = 'CONE_JET'
          JET_TEMPLATE(1) = 1.0
          JET_TEMPLATE(2) = 6.0
          JET_TEMPLATE(3) = 0.7  !CONE SIZE
        ELSEIF ( IALG.EQ.2 ) THEN
          ALG = 'CONE_JET'
          JET_TEMPLATE(1) = 1.0
          JET_TEMPLATE(2) = 6.0
          JET_TEMPLATE(3) = 0.5  !CONE SIZE
        ELSEIF ( IALG.EQ.3 ) THEN
          ALG = 'NEIGHBOR'
          JET_TEMPLATE(1) = 0.0
          JET_TEMPLATE(2) = 0.0
          JET_TEMPLATE(3) = 0.0
        ENDIF
C
        CALL SET_CAPH(ALG,JET_TEMPLATE,IER)
C
        IF ( IER .LT. 0 ) THEN
          CALL ERRMSG('TOP_FIT_MATCH_JETS','SET_CAPH',
     &      'Problem selecting jets','W')
          GOTO 999
        ELSE
          LJETS = GZJETS()
          LPJET = GZPJET()
          CALL MATCH_JETS(LPJET,LJETS,NPJET,NJETS(IALG),
     &      NMATCH(IALG),IER)
          NPJET = MIN(NPJET,NPJTMX)
          BUFFER(3) = NPJET
          BUFFER(3+IALG) = NJETS(IALG)
          BUFFER(6+IALG) = NMATCH(IALG)
          IF ( IER.EQ.0 ) THEN
            DO J = 1 , NPJET
              LJETS = GZ_MATCH_JETS(J,LPJET)
C
              IF ( IALG.EQ.1 ) THEN
C FILL IN PJET INFO
                IND = NHEAD+NREP*(J-1)+1
                CALL UCOPY(Q(LPJET+3),BUFFER(IND),4)
                IND = IND + 4
                BUFFER(IND) = Q(LPJET+2)  !ET
                BUFFER(IND+1) = Q(LPJET+10) !ETA
                BUFFER(IND+2) = Q(LPJET+8) !PHI
                LPJPT = LQ(LPJET-1)
                NL = IQ(LPJPT-3)
                IF ( NL.GE.2 ) THEN
                  LISAQ = LQ(LPJPT-2) ! 1ST PARTON
                  BUFFER(IND+3) = IQ(LISAQ+1) !ISAJET PARTON TYPE
                ELSEIF ( NL.GE.3 ) THEN
                  LISAQ1 = LQ(LPJPT-3) !2ND PARTON
                  BUFFER(IND+4) = IQ(LISAQ1+1) !ISAJET PARTON TYPE
                ENDIF
              ENDIF
C
              IF ( LJETS.EQ.0 ) THEN
C
C NO MATCH
C
              ELSE
C MATCH
                IND = NHEAD + NREP*(J-1) + 9*IALG + 1
                BUFFER(IND) = 1 !MATCH
                CALL UCOPY(Q(LJETS+2),BUFFER(IND+1),5)
                IND = IND+6
                BUFFER(IND) = Q(LJETS+9) !ETA
                BUFFER(IND+1) = Q(LJETS+8) !PHI
                ET_PJET = Q(LPJET+2)
                BUFFER(IND+2) = Q(LJETS+6)/ET_PJET  !RATIO
              ENDIF

            ENDDO
          ELSE
            CALL ERRMSG('TOP_FIT_MATCH_JETS','MATCH_JETS',
     &        'ERROR MATCHING JETS','W')
          ENDIF
        ENDIF
C
        CALL RESET_CAPH
      ENDDO
      CALL MATCH_JETS_END
C
      CALL DO_HFN('FIT2C',200,BUFFER)  !FILL NTUPLE
C
  999 RETURN
      END
