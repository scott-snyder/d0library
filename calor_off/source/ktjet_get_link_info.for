C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_GET_LINK_INFO.FOR
C *1     3-FEB-1994 14:38:14 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_GET_LINK_INFO.FOR
      SUBROUTINE KTJET_GET_LINK_INFO( ETA, PHI, EM, E, LINK, TYPE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the Eta,phi,em energy and total energy of
C-        the channel (LINK) under the TYPE algorithm.
C-        TYPE=1 (PARTONS) TYPE=2 (PARTICLES) TYPE=3 (CELLS)
C-
C-   Inputs  : LINK [I] : Channel number from 1 to NCHAN (see IQ(LKVEC+3))
C-             TYPE [I] : TYPE we are doing now (INPUT_TYPE)
C-   Outputs : ETA  [R] : Eta of channel
C-             PHI  [R] : Phi of channel
C-             EM   [R] : EM energy of channel
C-             E(5) [R] : Ex,Ey,EZ,E,ET
C-   Controls:
C-
C-   Created  27-JAN-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INTEGER LISAQ, GZISAQ, GZISV1, LISV1, LISP1, LPOINT, LDUM, LDUM1
      INTEGER LCAEH, GZCAEH, KPOINT, LINK, TYPE, ID, ILINK, LCATD, IER
      INTEGER N, ISEC, IETA, IPHI, ICATD, LCAPH, KTJET_FIND_CAPH, LJETS
      INTEGER GZCATD, JJ
      REAL ETA,PHI, EM, E(5), EN(5), DELETA
      REAL THETA
C----------------------------------------------------------------------
      REAL THETA_FROM_ETA, ETA1
      THETA_FROM_ETA( ETA1 ) = 2.0*ATAN(EXP(-(ETA1)))
C----------------------------------------------------------------------
      ILINK = LINK
      ETA = 0
      PHI = 0.
      EM  = 0.
      CALL VZERO(E,5)
C********************** partons *****************************
      IF ( TYPE .EQ. 1 ) THEN
        LISAQ = GZISAQ()
  220   ILINK = ILINK - 1
        IF ( ILINK .EQ. 0 .OR. LISAQ .LE. 0) THEN
          ETA = Q( LISAQ + 9 )
          PHI = Q( LISAQ + 7 )
          EM  = 0.0              ! Partons are hadronic
          DO JJ = 1, 4
            E(JJ) = Q( LISAQ+1 + JJ)
          ENDDO
          E(5)  = E(4)*SIN(THETA_FROM_ETA(ETA))
        ELSE IF ( ILINK .LT. 0 ) THEN
          GOTO 800
        ELSE
          LISAQ = LQ( LISAQ )
          IF ( LISAQ .LE. 0 ) GOTO 800
          GOTO 220
        ENDIF
C********************** particles *****************************
      ELSE IF ( TYPE .EQ. 2 ) THEN
        LISP1 = 0
        LISV1 = GZISV1()
        IF ( LISV1 .GT. 0 ) LISP1 = LQ( LISV1- IZISP1 )
  130   ILINK = ILINK - 1
        IF ( ILINK .EQ. 0 .OR. LISP1 .LE. 0) THEN
          ETA = Q( LISP1 + 9 )
          PHI = Q( LISP1 + 7 )
          EM  = 0.0
          ID = ABS( IQ( LISP1 + 1 ) )
          IF ( ID .EQ. 10 .OR. ID .EQ. 12 .OR. ID .EQ. 110 .OR. ID .
     &          EQ. 220 ) EM = Q( LISP1 + 5 )
          DO JJ = 1, 4
            E(JJ) = Q( LISP1+1 + JJ)
          ENDDO
          E(5)  = E(4)*SIN(THETA_FROM_ETA(ETA))
        ELSE IF ( ILINK .LT. 0 ) THEN
          GOTO 800
        ELSE
          LISP1 = LQ( LISP1 )
          IF ( LISP1 .LE. 0 ) THEN
            LISP1 = 0
            LISV1 = LQ( LISV1 )
            IF ( LISV1 .GT. 0 ) LISP1 = LQ( LISV1 - IZISP1 )
            IF ( LISP1 .LE. 0 ) GOTO 800
          ENDIF
          GOTO 130
        ENDIF
C********************** cells *****************************
      ELSE IF ( TYPE .EQ. 3 ) THEN
        LCAEH = GZCAEH()
        IF ( LCAEH .LE. 0 ) GOTO 800
        LPOINT = LCAEH + (ILINK-1)*IQ(LCAEH+2)
        CALL ETOETA( Q( LPOINT + 4 ), PHI, THETA, ETA )
        EM = 0.0
        IF ( IQ( LPOINT + 14 ) .GE. MNLYEM .AND. IQ( LPOINT + 14 ) .
     &        LE.
     &        MXLYEM ) EM = Q( LPOINT + 8 )
          DO JJ = 1, 4
            E(JJ) = Q( LPOINT + 3 + JJ)
          ENDDO
          E(5)  = Q( LPOINT + 8 )
C***************************************************
      ELSEIF (TYPE .EQ. 4 ) THEN
C        CALL QCD_GZMDST_ALL( LDUM, LCATD, LDUM1 )
        LCATD = GZCATD()
        IF ( LCATD .LE. 0 ) CALL ERRMSG('no catd','KTJET_GET_LINK_INFO',
     &    ' BUT WE HAD IT BEFORE!','F')
        N = IQ( LCATD + 8 )
        IF ( ILINK .LE. N ) THEN
          ID= 1
          LPOINT = ILINK + 8
          ISEC = 1
        ELSE
          ID=2
          ILINK = ILINK - N
          LPOINT = ILINK + N + 9
          ISEC = 2
        ENDIF
        ICATD = LCATD + LPOINT
        CALL UPCATD( ICATD, ISEC, IETA, IPHI, ETA, PHI, DELETA, EN)
        IF ( EN(4) .GT. 800. ) THEN
          EN(4) = .2
          EN(3) = .1
          EN(2) = 0.
          EN(1) = 0.1
        ENDIF
          DO JJ = 1, 4
            E(JJ) = EN(JJ)
          ENDDO
          E(5)  = E(4)*SIN(THETA_FROM_ETA(ETA+DELETA))
        EM= EN(4)
        IF ( ISEC .EQ. 2 ) EM = 0.
C----------------------------------------------------------------
      ELSE IF ( TYPE .EQ. 5 ) THEN
        LCAPH = KTJET_FIND_CAPH( KTCUT( IKTCUT-1 ) - .0001)
        LJETS = LQ( LCAPH - IZJETS )
        DO WHILE ( LJETS .GT. 0 )
          ILINK = ILINK - 1
          IF ( ILINK .EQ. 0 ) THEN
            ETA = Q( LJETS + 9)
            PHI = Q( LJETS + 8)
C            E   = Q( LJETS + 5)
            EM  = E(4)*Q( LJETS + 14)
          ENDIF
          LJETS = LQ(LJETS)
        ENDDO
      ELSE
        CALL ERRMSG('INVALIED TYPE','KTJET_GET_LINK_INFO',
     &        'INPUT_TYPE is invalid','E')
      ENDIF
  999 RETURN

  800 CALL ERRMSG('Links corrupted','KTJET_GET_LINK_INFO',
     &          'Links of cluster are corrupt','E')
      RETURN
      END
