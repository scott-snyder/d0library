      SUBROUTINE QCD_MDST_L1_FILL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill psuedo L1 common blocks with info from
C-                         the CATD bank of the MDST. This is incorrect
C-                         for many reasons. e.g. the CATD includes
C-                         CH/MG/ICD and vertex info
C-
C-                         Apply a 250 MeV/trigger tower suppression
C-                         Substitute found L1 towers from ESUM
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-JUN-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:JETS.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:JETVAR.INC'
      INCLUDE 'D0$INC:PSL1.INC'
      INTEGER I, N, LCATD, GZCATD
      INTEGER ICATD, ISEC, OFF, IETA, IPHI, L1ETAC, L1PHIC
      INTEGER LTETA, LTPHI, II
      REAL DELTAETA, ETA, PHI, E(4), ET
      REAL DIF
      LOGICAL ITSOK
      INTEGER EVT
      SAVE  EVT
      INCLUDE 'D0$INC:QCDJ.DEF'
C----------------------------------------------------------------------
      CALL VZERO( PSL1TOW, 40*32 )
      CALL VZERO( PSL1LT, 10*4 )
C
C: Get link to CATD
C
      LCATD = GZCATD()
      IF ( LCATD .LE. 0 ) THEN
        CALL ERRMSG('NO CATD','QCD_MDST_L1_FILL','Cant find CATD','W')
        GOTO 999
      ENDIF
C
C: Loop over EM and HAD portions of CATD
C
      DO II = 1, 2
        OFF = 8
        ISEC = 1
        N = IQ( LCATD + 8 )
        IF ( II .EQ. 2 ) THEN
          OFF = N + 9
          ISEC = 2
          N = IQ( LCATD + N + 9 )
        ENDIF
        DO I = 1, N
          ICATD = LCATD + OFF + I
          CALL UPCATD( ICATD, ISEC, IETA, IPHI, ETA, PHI, DELTAETA, E )
          CALL CPHTT(IETA,IPHI,L1ETAC,L1PHIC,ITSOK)
          IF ( ITSOK .AND. ABS(IETA) .LT. 36 ) THEN
            ET = E(4)*SIN( THETA_FROM_ETA( L1ETAC*.2 - .1*SIGN(1,
     &        L1ETAC)) )
            IF ( L1ETAC .GT. 0 ) THEN
              L1ETAC = L1ETAC + 20
            ELSE
              L1ETAC = L1ETAC + 21
            ENDIF
C: Fill psuedo L1
            PSL1TOW( L1ETAC, L1PHIC ) = PSL1TOW( L1ETAC, L1PHIC ) + ET
          ENDIF
        ENDDO
      ENDDO
C
C: Apply a 250 Mev suppression to it all
C
      DO LTPHI = 1, 32
        DO LTETA = 1, 40
          IF ( PSL1TOW( LTETA, LTPHI ) .LT. .25 ) PSL1TOW( LTETA, LTPHI
     &      )
     &      = 0.0
        ENDDO
      ENDDO
C
C: Substitute real L1 candidates
C
      NOWLIST = 4
      CALL GET_L1JETS
      CALL HFILL(901, FLOAT(NJET(4)), 0., 1. )
      DO I = 1, NJET(4)
        IETA = IETALST( I, 4 )
        IPHI = IPHILST( I, 4 )
        IF ( IETA .GT. 0 ) THEN
          IETA = IETA + 20
        ELSE
          IETA = IETA + 21
        ENDIF
        DIF = ETLST(I,4) - PSL1TOW( IETA, IPHI )
        CALL HFILL( 902, ETLST(I,4), DIF, 1. )
        CALL HFILL( 903, ABS(ETALST(I,4)), DIF, 1. )
        PSL1TOW( IETA, IPHI ) = ETLST( I, 4 )
      ENDDO

  999 RETURN
      END

