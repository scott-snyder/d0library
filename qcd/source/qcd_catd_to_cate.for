      SUBROUTINE QCD_CATD_TO_CATE( LCATD1 )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert CATD to CATE in order to rerun
C-                         jet reconstruction
C-
C-   Inputs  :    [I] LCATD : Pointer to CATD bank
C-   Outputs :
C-   Controls:
C-
C-   Created   1-JUN-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:L2LINK.INC'       ! borrow L2 zebra link area
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
C
      INTEGER GZCATE, LCATE, LCATD, GZCATD, ICATD, LCATD1
      INTEGER IETA, IPHI, OFF, ISEC, P, I, II, N, J
C
      INTEGER NTWMAX
      PARAMETER( NTWMAX = NPHIL*2*NETAL + NPHIL)
      REAL    ETLIST(NTWMAX,2)
      INTEGER NTOWERS(2), ORLIST( NTWMAX, 2 )
      REAL E(5), DELETA, ETA, PHI
      CHARACTER*4 OLD_PATH
C
      EQUIVALENCE ( L2LINK(1), LCATD )
      EQUIVALENCE ( L2LINK(2), LCATE )
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
C----------------------------------------------------------------------
C
C: Check for CATE
C
      IF ( GZCATE() .GT. 0 ) THEN
        CALL ERRMSG('CATE EXISTS','QCD_CATD_TO_CATE',
     &    'No need to make new one', 'W')
        GOTO 999
      ENDIF
C
C: Check for CATD
C
      IF ( LCATD1 .LE. 0 ) THEN
        CALL ERRMSG('NO CATD ','QCD_CATD_TO_CATE',
     &    'Cant make CATE', 'W')
        GOTO 900
      ENDIF
C
C: Activate link area
C
      CALL MZLINT(IXCOM,'/L2LINK/',DUM,L2LINK(NLNK),DUM)
      LCATD = LCATD1
C
C: Initialize
C
      CALL VZERO( NTOWERS, 2 )
      CALL VZERO( PTCATE, NTWMAX*2)
      CALL VZERO( ETLIST, NTWMAX*2)
      CALL VZERO( ORLIST, NTWMAX*2)
C
C: Unpack CATD
C
      DO II = 1,2
        OFF = 8
        ISEC = 1
        N   = IQ(LCATD + 8 )
        IF ( II .EQ. 2 ) THEN
          OFF = N + 9
          ISEC = 2
          N = IQ(LCATD + N + 9 )
        ENDIF
        DO I = 1, N
          ICATD = LCATD + OFF + I
          CALL UPCATD(ICATD,ISEC,IETA,IPHI,ETA,PHI,DELETA,E)
          IF ( E(4) .GT. 800. ) THEN
            E(4) = .2
            E(3) = .1
            E(2) = 0.
            E(1) = 0.1
          ENDIF
          E(5) = SQRT( E(1)**2 + E(2)**2 )
          IF ( II .EQ. 1 ) THEN           ! EM towers
            NTOWERS(II) = I
            ORLIST( NTOWERS(II),1 ) =I + OFF
            ETLIST( NTOWERS(II),1 ) = E(5)
            PTCATE( IETA, IPHI, 1 ) =I + OFF
            NTOWERS(2) = NTOWERS(2) + 1   ! Total towers
            ORLIST( NTOWERS(2),2 ) = I + OFF
            ETLIST( NTOWERS(2),2 ) = E(5)
            PTCATE( IETA, IPHI, 2 ) =0
          ELSE                            ! Hadronic towers
            P = PTCATE( IETA,IPHI, 1 ) - 8
            IF ( P .GT. 0 ) THEN
              ETLIST( P, 2 ) = ETLIST( P,2 ) + E(5)
              PTCATE( IETA, IPHI, 2 ) = I + OFF
            ELSE
              NTOWERS(2) = NTOWERS(2) + 1
              ETLIST( NTOWERS(2), 2 ) = E(5)
              ORLIST( NTOWERS(2), 2 ) = I + OFF
              PTCATE( IETA, IPHI, 2 ) = I + OFF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C: Book CATE bank
C
      N = NTOWERS(1) + NTOWERS(2)
      CALL PATHGT( OLD_PATH )
      CALL PATHST( 'RECO' )
      CALL BKCATE( N, LCATE )
      CALL PATHST( OLD_PATH )
C
C: Loop over EM and TOTAL and fill CATE
C
      IQ( LCATE + 3 ) = N + CATENM*NTOWERS(1)
      DO II = 1, 2
C: Sort
        CALL SRTFLT( ETLIST(1,II), NTOWERS(II), ORLIST(1,II) )
C: Loop
        DO I = NTOWERS(II), 1, -1
          P = (II-1)*NTOWERS(1) + NTOWERS(II) - I ! Pointer to CATE
          ICATD = ORLIST( I , II )
          ISEC  = 1
          IF ( ICATD .GT. NTOWERS(1) + 8 ) ISEC = 2
          ICATD = ICATD + LCATD
          CALL UPCATD(ICATD,ISEC,IETA,IPHI,ETA,PHI,DELETA,E)
          IF ( E(4) .GT. 800. ) THEN
            E(4) = .2
            E(3) = .1
            E(2) = 0.
            E(1) = 0.1
          ENDIF
          E(5) = SQRT( E(1)**2 + E(2)**2 )
          DO J = 4, 8
            Q( LCATE + P*IQ(LCATE+2) + J ) = Q( LCATE + P*IQ(LCATE+2) +
     &        J) + E(J-3)
          ENDDO
          IQ( LCATE + P*IQ(LCATE+2) + 11 ) = 1
          IQ( LCATE + P*IQ(LCATE+2) + 12 ) = IETA
          IQ( LCATE + P*IQ(LCATE+2) + 13 ) = IPHI
          IQ( LCATE + P*IQ(LCATE+2) + 14 ) = II
C
C: If this is a total, then look for other tower
C
          IF ( II .EQ. 2 .AND. ISEC .EQ. 1 .AND. PTCATE( IETA, IPHI, 2)
     &      .NE. 0 ) THEN
            ISEC = 2
            ICATD = PTCATE( IETA, IPHI, 2 ) + LCATD
            CALL UPCATD(ICATD,ISEC,IETA,IPHI,ETA,PHI,DELETA,E)
            IF ( E(4) .GT. 800. ) THEN
              E(4) = .2
              E(3) = .1
              E(2) = 0.
              E(1) = 0.1
            ENDIF
            E(5) = SQRT( E(1)**2 + E(2)**2 )
            DO J = 4, 8
              Q( LCATE + P*IQ(LCATE+2) + J ) = Q( LCATE + P*IQ(LCATE+2)
     &          + J )
     &          + E(J-3)
            ENDDO
            IQ( LCATE + P*IQ(LCATE+2) + 11 ) = IQ( LCATE + P*IQ(LCATE+2)
     &        +
     &        11) + 1
          ENDIF
        ENDDO
      ENDDO
C
C: Fill the PTCATE array properly
      PTTFLG = .FALSE.          ! We will set PTCATE
      CALL VZERO( PTCATE, NTWMAX*2 )
      DO II = 1,2
        DO I = 1, NTOWERS(II)
          P = (II-1)*NTOWERS(1) + I
          IETA = IQ( LCATE + (P-1)*IQ(LCATE+2) + 12 )
          IPHI = IQ( LCATE + (P-1)*IQ(LCATE+2) + 13 )
          PTCATE( IETA, IPHI, II ) = P
        ENDDO
      ENDDO

  900 CONTINUE
  999 LCATD1 = LCATD
      DUM(1) = 0                        ! Deactivate link area
      RETURN
      END
