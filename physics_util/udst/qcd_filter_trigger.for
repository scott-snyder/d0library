      SUBROUTINE QCD_FILTER_TRIGGER( LBANK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add L1 and L2 jet info onto the end of the
C-                         bank pointed to by LBANK
C-  ** WARNING: We are assuming that the trailer of this bank is supposed to
C-     be all 'floating'. Additionally, we assume that there are only
C-     the immediate supporting structural links which point to LBANK. This
C-     is because this bank will be moved and we are calling MZPUSH with the
C-     'I'(isolated) option.
C
C-   Inputs  :    [I]   LBANK : pointer to zebra bank
C-   Outputs :    [I]   LBANK : new pointer to enlarged zebra bank
C-           :          L1_L2 :
C-   Controls:
C-            Format of L1_L2:
C-
C- ** First we make room for at least 8 L1 Jet candidates **
C-      NL1 = total number of L1 candidates
C-      (For each L1 candidate, store eta,phi, ET )
C-      NL2 = total number of L2 jets with this conesize R=.7
C-      RCON= .7
C-      (For each L2 jet, store eta,phi,Et,width,emf)
C- Version 3:
C       L1 scalar Et
C       L2 scalar Et
C       L1 MPT
C       L2 MPT
C       L1 MPT phi
C       L2 MPT phi
C- **
C-

C-   Created   4-DEC-1992   Richard V. Astur
C-   Modified  4-JUL-1993   R. Astur " Add L1,L2 scalar and missing pt for
C-                                     version 3 of JUTL "
C    Modified  14-FEB-1994  R. Astur " Add Large Tiles - see ZEB " version 4
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INTEGER   ESUM_CONESIZE_TO_JET, ID, IER, FLAG, II
      REAL ETAD
      INTEGER L1ESUM, GZESUM, I,N , IETA, IPHI
      INTEGER LJAUX, GZJAUX, LBANK
      REAL ET, ETA, PHI
      REAL ETAWIDTH, PHIWIDTH, EMF, CONE_SIZE
      INTEGER IERJAUX, POINT_QCD, POINT, IPAR, POINT_SAVE
      INTEGER NFOUND(ID_ALL:LAST_TYPE)
      LOGICAL START, DO
C----------------------------------------------------------------------
C
C: We need to first decide how much extra room we will need.
C
C: Get L1 information first
      N = 0                               ! No words needed yet
      N = N + 1                           ! Space for number of L1 jets
      N = N + 1                           ! # of large tiles
      L1ESUM = GZESUM('TRG2')             ! Get TRGR ESUM bank
      IF ( L1ESUM .LE. 0 ) L1ESUM = GZESUM('TRGR') ! Take this one
      IF ( L1ESUM .LE. 0 ) THEN
        CALL ERRMSG('No ESUM','QCD_FILTER_TRIGGER',
     &    'No TRGR ESUM bank found', 'W')
      ELSE
        N = N + IQ( L1ESUM + 5 + ID_JET )*3 ! Eta,phi,Et of each
C: Add Large Tiles
        N = N + IQ( L1ESUM + 5 + ID_JET_1 )*3    ! Eta,phi,Et of each L.T.
      ENDIF

C: Make room for L2 jets
      LJAUX = GZJAUX()
      N = N + 1                               ! # of L2 jets
      N = N + 1                               ! Conesize of jets
      IF ( LJAUX .GT. 0 ) THEN
        N = N + IQ( LJAUX + 3 )*5               ! Eta,phi,ET,width,emf of each
      ELSE
        ID = ESUM_CONESIZE_TO_JET( 'FILT', .7, IER )
        IF ( IER .EQ. 0 ) THEN
          CALL GTESUM_COUNTS('FILT', NFOUND, IER)
          IF ( IER .EQ. 0 ) N = N + NFOUND(ID)*5
        ELSE
          ID = -2
        ENDIF
      ENDIF
C: Make room for L1 and L2 scalar and missing PT words
      N = N + 6
C: Get pointer to end of existing bank.
      POINT_QCD = IQ( LBANK - 1 )
C: Extend bank by N words
      CALL MZPUSH( IXCOM, LBANK, 0, N, 'I')
      POINT_QCD = POINT_QCD + LBANK
C: Now fill the bank with the words we need (all floating!!!)
C: First L1
      L1ESUM = GZESUM('TRG2')             ! Get TRGR ESUM bank
      IF ( L1ESUM .LE. 0 ) L1ESUM = GZESUM('TRGR') ! Take this one
      IF ( L1ESUM .GT. 0 ) THEN
C: L1 trigger tower candidates
C        Q( POINT_QCD + 1 ) = IQ( L1ESUM + 5 + ID_JET )
        POINT_SAVE = POINT_QCD + 1
        Q( POINT_SAVE )     = 0.0
        POINT_QCD = POINT_QCD + 1
        DO I = 1, IQ( L1ESUM+4)           ! Loop over ALL objects
          POINT = L1ESUM + IQ(L1ESUM+2) + (I-1)*IQ(L1ESUM+3)
          IF ( IQ(POINT+1) .EQ. ID_JET ) THEN    ! This is a jet object
            Q( POINT_SAVE )    = Q( POINT_SAVE ) + 1.0
            Q( POINT_QCD + 1 ) = Q(POINT+5)
            Q( POINT_QCD + 2 ) = Q(POINT+6)
            Q( POINT_QCD + 3 ) = Q(POINT+3)
            POINT_QCD = POINT_QCD + 3
          ENDIF
        ENDDO
C: L1 large tower candidates
C        Q( POINT_QCD + 1 ) = IQ( L1ESUM + 5 + ID_JET_1 )
        POINT_SAVE = POINT_QCD + 1
        Q( POINT_SAVE )   = 0.0
        POINT_QCD = POINT_QCD + 1
        DO I = 1, IQ( L1ESUM+4)           ! Loop over ALL objects
          POINT = L1ESUM + IQ(L1ESUM+2) + (I-1)*IQ(L1ESUM+3)
          IF ( IQ(POINT+1) .EQ. ID_JET_1 ) THEN    ! This is a jet object
            Q( POINT_SAVE )    = Q( POINT_SAVE ) + 1.0
            Q( POINT_QCD + 1 ) = Q(POINT+5)
            Q( POINT_QCD + 2 ) = Q(POINT+6)
            Q( POINT_QCD + 3 ) = Q(POINT+3)
            POINT_QCD = POINT_QCD + 3
          ENDIF
        ENDDO
      ELSE
        Q( POINT_QCD + 1 ) = 0        ! # of t.t.
        POINT_QCD = POINT_QCD + 1
        Q( POINT_QCD + 1 ) = 0        ! # of l.t.
        POINT_QCD = POINT_QCD + 1
      ENDIF
C: Now get L2 jets
C: R=.7
      CONE_SIZE = .7
      Q( POINT_QCD + 1 ) = 0.0        ! dont know # of l2 jets yet. Hold.
      Q( POINT_QCD + 2 ) = CONE_SIZE  ! Cone size
      POINT_QCD = POINT_QCD + 2
      N = 0
      IF ( GZJAUX() .GT. 0 ) THEN
        START = .TRUE.
c
        IPAR = 0
  200   CALL GTJAUX( START, CONE_SIZE, IPAR, ET, ETA, PHI, IETA, IPHI,
     &    ETAWIDTH, PHIWIDTH, EMF, IERJAUX )
        IF ( IERJAUX .EQ. 0 ) THEN
          N = N + 1
          Q( POINT_QCD + 1 ) = ETA
          Q( POINT_QCD + 2 ) = PHI
          Q( POINT_QCD + 3 ) = ET
          Q( POINT_QCD + 4 ) =  SQRT(ETAWIDTH**2 + PHIWIDTH**2)
     &      /(ABS(ET)+.01)
          Q( POINT_QCD + 5 ) = EMF/(ABS(ET)+.01)
          POINT_QCD = POINT_QCD + 5
          GOTO 200
        ENDIF
      ELSEIF ( ID .GE. -1 ) THEN
        DO II = 1, NFOUND(ID)
          CALL GTESUM('FILT', ID, II, ET, ETA, ETAD, PHI, FLAG, IER )
          IF ( IER .NE. 0 ) THEN
            ET = -999.
            ETA=-999.
            PHI=-999.
          ENDIF
          N = N + 1
          Q( POINT_QCD + 1 ) = ETA
          Q( POINT_QCD + 2 ) = PHI
          Q( POINT_QCD + 3 ) = ET
          Q( POINT_QCD + 4 ) =  -999.
          Q( POINT_QCD + 5 ) = -999.
          POINT_QCD = POINT_QCD + 5
        ENDDO
      ENDIF
C: Enter number of L2 jets
        Q( POINT_QCD - N*5 - 1 ) = N
C: Enter L1 Scalar ET and MPT info
        L1ESUM = GZESUM('TRGR')
        IF ( L1ESUM .GT. 0 ) THEN
          DO I = 1, IQ( L1ESUM+4)           ! Loop over ALL objects
            POINT = L1ESUM + IQ(L1ESUM+2) + (I-1)*IQ(L1ESUM+3)
            IF ( IQ(POINT+1) .EQ. ID_ETSUM ) THEN    ! SCALAR ET OBJECT
              Q( POINT_QCD + 1 ) = Q(POINT+3)
            ENDIF
            IF ( IQ(POINT+1) .EQ. ID_ETMISS ) THEN   ! MPT OBJECT
              Q( POINT_QCD + 3 ) = Q(POINT+3)
              Q( POINT_QCD + 4 ) = Q(POINT+6)
            ENDIF
          ENDDO
        ENDIF
C: Enter L2 Scalar ET and MPT info
        L1ESUM = GZESUM('FILT')
        IF ( L1ESUM .GT. 0 ) THEN
          DO I = 1, IQ( L1ESUM+4)           ! Loop over ALL objects
            POINT = L1ESUM + IQ(L1ESUM+2) + (I-1)*IQ(L1ESUM+3)
            IF ( IQ(POINT+1) .EQ. ID_ETSUM ) THEN    ! SCALAR ET OBJECT
              Q( POINT_QCD + 2 ) = Q(POINT+3)
            ENDIF
            IF ( IQ(POINT+1) .EQ. ID_ETMISS ) THEN   ! MPT OBJECT
              Q( POINT_QCD + 5 ) = Q(POINT+3)
              Q( POINT_QCD + 6 ) = Q(POINT+6)
            ENDIF
          ENDDO
        ENDIF
        POINT_QCD = POINT_QCD + 6
C: Now shrink the bank to get rid of any extra space we allocated
        I = POINT_QCD-LBANK - IQ(LBANK-1)
        IF ( I .NE. 0 ) CALL MZPUSH( IXCOM, LBANK, 0, I, 'I')
        IF ( I .GT. 0 ) CALL ERRMSG('Overwrite','QCD_FILTER_TRIGGER',
     &    'Written outside of bank?!','W')
C
  999   RETURN
        END
