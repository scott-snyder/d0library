C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_FILL_ARRAY.FOR
C *1     3-FEB-1994 14:37:21 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_FILL_ARRAY.FOR
      SUBROUTINE ktjet_fill_array
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill particle arrays.
C-          1-5-93: Instead of using arrays, now we use zebra banks. Banks
C-                : will be filled with either partons (isaq), particles
C-                : (isp1), cells (caeh) or jets from a previous KT algorithm
C-                : (jets).
C-          6-2-93: Or banks will be filled with a list of eta,phi,and et
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created                 Kate C. Frame
C-   Modified  27-JUL-1992   Richard V. Astur (combine parton and particle mode)
C-   Modified  02-JUN-1993   Kate C. Frame
C-   Modified  25-FEB-1995   R. Astur "Add KTCL usage"
c-   Updated   1-oct-1995    Fixed unpacking of ktcl to use bit backing
c-                           routines.
c-                           Gordon Watts
c-   Updated   24-oct-1995   Make sure KTCL bank gets used for input
c-                           type 6.
c-                           Use unpacking routines instead of accessing
c-                           ktcl bank directly.
c-                           Fixup i<->j mismatch in inner loop causing
c-                           ZEBRA corruption.
c-                           Gordon Watts
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$INC:KTJET.INC'              ! RCP parameters
      INCLUDE 'D0$INC:KTJET_LINK.INC'              ! RCP parameters
      REAL pp(4), phi, th, eta, et, x, y, z,etot, etot1, DELETA
      INTEGER lisaq, lisp1, lisv1, idv, id, i, pcluster(4), ifirst, I1
      INTEGER N_VEC, N_MAP, IPOINT, IETA, IPHI
      INTEGER ip, gzcaeh, gzcaep, LCAEH, LCAEP, LDUM, LDUM1, LCATD
      INTEGER OFF, N, II, ICATD, IER, LCAPH, LJETS, ISEC
      INTEGER KTJET_FIND_CAPH, J, GZCATD
      REAL E(5), THETA, ETT, KTP(4)
      LOGICAL OK
      INTEGER LKTCL
      LOGICAL NONINT_P
      REAL THETA_FROM_ETA, RETA
 
      integer word1, word2
      integer ktcl_get_input_type
      integer ktcl_get_ncluster

      integer gzktcl

      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
      THETA_FROM_ETA(RETA ) = 2*ATAN(EXP(-(RETA)))
C----------------------------------------------------------------------
C
C---Fill zebra bank KMAP with the relevant partons,particles,cells or jets
C---KMAP has pointers to the appropriate zebra banks.
C
C---Fill zebra bank KVEC with the relevant 4-vectors
C

C: Fill KMAP ( how we do this depends on INPUT_TYPE )
      N_MAP = 0       ! Start with 0 entries
      N_VEC = 0
      Q( LKMAP + 5 ) =  0.0     ! Sum of Ex of entries
      Q( LKMAP + 6 ) =  0.0     ! Sum of Ey of entries
      Q( LKMAP + 7 ) =  0.0     ! Sum of Ez of entries
      Q( LKMAP + 8 ) =  0.0     ! Sum of E  of entries
      Q( LKMAP + 9 ) =  0.0     ! Sum of ET  of entries
      KT_MAX_CHAN    =  0.0     ! Maximum channel found
C: Init cluster number and next pointer for all
      DO I = 1, IQ( LKMAP + 3 )
        IQ( CLASS_MAP( I ) ) = I
        IQ( NEXT_MAP(  I ) ) = I
        IQ( LINK_MAP(  I ) ) = I
        IQ( POINT( I )     ) = I
        IF ( I .GE. IQ( LKMAP + 3 ) - 1 ) IQ( LINK_MAP(I) ) = 0 ! Beam jets
      ENDDO
C:
C: Set ok flag to .false.
C
      OK  = .FALSE.
      IF ( input_type .EQ. 1 ) THEN         ! Run on partons
        LISAQ = 0
  201   CALL gtisaq(lisaq,lisaq,id,pp,phi,th,eta)
        IF(lisaq.GT.0)THEN
          OK  = .TRUE.
C          IF ( abs(id) .NE. 11 .AND. abs(id) .NE. 13 .AND. abs(id) .
C     &        ne. 15 .AND. abs(id) .NE. 14 ) THEN ! Skip muons/neutrinos
          IF ( .NOT. NONINT_P(ID) ) THEN
            N_MAP = N_MAP + 1
            N_VEC = N_VEC + 1
            DO I = 1, 4
              Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 5 + I) = PP(I)
              Q( LKMAP + 4 + I ) = Q( LKMAP + 4 + I ) + PP(I)
            ENDDO
            ETT = SQRT(PP(1)**2 + PP(2)**2)
            Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 10) = ETT
            Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 11) = ETA
            Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 12) = PHI
            Q( LKMAP + 9 ) = Q( LKMAP + 9 ) + ETT
            KT_MAX_CHAN = MAX( ETT, KT_MAX_CHAN )
          ENDIF
          GOTO 201
        ENDIF
      ELSE IF ( input_type .EQ. 2 ) THEN  ! Run on particles
        LISV1 = 0
  101   CALL gtisv1(lisv1,lisv1,idv,pp,x,y,z)
        IF(lisv1.GT.0)THEN
          lisp1=lisv1-izisp1

  301     CALL gtisp1(lisp1,lisp1,id,pp,phi,th,eta)
          IF(lisp1.GT.0)THEN
C            IF ( abs(id) .NE. 11 .AND. abs(id) .NE. 13 .AND. abs(id) .
C     &          ne. 15 .AND. abs(id) .NE. 14 ) THEN ! Skip muons/neutrinos
          IF ( .NOT. NONINT_P(ID) ) THEN
              OK  = .TRUE.
              N_MAP = N_MAP + 1
              N_VEC = N_VEC + 1
              DO I = 1, 4
                Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 5 + I) =
     &              PP(I)
                Q( LKMAP + 4 + I ) = Q( LKMAP + 4 + I ) + PP(I)
              ENDDO
              ETT = SQRT(PP(1)**2 + PP(2)**2)
              Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 10) = ETT
              Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 11) = ETA
              Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 12) = PHI
              Q( LKMAP + 9 ) = Q( LKMAP + 9 ) + ETT
              KT_MAX_CHAN = MAX( ETT, KT_MAX_CHAN )
            ENDIF
            GOTO 301
          ENDIF
          GOTO 101
        ENDIF
      ELSE IF ( input_type .EQ. 3 ) THEN ! Run on cells
C
C: Loop over channels
C
        lcaeh = gzcaeh()
        IF ( LCAEH .GT. 0 ) THEN
          OK = .TRUE.
          DO i = 1, iq( lcaeh + 3 )
            N_MAP = N_MAP + 1
            N_VEC = N_VEC + 1
            DO I1 = 1, 4
              E(I1) = Q(LCAEH + (I-1)*IQ(LCAEH+2) + 3 + I1 )
              Q( LKVEC + (N_VEC - 1)*IQ(LKVEC+4) + 5 + I1) = E(I1)
              Q( LKMAP + 4 + I1 ) = Q( LKMAP + 4 + I1 ) + Q( LCAEH +
     &            (I-1) * IQ(LCAEH+2) + 3 + I1 )
            ENDDO
            ETT =  Q( LCAEH + (I-1)*IQ(LCAEH+2) + 8 )
            Q( LKMAP + 9 ) = Q( LKMAP + 9 ) + ETT
            Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 10) = ETT
            CALL ETOETA(E,PHI,THETA,ETA)
            Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 11) = ETA
            Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 12) = PHI
            KT_MAX_CHAN = MAX( ETT, KT_MAX_CHAN )
          ENDDO
        ENDIF
      ELSEIF ( INPUT_TYPE .EQ. 4 ) THEN ! CATD
C        CALL QCD_GZMDST_ALL( LDUM, LCATD, LDUM1 )
        LCATD = GZCATD()
        IF ( LCATD .GT. 0 ) THEN
          OK  = .TRUE.
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
c              TYPE *, ' CATD overflow at eta-phi',ETA,PHI
              ENDIF
              E(5) = SQRT( E(1)**2 + E(2)**2 )
C            ICATD = IQ( LCATD + OFF + I)
C            CALL QCD_CATD_UNPACK( ICATD, ISEC, E, ETA, PHI, THETA, IER )
c            IF ( IER .EQ. 0 ) THEN
              N_MAP = N_MAP + 1
              N_VEC = N_VEC + 1
              DO I1 = 1, 4
                Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 5 + I1 ) = E(I1)
                Q( LKMAP + 4 + I1 ) = E(I1) + Q( LKMAP + 4 + I1)
              ENDDO
              ETT = E(5)
              Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 10) = ETT
              Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 11) = ETA
              Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 12) = PHI
              Q( LKMAP + 9 ) = Q( LKMAP + 9 ) + ETT
              KT_MAX_CHAN = MAX( ETT, KT_MAX_CHAN )
c            ENDIF
            ENDDO
          ENDDO
        ENDIF
      ELSEIF ( INPUT_TYPE .EQ. 5 ) THEN
        LCAPH = KTJET_FIND_CAPH( KTCUT( IKTCUT ) - .0001)
        OK  = .TRUE.
        LJETS = LQ( LCAPH - IZJETS )
        DO I = 1, IQ( LCAPH + 3 )
          N_MAP = N_MAP + 1
          N_VEC = N_VEC + 1
          IF ( LJETS .GT. 0 ) THEN
            DO I1 = 1, 4
              Q( LKVEC + (N_VEC - 1)*IQ(LKVEC+4) + 5 + I1) = Q( LJETS +
     &          1 + I1 )
              Q( LKMAP + 4 + I1 ) = Q( LKMAP + 4 + I1 ) +  Q( LJETS + 1
     &          + I1 )
            ENDDO
            ETT =  Q( LJETS + 1 + 5 )
            Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 10) = ETT
            Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 11) = Q( LJETS + 9 )
            Q( LKVEC + (N_VEC-1)*IQ(LKVEC+4) + 12) = Q( LJETS + 8 )
            Q( LKMAP + 9 ) = Q( LKMAP + 9 ) + ETT
            KT_MAX_CHAN = MAX( ETT, KT_MAX_CHAN )
          ELSE
            CALL ERRMSG('Missing jets','KTJTET_FILL_ARRAY',
     &          'Cant find a jets bank','W')
          ENDIF
        ENDDO
      ELSEIF ( INPUT_TYPE .EQ. 6 ) THEN       !GIAN'S STUFF
        DO J = 1, NGIAN
          N_MAP = N_MAP + 1
          N_VEC = N_VEC + 1
          CALL KTJET_4P(XGIAN(1+(J-1)*gian_block_size),
     1         XGIAN(2+(J-1)*gian_block_size),
     &         XGIAN(3+(J-1)*gian_block_size), KTP)
          DO I1 = 1, 4
            Q( LKVEC + (N_VEC - 1)*IQ(LKVEC+4) + 5 + I1) = KTP(I1)
            Q( LKMAP + 4 + I1 ) = Q( LKMAP + 4 + I1 ) +  KTP(I1)
          ENDDO
          Q( LKVEC + (N_VEC - 1)*IQ(LKVEC+4) + 11) = XGIAN(1+(J-1)
     &          *gian_block_size)
          Q( LKVEC + (N_VEC - 1)*IQ(LKVEC+4) + 12) = XGIAN(2+(J-1)
     &          *gian_block_size)
          Q( LKVEC + (N_VEC - 1)*IQ(LKVEC+4) + 10) = XGIAN(3+(J-1)
     &          *gian_block_size)
          q( lkvec + (n_vec - 1)*iq(lkvec+4) + 5 + kvec_ncell) =
     &         xgian(4+(j-1)*gian_block_size)
          q( lkvec + (n_vec - 1)*iq(lkvec+4) + 5 + kvec_em) =
     &         xgian(5+(j-1)*gian_block_size)
          q( lkvec + (n_vec - 1)*iq(lkvec+4) + 5 + kvec_icd) =
     &         xgian(6+(j-1)*gian_block_size)
          q( lkvec + (n_vec - 1)*iq(lkvec+4) + 5 + kvec_fh) =
     &         xgian(7+(j-1)*gian_block_size)
          q( lkvec + (n_vec - 1)*iq(lkvec+4) + 5 + kvec_cell_em) =
     &         xgian(8+(j-1)*gian_block_size)
          q( lkvec + (n_vec - 1)*iq(lkvec+4) + 5 + kvec_cell_icd) =
     &         xgian(9+(j-1)*gian_block_size)
          q( lkvec + (n_vec - 1)*iq(lkvec+4) + 5 + kvec_cell_fh) =
     &         xgian(10+(j-1)*gian_block_size)
          Q( LKMAP + 9 ) = Q( LKMAP + 9 ) +
     $         XGIAN(3+(J-1)*gian_block_size)
          KT_MAX_CHAN = MAX( XGIAN(3+(J-1)*gian_block_size),
     &         KT_MAX_CHAN )
        END DO
        if (ngian .gt. 0) then
           OK  = .TRUE.
        endif
      ENDIF
C
C: If we couldnt find the bank we wanted. Maybe it is in a KTCL bank
C
      IF ( .NOT. OK ) THEN
         lktcl = gzktcl ()
         DO WHILE( .NOT. OK .AND. LKTCL .GT. 0 )
            if (ktcl_get_input_type (lktcl) .eq. INPUT_TYPE) then
              OK  = .TRUE.

              N_VEC = ktcl_get_ncluster(lktcl)
              N_MAP = N_VEC
              word2 = 0
              DO J  = 1, N_VEC
                 word1 = iq(lktcl + ktcl_header_size
     $                + (j-1)*ktcl_block_size + 1)
                 word2 = iq(lktcl + ktcl_header_size
     $                + (j-1)*ktcl_block_size + 2)
                 CALL UNPKTCL( word1, word2, Q(KTETA(J)), Q(KTPHI(J)),
     &                Q(P0(J)),
     $                q(kt_em_frac(j)), q(kt_icd_frac(j)),
     $                q(kt_fh_frac(j)),
     $                q(kt_cell_em(j)), q(kt_cell_icd(j)),
     $                q(kt_cell_fh(j)), q(kt_tot_cells(j)))
                 Q(PX(J))  = Q(P0(J))*COS(Q(KTPHI(J)))/COSH(Q(KTETA(J)))
                 Q(PY(J))  = Q(P0(J))*SIN(Q(KTPHI(J)))/COSH(Q(KTETA(J)))
                 Q(PZ(J))  = Q(P0(J))*COS(THETA_FROM_ETA(Q(KTETA(J))))
                 Q(KTET(J))= Q(P0(J))/COSH(Q(KTETA(J)))
                 q(kt_tot_cells(j)) = q(kt_tot_cells(j))
     $                + q(kt_cell_em(j))
     $                + q(kt_cell_icd(j))
     $                + q(kt_cell_fh(j))
              ENDDO
           ENDIF
           LKTCL = LQ(LKTCL)
        ENDDO
      ENDIF
C
C
C Creates beam jets if they are not included in the particle/parton list
C
  555 N_VEC = N_VEC + 2
      N_MAP = N_MAP + 2

      Q( PZ( N_VEC - 1 ) )=  MAX( (COM_E - Q(LKMAP + 8 ) )/2., 1.)
      Q( P0( N_VEC - 1 ) )=  MAX( (COM_E - Q(LKMAP + 8 ) )/2., 1.)
      Q(KTETA(N_VEC- 1 ) )=  10.0
      Q( PZ( N_VEC     ) )= -MAX( (COM_E - Q(LKMAP + 8 ) )/2., 1.)
      Q( P0( N_VEC     ) )=  MAX( (COM_E - Q(LKMAP + 8 ) )/2., 1.)
      Q(KTETA(N_VEC    ) )= -10.0
C
C: The beam jets dont change Ex,Ey or Ez, but change E
C
      Q( LKMAP + 8 ) = Q( LKMAP + 8 ) + Q( P0( N_VEC - 1 ) )
      Q( LKMAP + 8 ) = Q( LKMAP + 8 ) + Q( P0( N_VEC     ) )

C
C: Check value of N_VEC to what we allotted space for
C: Allow for particles to be less than what we allotted
C
      IF ( N_VEC .LT. (IQ(LKVEC-1)-4)
     &    /IQ(LKVEC+4) ) THEN
        IF ( ABS(N_VEC-IQ(LKVEC+3)) .GT. 4 ) CALL ERRMSG('lost some', 
     &    'KTJET_FILL_ARRAY','Lost more than 4 partons/particles','W')
        IQ( LKVEC+3 ) = N_VEC
        IQ( LKMAP+3 ) = N_VEC
      ELSEIF ( N_VEC .NE. (IQ(LKVEC-1) - 4 )/IQ(LKVEC+4) ) THEN
        CALL ERRMSG('# of objects doesnt match','KTJET_FILL_ARRAY',
     &      ' Possible zebra overwrite-abort', 'E')
      ENDIF

  999 RETURN
      END
