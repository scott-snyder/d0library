C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_ANAL.FOR
C *1     3-FEB-1994 14:33:59 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_ANAL.FOR
      SUBROUTINE ktjet_anal
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyze KT jets
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-MAR-1993   Richard V. Astur
c-   Updated  07-Nov-1995   Use errmsg instead of type *
c-                          Gordon Watts
c-   Updated  13-Nov-1995   Use "i6" in formats instead of just plain "i"
c-                          Gordon Watts
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      INTEGER gzcaph, lcaph, ier, nj,j, id, ljets, lid, n_cut(35),
     &  n_max_chan(35), IKCF, n_20, JKCF, I
      INTEGER ICAPH, III, NNJETS, LLID
      REAL template(8), csize(3), shat, RCON, RKCF, LEAD_ET
      LOGICAL first
      DATA first /.true./
      DATA csize/ .3, .5, .7 /
      CHARACTER*4 path1
     
      character*80 line

C----------------------------------------------------------------------
C
C: Histrogram
C
      IF ( first ) THEN
        first = .false.
        CALL HCDIR('//PAWC',' ')
        CALL HMDIR('KTJET','S')
        CALL HBOOK2( 501,
     &    'AVE. # OF JETS VS. FRACTION OF LEADING JET ET',
     &    35, .01, .71, 16, .0025, .0825, 0.)
        CALL HBOOK2( 502,
     &    'AVE. # OF JETS VS. FRACTION OF LEADING JET ET',
     &    35, .01, .71, 16, .0025, .0825, 0.)
        CALL HBOOK2( 503,
     &    'AVE. # OF JETS VS. FRACTION OF LEADING JET ET',
     &    35, .01, .71, 16, .0025, .0825, 0.)
        CALL hbook2( 701, 'KTJET- N vs Ycut', 16, .0025, .0825, 21, -.5,
     &    20.5, 0. )
        CALL hbook2( 702, 'KTJET- N vs Ycut', 16, .0025, .0825, 21, -.5,
     &    20.5, 0. )
        CALL hbook2( 703, 'KTJET- N vs Ycut', 16, .0025, .0825, 21, -.5,
     &    20.5, 0. )
        DO I=1,3
          DO J=1,9
            CALL HBOOK2( 700 + I*10 + J, 'FRACTION ET CUT VS. YCUT', 35,
     &        .01, .71, 16, .0025, .0825, 0.)
          ENDDO
        ENDDO
C
c        CALL hbook2( 801, 'CONE- N vs R', 3, .2, .8, 21, -.5, 20.5, 0.)
c        CALL hbook2( 802, 'CONE- N vs R', 3, .2, .8, 21, -.5, 20.5, 0.)
c        CALL hbook2( 803, 'CONE- N vs R', 3, .2, .8, 21, -.5, 20.5, 0.)
c        CALL hbook2( 901, '10 GEV CUTOFF - PARTONS', 16, .0025, .0825,
c     &    21,-.5,20.5, 0. )
c        CALL hbook2( 902, '10 GEV CUTOFF - PARTICLES', 16, .0025, .0825,
c     &    21, -.5, 20.5, 0. )
c        CALL hbook2( 903, '10 GEV CUTOFF - CELLS', 16, .0025, .0825, 21,
c     &    -.5, 20.5, 0. )
      ENDIF
      CALL HCDIR('//PAWC/KTJET',' ')
C
C: Do KTJETS
C
      CALL pathgt(path1)                    ! Save path
      CALL pathst('RECO')                   ! change to reco
      template(1) = 0.
C      CALL set_caph('NN_JET', template, ier )
C      IF ( ier .NE. 0 ) THEN
C        lcaph = 0
C        CALL pathst(path1)
C        GOTO 999
C      ELSE
C        lcaph = gzcaph()
C        CALL reset_caph                       ! Reset paths
      CALL pathst(path1)
C      ENDIF
C
C:  Make Et cut on jets
C
C      id= 1
      ICAPH = 1
      LCAPH = GZCAPH()
      DO WHILE ( lcaph .GT. 0 )
        IF ( iq(lcaph+ 4) .eq. 6  .and. int( q(lcaph+7)) .ge. 1 .and.
     &    int(q(lcaph+7)) .le. 3 ) then
          NNJETS = IQ( LCAPH+3)
          RCON  = Q( LCAPH+ 6)
          lead_et = Q( LCAPH + 8 )
          lid = q( lcaph + 7 )
          CALL hfill(700+lid, RCON, float( NNJETS),
     &      1. )
          DO IKCF = 1,35
            n_max_chan(ikcf) = 0
          ENDDO
          ljets = lq( lcaph - izjets )
          DO WHILE ( ljets .GT. 0 )
            DO IKCF=1,35
              RKCF = IKCF*.02
              IF( Q(LJETS+6) .GT.lead_et*.02*ikcf)THEN
                CALL HFILL(500 +LID, RKCF, RCON,1.)
                n_max_chan(ikcf) = n_max_chan(ikcf) +1
              ENDIF
            ENDDO
            ljets = lq( ljets )
          ENDDO
          DO IKCF=1,35
            LLID = LID*10
            RKCF = IKCF*.02
            IF(N_MAX_CHAN(IKCF).EQ.0) then
               write (line, '(a,i10,a,i10,a)') 'EVENT # =',IQ(LHEAD+9),
     &        ' INPUT_TYPE =', LID,' THERE ARE NO JETS ABOVE THE CUTOFF'
               call errmsg ('no-jets', 'ktjet_anal', line, 'w')
            endif
            IF(N_MAX_CHAN(IKCF).EQ.1)CALL HFILL(701 + LLID, RKCF,
     &        RCON,1.)
            IF(N_MAX_CHAN(IKCF).EQ.2)CALL HFILL(702 + LLID, RKCF,
     &        RCON,1.)
            IF(N_MAX_CHAN(IKCF).EQ.3)CALL HFILL(703 + LLID, RKCF,
     &        RCON,1.)
            IF(N_MAX_CHAN(IKCF).EQ.4)CALL HFILL(704 + LLID, RKCF,
     &        RCON,1.)
            IF(N_MAX_CHAN(IKCF).EQ.5)CALL HFILL(705 + LLID, RKCF,
     &        RCON,1.)
            IF(N_MAX_CHAN(IKCF).EQ.6)CALL HFILL(706 + LLID, RKCF,
     &        RCON,1.)
            IF(N_MAX_CHAN(IKCF).EQ.7)CALL HFILL(707 + LLID, RKCF,
     &        RCON,1.)                             
            IF(N_MAX_CHAN(IKCF).EQ.8)CALL HFILL(708 + LLID, RKCF,
     &        RCON,1.)
            IF(N_MAX_CHAN(IKCF).EQ.9)CALL HFILL(709 + LLID, RKCF,
     &        RCON,1.)
          ENDDO
        ENDIF
        ICAPH = ICAPH + 1
        LCAPH = GZCAPH()
        DO III = 1,ICAPH-1
          LCAPH = LQ(LCAPH)
        ENDDO
      ENDDO

C
C: Now do cone jets
C
c      DO j = 1, 3
c        template(1) = 1.
c        template(2) = 6.
c        template(3) = csize(j)
c        CALL set_caph('CONE_JET', template, ier )
c        IF ( ier .EQ. 0 ) THEN
c          CALL gtjets_total( nj, ier )
c          IF ( ier .EQ. 0 ) CALL hfill(801, csize(j), float(nj), 1.)
c          CALL reset_caph
c        ENDIF
c      ENDDO
  999  CALL HCDIR('//PAWC/KTJET',' ')

       RETURN
      END
