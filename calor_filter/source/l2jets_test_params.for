      SUBROUTINE L2JETS_TEST_PARAMS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 1) Find number of algorithm independent
C-                            JETS parameter sets
C-                         2) Make sure it doesnt exceed our maximum
C-                         3) Define a map of all parameter sets to ONE
C-                            Independent parameter set.
C-
C-   Inputs  :
C-   Outputs : array IND_PARAM defines a ind. par set for each par set.
C-   Controls: INIT_OK is set false if we have problems
C-
C-   Created   1-JUN-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$INC:L2JETS_CONT.INC'  ! control common
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
      INCLUDE 'D0$INC:L2JETS_PAR.INC'   ! filled parameter set common
      INTEGER NPAR,NIPAR
C**Parameter specific:
      INTEGER ICON_ENG_HOLD( NPAR_IND_MAX)
      INTEGER ICON_CEN_HOLD( NPAR_IND_MAX)
C----------------------------------------------------------------------
C
C---This routine must change as parameter sets change. Currently the
C---only algorithm dependent parameters are: ICON_CEN and ICON_ENG.
C
      NUM_IND_PARAMS = 1                ! There is at least one ind. set
      ICON_ENG_HOLD( 1 ) = ICON_ENG( 1 )
      ICON_CEN_HOLD( 1 ) = ICON_CEN( 1 )
      IND_PARAM( 1 )     =  1           ! First set maps to first ind.
C                                       ! set

      DO NPAR = 2 , NUM_PARAMS          ! cycle over all param sets

        DO NIPAR = 1 , NUM_IND_PARAMS   ! cycle over ind. param sets
          IF ( ICON_ENG(NPAR) .EQ. ICON_ENG_HOLD(NIPAR)
     &                 .AND.
     &         ICON_CEN(NPAR) .EQ. ICON_CEN_HOLD(NIPAR) )  GOTO 200
        END DO
C
C---If we got here, we did not find an independent set that matched this
C---parameter set. So it is algorithm independent as well.
C
        NUM_IND_PARAMS = NUM_IND_PARAMS + 1
        IF (NUM_IND_PARAMS .GT. NPAR_IND_MAX) THEN
          NOWERRMESS = ' Too many independent parameter sets '
          NOWSMESS   = 'IndParam overflo'
          NOWERR     = ERR_IND_PARAM_OVERFLOW
          GOTO 900
        END IF
        ICON_CEN_HOLD( NUM_IND_PARAMS) = ICON_CEN( NPAR )
        ICON_ENG_HOLD( NUM_IND_PARAMS) = ICON_ENG( NPAR )
        NIPAR = NUM_IND_PARAMS     ! set this for below
  200   CONTINUE
        IND_PARAM( NPAR ) = NIPAR
      END DO

      GOTO 999                          ! No error if reached here
  900 CONTINUE                          ! ERROR
      INIT_OK = .FALSE.
  999 RETURN
      END
