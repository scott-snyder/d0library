      FUNCTION NONINT_P ( ID )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines whether the particle denoted by (ISAJET)
C-   ID is non-interacting, i.e. would escape the calorimeter.
C-
C-   Returned value  : .TRUE. if non-interacting, .FALSE. otherwise
C-   Inputs  : ID       - the ISAJET id of the particle in question
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-SEP-1990   Marc Paterno
C-   Updated  29-NOV-1991   Marc Paterno  Add MUONs to the noninteracting
C-   particle list.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL NONINT_P
      INTEGER  ID, ABSID
C----------------------------------------------------------------------
      ABSID = ABS(ID)
      IF ( ABSID .EQ. 11 ) THEN             ! electron neutrino
        NONINT_P = .TRUE.
      ELSE IF ( ABSID .EQ. 13 ) THEN        ! muon neutrino
        NONINT_P = .TRUE.
      ELSE IF ( ABSID .EQ. 14 ) THEN        ! muon
        NONINT_P = .TRUE.
      ELSE IF ( ABSID .EQ. 15 ) THEN        ! tau neutrino
        NONINT_P = .TRUE.
      ELSE IF ( ABSID .EQ. 31 ) THEN        ! electron sneutrino
        NONINT_P = .TRUE.
      ELSE IF ( ABSID .EQ. 33 ) THEN        ! muon sneutrino
        NONINT_P = .TRUE.
      ELSE IF ( ABSID .EQ. 35 ) THEN        ! tau sneutrino
        NONINT_P = .TRUE.
      ELSE IF ( ABSID .EQ. 30 ) THEN        ! Z1SS
        NONINT_P = .TRUE.
      ELSE                                      ! INTERACTING PARTICLE
        NONINT_P = .FALSE.
      ENDIF

      RETURN
      END
