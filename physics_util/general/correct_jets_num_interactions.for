      FUNCTION CORRECT_JETS_NUM_INTERACTIONS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the best estimate of the number of
C-                         interactions which were present on this event.
C-                         This number is always at least one.
C-
C-   Returned value  : [R] CORRECT_JETS_NUM_INTERACTIONS
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   8-MAY-1995   Richard V. Astur
C-   Updated  28-SEP-1995   Bob Hirosky  Add ENTRY correct_jets_user_mitool
C-                                        for use in correcting ntuple data
C-   Updated   3-OCT-1995   Bob Hirosky   Add ENTRY correct_jets_user_vaxtime
C-                                        for use w/ ntuple or old MDST data
C-                                        user entries supersede rcp switch
C-   Updated  19-OCT-1995   Dhiman Chakraborty
C-                                        Add () at the end of first line
C-                                        for IBM compatibility
C-   Updated   4-NOV-1996   Freedy Nang   introduce getlum_input for alpha and
C-                                        vax compatibility.
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL     correct_jets_num_interactions
      REAL     correct_jets_user_mitool, correct_jets_user_vaxtime
      EXTERNAL multiple_interaction_tool, gzreco
      INTEGER  multiple_interaction_tool, gzreco
      INTEGER  mitool, inmitool, user_mitool
      INTEGER  ier1, iertot, lumerr
      INTEGER user_vaxtime(2), invaxtime(2)
      INTEGER getlum_input(2)
      REAL    lum(2), age(2), ilum
      LOGICAL first, use_lum, rcp_use_lum
      LOGICAL use_user_mitool, use_user_vaxtime
      REAL    numperlum
      SAVE    numperlum
      SAVE    first, use_lum
      DATA    first /.true./
      DATA    use_lum / .false./
      DATA    use_user_mitool, use_user_vaxtime /.false.,.false./
C----------------------------------------------------------------------
      correct_jets_num_interactions  = 1.0       ! Default
      getlum_input(1)=0
      getlum_input(2)=0
c
c: Initialize
c
      IF ( first ) THEN
        first = .false.
        CALL inrcp('QCD_JET_CORRECTION_RCP', ier1 )
        IF ( ier1 .NE. 0 ) THEN
          CALL errmsg('RCP error','CORRECT_JETS_NUM_INTERACTIONS',
     &        'Cant read in RCP file','W')
        ELSE
          CALL ezpick('QCD_JET_CORRECTION_RCP')
          CALL ezerr( ier1 )
          IF ( ier1 .NE. 0 ) THEN
            CALL errmsg('RCP error','CORRECT_JETS_NUM_INTERACTIONS',
     &        'Cant find bank ','W')
          ELSE
            iertot = 0
            CALL ezget('USE_LUM',use_lum, ier1 )
            iertot = iertot + abs(ier1)
            CALL ezget('NUM_PER_LUM',numperlum, ier1 )
            iertot = iertot + abs(ier1)
            CALL ezrset
            IF ( iertot .NE. 0 ) THEN
              CALL errmsg('RCP error','CORRECT_JETS_NUM_INTERACTIONS',
     &          'Read error:abort ','W')
            ENDIF
          ENDIF
        ENDIF
        IF ( use_lum ) THEN
          CALL errmsg('Using GETLUM','CORRECT_JETS_NUM_INTERACTIONS',
     &      ' Using Lum dependence', 'I' )
        ELSE
          CALL errmsg('NOT Using GETLUM',
     &      'CORRECT_JETS_NUM_INTERACTIONS',
     &      ' NOT Using Lum dependence', 'I' )
        ENDIF
      ENDIF

C
C ****  user vaxtime or mitool inputs via entry supersede rcp flags
C
      RCP_USE_LUM = USE_LUM
      IF (use_user_mitool) THEN
        USE_LUM = .FALSE.
      ENDIF
C
C: Luminosity dependent
C
      IF ( USE_LUM ) THEN
        IF (use_user_vaxtime) THEN
          CALL getlum( user_vaxtime, lum, age, lumerr )
          use_user_vaxtime = .false.
        ELSE
          CALL getlum( getlum_input, lum, age, lumerr )
        ENDIF
        IF ( lumerr .NE. 0 ) THEN
          CALL errmsg('getlum error','CORRECT_JETS_NUM_INTERACTIONS',
     &      'Assume 1 interaction','W')
          lum(1)  = 0.
          lum(2)  = 0.
          age(2)  = 1.0
          age(1)  = 0.0
        ENDIF

C
C: Time interpolated instantaneous luminosity
C
        ilum =   age(1)*(lum(2)-lum(1))/(age(2)+age(1)) + lum(1)

        correct_jets_num_interactions = ilum*numperlum + 1.0
C
C: Use MI tool.
C
      ELSE
        IF (use_user_mitool) THEN
          mitool = user_mitool
          use_user_mitool = .FALSE.
        ELSE
          mitool  =  0
          IF ( gzreco() .GT. 0 ) mitool  =  multiple_interaction_tool()
        ENDIF

        IF ( mitool .EQ. 3 .OR. mitool .EQ. 4 )
     &    correct_jets_num_interactions = 2.0

      ENDIF
C
      USE_LUM = RCP_USE_LUM
C
  999 RETURN

C
C ****  entry to enter mitool value for ntuple processing
C
      ENTRY correct_jets_user_mitool( inmitool )
      user_mitool = inmitool
      use_user_mitool = .true.
      correct_jets_user_mitool = 1.0
      RETURN
C
C ****  entry to enter mitool value for ntuple processing
C
      ENTRY correct_jets_user_vaxtime( invaxtime )
      user_vaxtime(1) = invaxtime(1)
      user_vaxtime(2) = invaxtime(2)
      use_user_vaxtime = .true.
      correct_jets_user_vaxtime = 1.0
      RETURN
      END
