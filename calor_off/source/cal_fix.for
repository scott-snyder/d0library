      FUNCTION CAL_FIX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control calorimeter packages in D0FIX
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-AUG-1995   Richard V. Astur
C-   Updated  22-NOV-1995   Rajendran Raja  ADDED CALL TO RESCALE_EM 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPNUT.LINK'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
      INCLUDE 'D0$INC:L2LINK.INC'           ! Add zebra link common
      LOGICAL CAL_FIX, CAL_FIX_INI, CAL_FIX_FIN, CAL_FIX_DROP_DST
      LOGICAL CAL_FIX_SETUP
      LOGICAL CAHITS, CAJETS, CAJNEP_EVT, CHTINI, CJTINI, CAJNEP_INI
      EXTERNAL CAHITS, CAJETS, CAJNEP_EVT, CHTINI, CJTINI, CAJNEP_INI
      LOGICAL CHTRUN, CHTFIN, CJTFIN, CAJNEP_FIN, CAJETS_DROP_DST
      EXTERNAL CHTRUN, CHTFIN, CJTFIN, CAJNEP_FIN, CAJETS_DROP_DST
      INTEGER CAL_FIX_LEVEL, IER, LRCP
      INTEGER LLINK, LLINK_NEXT, GZCATE, GZCAEH, GZPROC, GZPARH
C
      EXTERNAL GZCATE, GZCAEH, GZPROC, GZPARH
      EQUIVALENCE( L2LINK(5), LLINK )
      EQUIVALENCE( L2LINK(6), LLINK_NEXT)
C
      LOGICAL DO_PHI_CORRECTIONS
      SAVE DO_PHI_CORRECTIONS
      LOGICAL DO_RESCALE_EM
      SAVE DO_RESCALE_EM
C
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET('DO_PHI_CORRECTIONS',DO_PHI_CORRECTIONS,IER)
        CALL EZGET('DO_RESCALE_EM',DO_RESCALE_EM,IER)
        CALL EZRSET
      ENDIF
C
C: ZEBRA link common
C
      CALL MZLINT(IXCOM,'/L2LINK/',DUM,L2LINK(NLNK),DUM)
C
C: How much processing are we doing? 0=none
C
      CALL EZPICK('CAL_FIX_RCP')
      CALL EZGET('CAL_FIX_LEVEL',CAL_FIX_LEVEL,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('CAL_FIX_LEVEL?','CAL_FIX',
     &    'RCP missing parameters ', 'F')
        CAL_FIX = .FALSE.
        GOTO 900
      ENDIF
      CALL EZRSET


C
C: Do processing- drop banks we dont need
C
      CAL_FIX = .TRUE.
      IF ( CAL_FIX_LEVEL .GE. 1 .AND. CAL_FIX ) THEN
        LLINK = GZCAEH()            ! not usually there
        IF ( LLINK .GT. 0 ) CALL MZDROP(IXCOM, LLINK,' ' )
        LLINK = GZCATE()            ! not usually there
        IF ( LLINK .GT. 0 ) CALL MZDROP(IXCOM, LLINK,' ' )
        LLINK = GZPARH()            ! not usually there
        IF ( LLINK .GT. 0 ) THEN
          LLINK = LQ(LLINK-IZPNUT)
          IF ( LLINK .GT. 0 ) CALL MZDROP(IXCOM, LLINK,'L' )
        ENDIF
        CAL_FIX = CAHITS()
C
        IF ( DO_RESCALE_EM.AND.DO_PHI_CORRECTIONS ) THEN
          CALL RESCALE_EM_OBJECTS
        ENDIF
C
      ENDIF
      IF ( CAL_FIX_LEVEL .GE. 2 .AND. CAL_FIX ) THEN
        LLINK = GZPROC()
        IF ( LLINK .GT. 0 ) THEN
          LLINK = LQ( LLINK - IZCAPH )
          DO WHILE (LLINK .GT. 0 )
            LLINK_NEXT = LQ(LLINK)
            IF (IQ(LLINK+4) .GT. 1 ) THEN
              CALL MZDROP(IXCOM,LLINK,' ')
            ENDIF
            LLINK      = LLINK_NEXT
          ENDDO
        ENDIF
        CAL_FIX = CAJETS()
      ENDIF
      IF ( CAL_FIX_LEVEL .GE. 3 .AND. CAL_FIX ) THEN
        CAL_FIX = CAJNEP_EVT()
      ENDIF
  900 DUM(1) = 0
      RETURN

C--------------------------------------------------------------------------
      ENTRY CAL_FIX_INI
C
C: Read in RCP file
C
      CALL EZLOC('CAL_FIX_RCP', LRCP )
      IF ( LRCP .LE. 0 ) THEN
        CALL INRCP('CAL_FIX_RCP', IER )
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('RCP READ FAIL','CAL_FIX_INI','Cant read RCP', 'F'
     &      )
          CAL_FIX_INI = .FALSE.
          RETURN
        ENDIF
      ENDIF
C
C: How much processing are we doing? 0=none
C
      CALL EZPICK('CAL_FIX_RCP')
      CALL EZGET('CAL_FIX_LEVEL',CAL_FIX_LEVEL,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('CAL_FIX_LEVEL?','CAL_FIX_INI',
     &      'RCP missing parameters ', 'F')
        CAL_FIX_INI = .FALSE.
        RETURN
      ENDIF
      CALL EZRSET

      CAL_FIX_INI = .TRUE.
      IF ( CAL_FIX_LEVEL .GE. 1 .AND. CAL_FIX_INI ) THEN
        CAL_FIX_INI = CHTINI()
      ENDIF
      IF ( CAL_FIX_LEVEL .GE. 2 .AND. CAL_FIX_INI ) THEN
        CAL_FIX_INI = CJTINI()
      ENDIF
      IF ( CAL_FIX_LEVEL .GE. 3 .AND. CAL_FIX_INI ) THEN
        CAL_FIX_INI = CAJNEP_INI()
      ENDIF

      RETURN
C---------------------------------------------------------------------------
      ENTRY CAL_FIX_SETUP
      CAL_FIX_SETUP = .TRUE.


C
C: How much processing are we doing? 0=none
C
      CALL EZPICK('CAL_FIX_RCP')
      CALL EZGET('CAL_FIX_LEVEL',CAL_FIX_LEVEL,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('CAL_FIX_LEVEL?','CAL_FIX_SETUP',
     &      'RCP missing parameters ', 'F')
        CAL_FIX_SETUP = .FALSE.
        RETURN
      ENDIF
      CALL EZRSET

      IF ( CAL_FIX_LEVEL .GE. 1 .AND. CAL_FIX_SETUP ) THEN
        CAL_FIX_SETUP = CHTRUN()
      ENDIF
      IF ( CAL_FIX_LEVEL .GE. 2 .AND. CAL_FIX_SETUP ) THEN
        CONTINUE
      ENDIF
      IF ( CAL_FIX_LEVEL .GE. 3 .AND. CAL_FIX_SETUP ) THEN
        CONTINUE
      ENDIF
      RETURN
C--------------------------------------------------------------------------
      ENTRY CAL_FIX_FIN
      CAL_FIX_FIN = .TRUE.


C
C: How much processing are we doing? 0=none
C
      CALL EZPICK('CAL_FIX_RCP')
      CALL EZGET('CAL_FIX_LEVEL',CAL_FIX_LEVEL,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('CAL_FIX_LEVEL?','CAL_FIX_FIN',
     &      'RCP missing parameters ', 'F')
        CAL_FIX_FIN = .FALSE.
        RETURN
      ENDIF
      CALL EZRSET

      IF ( CAL_FIX_LEVEL .GE. 1 .AND. CAL_FIX_FIN ) THEN
        CAL_FIX_FIN = CHTFIN()
      ENDIF
      IF ( CAL_FIX_LEVEL .GE. 2 .AND. CAL_FIX_FIN ) THEN
        CAL_FIX_FIN = CJTFIN()
      ENDIF
      IF ( CAL_FIX_LEVEL .GE. 3 .AND. CAL_FIX_FIN ) THEN
        CAL_FIX_FIN = CAJNEP_FIN()
      ENDIF

      RETURN
C------------------------------------------------------------------------
      ENTRY CAL_FIX_DROP_DST
      CAL_FIX_DROP_DST = .TRUE.


C
C: How much processing are we doing? 0=none
C
      CALL EZPICK('CAL_FIX_RCP')
      CALL EZGET('CAL_FIX_LEVEL',CAL_FIX_LEVEL,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('CAL_FIX_LEVEL?','CAL_FIX_DROP_DST',
     &      'RCP missing parameters ', 'F')
        CAL_FIX_DROP_DST = .FALSE.
        RETURN
      ENDIF
      CALL EZRSET

      IF ( CAL_FIX_LEVEL .GE. 1 .AND. CAL_FIX_DROP_DST ) THEN
        CAL_FIX_DROP_DST = CAJETS_DROP_DST()
      ENDIF
      IF ( CAL_FIX_LEVEL .GE. 2 .AND. CAL_FIX_DROP_DST ) THEN
        CONTINUE
      ENDIF
      IF ( CAL_FIX_LEVEL .GE. 3 .AND. CAL_FIX_DROP_DST ) THEN
        CONTINUE
      ENDIF
      RETURN

      END
