      SUBROUTINE PX_SET_PATH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Show a menu of available paths.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Modified 15-APR-1993   Nobuaki Oshima - Added 'MDST' Path.
C-   Created  15-NOV-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*4  PATH
      CHARACTER*40 COMMAND
      LOGICAL FLGVAL
      INTEGER GZRECO,GZGEAN,GZFILT,GZMDST, LBANK
C----------------------------------------------------------------------
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
C
C ****  Get command either from FIFO or from menu
C
        IF ( FLGVAL('USE_COMMAND_FIFO') ) THEN
          CALL PX_GET_NEXT_COMMAND(COMMAND)
        ELSE
          CALL MENUDO('Set Path','SETPATH',COMMAND)
        ENDIF
C
        IF ( COMMAND .NE. 'EXIT' ) THEN
          IF     ( COMMAND .EQ. 'SHOW' ) THEN
            CALL PATHGT(PATH)
          ELSEIF ( COMMAND .EQ. 'DEFAULT' ) THEN
            CALL PATHRS                   ! Reset to default path
            CALL PATHGT(PATH)
          ELSE
            PATH = COMMAND(1:4)
            CALL PATHST(PATH)
          ENDIF
C
C ****  Check if PATH exists
C
          IF     ( PATH .EQ. 'RECO' ) THEN
            LBANK = GZRECO()
          ELSEIF ( PATH .EQ. 'GEAN' ) THEN
            LBANK = GZGEAN()
          ELSEIF ( PATH .EQ. 'FILT' ) THEN
            LBANK = GZFILT()
          ELSEIF ( PATH .EQ. 'MDST' ) THEN
            LBANK = GZMDST()
          ELSE
            LBANK = 0
          ENDIF
C
C ****  Display path if present
C
          IF ( LBANK .GT. 0 ) THEN
            CALL STAMSG(' Path set to '//PATH,.TRUE.)
          ELSE
            CALL STAMSG
     &      (' ** WARNING ** '//PATH//' path NOT present',.TRUE.)
            CALL PATHRS                   ! Set to default
          ENDIF
        ENDIF
      ENDDO
  999 RETURN
      END
