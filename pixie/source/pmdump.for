      SUBROUTINE PMDUMP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine to dump muon banks from
C-   PIXIE package MUODIS.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  12-APR-1991   Silvia T. Repond, Harrison B. Prosper
C-   Updated   7-JAN-1992   Lupe Howell  Modify call to PX_DISPLAY_ITEMS
C-   Updated   2-NOV-1992   Lupe Howell  Add call to DISPLAY_ITEMS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LUN,ERR,ID
      INTEGER JDUM,EVONUM,RUNNO,RUN,EVENT
      LOGICAL OK,EVE
      INTEGER NOPTION
      PARAMETER( NOPTION = 4 )
      CHARACTER*16 OPTION(NOPTION)
      CHARACTER*40 OPTION_REM(NOPTION)
      CHARACTER*64 FILENAME
      CHARACTER*1 CDUM
C----------------------------------------------------------------------
      DATA JDUM         /0/
      DATA CDUM         /'.'/
      DATA OPTION       /'MUD1','MUOH','MUPRT','Call EVE'/
      DATA OPTION_REM   /' Dump MUD1', ' Dump MUOH',
     &                   ' Dump MUPRT',' Call editor EVE'/
C----------------------------------------------------------------------
C
C ****  Get unit number
C
      CALL GTUNIT(2,LUN,ERR)
      IF ( ERR .NE. 0 ) THEN
        CALL ERRMSG('NOUNIT','PMDUMP',
     &      'Cannot Get UNIT number','W')
        GOTO 999
      ENDIF
C
C ****  Create file-name
C
      RUN   = RUNNO()
      EVENT = EVONUM()
      FILENAME = ' '
      WRITE(FILENAME,'(''MUO_'',I8.8,''_'',I8.8,''.DMP'')') RUN, EVENT
C
C ****  Open file
C
      CALL D0OPEN(LUN,FILENAME,'OF',OK)
      IF ( .NOT. OK ) THEN
        CALL ERRMSG('NOTOPEN','PMDUMP',
     &      'Unable to open dump file '//FILENAME,'W')
        GOTO 999
      ENDIF
C
C ****  Dump muon banks
C
      CALL PRHEAD(LUN,0,2,'EVENT',0)    ! Dump HEAD bank
C
      EVE = .FALSE.
      ID = 1
      DO WHILE ( ID .GT. 0 )
        CALL DISPLAY_ITEMS
     &    (NOPTION,OPTION,OPTION_REM,'DUMP MUON BANKS',ID)
        IF ( ID .GT. 0 ) THEN
          IF ( ID .LE. 3 ) THEN
            CALL STAMSG
     &        (' '//OPTION(ID)(1:5)//' to file '//FILENAME,.FALSE.)
          ENDIF
          IF     ( OPTION(ID) .EQ. 'MUD1' ) THEN
            CALL PRMUD1(LUN,JDUM,JDUM,CDUM,JDUM)
          ELSEIF ( OPTION(ID) .EQ. 'MUOH' ) THEN
            CALL PRMUOH(LUN,JDUM,JDUM,CDUM,JDUM)
          ELSEIF ( OPTION(ID) .EQ. 'MUPRT') THEN
            CALL MUPRT(LUN)
          ELSEIF ( OPTION(ID) .EQ. 'Call EVE') THEN
            EVE = .TRUE.
            ID  = 0
          ENDIF
        ENDIF
      ENDDO
      CLOSE(LUN)
      CALL RLUNIT(2,LUN,ERR)
      CALL HDSRES
C
C ****  Look at file using EVE
C
      IF ( EVE ) THEN
        CALL STAMSG(' Invoking EVE editor...',.FALSE.)
        CALL DMPSCR(FILENAME)
      ENDIF
  999 RETURN
      END
