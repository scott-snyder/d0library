      SUBROUTINE PXMAIN( NOMORE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Handle the D0 general event display
C-
C-   Inputs  :
C-   Outputs : NOMORE [L] : .TRUE. if we haven't asked for another event.
C-                          DI3000 is will be terminated in this case.
C-                          
C-   Created  DD-MMM-198X   ???
C-   Updated  12-SEP-1990   Harrison B. Prosper   
C-      Complete re-write
C-   Updated  15-NOV-1990   Harrison B. Prosper   
C-      Added path reset
C-   Updated  21-NOV-1991   Lupe Howell, Harrison B. Prosper
C-      Cancel interrupt at the beining and at the end if active   
C-   Updated  15-APR-1993   Nobuaki Oshima
C-      Remove PATH reset call. It will stay until reset by a user.
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL NOMORE, FLGVAL, OK,INTER_ON,INTAST
      CHARACTER*4  PATH
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------

C
C ****  Check if an interrupt menu is set.
C ****  If it is set cancel it before entering PIXIE
C
      INTER_ON = INTAST()
      IF ( INTER_ON ) THEN
        CALL CANMEN
      ENDIF
C
C ****  Initialize PIXIE 
C
      CALL PUINIT(OK)                   ! Setup Top Menu 
      IF ( .NOT. OK ) THEN
        NOMORE = .TRUE.
        GOTO 999
      ENDIF
C
C ****  Main command loop
C
      COMMAND = ' '                     ! COMMAND = Package name
      DO WHILE ( COMMAND(1:4) .NE. 'EXIT' )
        CALL PUMENUDO('PIXIE','PIXIE',COMMAND)
        IF ( COMMAND(1:4) .NE. 'EXIT' ) THEN
          CALL PUEXEC(COMMAND)          ! Activate appropriate package
          CALL PXEXEC                   ! Call activated package
        ENDIF
      ENDDO

      NOMORE = .NOT. FLGVAL('NEXT_EVENT')

  999 CONTINUE
C
      RETURN
      END
