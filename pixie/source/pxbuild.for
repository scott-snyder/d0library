      PROGRAM PXBUILD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given one or more RCP files which define
C-   the set of PIXIE action routines (plus menu commands) write
C-   interface routines as directed by the user.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER,I,J,L
      CHARACTER*5  VERSION
      CHARACTER*72 VPIXIE,VNPIXIE
C----------------------------------------------------------------------
C
C ****  Setup ZEBRA
C
      CALL MZEBRA(0)
      CALL INZSTP
C
C ****  Read control file
C
      CALL INRCP('PXBUILD_RCP',IER)
      IF ( IER .NE. 0 ) THEN
        CALL INTMSG(' Could not find PXBUILD_RCP')
        GOTO 999
      ENDIF
C
C ****  Setup COMPACK
C
      CALL EZ_SETUP_COMPACK('PXBUILD_RCP',IER)
      IF ( IER .NE. 0 ) THEN
        CALL INTMSG(' EZ_SETUP_COMPACK problem')
        GOTO 999
      ENDIF
C
C ****  Display version number
C
      CALL PXBUILD_VERSION(VERSION)
      VNPIXIE = VPIXIE()
      CALL SWORDS(VNPIXIE,I,J,L)
      CALL STAMSG(' Welcome to PXBUILD '//
     &            VERSION//'; '//
     &            VNPIXIE(I:J),.TRUE.)
C
C ****  Perform user actions
C
      CALL PXBUILD_ACTIONS
C
  999 CONTINUE
      END
