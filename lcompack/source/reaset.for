      SUBROUTINE REASET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up for screen manipulation via SMG routines
C-                         VAX-specific
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: Many of the flags in /COMNUM/ and /SMGCOM/ are set here
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated 17-SEP-1991   Herbert Greenlee
C-   Modified 14-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C&IF VAXVMS
      INCLUDE '($SMGDEF)'
C&ELSE
C&	INCLUDE 'D0$INC:SMGDEF.DEF'
C&ENDIF
      LOGICAL GETDEV
      LOGICAL ISTAT,SMG$CREATE_PASTEBOARD
      LOGICAL SMG$CREATE_VIRTUAL_DISPLAY,SMG$PASTE_VIRTUAL_DISPLAY
      LOGICAL SMG$CREATE_VIRTUAL_KEYBOARD,TRALOG
      INTEGER OUTLEN
      CHARACTER*132 OUTNAM
C----------------------------------------------------------------------
      PBCOLS=80
      PBROWS=10000
      TRMFLG=.TRUE.                        ! Indicate reading from terminal
      IF(.NOT.ONEFLG) THEN
        SMGON=.FALSE.
      ELSE
        TRMFLG=.TRUE.       !Indicate terminal reading in ONEFLG mode
      ENDIF
      RETURN
      END
