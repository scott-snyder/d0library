      SUBROUTINE ERRSCR( IDSTRG, SUBRID,VARSTR, LABEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To write message to the warning device without
C-                              checking counts.
C-
C-   Inputs:   IDSTRG      A character string identifies the message
C-             SUBRID      A character string identifies calling routine
C-             VARSTR      A character string provides additional
C-                         information
C-             LABEL       The values of corresponding SEVRTY which is one
C-                         of the arguments of MESSAG called by users
C-
C-   ENTRY POINTS
C-      ERRSON          Reenable 'S' messages
C-      ERRSOF          Disable 'S' messages
C-      ERRSGT          Return current status of 'S' messages
C-
C-   Outputs : Display the messages on request
C-             SCREEN, from ERRSGT
C-
C-   Controls: WARN             .FALSE.=> supress screen display of 'S' and 'W'
C-                                      messages completely
C-                              .TRUE. => send 'S' and 'W' via warning unit
C-
C-             SCREEN           .TRUE. => enable 'S' messages
C-                              .FALSE. => supress 'S' messages completely
C-
C-   Created 25-JAN-1990   James T. Linnemann   
C-   Updated   5-NOV-1991   Krzysztof L. Genser  
C       to handle FATMEN long generic names 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) IDSTRG, SUBRID
      CHARACTER*(*) VARSTR
      CHARACTER*132 LINE1,LINE2
      INTEGER POS, I1,J1,N1, I2,J2,N2
      CHARACTER*(*) LABEL
      LOGICAL SCREEN, SCR
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
C----------------------------------------------------------------------
      DATA SCREEN /.TRUE./              ! Default is enabled
C----------------------------------------------------------------------
C
      IF ( SCREEN .AND. WARN ) THEN
C
C **** send message to warning device
C
        CALL ERRDSP(IDSTRG,SUBRID,LABEL,VARSTR,LINE1,LINE2)
        CALL EWRNWR(LINE1)
        CALL EWRNWR(LINE2)
      ENDIF
      RETURN
C
C----
      ENTRY ERRSON
      SCREEN = .TRUE.                   ! Reenable 'S' messages
      RETURN
C
C---
      ENTRY ERRSOF
      SCREEN = .FALSE.                  ! Disable 'S' messages
      RETURN
C
C---
      ENTRY ERRSGT(SCR)                 ! Return 'S' message status
      SCR = SCREEN

  999 RETURN
      END
