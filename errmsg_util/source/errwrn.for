      SUBROUTINE ERRWRN( IDSTRG, SUBRID,VARSTR, LABEL, POS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To write message to the warning device.
C-
C-   Inputs:   IDSTRG      A character string identifies the message
C-             SUBRID      A character string identifies calling routine
C-             VARSTR      A character string provides additional
C-                         information
C-             LABEL       The values of corresponding SEVRTY which is one
C-                         of the arguments of MESSAG called by users
C-             POS         Position of the warning message in storage
C-
C-   Outputs : Display the messages on request
C-
C-   Controls: MAXWRN(POS) Maximum number of messages sent to warning
C-                              device allowed
C-             WARN             .FALSE.=> only count, and sent to logging unit
C-                              .TRUE. => sent via warning unit
C-
C-   Created  16-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated   3-JAN-1989   James T. Linnemann
C-   Updated  15-JUN-1989   Jason McCampbell (MSU)
C-   Updated  15-JUL-1990   James T. Linnemann   
C-   Updated   5-NOV-1991   Krzysztof L. Genser  
C       to handle FATMEN long generic names 
C-   Updated  14-APR-1992   James T. Linnemann   ELN version: no warning device
C-   Updated   9-MAY-1992   James T. Linnemann   warning of last messages
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) IDSTRG, SUBRID
      CHARACTER*(*) VARSTR
      CHARACTER*132 LINE1,LINE2
      INTEGER POS, I1,J1,N1, I2,J2,N2
      CHARACTER*(*) LABEL
      CHARACTER*80 AUX_MSG
      LOGICAL WRITEIT,NO_MAX_WARN
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
      SAVE NO_MAX_WARN
      DATA NO_MAX_WARN/.TRUE./
C----------------------------------------------------------------------
C&IF VAXELN
C&ELSE
C
C **** check if that message warning too many times ?
C
      IF ( WARN ) THEN
C
        WRITEIT = .FALSE.
        AUX_MSG = '  '
        IF ( OVFL ) THEN
          IF (OVLCNT .EQ. 1) THEN
            AUX_MSG = ' Too many distinct IDs: ignore new ones '
            WRITEIT = .TRUE.
          ENDIF
        ELSEIF ( COUNT(POS).LE.MAXWRN(POS) ) THEN
          WRITEIT = .TRUE.
        ELSEIF (NO_MAX_WARN.AND.(COUNT(POS).EQ.MAXLOG(POS))) THEN
          AUX_MSG = ' Framework max count hit for some message(s)'//
     &        ': see message counts in summary'
          NO_MAX_WARN = .FALSE.
        ENDIF
        IF (WRITEIT) THEN
          CALL ERRDSP(IDSTRG,SUBRID,LABEL,VARSTR,LINE1,LINE2)
          CALL EWRNWR(LINE1)
          CALL EWRNWR(LINE2)
        ENDIF
        IF (AUX_MSG(1:2).NE.'  ') THEN !auxiliary message to be sent
          CALL ERRDSP(IDSTRG,SUBRID,LABEL,AUX_MSG,LINE1,LINE2)
          CALL EWRNWR(LINE1)
          CALL EWRNWR(LINE2)
        ENDIF
      ENDIF
C&ENDIF
  999 RETURN
      END
