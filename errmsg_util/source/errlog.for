      SUBROUTINE ERRLOG( IDSTRG, SUBRID,VARSTR,LABEL,POS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To write message to the logging device and
C-                         increment the count of calls of that message
C-
C-   Inputs:   IDSTRG      A character string identifies the message
C-             SUBRID      A character string identifies calling routine
C-             VARSTR      A character string provides additional
C-                         information
C-             LABEL       The values of corresponding SEVRTY which is one
C-                         of the arguments of MESSAG called by users
C-             POS         Position of the logging message in storage
C-
C-   Outputs : Display the messages on request
C-
C-   Controls: MAXLOG(POS) Maximum number of messages sent to logging
C-                              device allowed
C-
C-   Created  16-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated   3-JAN-1989   James T. Linnemann
C-   Updated  15-JUN-1989   Jason McCampbell (MSU)
C-   Updated  15-JUL-1990   James T. Linnemann
C-   Updated   5-NOV-1991   Krzysztof L. Genser   FATMEN long generic names
C-   Updated  14-APR-1992   James T. Linnemann   ELN version
C-   Updated  17-APR-1992   Jan S. Hoftun   Added call to L2_ALARM_FORTRAN
C-   Updated  18-APR-1992   James T. Linnemann  squeeze out spaces
C-   Updated   9-MAY-1992   James T. Linnemann  warn about "last message"
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER POS
      CHARACTER*(*) IDSTRG
      CHARACTER*(*) SUBRID
      CHARACTER*(*) VARSTR
      CHARACTER*32  SUB
      CHARACTER*(*)  LABEL
      CHARACTER*132 LINE1, LINE2
      CHARACTER*80 AUX_MSG
      LOGICAL WRITEIT,NO_MAX_WARN
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
C&IF VAXELN
C&      INTEGER I,J,N
C&      CHARACTER*12 DEVICE
C&      CHARACTER*4 ATTRIBUTE
C&      BYTE PRIORITY
C&ELSE
C&ENDIF
      SAVE NO_MAX_WARN
      DATA NO_MAX_WARN/.TRUE./
C&IF VAXELN
C&C----------------------------------------------------------------------
C&C
C&C...the ALARM system is treated as the logging unit, which will log and
C&C     display All messages; online there is NO warning unit (no console)
C&C
C&C...E and F class messages get any special treatment (eg event capture) in
C&C   the ERRHAN_E and ERRFAT routines.
C&C
C&      CALL SWORDS(SUBRID,I,J,N) !remove any leading blanks
C&      DEVICE = SUBRID(I:I+11)   !1st 12 nonblank
C&      CALL SWORDS(IDSTRG,I,J,N) !remove any leading blanks
C&      ATTRIBUTE = IDSTRG(I:I+3) !1st 4 nonblank
C&C
C&C...encode message priority
C&      IF (LABEL(1:1).EQ.'I') THEN
C&        PRIORITY = 0
C&      ELSEIF (LABEL(1:1).EQ.'W') THEN
C&        PRIORITY = 1
C&      ELSEIF (LABEL(1:1).EQ.'E') THEN
C&        PRIORITY = 2
C&      ELSEIF (LABEL(1:1).EQ.'F') THEN
C&        PRIORITY = 3
C&      ELSE
C&        PRIORITY = 1
C&      ENDIF
C&C----------------------------------------------------------------------
C&      LULOG = 999   !fake log unit for alarm system
C&ELSE
C&ENDIF
C
      IF ( LULOG .GT. 0 ) THEN
C
C Message is logged on overflow and/or message has not been
C logged too many times.
C
        WRITEIT = .FALSE.
        AUX_MSG = '  '
        IF ( OVFL ) THEN
          IF (OVLCNT .EQ. 1) THEN
            AUX_MSG = ' Too many distinct IDs: ignore new ones '
            WRITEIT = .TRUE.
          ENDIF
        ELSEIF ( COUNT(POS).LE.MAXLOG(POS) ) THEN
          WRITEIT = .TRUE.
        ELSEIF (NO_MAX_WARN.AND.(COUNT(POS).EQ.MAXLOG(POS))) THEN
          AUX_MSG = ' Framework max count hit for some message(s)'//
     &        ': see message counts in summary'
          NO_MAX_WARN = .FALSE.
        ENDIF
        IF (WRITEIT) THEN
          CALL ERRDSP(IDSTRG,SUBRID,LABEL,VARSTR,LINE1,LINE2)
C&IF VAXELN
C&C...ship message to alarm system
C&          CALL L2_ALARM_FORTRAN(PRIORITY,DEVICE,ATTRIBUTE,LINE1)
C&ELSE
          CALL ELOGWR(LULOG,LINE1)
          CALL ELOGWR(LULOG,LINE2)
C&ENDIF
        ENDIF
        IF (AUX_MSG(1:2).NE.'  ') THEN !auxiliary message to be sent
          CALL ERRDSP('SUPPRESS_MSGS','ERRLOG','W',AUX_MSG,LINE1,LINE2)
C&IF VAXELN
C&C...ship message to alarm system
C&          DEVICE = 'ERRLOG'
C&          ATTRIBUTE = 'SUPP'
C&          PRIORITY = 1
C&          CALL L2_ALARM_FORTRAN(PRIORITY,DEVICE,ATTRIBUTE,LINE1)
C&ELSE
          CALL ELOGWR(LULOG,LINE1)
          CALL ELOGWR(LULOG,LINE2)
C&ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
