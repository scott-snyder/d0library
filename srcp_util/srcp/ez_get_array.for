      SUBROUTINE EZ_GET_ARRAY (ARRAY_NAME,PARAM_NAME,NPARAM,
     &  IVAL, CVAL,TYPE,REMARK,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For given array and list of parameters within
C-   array return the VALUE, TYPE and REMARK. The array is assumed to have
C-   the following structure:
C-
C-   \ARRAY array-name
C-      'Param-name'    value   'Remark'
C-              :       :       :
C-   \END
C-
C-   Note: Value can be of type INTEGER, REAL, LOGICAL or CHARACTER
C-   (enclosed within single quotes).
C-
C-   Note:  To get ALL the parameters within the array set PARAM_NAME(1)
C-          to a blank.
C-
C-   Inputs  : ARRAY_NAME       [C*]    Name of RCP array
C-             PARAM_NAME(*)    [C*]    Parameter name or key-word (or ' ')
C-             NPARAM           [I]     Number of parameters required
C-
C-   Outputs : PARAM_NAME(*)    [C*]    Parameter names
C-             NPARAM           [I]     Number of parameters within an array
C-             IVAL(*)                  REAL, INTEGER, LOGICAL values
C-             CVAL(*)          [C*]    CHARACTER values
C-             TYPE(*)          [I]     RCP value types
C-             REMARK(*)        [C*]    Associated remarks
C-             IER              [I]     Error code; 0 -- OK
C-   Controls: None
C-
C-   Created  27-MAR-1990   Lupe Howell, Harrison B. Prosper
C-   Updated  17-MAY-1990   Lupe Howell  An extra output parameter was added
C-   Updated   9-APR-1991   Lupe Howell, Harrison B. Prosper The bank
C-            manipulation is done using poiters not local arrays
C-   Updated  18-SEP-1991   Lupe Howell  Comapison of the current param done
C-      with full size
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) PARAM_NAME(*)
      INTEGER NPARAM
      INTEGER IVAL(*)
      CHARACTER*(*) CVAL(*)
      INTEGER TYPE(*)
      CHARACTER*(*) REMARK(*)
      INTEGER IER
C
      LOGICAL ACTIVE, GET_ALL
C
      CHARACTER*32 NAME,PARNAME
      CHARACTER*132 AVAL,REM
C
      INTEGER EZZSHFT,EZZAND,ID,IPVAL,IPTV,IPTT,IPTO,IPTI
      INTEGER TOTAL,NNN,IPOINT,VALPOINT,I,J
      INTEGER LARR,LNAM,JVAL,LVAL,ITYP,XX,YY,PLEN
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
C
C ****  Get pointers into RCP-bank only if the bank and parameter
C ****  has changed from the previous call.
C
      LARR = LEN(ARRAY_NAME)
C
C ****  Check if an SRCP bank has been selected
C
      IF ( ISRCP .LE. 0 ) THEN
        IER = EZS_BANK_NOTSELECTED
        ERRSRC = IER
        GOTO 999
      ENDIF
C
C ****  Get the bank address
C
      LSRCP = KSRCP(ISRCP)
      IF ( LSRCP .LE. 0 ) THEN
        IER = EZS_BANK_NOTFOUND
        ERRSRC = IER
        GOTO 999
      ENDIF
C
C ****  Get pointers to data within SRCP bank
C
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
C
C ****  Get index into RCP bank for the given array-name
C
      CALL EZGETI (ARRAY_NAME(1:LARR),ID,IER)
C
C ****  Get number of values/identifier from type list
C
      IPVAL = EZZSHFT(IC(IPTO+ID),-NBITS)       ! Pointer to values-list
      TOTAL = EZZAND(IC(IPTT+IPVAL),MASK)       ! Zero upper word
C
C ****  Make pointers point to absolute
C
      IPTV = IPTV + IPVAL
      IPTT = IPTT + IPVAL
C
C ****  Check for key-words
C
      GET_ALL  = PARAM_NAME(1)(1:1) .EQ. ' '
      CALL SWORDS(PARAM_NAME,I,J,LNAM)
C
C ****  Loop over values, check types and decode into strings
C ****  and values.
C
      IF ( GET_ALL ) THEN
        NPARAM = 0
      ENDIF
C
      ACTIVE = .TRUE.
      NNN = 1
      IPOINT = 1                        ! Start at first word in local array
      DO WHILE ( ACTIVE )
C
C ****  Exit if we've gone beyond the given array
C
        IF ( IPOINT .GT. TOTAL ) GOTO 999
C
C ****  Get element of array (param, value, remark) and update pointer
C
        CALL EZ_GET_NEXT_ELEMENT(IPOINT,IC(IPTV),IC(IPTT),
     &    PARNAME,JVAL,AVAL,LVAL,ITYP,REM,VALPOINT)
C
        CALL SWORDS(PARNAME,XX,YY,PLEN)
        IF ( GET_ALL ) THEN
          NPARAM = NPARAM + 1
          PARAM_NAME(NPARAM) = PARNAME
          TYPE(NPARAM) = ITYP
          IVAL(NPARAM) = JVAL
          CVAL(NPARAM) = AVAL
          REMARK(NPARAM) = REM
        ELSE
          IF ( PARNAME(1:PLEN) .EQ. PARAM_NAME(1)(1:LNAM) ) THEN
            TYPE(NNN) = ITYP
            IF ( TYPE(NNN) .GE. VTCHR ) THEN
              CVAL(NNN) = AVAL
            ELSE
              IVAL(NNN) = JVAL
            ENDIF
            REMARK(NNN) = REM
            NNN = NNN + 1
          ENDIF
C
C ****  Exit if number of parameters found equal number requested.
C
          IF ( NNN .GT. NPARAM ) GOTO 999
        ENDIF
      ENDDO
  999 RETURN
      END
