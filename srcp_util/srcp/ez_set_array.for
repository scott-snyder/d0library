
      SUBROUTINE EZ_SET_ARRAY(ARRAY_NAME,PARAM_NAME,NWIVAL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For given array and parameter's name within
C-   array sets the NWVALUE.   The array is assumed to have the following
C-   structure:
C-
C-   \ARRAY array-name
C-      'Param-name'    value   'Remark'
C-              :       :       :
C-   \END
C-
C-   Note: Value can be of type INTEGER, REAL, LOGICAL or CHARACTER
C-   (enclosed within single quotes).
C-
C-
C-   Inputs  : ARRAY_NAME       [C*]    Name of RCP array
C-             PARAM_NAME       [C*]    Parameter name
C-             NWIVAL                   REAL, INTEGER, LOGICAL values
C-                                      or HOLLERITH
C-             IER              [I]     Error code; 0 -- OK
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Created  27-MAR-1990   Lupe Howell, Harrison B. Prosper
C-   Updated   9-APR-1991   Lupe Howell, Harrison B. Prosper The bank
C-            manipulation is done using poiters not local arrays
C-   Updated  18-SEP-1991   Lupe Howell  Comparison of the current param done
C-       with full size
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) PARAM_NAME
      INTEGER  NWIVAL
      INTEGER IER
C
      LOGICAL ACTIVE
C
      CHARACTER*32 PARNAME
      CHARACTER*132 STRING,AVAL,REM
C
      INTEGER ID,TOTAL,IPOINT,VALPOINT
      INTEGER EZZSHFT,EZZAND,IPVAL,IPTV,IPTT,IPTO,IPTI
      INTEGER LRCP,LARR,LNAM,JVAL,LVAL,ITYP,I,J,XX,YY,PLEN
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C
C----------------------------------------------------------------------
C
C ****  Get pointers into RCP-bank only if the bank and parameter
C ****  has changed from the previous call.
C
      LARR = LEN(ARRAY_NAME)
      CALL SWORDS(PARAM_NAME,I,J,LNAM)
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
      TOTAL = EZZAND(IC(IPTT+IPVAL),MASK)     ! Zero upper word
C
C ****  Make pointers point to absolute
C
      IPTV = IPTV + IPVAL
      IPTT = IPTT + IPVAL
C
C ****  Loop over values, check types and decode into strings
C ****  and values.
C
      IPOINT = 1       ! Start at first word in local array
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        IF ( IPOINT .GT. TOTAL ) GOTO 999
C
C ****  Get element of array (param, value, remark) and update pointer
C
        CALL EZ_GET_NEXT_ELEMENT(IPOINT,IC(IPTV),IC(IPTT),
     &    PARNAME,JVAL,AVAL,LVAL,ITYP,REM,VALPOINT)
        CALL SWORDS(PARNAME,XX,YY,PLEN)
C
C ****  Compare current parameter name with requested parameter name
C
        IF ( PARNAME(1:PLEN) .EQ. PARAM_NAME(1:LNAM) ) THEN
          ACTIVE = .FALSE.
C
          CALL EZSET1(ID,VALPOINT,VALPOINT,1,NWIVAL,IER)
        ENDIF
      ENDDO
  999 RETURN
      END
