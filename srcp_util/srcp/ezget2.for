      SUBROUTINE EZGET2 (ID,JSTART,JEND,JSTEP,IVAL,TYPE,NUMV,TOTAL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return data and types associated with parameter
C-                         index ID from a pre-selected SRCP bank. See also
C-                         EZGET1.
C-
C-   Inputs  : ID       [I]     Parameter index
C-             JSTART   [I]     Value of first DO loop index. If zero
C-                              Parameter size is returned in IVAL and
C-                              NUMV and TYPE is set to 1.
C-             JEND     [I]     Value of Last DO loop index. If zero
C-                              then the parameter size will be assumed,
C-                              that is, the number of values/parameter ID.
C-             JSTEP    [I]     Value of step in DO loop. If zero, then
C-                              1 will be assumed.
C-
C-
C-   Outputs : IVAL(*)          Value(s) pertaining to ID.
C-             TYPE(*)  [I*]    Value type(s)
C-             NUMV     [I]     Number of values returned
C-             TOTAL    [I]     Total number of values/parameter ID
C-             IER      [I]     Error code
C-
C-                         Error code. Use EZERR to check for errors.
C-                         See also EZGET_ERROR_TEXT(IER,STRING)
C-
C-                           0 --> OK
C-
C-   ENTRY EZSET2 (ID,JSTART,JEND,JSTEP,IVAL,TYPE,NUMV,TOTAL,IER)
C-
C-   Note:      For EZSET2 the values of arguments NUMV and TOTAL
C-              are not used. However, their values are altered
C-              by EZSET2 and have the same meaning as for EZGET2.
C-
C-   Controls: None
C-
C-   Created   7-MAR-1989   Harrison B. prosper
C-   Updated  11-MAY-1990   Harrison B. Prosper
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C-      Use symbolic constants
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ID
      INTEGER JSTART
      INTEGER JEND
      INTEGER JSTEP
      INTEGER IVAL(*)
      INTEGER TYPE(*)
      INTEGER NUMV
      INTEGER TOTAL
      INTEGER IER
C
      INTEGER KSTART,KEND,KSTEP,IPVAL
      INTEGER IPTI,IPTV,IPTO,IPTT
      INTEGER II,JJ,I,J,K
      INTEGER EZZAND,EZZSHFT
      LOGICAL GET
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GET = .TRUE.
      GOTO 5
C
C ****  Entry point for setting routine
C
      ENTRY EZSET2 (ID,JSTART,JEND,JSTEP,IVAL,TYPE,NUMV,TOTAL,IER)
      GET = .FALSE.
    5 CONTINUE
C
C ****  Clear error code
C
      IER = EZS_SUCCESS
      ERRSRC = IER
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
C ****  Get number of values/identifier from type list
C
      IPVAL = EZZSHFT(IC(IPTO+ID),-NBITS)       ! Pointer to values-list
      TOTAL = EZZAND(IC(IPTT+IPVAL),MASK)     ! Zero upper word
C
C ****  Check DO loop indices
C
      KSTART = JSTART
      KEND   = JEND
      KSTEP  = JSTEP
      KEND   = MIN(KEND,TOTAL)
      IF ( KEND  .LE. 0 ) KEND  = TOTAL
      IF ( KSTEP .EQ. 0 ) KSTEP = 1
C
      IF ( GET ) THEN
C
C ****  Get values
C
        IF ( KSTART .LE. 0 ) THEN
          IVAL(1) = TOTAL  ! Return array size
          TYPE(1) = 1
          NUMV  = TOTAL
        ELSE
          K = 0
          DO 300 II =  KSTART,KEND,KSTEP
            J = IPVAL+II-1
            K = K + 1
            IVAL(K) = IC(IPTV+J)
            TYPE(K) = EZZSHFT(IC(IPTT+J),-NBITS)
  300     CONTINUE
          NUMV = K
        ENDIF
C
      ELSE
C
C ****  Set values
C
        IF ( KSTART .GT. 0 ) THEN
          K = 0
          DO 310 II =  KSTART,KEND,KSTEP
            J = IPVAL+II-1
            K = K + 1
            IC(IPTV+J) = IVAL(K)
            IC(IPTT+J) = EZZSHFT(TYPE(K),NBITS)
            IC(IPTT+J) = ior(IC(IPTT+J), TOTAL)
  310     CONTINUE
          NUMV = K
        ENDIF
      ENDIF
C
  999 RETURN
      END
