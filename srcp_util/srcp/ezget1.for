      SUBROUTINE EZGET1 (ID,JSTART,JEND,JSTEP,IVAL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the value(s) associated with the
C-                         parameter index ID from the
C-                         currently selected SRCP bank. Use EZPICK to
C-                         select an SRCP bank.  If the parameter file has
C-                         been read in with the routine EZRDF
C-                         then the SRCP bank hanging from STPH (via SCPH)
C-                         will be automatically selected. EZGET1
C-                         is the same as EZGET except that the array
C-                         indices are given explicitly and access is by
C-                         the index ID. This avoids all the character
C-                         decoding done in EZGET. Use EZSET1 to set values.
C-                         See also EZGET2.
C-
C-                         Use EZGETI to get index ID associated with a
C-                         parameter.
C-
C-   Inputs  : ID       [I]     Parameter ID
C-             JSTART   [I]     First array index. If JSTART = 0 then the
C-                              length of the array is returned in IVAL.
C-             JEND     [I]     Last array index
C-             JSTEP    [I]     Step size:
C-
C-                              DO I = JSTART, JEND, JSTEP
C-
C-   Outputs : IVAL(*)          Value(s) pertaining to parameter ID.
C-             IER              Error code. 
C-                                      Use EZGET_ERROR_TEXT(IER,STRING)
C-
C-                               0 --> OK
C-
C-   Controls: None
C-
C-   ENTRY EZSET1 (ID,JSTART,JEND,JSTEP,IVAL,IER)
C-
C-   Created   6-APR-1989   Harrison B. Prosper
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C-      Use symbolic constants
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER       ID
      INTEGER       JSTART
      INTEGER       JEND
      INTEGER       JSTEP
      INTEGER       IVAL(*)
      INTEGER       IER
C
      INTEGER KSTART,KEND,KSTEP,TOTAL,IPVAL
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
      ENTRY EZSET1 (ID,JSTART,JEND,JSTEP,IVAL,IER)
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
        ELSE
          K = 0
          DO 300 II =  KSTART,KEND,KSTEP
            J = IPVAL+II-1
            K = K + 1
            IVAL(K) = IC(IPTV+J)
  300     CONTINUE
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
  310     CONTINUE
        ENDIF
      ENDIF
C
  999 RETURN
      END
