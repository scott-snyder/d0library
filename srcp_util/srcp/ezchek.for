      FUNCTION EZCHEK   (PARAM1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if specified parameter is in the
C-                         currently selected SRCP bank.
C-
C-   Returned values:      TRUE if parameter present
C-   Inputs  : PARAM1      Name of parameter (CHARACTER string)
C-   Outputs : None
C-
C-                         Error code. Use EZERR to check for errors.
C-                         See also EZREPORT_ERROR(IER,STRING).
C-
C-                           0 --> OK
C-   Controls: None
C-
C-   Created   7-MAR-1989   Harrison B. prosper
C-   Updated  11-MAY-1990   Harrison B. Prosper   
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL EZCHEK,EZCHECK
      CHARACTER*(*) PARAM1
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      INTEGER IER,LIDS,IPTI,IPTO,IPTV,IPTT,LENG,CHRIDS
      INTEGER LUP,LDOWN,LMEAN,IPNT,IP,II,JJ,I,J,K,L,N,WRDIDS
      INTEGER EZZAND
      LOGICAL FIRST_ENTRY,FOUND
      CHARACTER*(NUMCHR)  PARAM,IDENTF
C----------------------------------------------------------------------
      FIRST_ENTRY = .TRUE.
      GOTO 1
      ENTRY    EZCHECK  (PARAM1)
      FIRST_ENTRY = .FALSE.
C
    1 CONTINUE
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
      L = LEN(PARAM1)
      PARAM = PARAM1(1:L)
      CALL UPCASE (PARAM,PARAM)
C
C ****  Get pointers to data within SRCP bank
C
      LIDS = IC(LSRCP+JJIDS)           ! Number of identifiers
      WRDIDS = IC(LSRCP+JJNWRD)        ! Number of words/record
      CHRIDS = WRDIDS*4
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
C
C ****  Do a binary search for PARAM
C
      LUP = LIDS + 1
      LDOWN = 0        ! To handle boundary conditions

  100 IF((LUP-LDOWN).LE.1) GOTO 400
      LMEAN = (LUP+LDOWN)/2
C
C ****  Get record at median position
C
      IP = EZZAND(IC(IPTO+LMEAN),MASK)
      IPNT = IPTI + WRDIDS*(IP-1)
      CALL UHTOC (IC(IPNT+1),4,IDENTF,NUMCHR)
C
      IF( PARAM .LT. IDENTF )THEN
        LUP=LMEAN
        GOTO 100
C
      ELSEIF ( PARAM .EQ. IDENTF ) THEN
C
C ***** A MATCH has been FOUND
C
        FOUND = .TRUE.
        GOTO 999
C
      ELSEIF( PARAM .GT. IDENTF ) THEN
        LDOWN=LMEAN
        GOTO 100
      ENDIF
C
C **** No match found
C
  400 CONTINUE
      IER = EZS_PARAM_NOTFOUND
      ERRSRC = IER
      FOUND = .FALSE.
C
  999 CONTINUE
      IF ( FIRST_ENTRY ) THEN
        EZCHEK = FOUND
      ELSE
        EZCHECK= FOUND
      ENDIF
      RETURN
      END
