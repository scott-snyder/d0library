      SUBROUTINE EZGETI (PARAM1,ID,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get Index associated with parameter PARAM1.
C-                         The index ID can then be used with EZGET1 and
C-                         EZSET1 for fast access to parameters. 
C-                         IMPORTANT: Parameter names MUST be given in
C-                         upper case.
C-
C-   Inputs  : PARAM1      Name of parameter (CHARACTER string)
C-
C-   Outputs : ID          Index associated with parameter PARAM1.
C-             IER         Error code. See EZGET_ERROR_TEXT.
C-
C-                           0 --> OK
C-   Controls: None
C-
C-   Created  21-APR-1989   Harrison B. prosper
C-   Updated  18-SEP-1989   Harrison B. Prosper   
C-   Removed calls to UPCASE and WORD
C-   Updated  11-MAY-1990   Harrison B. Prosper   
C-   
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM1
      INTEGER       ID
      INTEGER       IER
C
      CHARACTER*32  PARAM,IDENTF
      INTEGER LIDS,IPTI,IPTV,IPTO,IPTT,LENG,CHRIDS
      INTEGER LUP,LDOWN,LMEAN,IPNT,IP,II,JJ,I,J,K,L,WRDIDS
      INTEGER EZZAND
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
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
      LIDS = IC(LSRCP+JJIDS)           ! Number of identifiers
      WRDIDS = IC(LSRCP+JJNWRD)        ! Number of words/record
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
C
C ****  Do a binary search for PARAM
C
      L = LEN(PARAM1)
      PARAM = PARAM1(1:L)
C++++++++++++++++++++++++++++++++++++++
C+++   CALL UPCASE (PARAM,PARAM)       
C++++++++++++++++++++++++++++++++++++++
      LUP = LIDS + 1
      LDOWN = 0        ! To handle boundary conditions

  100 CONTINUE
      IF((LUP-LDOWN).LE.1) GOTO 400
      LMEAN = (LUP+LDOWN)/2
C
C ****  Get identifier at median position
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
        ID = IP                         ! Return index
        GOTO 999
C
      ELSEIF( PARAM .GT. IDENTF ) THEN
        LDOWN=LMEAN
        GOTO 100
      ENDIF
C
C **** No match found return
C
  400 CONTINUE
      ID  = 0
      IER = EZS_PARAM_NOTFOUND
      ERRSRC = IER
C
  999 RETURN
      END
