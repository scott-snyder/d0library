      SUBROUTINE EZGNXT (PREFIX,NID,ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the ID of the next parameter whose
C-                         name begins with the given prefix. Set the
C-                         (variable) NID = 1 to get the first
C-                         parameter (in alphabetical order). Upon
C-                         exit EZGNXT increments NID by 1 so that
C-                         on subsequent calls the next parameter ID
C-                         will be returned. Use EZGETN to get the
C-                         name of the parameter with index ID.
C-
C-   Inputs  : PREFIX   [C*]    Parameter name prefix.
C-             NID      [I]     1 ---> Get 1st parameter
C-                              2 ---> Get 2nd parameter etc.
C-
C-   Outputs : ID       [I]     Parameter ID; 0 --- Parameter not found
C-                              or no more parameters with given prefix.
C-
C-   Controls: None
C-
C-                               Error code
C-
C-                               0 --> OK
C-                               See EZERR and EZGET_ERROR_TEXT.
C-
C-   Created  13-SEP-1989   Harrison B. Prosper
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C-      Use symbolic constants
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) PREFIX
      INTEGER       NID
      INTEGER       ID
C
      INTEGER IP,IPNT,IPTI,IPTV,IPTO,IPTT,IER,LIDS,WRDIDS
      INTEGER II,JJ,I,J,K,L,N,LENPRE
      INTEGER LUP,LDOWN,LMEAN,LPOSN,LBASE
      INTEGER EZZAND
      CHARACTER*32 PARAM,IDENTF
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      SAVE LBASE
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
C
C ****  Check NID parameter
C
      IF ( (NID .LT. 1) .OR. (NID .GT. LIDS) ) THEN
        IER = EZS_BAD_ARGUMENT
        ERRSRC = IER
        GOTO 999
      ENDIF
C
      WRDIDS = IC(LSRCP+JJNWRD)        ! Number of words/record
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
C
C ****  Get first parameter by binary search and back-stepping
C
      IF ( NID .EQ. 1 ) THEN
C
        L = LEN(PREFIX)
        CALL WORD (PREFIX(1:L),I,J,LENPRE)
        PARAM = PREFIX(I:J)
        CALL UPCASE (PARAM(1:LENPRE),PARAM(1:LENPRE))
C
        LUP = LIDS + 1
        LDOWN = 0        ! To handle boundary conditions

  100   IF((LUP-LDOWN).LE.1) GOTO 400
        LMEAN = (LUP+LDOWN)/2
C
C ****  Get identifier at median position
C
        IP   = EZZAND(IC(IPTO+LMEAN),MASK)
        IPNT = IPTI + WRDIDS*(IP-1)
        CALL UHTOC (IC(IPNT+1),4,IDENTF,NUMCHR)
C
        IF( PARAM(1:LENPRE) .LT. IDENTF(1:LENPRE) )THEN
          LUP=LMEAN
          GOTO 100
C
        ELSEIF ( PARAM(1:LENPRE) .EQ. IDENTF(1:LENPRE) ) THEN
C
C ***** A MATCH has been FOUND; get first parameter with
C ***** specified prefix.
C
          LPOSN = LMEAN                 ! Remember position in order map
C
  110     CONTINUE
          ID   = IP                     ! Return index
          LPOSN= LPOSN - 1              ! Back step through order map
          LBASE= LPOSN
          IF ( LPOSN .LT. 1 ) GOTO 900
C
          IP   = EZZAND(IC(IPTO+LPOSN),MASK)
          IPNT = IPTI + WRDIDS*(IP-1)
          CALL UHTOC (IC(IPNT+1),4,IDENTF,NUMCHR)
          IF ( PREFIX(1:LENPRE) .EQ. IDENTF(1:LENPRE) ) GOTO 110
C
          GOTO 900
C
        ELSEIF( PARAM(1:LENPRE) .GT. IDENTF(1:LENPRE) ) THEN
          LDOWN=LMEAN
          GOTO 100
        ENDIF
C
C **** No match found return.
C
  400   CONTINUE
        ID  = 0
        IER = EZS_PARAM_NOTFOUND
        ERRSRC = IER

      ELSE
C
C ****  Get next ID with same prefix
C
        LPOSN= LBASE + NID              ! Forward step through order map
        IP   = EZZAND(IC(IPTO+LPOSN),MASK)
        IPNT = IPTI + WRDIDS*(IP-1)
        CALL UHTOC (IC(IPNT+1),4,IDENTF,NUMCHR)
        IF ( PREFIX(1:LENPRE) .EQ. IDENTF(1:LENPRE) ) THEN
          ID = IP                       ! Match found; return ID
        ELSE
          ID = 0                        ! No more parameters
        ENDIF
      ENDIF
C
  900 CONTINUE
      NID = NID + 1
C
  999 RETURN
      END
