      SUBROUTINE CAISTP (FILNAM,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read Calorimeter geometry banks into ZEBSTP. 
C-                              Modeled on CDISTP.FOR by
C-                              Olivier and Ghita Callot
C-
C-   Inputs  : FILNAM [C*]      Input file name
C-   Outputs : IERR [I]          0 if open was succesful
C-
C-   Created  27-JUN-1988   A.M.Jonckheere
C-   Updated  18-JAN-1989   Harrison B. Prosper
C-                          Add a check on LSCAL; Bomb if zero
C-   Updated  25-JAN-1989   Harrison B. Prosper
C-                          Put declaration of SRCP routines here rather
C-                          than in INICAL.
C-   Updated  10-FEB-1989   Harrison B. Prosper
C-                          Replace OPNFIL with D0OPEN
C-   Updated  12-FEB-1989   Harrison B. Prosper
C-                          Replace IERR with logical varaiable OK to
C-                          make consistent with CAL offline use.
C-   Updated  13-FEB        S. Protopopescu 
C-                          added GTUNIT and INZSTP. Disable multiple calls.
C-   Updated  27-APR-1989   Harrison B. Prosper
C-                          Replace logical varaiable OK  with IERR to
C-                          make consistent with CAL offline RULES.
C-   Updated  26-JUL-1991   James T. Linnemann  fix LSTPC link error 
C-   Updated  20-NOV-1991   K. Wyatt Merritt   Replace ABORT with ERRMSG call
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL OK
      INTEGER LSTPC
      CHARACTER*(*) FILNAM
      INTEGER LUNIN,L,LBANK,IERR
C
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCPDH.LINK'
      INCLUDE 'D0$LINKS:IZCGNH.LINK'
      INCLUDE 'D0$LINKS:IZCGEH.LINK'
      INCLUDE 'D0$LINKS:IZCUCL.LINK'
      INCLUDE 'D0$LINKS:IZCECL.LINK'
      INCLUDE 'D0$LINKS:IZCRST.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      OK=.TRUE.
C     
      IF(FIRST) THEN   ! do this only once
C
        CALL INZSTP      ! initialize ZEBSTP
C
C ****  Open the requested file
C
        CALL GTUNIT(80,LUNIN,IERR)
        L = LEN (FILNAM)
        CALL D0OPEN (LUNIN,FILNAM(1:L),'IU',OK)
        IF ( .NOT. OK ) THEN
          IERR = -1
          GOTO 999
        ELSE
          IERR= 0
        END IF
C
C ****  Declare file to ZEBRA
C
        CALL FZFILE( LUNIN, 0, 'I' )
C
C ****  Check for existence of STPC bank
C
        LSTPC = LC ( LSTPH - IZSTPC )
        IF ( LSTPC .LE. 0 ) THEN
          CALL ERRMSG('CAL','CAISTP','ERROR ** LSTPC is ZERO !!!!','F')
        ENDIF
C
C ****  Read in data banks
C
        CALL FZIN  ( LUNIN, IDVSTP, LSTPC, -IZSCAL, ' ', 0, 0 )
C
C ****  Terminate and Close file
C
        CALL FZENDI( LUNIN, 'TU' )
        CLOSE (UNIT=LUNIN)
C
C ****  Check for SCAL bank
C
        LSTPC = LC( LSTPH - IZSTPC )
        LSCAL = LC( LSTPC - IZSCAL )
        IF ( LSCAL .LE. 0 ) THEN
          CALL ERRMSG ('CAL','CAISTP','LSCAL is ZERO !!!!','F')
        ENDIF
C
C ****  INITIALIZE CLINKS LINK ARAEA FOR CELXYZ
C
        CALL MZLINK(IXSTP,'/CLINKS/',LQSTPH,LQCLYR,LQSTPH)     
C
        LCPDH = LC( LSCAL - IZCPDH )
        LCGNH = LC( LSCAL - IZCGNH )
        LCGEH = LC( LSCAL - IZCGEH )
C
C ******************************************************************
C ****  Declare Calorimeter SRCP geometry banks to SRCP package ****
C ******************************************************************
C
C
        IF ( LCGEH .GT. 0 ) THEN
C
C ****  DECLARE SRCP_UCAL
C
          LBANK = LC(LCGEH-IZCUCL)
          IF ( LBANK .GT. 0 ) THEN
            CALL EZNAME ('SRCP_UCAL',LCGEH,IZCUCL)
          ENDIF
C
C ****  DECLARE SRCP_ECAL
C
          LBANK = LC(LCGEH-IZCECL)
          IF ( LBANK .GT. 0 ) THEN
            CALL EZNAME ('SRCP_ECAL',LCGEH,IZCECL)
          ENDIF
C
C ****  DECLARE SRCP_REST
C
          LBANK = LC(LCGEH-IZCRST)
          IF ( LBANK .GT. 0 ) THEN
            CALL EZNAME ('SRCP_REST',LCGEH,IZCRST)
          ENDIF
C
        ELSE
          CALL ERRMSG ('CAL','CAISTP','LCGEH is ZERO !!!!','F')
        ENDIF
        FIRST=.FALSE.
      ENDIF
C
  999 RETURN
      END
