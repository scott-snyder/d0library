      SUBROUTINE SAMUS_CONSTANTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in SAMUS calibration constants
C        for level 2 processing.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  12-SEP-1992   Oleg Eroshin
C-   Updated  27-OCT-1992   Alexei Volkov   
C-   Updated  28-OCT-1992   Alexei Volkov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSL2H.LINK'   
      INCLUDE 'D0$LINKS:IZ2SSAM.LINK'   
C      INCLUDE 'IZ2SSAM.LINK'   
C----------------------------------------------------------------------
      CHARACTER*80 MSG
C----------------------------------------------------------------------
      LOGICAL           ERROR
      CHARACTER*64      CONFIL    /'SA$CONST'/
      INTEGER LOGLEV,KSL2H,KSSAM,LSL2H
      INTEGER IUNIT,IUSER,IER
      CHARACTER*4 CHOPT
      LOGICAL OK
      DATA IUSER/532/
      DATA LOGLEV/-2/
C----------------------------------------------------------------------
C......   OPEN FILE
C
      CALL GTUNIT(IUSER,IUNIT,IER)
      CHOPT = 'IU'
      CALL D0OPEN(IUNIT,CONFIL,CHOPT,OK)
      IF(.NOT.OK) THEN
        MSG = ' SAMUS_CONSTANTS: Cannot open file: '
        CALL ERRMSG('SAMUS_L2','SAMUS_CONSTANS',MSG,'W')
        GO TO 999
      END IF
C
      CALL FZFILE (IUNIT, 0,'I')
      CALL FZLOGL (IUNIT,LOGLEV)
C
C......   CHECK HEADER BANK FOR STP STRUCTURE
C
      IF(LSTPH.EQ.0) THEN
        MSG = ' SAMUS_CONSTANTS: no STP header '
        CALL ERRMSG('SAMUS_L2','SAMUS_CONSTANS',MSG,'W')
        CALL INZSTP
      ENDIF
C
      CALL BKSL2H  (LSL2H)
      IF(LSL2H.EQ.0) THEN
        MSG = ' SAMUS_CONSTANTS: UNABLE TO BOOK SL2H. '
        CALL ERRMSG('SAMUS_L2','SAMUS_CONSTANS',MSG,'W')
        MSG = '                  THIS IS VERY BAD.    '
        CALL ERRMSG('SAMUS_L2','SAMUS_CONSTANS',MSG,'W')
        GOTO 999
      ENDIF

      KSL2H = LC(LSTPH-IZSL2H)
      IF(KSL2H.EQ.0) THEN
        MSG = ' SAMUS_CONSTANTS: no KSL2H header '
        CALL ERRMSG('SAMUS_L2','SAMUS_CONSTANS',MSG,'W')
        GO TO 999
      ENDIF
C
C......   READ IN NEW FILE AND PLACE ON ZEBRA STRUCTURE
C
      KSSAM = LC(KSL2H-IZ2SSAM)
      IF (KSSAM .NE. 0) THEN
        CALL MZDROP(IXSTP, KSSAM, ' ')        ! delete old tree
      ENDIF
      CALL FZIN (IUNIT,IDVSTP,KSL2H,-IZ2SSAM,' ',0,0)
C
      CALL FZENDI(IUNIT,'T')
      CLOSE(IUNIT)
      CALL RLUNIT(IUSER,IUNIT,IER)
C
  999 RETURN
      END
