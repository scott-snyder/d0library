      SUBROUTINE MUON_CONSTANTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in muon calibration constants
C        for level 2 processing. Note we assume that all relevant
C        muon ZEBRA banks have been combined into a single file
C        ADAPTED FROM MRZCON
C
C      USRPAR --> FILTER_RUN --> MAIN_FILTER_BEGIN --> MUON_CONSTANTS
C         MUON_CONSTNTS is called if the appropriate flag is set
C         in FILTER_RUN
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  31-JAN-1989   Jan S. Hoftun
C-   D. Hedin    MAR-1990   initial non-dummy version
C-   T. DIEHL 27-JUN-1991   USE D0OPEN + GTUNIT INSTEAD OF EOPEN 
C-                          MOVE SMUO BANK TO THE SL2H BANK UNTIL 
C-                          MUON_L2_PARAMETERS WHICH PUTS IT BACK.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSL2H.LINK'
      INCLUDE 'D0$LINKS:IZ2SMUO.LINK'
      LOGICAL           ERROR
      CHARACTER*64      CONFIL    /'MU$CONST'/
      INTEGER LOGLEV,KSL2H,KSMUO,LSL2H
      INTEGER IUNIT,IUSER,IER
      CHARACTER*4 CHOPT
      LOGICAL OK
      DATA IUSER/532/
      DATA LOGLEV/-2/
C----------------------------------------------------------------------

C         OPEN FILE
      CALL GTUNIT(IUSER,IUNIT,IER)
      CHOPT = 'IU'
      CALL D0OPEN(IUNIT,CONFIL,CHOPT,OK)
      IF(.NOT.OK) THEN
        CALL INTMSG(' MUON_CONSTANTS: Cannot open file: ')
        GO TO 999
      END IF
C
      CALL FZFILE(IUNIT,0,'I')
      CALL FZLOGL (IUNIT,LOGLEV)
C
C  CHECK HEADER BANK FOR STP STRUCTURE
C
      IF(LSTPH.EQ.0) THEN
        CALL INTMSG(' MUON_CONSTANTS: no STP header')
        CALL INZSTP
      ENDIF
C
      CALL BKSL2H(LSL2H)
      IF(LSL2H.EQ.0) THEN
        CALL INTMSG(' MUON_CONSTANTS: UNABLE TO BOOK SL2H.')
        CALL INTMSG('                 THIS IS VERY BAD.   ')
        GOTO 999
      ENDIF

      KSL2H=LC(LSTPH-IZSL2H)
      IF(KSL2H.EQ.0) THEN
        CALL INTMSG(' MUON_CONSTANTS: no KSL2H header')
        GO TO 999
      ENDIF
C
C      READ IN NEW FILE AND PLACE ON ZEBRA STRUCTURE
      KSMUO=LC(KSL2H-IZ2SMUO)
      IF (KSMUO .NE. 0) THEN
        CALL MZDROP(IXSTP, KSMUO, ' ')        ! delete old tree
      ENDIF
      CALL FZIN (IUNIT,IDVSTP,KSL2H,-IZ2SMUO,' ',0,0)
C      CALL INTMSG(' MU CONSTANTS init OK')
C
      CALL FZENDI(IUNIT,'T')
      CLOSE(IUNIT)
      CALL RLUNIT(IUSER,IUNIT,IER)
  999 RETURN
      END
