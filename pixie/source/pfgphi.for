      SUBROUTINE PFGPHI( PHI1, PHI2, PHI3, PHI4 )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get phi region for FDC z display
C    If the FDC R-Z display is used in a combined detector display, then
C  first try to get phi limits from the CDC.  If the CDC hasn't set
C  limits, then use the parameters in PX_SYSTEM_RCP.
C    If the FDC R-Z display is used alone, use 'FDC PHI MODE' from 
C  PX_FDCDIS_RCP. 
C       MODE = -1: Display full detector region, x>0 in top half.
C       MODE = 0:  Display full detector region, y>0 in top half.
C       MODE = 1:  Use phi parameters in PX_SYSTEM_RCP
C       MODE = 2:  Prompt user for PHI region.
C-
C-   Inputs  : none
C-   Outputs : PHI1 = minimum phi for top part of display
C-             PHI2 = max phi for top part of display
C-             PHI3 = min phi for bottom part of display
C-             PHI4 = max phi for bottom part of display
C-   Controls:
C-
C-   Created  18-SEP-1990   Jeffrey Bantly   modified from PVGPHI
C-   Updated  30-APR-1991   Jeffrey Bantly  make better use of Compack
C-   Modified 09-SEP-1991   Nobu Oshima( Get PHI from PX_SYSTEM but CAL. )
C-   Updated  15-NOV-1991   Robert E. Avery  Simplify and allow phi1 < 0.0,
C-                                              phi2 > 2 * pi.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:PXPARA.INC'
C
      INTEGER LENGTH, LINE, IMODE
      INTEGER II, JJ, IER
C
C
      REAL PHI1, PHI2, PHI3, PHI4
      REAL PHIPK, DPHIPK
C
      CHARACTER*66 PROMPT
      CHARACTER*80 STRING
C
      LOGICAL FDONLY, CDC, EZERROR
      LOGICAL FIRST
C
      INTEGER NHELP
      PARAMETER ( NHELP = 6 )
      CHARACTER*70 HELP(NHELP)
      DATA HELP / ' ',
     &          ' The FDC R-Z display is split into two halves.',
     &          ' The top half of the display covers the phi region',
     &          ' from Phi low to Phi high, which you must choose.',
     &          ' The bottom half covers the region symetrically',
     &          ' opposite.'/
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      CALL PUGET_l( 'FDC ONLY', FDONLY )
      IF ( .NOT. FDONLY ) THEN
        CALL PDGTRD( CDC, PHI1, PHI2, PHI3, PHI4 )
        IF ( CDC ) THEN    
          GOTO 999                      ! Done, use CDC road
        ELSE
          IMODE=1                       ! use PX_SYSTEM_RCP PHI
        ENDIF
      ELSE
        CALL PUGET_i( 'FDC PHI MODE', IMODE )
      ENDIF
C
      IF ( IMODE .LT. 0 ) THEN
C
C ****  Mode < 0: full detector ( X>0 on top, X<0 on bottom) (for NWA)
C
        PHI1 = -HALFPI
        PHI2 = HALFPI
        IF ( FIRST ) THEN
          CALL INTMSG(' Top of R-Z plot is X>0 for TB NWA data')
          FIRST = .FALSE.
        ENDIF
      ELSEIF ( IMODE .EQ. 0 ) THEN
C
C ****  Mode 0: full detector (0 to pi on top, pi to twopi on bottom)
C
        PHI1 = 0.
        PHI2 = PI
      ELSEIF ( IMODE .EQ. 1 ) THEN
C
C ****  Mode 1: get phi range from PX_SYSTEM
C
        CALL EZPICK('PX_SYSTEM_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PFGPHI',
     &          'Bank PX_SYSTEM_RCP NOT FOUND','W')
          GOTO 999
        ENDIF
        CALL PUGETV('PHI CENTER',PHIPK)
        CALL PUGETV('PHI WIDTH',DPHIPK)
        CALL EZRSET
C
        PHIPK = PHIPK + 2.8125
        IF (DPHIPK .GE. 87.1875) THEN
          DPHIPK= 90.
        ENDIF
        PHI1=RADIAN*(PHIPK-DPHIPK)
        PHI2=RADIAN*(PHIPK+DPHIPK)
      ELSE
C
C ****  Mode 2: Request user input
C
        CALL OUTMSG('1')
  100   CONTINUE
        PROMPT = ' Enter lower phi limit (-90. to 180.)'//
     &    ' [0.] (? for help)>'
        CALL GETPAR(1,PROMPT,'U',STRING)
        CALL SWORDS(STRING,II,JJ,LENGTH)
        IF ( (STRING(1:1).EQ.'?') 
     &    .OR. (STRING(1:1).EQ.'h')  
     &    .OR. (STRING(1:1).EQ.'H')  ) THEN
          DO LINE = 1, NHELP
            CALL INTMSG(HELP(LINE))
          ENDDO
          GOTO 100
        ENDIF
        IF ( LENGTH .GT. 0 ) THEN
          READ (STRING(1:LENGTH),*,ERR=100) PHI1
          PHI1 = PHI1 * RADIAN
        ELSE
          PHI1 = 0.
        ENDIF
        IF ( PHI1 .LT. -HALFPI ) THEN
          PHI1 = -HALFPI 
        ELSEIF ( PHI1 .GT. PI ) THEN
          PHI1 = PI
        ENDIF
C
  200   CONTINUE
        PROMPT = ' Enter upper phi limit (0. to 270.) [180.]>'
        CALL GETPAR(1,PROMPT,'U',STRING)
        CALL SWORDS(STRING,II,JJ,LENGTH)
        IF ( LENGTH .GT. 0 ) THEN
          READ (STRING(1:LENGTH),*,ERR=200) PHI2
          PHI2 = PHI2 * RADIAN
        ELSE
          PHI2 = PI
        ENDIF
        IF ( PHI2 .LT. 0. ) THEN
          PHI2 = 0.
        ELSEIF ( PHI2 .GT. 1.5 * PI ) THEN
          PHI2 = 1.5 * PI
        ENDIF
        IF ( (PHI2 - PHI1) .LT. 0.1 ) THEN
          PHI2 = PHI1 + 0.1
        ENDIF
        IF ( (PHI2 - PHI1) .GT. PI ) THEN
          PHI2 = PHI1 + PI
        ENDIF
      ENDIF
C
      PHI3 = PHI1 + PI
      PHI4 = PHI2 + PI
C-----------------------------------------------------------------------------
  999 RETURN
      END
