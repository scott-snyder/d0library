      SUBROUTINE PVGPHI( PHI1, PHI2, PHI3, PHI4 )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get phi region for VTX z display
C-
C-   Inputs  : none
C-   Outputs : PHI1 = minimum phi for top part of display
C-             PHI2 = max phi for top part of display
C-             PHI3 = min phi for bottom part of display
C-             PHI4 = max phi for bottom part of display
C-   Controls:
C-
C-   Created  29-MAR-1990   Peter M. Grudberg
C-   Updated  10-MAY-1990   Peter M. Grudberg : call PDGTRD 
C-   Updated  30-JAN-1991   Lupe Howell  The number of segments opened were
C-      decreased JPURGE was taken out.
C-   Modified 09-SEP-1991   Nobu Oshima( Get PHI from PX_SYSTEM but CAL. )
C-   Updated   3-JUN-1992   Lupe Howell The prompt messages construction modif
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:PXPARA.INC'
C
      REAL PHI1, PHI2, PHI3, PHI4, PHI11, PHI22
      REAL TEMP, PIE, PHIPK, DPHIPK, RTEMP
      INTEGER LENGTH, LINE, NHELP, NMODE, ICAPHI, IDPHI, IMODE
      INTEGER IUSPHI,IER,II,JJ
      PARAMETER ( NHELP = 6 )
      PARAMETER ( NMODE = 4 )
      CHARACTER*70 HELP(NHELP), MODE(NMODE)
      DATA MODE / ' Choose a phi region.  Available entry modes:',
     &            '  Mode 0: full detector (0.-180. degrees)',
     &            '  Mode 1: use PHI region from PX_SYSTEM.RCP',
     &            '  Mode 2: choose PHI limits interactively' /
      DATA HELP / ' The VTX R-Z display is split into two halves',
     &          ' separated by the y-z plane.  The top half of the',
     &          ' display covers the phi region from 0 to 180 degrees',
     &          ' and the bottom half covers the rest.  Your job is to',
     &          ' select the phi region for the upper part.  The phi',
     &          ' region for the lower half is symetrically opposite.' /
      DATA PHI11 /0.0/, PHI22 /180.0/
      CHARACTER*5 CTEMP,CTEMP2
      CHARACTER*1 YES,CMODE
      CHARACTER*66 PROMPT
      CHARACTER*80 STRING
      LOGICAL VTONLY, CDC, EZERROR
C----------------------------------------------------------------------
C
      PIE = REAL(PI)
C
C ****  Picking PIXIE RCP
C
      CALL EZPICK('PX_VTXDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PVGPHI','Cannot find PX_VTXDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV( 'VTX ONLY', VTONLY )
      IF ( .NOT. VTONLY ) THEN
C
C ****  If the VTX R-Z display is used in a combined detector display, then
C ****  first try to get phi limits from the CDC.  If the CDC hasn't set
C ****  limits, then try to get the calorimeter phi road.  If that road is not
C ****  defined, then plot the whole detector.
C
        CALL PDGTRD( CDC, PHI1, PHI2, PHI3, PHI4 )
        IF ( .NOT. CDC ) THEN
C--- Get PHI range from PX_SYSTEM.RCP
          CALL EZPICK('PX_SYSTEM_RCP')          
          IF ( EZERROR(IER) ) THEN
            CALL ERRMSG('PIXIE','PVGPHI',
     &          'Bank PX_SYSTEM_RCP NOT FOUND','W')
            GOTO 999
          ENDIF
          CALL PUGETV('PHI CENTER',PHIPK)
          CALL PUGETV('PHI WIDTH',DPHIPK)
          IF (DPHIPK .GE. 87.1875) THEN
            PHIPK = 90.
            DPHIPK= 90.
          ENDIF
C- DEFINE PHI1..4 BY PX_SYSTEM_RCP
          PHI1=RADIAN*(PHIPK-DPHIPK)
          PHI2=RADIAN*(PHIPK+DPHIPK)
          PHI3 = PHI1 + PI
          PHI4 = PHI2 + PI
          CALL EZRSET
        ENDIF
      ELSE
        CALL PUGETV( 'VTX USE PHI MODE', IUSPHI )
        IF ( IUSPHI .GT. 0 ) THEN
          CALL PUGETV( 'VTX PHI MODE', IMODE )
          IF ( IMODE .LE. 0 ) THEN
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
              CALL ERRMSG('PIXIE','PVGPHI',
     &          'Bank PX_SYSTEM_RCP NOT FOUND','W')
              GOTO 999
            ENDIF
            CALL PUGETV('PHI CENTER',PHIPK)
            CALL PUGETV('PHI WIDTH',DPHIPK)
            IF (DPHIPK .GE. 87.1875) THEN
              PHIPK = 90.
              DPHIPK= 90.
            ENDIF
C- DEFINE PHI1..4 BY PX_SYSTEM_RCP
            PHI1=RADIAN*(PHIPK-DPHIPK)
            PHI2=RADIAN*(PHIPK+DPHIPK)
            CALL EZRSET
          ELSE
            CALL JBGBAT(0)              
C
C ****  Mode 2: Request user input
C
            PROMPT = ' Enter lower phi limit (0.-180.) [0.]>'
            CALL PURSTR(PROMPT, STRING, LENGTH)
            IF ( LENGTH .GT. 0 ) THEN
              READ (STRING(1:LENGTH),5) PHI1
    5         FORMAT(F8.4)
              PHI1 = PHI1 * RADIAN
            ELSE
              PHI1 = 0.
            ENDIF
            IF ( PHI1 .LT. 0. ) THEN
              PHI1 = 0.
            ELSEIF ( PHI1 .GT. PIE ) THEN
              PHI1 = PI
            ENDIF
            PROMPT = ' Enter upper phi limit (0.-180.) [180.]>'
            CALL PURSTR(PROMPT, STRING, LENGTH)
            IF ( LENGTH .GT. 0 ) THEN
              READ (STRING(1:LENGTH),5) PHI2
              PHI2 = PHI2 * RADIAN
            ELSE
              PHI2 = PI
            ENDIF
            IF ( PHI2 .LT. 0. ) THEN
              PHI2 = 0.
            ELSEIF ( PHI2 .GT. PIE ) THEN
              PHI2 = PI
            ENDIF
            IF ( PHI1 .GT. PHI2 ) THEN
              TEMP = PHI1
              PHI1 = PHI2
              PHI2 = TEMP
            ENDIF
            IF ( (PHI2 - PHI1) .LT. 0.1 ) THEN
C
C ****  Make PHI1 and PHI2 separate by 0.1 radian
C
              PHI2 = MIN( PHI1 + 0.1, PIE )
              PHI1 = PHI2 - 0.1
            ENDIF
            CALL JENBAT
          ENDIF
        ELSE
          CALL JBGBAT(0)                
C
C ****  Request entry mode interactively
C
          CALL OUTMSG('1')
          DO LINE = 1, NMODE
            CALL OUTMSG(MODE(LINE))
          ENDDO
          WRITE (CMODE,10)IMODE
   10     FORMAT(I1)
          PROMPT = ' Choose an entry more ['//
     &             CMODE//']>' 
C
  100     CALL GETPAR(1,PROMPT,'U',STRING)
          CALL SWORDS(STRING,II,JJ,LENGTH)
          IF ( LENGTH .GT. 0 ) THEN
            READ (STRING(1:LENGTH),15) IMODE
   15       FORMAT(I1)
          ENDIF
          IF ( IMODE .GT. 2 .OR. IMODE .LT. 0) THEN
C
C ****  Print help lines then ask again
C
            DO LINE = 1, NHELP
              CALL OUTMSG(HELP(LINE))
            ENDDO
            GO TO 100
          ENDIF
          IF ( IMODE .EQ. 0 ) THEN          
            PHI1 = 0.
            PHI2 = PI
          ELSEIF ( IMODE .EQ. 1 ) THEN      
            CALL EZPICK('PX_SYSTEM_RCP')          
            IF ( EZERROR(IER) ) THEN
              CALL ERRMSG('PIXIE','PVGPHI',
     &          'Bank PX_SYSTEM_RCP NOT FOUND','W')
              GO TO 100
            ENDIF
            CALL PUGETV('PHI CENTER',PHIPK)
            CALL PUGETV('PHI WIDTH',DPHIPK)
            IF (DPHIPK .GE. 87.1875) THEN
              PHIPK = 90.
              DPHIPK= 90.
            ENDIF
C- DEFINE PHI1..4 BY PX_SYSTEM_RCP
            PHI1=RADIAN*(PHIPK-DPHIPK)
            PHI2=RADIAN*(PHIPK+DPHIPK)
            CALL EZRSET
          ELSE                              
            PROMPT = ' Enter lower phi limit (0.-180.) [0.]>'
  200       CALL GETPAR(1,PROMPT,'U',STRING)
            CALL SWORDS(STRING,II,JJ,LENGTH)
            IF ( LENGTH .GT. 0 ) THEN
              READ (STRING(1:LENGTH),5) PHI1
              PHI1 = PHI1 * RADIAN
            ELSE
              PHI1 = 0.
            ENDIF
            IF ( PHI1 .LT. 0. .OR. PHI1 .GT. PIE ) THEN
              CALL OUTMSG
     &      (' Phi must be between 0. and 180. Try again')
              GO TO 200
            ENDIF
            RTEMP = PHI1 / RADIAN
            WRITE (CTEMP,20) RTEMP
   20       FORMAT(F5.1)
            PROMPT = ' Enter upper phi limit ('//
     &               CTEMP//'-180.) [180.]>'
  300       CALL GETPAR(1,PROMPT,'U',STRING)
            CALL SWORDS(STRING,II,JJ,LENGTH)
            IF ( LENGTH .GT. 0 ) THEN
              READ (STRING(1:LENGTH),5) PHI2
              PHI2 = PHI2 * RADIAN
            ELSE
              PHI2 = PI
            ENDIF
            IF ( PHI2 .LT. 0. .OR. PHI2 .GT. PIE ) THEN
              CALL OUTMSG
     &      (' Phi must be between 0 and 180. Try again')
              GO TO 300
            ENDIF
            IF ( PHI1 .GT. PHI2 ) THEN
              TEMP = PHI1
              PHI1 = PHI2
              PHI2 = TEMP
            ENDIF
            IF ( (PHI2 - PHI1) .LT. 0.1 ) THEN
              PHI2 = MIN( (PHI1 + 0.1), PIE )
              PHI1 = PHI2 - 0.1
            ENDIF
          ENDIF
          RTEMP = PHI1 / RADIAN
          WRITE (CTEMP,25) RTEMP
          RTEMP =  PHI2 / RADIAN
          WRITE(CTEMP2,25)RTEMP
   25     FORMAT(F5.1)
          PROMPT = ' Phi limits: '//CTEMP//' to '//
     &            CTEMP2//' degrees'
          CALL PUMESS(PROMPT)
          CALL JENBAT                     
        ENDIF
        PHI3 = PHI1 + PI
        PHI4 = PHI2 + PI
      ENDIF
C
C ****  Reseting RCP file
C
  900 CALL EZRSET
  999 RETURN
      END
