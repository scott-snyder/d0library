      SUBROUTINE PDGPHI(PHI1, PHI2, PHI3, PHI4)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get phi region for the CDC R-Z view display
C-
C-   Inputs  :
C-   Outputs : PHI1: minimum phi for the up part R-Z display
C-             PHI2:  maxmum phi for the up part R-Z display
C-             PHI3: minimum phi for the down part R-Z display
C-             PHI4:  maxmum phi for the down part R-Z display
C-
C-   Created  18-OCT-1989   Qizhong Li-Demarteau
C-   Updated  28-MAR-1990   Peter Grudberg - purge dialog with batch of updates
C-   Updated  10-MAY-1990   Qizhong Li-Demarteau  add a call to PDSTRD
C-   Updated  ??-???-1990   C. Yoshikawa add ALLPHIPK param
C-   Updated  28-JAN-1991   Lupe Howell  JPURGE were taken out since PUMESS do
C-                          not use retained segments.
C-   Updated   4-MAR-1991   Lupe Howell  Messages to the user put out in
C-                          COMPACK window .
C-   Modified  6-SEP-1991   Nobu Oshima
C-      Get PHI from PX_SYSTEM instead of CAL, remove ALLPHIPK param which
C-      put by C.Y. and fix a bug for PHI3&PHI4.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:PI.DEF'
      REAL    PHI1, PHI2, PHI3, PHI4
      REAL    PHI11, PHI22, PHIPK,DPHIPK
      INTEGER LENGTH, MODE, ICAPHI, IDPHI,II,JJ,IER
      CHARACTER*80 STRING, PROM1, PROM7(6)
      CHARACTER*66 PROMPT
      CHARACTER*40 PROM2(3)
      CHARACTER*55 PROM3, PROM4, PROM5
      CHARACTER*64 PROM6
      CHARACTER*1  YES
      LOGICAL EZERROR
C-
      DATA PROM1/
     &' select a mode to determine the phi region for the R-Z display'/
      DATA PROM2/'   0: default (0. - 180. degree)',
     &           '   1: use PHI values from PX_SYSTEM.RCP',
     &           '   2: enter PHI values by yourself    '/
      DATA PROM3/' Enter the mode number [0 or 1 or 2] (default is 0)'/
      DATA PROM4
     &  /' Enter minimum PHI value [0.0 - 180.0 degree]:'/
      DATA PROM5
     &  /' Enter maxmum PHI value  [0.0 - 180.0 degree]: '/
      DATA PROM6
     &  /' YOU SHOULD ENTER A VALUE BETWEEN [0.0 - 180.0 degree]!'/
      DATA PROM7
     &  /' The CDC R-Z view display is split into two hemispheres.',
     &' The Upper hemisphere is shown on the upper part of the screen,',
     &' The Lower hemisphere is shown on the lower part of the screen.',
     &' One can select a phi region for the R-Z view display.',
     &' The phi region has to be defined for the Upper part. The lower',
     &' part is always the opposite region in phi for the lower
     & hemisphere.'/
      DATA PHI11/0.0/, PHI22/180.0/
C----------------------------------------------------------------------
C
      CALL JBGBAT(0)
      WRITE (PROMPT,1020) PHI11, PHI22
 1020 FORMAT(1X, ' phi from',F6.1,' to',F6.1,' (degree) Y/N>
     & (?:for help; default:Y)')

      CALL OUTMSG('1')
  200 CALL GETPAR(1,PROMPT,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LENGTH)

CC  200 CALL PURSTR(PROMPT,STRING,LENGTH)
      IF (LENGTH.NE.0) THEN
        READ(STRING(1:LENGTH),1021) YES
 1021   FORMAT (A1)
      ELSE
        YES = 'Y'
      ENDIF
      IF (YES .EQ. '?') THEN
        CALL OUTMSG(PROM7(1))
        CALL OUTMSG(PROM7(2))
        CALL OUTMSG(PROM7(3))
        CALL OUTMSG(PROM7(4))
        CALL OUTMSG(PROM7(5))
        CALL OUTMSG(PROM7(6))
        GOTO 200
      ENDIF
      IF (YES .EQ. 'Y' .OR. YES .EQ. 'y') THEN
        PHI1 = PHI11 * PI / 180
        PHI2 = PHI22 * PI / 180
        PHI3 = PHI1 + PI
        PHI4 = PHI2 + PI
        GOTO 900
      ENDIF
C
      CALL OUTMSG(PROM1)
      CALL OUTMSG(PROM2(1))
      CALL OUTMSG(PROM2(2))
      CALL OUTMSG(PROM2(3))
  100 CALL GETPAR(1,PROM3,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LENGTH)
      IF (LENGTH.NE.0) THEN
        READ(STRING(1:LENGTH),1010) MODE
 1010   FORMAT (I1)
      ELSE
        MODE = 0
      ENDIF
      IF (MODE .EQ. 0) THEN
        PHI1 = 0.
        PHI2 = PI
        PHI3 = PHI1 + PI
        PHI4 = PHI2 + PI
      ELSE
        IF (MODE .EQ. 1) THEN
          CALL EZPICK('PX_SYSTEM_RCP')          ! Selecting SYSTEM bank
          IF ( EZERROR(IER) ) THEN
            CALL ERRMSG('PIXIE','PDGPHI','Bank PX_SYSTEM_RCP NOT FOUND',
     &     'W')
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
          PHI3=PHI1 + PI
          PHI4=PHI2 + PI
          CALL EZRSET
          GOTO 900
        ELSE
  201     CALL GETPAR(1,PROM4,'U',STRING)
          CALL SWORDS(STRING,II,JJ,LENGTH)
          IF (LENGTH.NE.0) THEN
            READ(STRING(1:LENGTH),1011) PHI1
 1011       FORMAT (F8.4)
            IF (PHI1 .NE. 0.) PHI1 = PHI1 * PI / 180
          ENDIF
          IF (PHI1 .LT. 0.) PHI1 = 0.
          IF (PHI1 .GT. PI) THEN
            CALL OUTMSG(PROM6)
            GOTO 201
          ENDIF
  202     CALL GETPAR(1,PROM5,'U',STRING)
          CALL SWORDS(STRING,II,JJ,LENGTH)
          IF (LENGTH.NE.0) THEN
            READ(STRING(1:LENGTH),1011) PHI2
            IF (PHI2 .NE. 0.) THEN
              PHI2 = PHI2 * PI / 180
            ELSE
              PHI2 = PI
            ENDIF
          ENDIF
          IF (PHI2 .LT. 0.) PHI2 = 0.
          IF (PHI2 .GT. PI) THEN
            CALL OUTMSG(PROM6)
            GOTO 202
          ENDIF
          IF (PHI1 .GT. PHI2) THEN
            PHI3 = PHI1
            PHI1 = PHI2
            PHI2 = PHI3
            PHI3 = PHI1 + PI
            PHI4 = PHI2 + PI
          ELSE
            PHI3 = PHI1 + PI
            PHI4 = PHI2 + PI
          ENDIF
        ENDIF
      ENDIF
C
  900 CALL JENBAT
C
  999 RETURN
      END
