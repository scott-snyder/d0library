      SUBROUTINE PC3DCL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws 3D view of cal cells color coded for E
C-
C-   Created   1-JUL-1988   Michael Peters
C-   Updated  27-OCT-1988   Michael Peters adapt to PIXIE
C-   Updated   7-APR-1989   Michael W. Peters  use CAEP
C-   Updated  19-DEC-1989   Lupe Rosas Using Color Table
C-   Updated  27-FEB-1990   Lupe Howell PCECOL and LEGEND
C-   Updated   4-DEC-1990   Harrison B. Prosper
C-                          Added call to PCPICK
C-   Updated  28-JAN-1991   Lupe Howell  Rotation check
C-   Updated  14-MAY-1991   Harrison B. Prosper, Nobu Oshima
C-   Updated  18-JUN-1991   Nobu Oshima
C-      Adapts for System picking and rotating + adds 'CAL E COLORANGE'
C-   Modified 12-JAN-1992   Nobu Oshima - Use BYTE_ORDER.PARAMS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C----------------------------------------------------------------------
      INTEGER GZCAEP,LDCAEP
      INTEGER INDCES,LCAEP,NRP,NCH
      INTEGER I,IL,IP,IE,IZ,IC, IFLAG
      INTEGER KCOL, KINT, KFIL, KSTY, IER
      INTEGER NS,IERR, COLORS(5), NUM, J
      INTEGER IDBUT,IDEXIT,IETAMN,IETAMX, IX,JBIT
      REAL    ENER,EMIN,CUTEMIN,PMIN,ESUM, COLRNGE
      REAL    XS(2,20),YS(2,20),ZS(2,20),VWSAVE(85)
      REAL    VXPICK,VYPICK,WXPICK,WYPICK
      REAL    RLEVEL
      CHARACTER*32 STR1,STR2
      CHARACTER*15 LABELS(5)
      CHARACTER*3  CCOL
      CHARACTER*48 TMPMESS
      LOGICAL EZERROR,PU_PICK_ACTIVE
      LOGICAL BTEST,LSUPHOT
      BYTE BYTES(4)
      EQUIVALENCE (BYTES(1),INDCES)
C-
      DATA NUM     /5/
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C-
      CALL JIQDIL(RLEVEL)
C-
C--- Check do picking...
C-
      IF ( PU_PICK_ACTIVE() ) THEN
        CALL PCPICK
        IF( RLEVEL .EQ. -2. )   CALL PX_PICK_QUIT
        GO TO 999
      ENDIF
C-
      LCAEP = GZCAEP()
      IF(LCAEP .LE. 0) THEN
        CALL INTMSG(' PC3DCL- CAEP BANK DOES NOT EXIST')
        GO TO 999
      ENDIF
C-
      CALL PUGETV('CAL EMIN',EMIN)
      CALL PUGET_i('CAL IETAMIN',IETAMN)
      CALL PUGET_i('CAL IETAMAX',IETAMX)
      CALL PUGETV('CAL IETACUT EMIN',CUTEMIN)
      CALL PUGETV('CAL E COLORANGE',COLRNGE)
      CALL PUGET_l('CAL SUPHOTC',LSUPHOT)
C
      NRP = IQ(LCAEP+2)
      NCH = IQ(LCAEP+3)
      IF(LCAEP.GT.0 .AND. NCH.GT.0) THEN
        IX=JBIT(IQ(LCAEP+4),6)
      ENDIF
C-
C--- Scan cells
C-
      CALL PUOPEN
C--- Draw X, Y, Z axes and beam pipe
      CALL PLCA3D
C-
      ESUM = 0.
      DO 100 I = 1,NCH
        LDCAEP = LCAEP + (I-1)*NRP
        INDCES = IQ(LDCAEP+4)
        ENER   = Q(LDCAEP+5)
        IE     = BYTES(BYTE4)
        IP     = BYTES(BYTE3)
        IL     = BYTES(BYTE2)
        IFLAG  = BYTES(BYTE1)
        ESUM   = ESUM + ENER
        IF ( BTEST(IFLAG,7) .AND. LSUPHOT) THEN
          WRITE(TMPMESS,1000) IE,IP,IL,ENER
          CALL INTMSG(TMPMESS)
          GO TO 100
        ENDIF
C-
C--- Check for displaying cells or not...
C-
        IF(IL.LT.MNLYEM .OR. IL.GT.MXLYCH) GO TO 100
        IF(IE.LE.IETAMN .OR. IE.GE.IETAMX) THEN
          IF(ENER .LT. CUTEMIN)          GO TO 100
        ENDIF
        IF(ENER .LT. EMIN)                 GO TO 100
C--- Determinig color for the cells
C-
        CALL PCECOL(ENER,EMIN,COLRNGE,IC,CCOL,NUM)
        CALL PXCOLN('CDC',IC,3,.FALSE.,KCOL,KINT,KFIL,KSTY)
        CALL JCOLOR(KCOL)
        CALL JLWIDE(KSTY)
        CALL CELVEC(IE,IP,IL,XS,YS,ZS,NS,IERR)
        IF(IERR.NE.0)
     &    GO TO 100
        IF(NS.LE.2.OR.NS.GT.20)
     &    GO TO 100
        CALL PLTVEC(XS,YS,ZS,NS)
  100 CONTINUE
C-
C---
 1000 FORMAT(1X,'IETA=',I3,' IPHI=',I3,' ILAYER=',I3,' ENERGY=',F7.2)
C---
      CALL JRCLOS
      IF (IX .EQ. 0) THEN
        WRITE(STR1,200) EMIN
        WRITE(STR2,202) ESUM
      ELSE
        WRITE(STR1,210) EMIN
        WRITE(STR2,212) ESUM
      ENDIF
      CALL PCTEXT(1,STR1)
      CALL PCTEXT(2,STR2)
  200 FORMAT(' CAL EMIN =',F5.1,' GeV')
  210 FORMAT(' CAL EMIN =',F6.1,' ADC counts')
  202 FORMAT(' CAEP E SUM =',F6.1,' GeV')
  212 FORMAT(' CAEP E SUM =',F8.0,' ADC counts')
C-
C--- Drawing Legend (setting viewing parameters to X Y viwe)
C-
      CALL PCELAB(COLORS,LABELS)
      CALL LEGEND3D(COLORS,LABELS,NUM)
C-
  999 RETURN
      END
