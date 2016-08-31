      SUBROUTINE PC3DELEC
C---
C-   Purpose and Methods : Plot 3-D cells for clusters in PELC BANK
C-
C-   Inputs  : none
C-   Outputs :
C-   Controls:
C-
C-   Created  21-MAY-1990 S. Hagopian
C-   Updated  28-JAN-1991   Lupe Howell  Isajet track displays was eliminated
C-   Updated  09-FEB-1991   Nobu Oshima  Add rotate/picking + clean up.
C-   Updated  18-JUN-1991   Nobu Oshima
C-      Adapts for System picking and rotating + adds 'CAL E COLORANGE'
C-   Modified 12-JAN-1992   Nobu Oshima - Use BYTE_ORDER.PARAMS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZCACH.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      INTEGER KCOL, KINT, KFIL, KSTY
      INTEGER IL,IP,IE,I
      INTEGER NCELLS,NREP,NCH, NS,IERR,IER
      INTEGER LCACH,LCAEP,LCACL,LPELC,ICAEP
      INTEGER GZCAEP,GZCACL,GZPELC
      INTEGER IC, COLORS(5), NUM
      INTEGER IDBUT,IDEXIT
      REAL    ENER,COLRNGE
      INTEGER INDCES
      BYTE    BYTES(4)
      EQUIVALENCE (BYTES(1),INDCES)
      REAL XS(2,20),YS(2,20),ZS(2,20)
      REAL EMIN   ! MINIMUM ENERGY/CELL  TO APPEAR IN PLOT
C----------------------------------------------------------------------
      CHARACTER*16 STR1
      CHARACTER*40 MESS2
      CHARACTER*15 LABELS(5)
      CHARACTER*3 CCOL
      LOGICAL EZERROR,PU_PICK_ACTIVE
C =======================================================================
      DATA MESS2/'PC3DELEC- Bank does not exist   '/
      DATA NUM     /5/
C ========================================================================
C-
C--- Check do picking...
C-
      IF ( PU_PICK_ACTIVE() ) THEN
        CALL PCPICK
        GO TO 999
      ENDIF
C
C ****  Make sure we pick correct bank
C
      CALL EZPICK('PX_CALDIS_RCP')          ! Selecting Caldis bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PC3DELEC',
     &    'Bank PX_CALDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
C-
C-
C Get minimum energy for cell to be plotted
      CALL PUGETV('CAL EMIN',EMIN)
      CALL PUGETV('CAL E COLORANGE',COLRNGE)
C-
      LPELC = GZPELC()
      IF(LPELC.LE.0)GO TO 900
      LCAEP = GZCAEP()
      IF(LCAEP.LE.0)GO TO 900
      CALL PUOPEN
C-
C--- Draw X, Y, Z axes and beam pipe
      CALL PLCA3D
C find associated CACL BANK
   10 LCACL=LQ(LPELC-2)
      NCELLS = 0
      IC     = 0
   20 IF (LCACL .LE. 0)   GO TO 150
      IC = IC + 1
      LCACH = LQ(LCACL - IZCACH)
      IF(LCACH .LE. 0)    GO TO 150
      NCH = IQ(LCACH+2)
      IF (NCH .LT. 1)     GO TO 150
      NCELLS = NCELLS + 1
      NREP   = IQ(LCAEP+2)
C Enter Maximum Energy  to make color intervals
C
      DO 100 I=1,NCH
        ICAEP  = LCAEP + NREP*(IQ(LCACH+2+I)-1)
        INDCES = IQ(ICAEP+4)
        ENER   = Q(ICAEP+5)
        IF(ENER.LE.EMIN)GO TO 100
        IE=BYTES(BYTE4)
        IP=BYTES(BYTE3)
        IL=BYTES(BYTE2)
        IF(IL.LT.MNLYEM.OR.IL.GT.MXLYCH)GO TO 100
C Determinig color for the cells
        CALL PCECOL(ENER,EMIN,COLRNGE,IC,CCOL,NUM)
        CALL PXCOLN('CDC',IC,3,.FALSE.,KCOL,KINT,KFIL,KSTY)
        CALL JCOLOR(KCOL)
        CALL JLWIDE(KSTY)
        CALL CELVEC(IE,IP,IL,XS,YS,ZS,NS,IERR)
        IF(IERR.NE.0 .OR. NS.EQ.0)
     &    GO TO 100
        CALL PLTVEC(XS,YS,ZS,NS)
  100 CONTINUE
C MARK CENTER OF ENERGY CLUSTER
   21 CALL PC3DCEN(LCACL)
C GO TO THE NEXT PELC BANK
      LPELC = LQ(LPELC)
      IF(LPELC.LE.0)GO TO 150
      GO TO 10
  150 CONTINUE
C-
      CALL JRCLOS
      WRITE(STR1,200) EMIN
  200 FORMAT('CAL EMIN= ',F6.2)
C *** Drawing Legend (setting viewing parameters to X Y viwe)
      CALL PCELAB(COLORS,LABELS)
      CALL LEGEND3D(COLORS,LABELS,NUM)
      CALL PUMESS(STR1)
C
      GO TO 950
  900 CALL PUMESS(MESS2)
C
C ****  Reset RCP bank
C
  950 CALL EZRSET
  999 RETURN
      END
