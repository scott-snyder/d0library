      SUBROUTINE PC3DJC
C---
C-   Purpose and Methods : Plot 3-D cells for clusters in CACL BANK
C-
C-   Inputs  : none
C-   Outputs :
C-   Controls:
C-
C-   Modified 12-JAN-1992   Nobu Oshima - Use BYTE_ORDER.PARAMS
C-   Updated  18-JUN-1991   Nobu Oshima
C-      Adapts for System picking and rotating + adds 'CAL E COLORANGE'
C-   Modified 31-JAN-1991   N. Oshima ( Adds PC_SET_CAPH )
C-   Updated   2-MAY-1990   Lupe Howell  Implementing RCP parameters & COMPACK
C-   Modified 18-MAY-1989   N. Oshima
C-            New CACL/CACH Banks are used to read CAEP Bank
C-   Created  21-MAR-1989   S. Hagopian
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
      INTEGER IE,IP,IL,I
      INTEGER INDCES,NCH,NCELLS,NREP
      REAL    ENER, COLRNGE
      BYTE    BYTES(4)
      EQUIVALENCE (BYTES(1),INDCES)
      INTEGER LCACL,GZCACL,LCACH
      INTEGER LCAEP,GZCAEP,ICAEP
      INTEGER IC, COLORS(5), NUM
      INTEGER NS,IERR,IER
      INTEGER IDBUT,IDEXIT
      REAL    XS(2,20),YS(2,20),ZS(2,20)
      REAL    EMIN   ! MINIMUM ENERGY/CELL  TO APPEAR IN PLOT
      LOGICAL EZERROR,PU_PICK_ACTIVE
C----------------------------------------------------------------------
      CHARACTER*16 MESS1
      CHARACTER*26 MESS2
      CHARACTER*15 LABELS(5)
      CHARACTER*4  PATH
      CHARACTER*3  CCOL
C =======================================================================
      DATA MESS2/'PC3DJC-Bank does not exist'/
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
        CALL ERRMSG('PIXIE','PC3DCL','Bank PX_CALDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
C
C Get minimum energy for cell to be plotted
      CALL PUGETV('CAL EMIN',EMIN)
      CALL PUGETV('CAL E COLORANGE',COLRNGE)
C-
C---
      IF (PATH .EQ. 'RECO') THEN
        CALL PC_SET_CAPH('ELEC',IER)
        IF (IER .NE. 0)      GO TO 900
      ENDIF
      IF (IER .NE. 0)        GO TO 900
      LCACL = GZCACL()
      IF (LCACL .LE. 0)      GO TO 900
      LCAEP = GZCAEP()
      IF (LCAEP .LE. 0)      GO TO 900
      NCELLS = 0
      IC     = 0
      CALL PUOPEN
C-
C--- Draw X, Y, Z axes and beam pipe
      CALL PLCA3D
C-
   20 IF (LCACL .LE. 0)   GO TO 150
      IC = IC + 1
      LCACH = LQ(LCACL-IZCACH)
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
C GO TO THE NEXT JET BANK
   21 LCACL = LQ(LCACL)
      GO TO 20
  150 CONTINUE
C-
      CALL JRCLOS
C *** Drawing Legend (setting viewing parameters to X Y viwe)
      CALL PCELAB(COLORS,LABELS)
      CALL LEGEND3D(COLORS,LABELS,NUM)
      WRITE(MESS1,200) EMIN
  200 FORMAT('CAL EMIN= ',F6.2)
      CALL PUMESS(MESS1)
C-
      GO TO 950
  900 CALL PUMESS(MESS2)
C
C ****  Reset RCP bank
C
  950 CALL EZRSET
C-
C---  RESET CAPH
C-
  999 IF (IER .EQ. 0) THEN
        CALL PC_RESET_CAPH
      ENDIF
C-
      RETURN
      END
