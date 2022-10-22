       SUBROUTINE PC_PELC_LEGO
C-
C-   Purpose and Methods : SET UP lego PLOT for cells in CACL assoc. with PELC
C-
C-   Inputs  : none
C-   Outputs :
C-   Controls:
C-
C-   Created  21-MAY-1990 S. Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      REAL ARRAY(NPHIL,2*NETAL)
      INTEGER IARRAY(NPHIL,2*NETAL)
      INTEGER IOK
      INTEGER LCACL,GZCACL
      INTEGER LPELC,GZPELC
C----------------------------------------------------------------------
      INTEGER NX,NY
      REAL XMIN,XMAX,YMIN,YMAX,ZMAX
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*32 PLTITL
      INTEGER NXMIN,NYMIN,N
      INTEGER NXG,NYG
      REAL ZSCAL
      REAL EMIN   ! MINIMUM ENERGY/CELL  TO APPEAR IN LEGO PLOT
      INTEGER JTYPE,IMARK,NBANK,TYP,IER
      CHARACTER*4 REM, CVAL
      CHARACTER*8 CEMIN
      CHARACTER*16 IMESS
      CHARACTER*30 MESS1,MESS2,MESS3,MESS4
      LOGICAL EZERROR
C =======================================================================
      DATA IMARK/1/
      DATA MESS2/'error - bank does not exist'/
      DATA IMESS/'     CALEGO EMIN'/
      DATA MESS3/'               EM Towers'/
      DATA MESS4/'     Total Energy Towers'/
C ========================================================================
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PC_PELC_LEGO',
     &       'Cannot find PX_CALDIS_RCP','W')
        GOTO 999
      ENDIF
C Get minimum energy for cell to be plotted
      CALL EZ_GET_ARRAY('PXPARAMS','CALEGO EMIN',1,EMIN,CVAL,
     &       TYP,REM,IER)
CC      CALL PUGETV('CALEGO EMIN',EMIN)
      LPELC=GZPELC()
      IF(LPELC.LE.0)GO TO 900
      NBANK=1
      CALL VZERO(ARRAY,NPHIL*2*NETAL)
      CALL VZERO_i(IARRAY,NPHIL*2*NETAL)
C find associated CACL BANK
  100 LCACL=LQ(LPELC-2)
      IF(LCACL.LE.0)GO TO 900
C get Cluster type
      JTYPE=IQ(LCACL+3)
C Fill ARRAY in ETA-PHI bins with Et from CAEH bank
C using hit numbers in CACH bank
      CALL PC_PELC_CACL(LCACL,NBANK,ARRAY,IARRAY,IOK)
      IF(IOK.NE.0)GO TO 900
C CHECK IF THERE ARE ANY MORE PELC BANKS
      LPELC=LQ(LPELC)
      NBANK=NBANK+1
      IF(LPELC.GT.0)GO TO 100
      NY=NETAL*2
      NX=NPHIL
      YMIN=-NETAL
      YMAX=NETAL
      XMIN=1.
      XMAX=NPHIL
      ZMAX=-1.
      PLTITL='PELC ETA-PHI ET'
      XLAB='PHI'
      YLAB='ETA'
      ZLAB='ET'
      NXMIN=1
      NYMIN=1
      NXG=1
      NYG=1
      N=NX
      ZSCAL=.2
      CALL P3LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,EMIN,ZMAX,PLTITL,
     X     XLAB,YLAB,ZLAB,ARRAY,IARRAY,
     X     NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK)
      CALL PXFTOC(EMIN,CEMIN)
      MESS1=IMESS//CEMIN
      CALL PUMESS(MESS1)
      IF(JTYPE.EQ.1)THEN
         CALL PUMESS(MESS3)
      ELSE
         CALL PUMESS(MESS4)
      ENDIF
      GO TO 999
  900 CALL PUMESS(MESS2)
      CALL EZRSET
  999 RETURN
      END
