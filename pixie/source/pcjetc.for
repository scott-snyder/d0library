      SUBROUTINE PCJETC
C-
C-   Purpose and Methods : SET UP lego PLOT for jets in RECO CACL bank
C-
C-   Inputs  : none
C-   Outputs :
C-   Controls:
C-
C-   Created  21-MAR-1989   S. Hagopian
C-   Modified 29-JAN-1991   N. Oshima( Adds PC_SET_CAPH )
C-   Updated  25-FEB-1991   Lupe Howell  Picking PIXIE RCP file
C-   Modified 13-JUN-1991   N. Oshima( Adds a message 'CC/EC Region' )
C-   Modified 13-JAN-1992   N. Oshima( Change labels )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      REAL    ARRAY(NPHIL,2*NETAL)
      INTEGER IARRAY(NPHIL,2*NETAL)
      INTEGER NX,NY,NXMIN,NYMIN,N,NXG,NYG,IOK,IER
      INTEGER LCACL,GZCACL
      INTEGER JTYPE,IMARK
      REAL    XMIN,XMAX,YMIN,YMAX,ZMAX, ZSCAL
      REAL    EMIN   ! MINIMUM ENERGY/CELL  TO APPEAR IN LEGO PLOT
      LOGICAL EZERROR
      CHARACTER*4  PATH
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*32 PLTITL, MESS1
      CHARACTER*30 MESS2
C =======================================================================
      DATA IMARK/1/
      DATA MESS2/'PCJETC- Bank does not exist   '/
C ========================================================================
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCJETC','Cannot find PX_CALDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PATHGT(PATH)
C-
C--- Get minimum energy for cell to be plotted
      CALL PUGETV('CALEGO EMIN',EMIN)
C-
      IF (PATH .EQ. 'RECO') THEN
        CALL PC_SET_CAPH('ELEC',IER)
        IF (IER .NE. 0)       GO TO 950
      ENDIF
      LCACL = GZCACL()
      IF (LCACL .LE. 0) THEN
        CALL PUMESS(MESS2)
        GO TO 900
      ENDIF
      CALL VZERO(ARRAY,NPHIL*2*NETAL)
      CALL VZERO(IARRAY,NPHIL*2*NETAL)
C-
C--- Fill ARRAY in ETA-PHI bins with Et from CAEH bank
C--- using hit numbers in CACH bank
C-
      CALL PCCAEH(LCACL,ARRAY,IARRAY,IOK)
      IF(IOK .NE. 0)          GO TO 900
C- Get Cluster type
      JTYPE = IQ(LCACL+3)
      NX    = NPHIL
      NY    = NETAL*2
      XMIN  = 1.
      XMAX  = NPHIL
      YMIN  =-NETAL/10.
      YMAX  = NETAL/10.
      ZMAX  =-1.
      PLTITL= 'CACL ETA-PHI ET'
      XLAB  = 'PHI'
      YLAB  = 'ETA'
      ZLAB  = 'ET'
      NXMIN = 1
      NYMIN = 1
      NXG   = 1
      NYG   = 1
      N     = NX
      ZSCAL = .2
      CALL P3LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,EMIN,ZMAX,PLTITL,
     &     XLAB,YLAB,ZLAB,ARRAY,IARRAY,
     &     NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK)
C-
      IF (JTYPE .EQ. 1) THEN
        WRITE(MESS1,200) EMIN
      ELSEIF(JTYPE .EQ. 2) THEN
        WRITE(MESS1,202) EMIN
      ELSEIF(JTYPE .EQ. 2) THEN
        WRITE(MESS1,204) EMIN
      ENDIF
      CALL PCTEXT(1,MESS1)
  200 FORMAT(' CALEGO EMIN=',F6.2,' EM  Towers')
  202 FORMAT(' CALEGO EMIN=',F6.2,' HAD Towers')
  204 FORMAT(' CALEGO EMIN=',F6.2,' CC/EC Region')
C-
C---  RESET CAPH and EZ package
C-
  900 CALL PC_RESET_CAPH
  950 CALL EZRSET
  999 RETURN
      END
