      SUBROUTINE PCJETL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do Lego Plot for all jets in JETS bank
C-                         [ This routine was PCJETR/PCJETG ]
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  29-JAN-1991   S. Hagopian/Nobuaki Oshima
C-   Updated  25-FEB-1991   Lupe Howell  Picking PIXIE RCP file 
C-   Modified 18-AUG-1991   Nobu Oshima ( Change TITLE Label )
C-   Modified 18-JAN-1992   Nobu Oshima ( Change Labels )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IOK
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LJETS,GZJETS,LCAPH,GZCAPH
      REAL ARRAY(NPHIL,2*NETAL),PT(100)
      INTEGER IARRAY(NPHIL,2*NETAL)
      INTEGER NX,NY,NXMIN,NYMIN,N,NXG,NYG, IER,IV,IJ,NJET
      REAL    XMIN,XMAX,YMIN,YMAX,ZMAX
      REAL    ZSCAL,EMIN,P1
      INTEGER IMARK,ICOL(14),KCOL
      CHARACTER*4  PATH
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*15 PLTITL,ALGORITHM,LABELS(50)
      CHARACTER*23 MESS1
      CHARACTER*35 MESS2
      CHARACTER*3 COLORS(50)
      LOGICAL EZERROR
C-
      DATA IMARK  /1/
      DATA MESS2/'PCJETC - JETS Bank does not exist !'/
      DATA ICOL/13,16,15, 4, 6, 9, 7,14,10,12,11,8,5,3/
C----------------------------------------------------------------------
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCJETL','Cannot find PX_CALDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PATHGT(PATH)
C-
C--- Get minimum energy for cell to be plotted
      CALL PUGETV('CALEGO EMIN',EMIN)
C-
      IF (PATH .EQ. 'RECO') THEN
        CALL PC_SET_CAPH('JETS',IER)
        IF (IER .NE. 0)      GO TO 950
      ENDIF
      LJETS = GZJETS()
      IF (LJETS .LE. 0)  THEN
        CALL PUMESS(MESS2)
        GO TO 900
      ENDIF
C--- Fill ARRAY in ETA-PHI bins with Et from CAEH bank
C--- using hit numbers in JPTS bank
      CALL PCCAEH(LJETS,ARRAY,IARRAY,IOK)
C-
      IF (IOK .NE. 0)      GO TO 900
      NX   = NPHIL
      NY   = NETAL*2
      XMIN = 1.
      XMAX = NPHIL
      YMIN = -NETAL/10.
      YMAX =  NETAL/10.
      ZMAX = -1.
      PLTITL= 'JETS ET LEGO  '
      XLAB = 'PHI'
      YLAB = 'ETA'
      ZLAB = 'ET'
      NXG  = 1
      NYG  = 1
      NXMIN= 1
      NYMIN= 1
      N    = NX
      ZSCAL= .2
C-
      CALL P3LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,EMIN,ZMAX,PLTITL,
     &     XLAB,YLAB,ZLAB,ARRAY,IARRAY,
     &     NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK)
C-
      WRITE(MESS1,200) EMIN
  200 FORMAT(' CALEGO ETMIN=',F6.2)
      CALL PCTEXT(1,MESS1)
      CALL SHOW_CAPH (ALGORITHM,IV,IER)
      LCAPH = GZCAPH()
      P1 = Q(LCAPH+6)
      WRITE(MESS1,300) ALGORITHM,P1
  300 FORMAT(1x,A4,' ALGORITHM ',F6.1)
      CALL PCTEXT(2,MESS1)
C
C ****  LEGEND JET COUNTING
C
      CALL PCCAEH_PT(NJET,PT)
      IJ = 0 
      DO IV = NJET,1,-1
        KCOL=ICOL(MOD(IV,14))
        IJ = IJ + 1
        CALL PXCOLITOC(KCOL,COLORS(IJ))
        WRITE(LABELS(IJ),'(I2,1X,F5.1)')IV,PT(IV)
      END DO
      CALL LEGEND_LEFT(COLORS,LABELS,NJET)
C-
C---  RESET CAPH and EZ package
C-
  900 IF (PATH .EQ. 'RECO')CALL PC_RESET_CAPH
  950 CALL EZRSET
C-
  999 RETURN
      END
