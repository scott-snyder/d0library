       SUBROUTINE PPJET_LEGO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make LEGO plot from PJET with ISAQ's ID.
C-
C-
C-   Created  05-MAR-1994 Nobuaki Oshima
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C----------------------------------------------------------------------
      INTEGER IARRAY(NPHIL,2*NETAL)
      INTEGER NTYP(9)
      INTEGER NX,NY,NXMIN,NYMIN,N
      INTEGER NXG,NYG
      INTEGER IMARK,NMISS,NELEC,NPHOTON,NTAUS
      INTEGER J,K,IETA,IPHI, ICODE,IOK
      INTEGER INDP,INDE
C-
      REAL    ARRAY(NPHIL,2*NETAL),XPMUO(3,50)
      REAL    XMIN,XMAX,YMIN,YMAX,ZMAX
      REAL    ZSCAL,XSIZ,YSIZ
      REAL    ETMIN   ! MINIMUM ET TO APPEAR IN LEGO PLOT
      REAL    RLEVEL,RAD,ZMIN
      REAL    TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX
      REAL    UMIN,UMAX,VMIN,VMAX
      REAL    PLZMAX,ZMED,ZDIV
      REAL    TXMID,TYMID,TXARROW,TYARROW,TZARROW
      REAL    ZMAXP
      REAL    TXDEL,TYDEL,TZDEL
C-
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*15 PLTITL
      CHARACTER*24 MESS1
      CHARACTER*24 IMESS1
C-
      DATA TZDEL/.7/
      DATA XSIZ,YSIZ /1.0,.67/
      DATA IMESS1/' No objs inside bounds'/
C----------------------------------------------------------------------
C-
      CALL JIQDIL(RLEVEL)
      CALL PUGETV('PHYDIS ETMIN',ETMIN)
C-
      NY=NETAL*2
      NX=NPHIL
      YMIN=-NETAL
      YMAX=NETAL
      XMIN=0.
      XMAX=TWOPI
      YMIN=YMIN/10.
      YMAX=YMAX/10.
      ZMIN=0.
      ZMAX=-1.
      PLTITL='ET PJET ETA-PHI'
      XLAB='PHI'
      YLAB='ETA'
      ZLAB='ET'
      NXMIN=1
      NYMIN=1
      NXG=1
      NYG=1
      N=NX
      ZSCAL=.2
C---
      CALL PPJET_LFILL(ARRAY,IARRAY,XPMUO,NTYP,IOK)
C-
C--- DETERMINING ZMAX
      ZMAX=PLZMAX(NXMIN,NX,NYMIN,NY,ARRAY,ARRAY,ZMAX,N,0)
      CALL PUGET_MAXET(ZMAXP)
      IF (ZMAXP .GT. ZMAX)   ZMAX = ZMAXP
      IF(ZMAX .LE. 0.)THEN
        CALL PUMESS(IMESS1)
        GO TO 999
      ENDIF
C--- Set viewing parameters
      IF(RLEVEL .NE. -2.) THEN
        CALL PLSETV(NX,NY,UMIN,UMAX,VMIN,VMAX)
      ENDIF
C--- MAKING GRID
      CALL PLGRID(NXMIN,NX,NYMIN,NY,NXG,NYG,XMIN,XMAX,
     X        YMIN,YMAX)
C--- DRAW Z-AXIS
      ZMED=(XMAX+ZMIN)/2.
      CALL PLZAXS(NXMIN,NYMIN,ZMAX,ZMIN,ZMED,ZSCAL)
      CALL PUOPEN
C-
C--- BUILDING BLOCKS
C-
      DO 100 IPHI=NXMIN,NX
        J = NX - IPHI + NXMIN
        TXMIN=FLOAT(J)
        TXMAX=TXMIN+1.0
        DO 100 IETA=NYMIN,NY
          CALL JPINTR(1)
          CALL PXCOLR('FOR') ! DEFAULT COLOR
          TYMIN=FLOAT(IETA)
          TYMAX=TYMIN+1.
          TZMIN=0.
          TZMAX=0.
          ICODE = IARRAY(J,IETA)
          IF (ICODE .LE. 0) GO TO 100
C--- code=9 FOR JET
          IF (ICODE.EQ.9)THEN
C--- DRAW JET SIZE
            CALL PXCOLR('FOR')
            CALL JPINTR(0)
            RAD=3.
            CALL JCIRCL(TXMIN+.5,TYMIN+.5,TZMIN,RAD,0)
          ENDIF
C--- DEFINE BAR
          IF (ARRAY(J,IETA) .LT. ETMIN) GO TO 60
          TZMAX = ARRAY(J,IETA)/(ZMAX*ZSCAL)
C-
C--- Draw Jets
          IF (ICODE.EQ.9) THEN  
            CALL PXCOLFILL('CYA')
          ELSEIF (ICODE.EQ.12) THEN  
            CALL PXCOLFILL('RED')
          ENDIF
C-
          CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,0)
   60     CONTINUE
  100 CONTINUE
C-
C--- Draw Muon with checking overlapped PMUO to JETS in (IPHI,IETA)
C-
      K = 0
  120 K = K + 1
      IF (XPMUO(1,K).EQ.0. .OR. K.GT.50) GO TO 150
      INDP = XPMUO(1,K)
      INDE = XPMUO(2,K)
      TXMIN = XPMUO(1,K)
      TXMAX = TXMIN + 1.
      TYMIN = XPMUO(2,K)
      TYMAX = TYMIN + 1.
      IF (ARRAY(INDP,INDE) .GT. 0.) THEN
        TXMAX = TXMAX - .5
        TYMIN = TYMIN + .5
      ENDIF
      TZMIN = 0.
      TZMAX = XPMUO(3,K)/(ZMAX*ZSCAL)
      CALL PXCOLFILL('GRE')
      CALL JSIZE(XSIZ,YSIZ)
      TXDEL=(TXMAX-TXMIN)/2.
      TYDEL=(TYMAX-TYMIN)/2.
      CALL J3MOVE(TXMIN+TXDEL,TYMIN+TYDEL,TZMAX+TZDEL)
      CALL JJUST(2,2)
      CALL J1STRG('MUO')
C--- Draw vertical line topoint to bin
      CALL J3MOVE(TXMIN+TXDEL,TYMIN+TYDEL,TZMAX+(TZDEL/2.))
      CALL J3DRAW(TXMIN+TXDEL,TYMIN+TYDEL,TZMAX)
      CALL PLDBAR(3,TXMIN,TXMAX,TYMIN,TYMAX,TZMIN,TZMAX,0)
      GO TO 120
  150 CONTINUE
C-
C--- SPECIAL MARKS FOR OBJECTS
C-
      DO 200 IPHI=NXMIN,NX
        J = NX - IPHI + NXMIN
        DO 200 IETA=NYMIN,NY
          ICODE = IARRAY(J,IETA)
          IF (ICODE .NE. 0) THEN
            TXMIN=FLOAT(J)
            TXMAX=TXMIN+1.
            TYMIN=FLOAT(IETA)
            TYMAX=TYMIN+1.
            TZMIN=0.
            TZMAX = ARRAY(J,IETA)/(ZMAX*ZSCAL)
            TXMID=TXMIN +(TXMAX-TXMIN)/2.
            TYMID=TYMIN +(TYMAX-TYMIN)/2.
            CALL PXCOLR('FOR')
            IF (ICODE.EQ.12) THEN   ! ELE
              CALL JSIZE(XSIZ,YSIZ)
              CALL J3MOVE(TXMIN,TYMIN,TZMAX+1.)
              CALL JJUST(2,2)
              CALL J1STRG('ELE')
              CALL J3MOVE(TXMID,TYMID,TZMAX+.5)
              CALL J3DRAW(TXMID,TYMID,TZMAX)
            ELSEIF (ICODE.EQ.16) THEN   ! TAU
              CALL JSIZE(XSIZ,YSIZ)
              CALL J3MOVE(TXMIN,TYMIN,TZMAX+1.)
              CALL JJUST(2,2)
              CALL J1STRG('TAU')
              CALL J3MOVE(TXMID,TYMID,TZMAX+.5)
              CALL J3DRAW(TXMID,TYMID,TZMAX)
            ENDIF
          ENDIF
  200 CONTINUE
C-
      ZDIV=ZMAX*ZSCAL
      TXMIN=0.
      TXMAX=XMAX*10.+1.16815
      TYMIN=0.
      TYMAX=YMAX*20.+1.
C-
      CALL JRCLOS
C--- PRINTING AXIS
      CALL PLABEL(VMIN,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,XLAB,YLAB,ZLAB,
     X       TYMAX,TXMAX,TZMAX,NX,NXMIN,NY,NYMIN,ZSCAL)
C--- PRINTING TITLE(NOTE: PXTITL SETS VIEWING PARAMETES TO X-Y VIEW)
      CALL PXTITL(PLTITL)
C-
C--- Drawing the legend
C
      WRITE(MESS1,2000) ETMIN
 2000 FORMAT('PHYDIS ETMIN= ',F6.2)
      CALL PCTEXT(1,MESS1)
      CALL LEGEND_DST_LEGO(NTYP)
  999 RETURN
      END
