       SUBROUTINE PPTOP_LEGO(ITYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make LEGO plot from ISAQ banks of top decays
C-
C-   Inputs  : ITYPE 0 = ISP1
C-                   1 = ISAQ
C-   Outputs : None
C-
C-   Created 23-APR-1993   Chip Stewart
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C----------------------------------------------------------------------
      CHARACTER*3 COL1, COL2
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*19 PLTITL
      CHARACTER*24 MESS1
      CHARACTER*8  CEMIN
      CHARACTER*24 MESS
      CHARACTER*3 COLORS(6)*3,LABELS(6)*5
      INTEGER ICOLOR(4)
      INTEGER NTYP(9),NMARK(4)
C-
      INTEGER NX,NY,IMARK
      INTEGER NXMIN,NYMIN,N
      INTEGER NXG,NYG
      INTEGER GZCAEP, LCAEP,LDCAEP,NRP,NCH
      INTEGER IOK,ITYPE
      INTEGER RUNSAVE,IDSAVE
C-
      REAL XMIN,XMAX,YMIN,YMAX,ZMAX
      REAL ZSCAL,RLEVEL
      REAL ETMIN   ! MINIMUM ET TO APPEAR IN LEGO PLOT
C-
      REAL ARRAY(NPHIL,2*NETAL)
      INTEGER IARRAY(NPHIL,2*NETAL),I,J,K,NLAB,ICOLORS(6)
C-
      SAVE RUNSAVE,IDSAVE
      DATA RUNSAVE,IDSAVE/-1,-1/
      DATA IMARK/0/
      DATA NLAB/6/
      DATA LABELS/' W+',' B',' W-',' BB',' IR',' UNDL'/
C
C----------------------------------------------------------------------
C-
      CALL PUGETV('TOPDIS ETMIN',ETMIN)
      NY=NETAL*2
      NX=NPHIL
      YMIN=-NETAL
      YMAX=NETAL
      XMIN=0.
      XMAX=TWOPI
      YMIN=YMIN/10.
      YMAX=YMAX/10.
      ZMAX=-1.
      XLAB='PHI'
      YLAB='ETA'
      ZLAB='ET'
      NXMIN=1
      NYMIN=1
      NXG=1
      NYG=1
      N=NX
      ZSCAL=.2
      IF(ITYPE.EQ.1) THEN
        PLTITL='Et TOP ISAQ ETA-PHI'
        CALL PPTOP_QFILL(ARRAY,IARRAY,IOK)
      ELSE
        PLTITL='Et TOP ISP1 ETA-PHI'
        CALL PPTOP_PFILL(ARRAY,IARRAY,IOK)
      END IF

C
      CALL P6LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,ETMIN,ZMAX,PLTITL,
     &     XLAB,YLAB,ZLAB,ARRAY,IARRAY,
     &     NXMIN,NYMIN,NXG,NYG,N,ZSCAL,IMARK)
C
C ****  Drawing the legend
C
      WRITE(MESS1,200) ETMIN
  200 FORMAT('TOPDIS ETMIN= ',F6.2)
      CALL PUMESS(MESS1)
      CALL PUGETA('TOPDIS WCOL',COLORS(1))
      CALL PUGETA('TOPDIS BCOL',COLORS(2))
      CALL PUGETA('TOPDIS WBCOL',COLORS(3))
      CALL PUGETA('TOPDIS BBCOL',COLORS(4))
      CALL PUGETA('TOPDIS IRCOL',COLORS(5))
      CALL PUGETA('TOPDIS UNCOL',COLORS(6))
      CALL LEGEND(COLORS,LABELS,NLAB)
  999 RETURN
      END
