       SUBROUTINE PCDST_LEGO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make LEGO plot from DST banks
C-                         JETS,PELC,MUO and PNUT
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  11-SEP-1991 Sharon Hagopian
C-   Updated   4-OCT-1991 Lupe Howell  The box legend implemented instead of
C-                        line
C-   Updated  21-NOV-1991 j.f. det÷uf Modified legend
C-                                    6 lines (particle number for each type)
C-   Updated  14-JAN-1992 j.f. det÷uf, Eliminated second array
C-   Updated  24-FEB-1992 S. Hagopian -changed from boxs to arrows 
C-                                     for particle tracks
C-   Updated  30-MAR-1992   Lupe Howell  Remobe BYT (not used) Clean up
C-   Updated   1-JUN-1992   S. Hagopian  added Photons
C-   Updated  16-JUN-1992   S.H.?  Legend moved to SUBROUTINE LEGEND_LEGO
C-   Modified  4-FEB-1994   N. Oshima - Modified for updated PCDST_LFILL
C-                                      and P4LEGO
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C----------------------------------------------------------------------
      CHARACTER*3 COL1, COL2
      CHARACTER*24 XLAB,YLAB,ZLAB
      CHARACTER*14 PLTITL
      CHARACTER*24 MESS1
      CHARACTER*8  CEMIN
      CHARACTER*24 MESS
      INTEGER NTYP(9),NMARK(4)
C-
      INTEGER NX,NY,IMARK
      INTEGER NXMIN,NYMIN,N
      INTEGER NXG,NYG
      INTEGER GZCAEP, LCAEP,LDCAEP,NRP,NCH
      INTEGER IOK
      INTEGER I,ICOLOR(4)
      INTEGER RUNSAVE,IDSAVE
C-
      REAL XMIN,XMAX,YMIN,YMAX,ZMAX
      REAL ZSCAL,RLEVEL
      REAL ETMIN   ! MINIMUM ET TO APPEAR IN LEGO PLOT
C-
      REAL ARRAY(NPHIL,2*NETAL),XPMUO(3,50)
      INTEGER IARRAY(NPHIL,2*NETAL),J
C-
      SAVE RUNSAVE,IDSAVE
      DATA RUNSAVE,IDSAVE/-1,-1/
      DATA IMARK/1/
C----------------------------------------------------------------------
C-
C
      CALL PUGETV('PHYDIS ETMIN',ETMIN)
      NY=NETAL*2
      NX=NPHIL
      YMIN=-NETAL
      YMAX=NETAL
      XMIN=0.
      XMAX=TWOPI
      YMIN=YMIN/10.
      YMAX=YMAX/10.
      ZMAX=-1.
      PLTITL='ET DST ETA-PHI'
      XLAB='PHI'
      YLAB='ETA'
      ZLAB='ET'
      NXMIN=1
      NYMIN=1
      NXG=1
      NYG=1
      N=NX
      ZSCAL=.2
      CALL PCDST_LFILL(ARRAY,IARRAY,XPMUO,NTYP,IOK)
C---
      CALL P4LEGO(NX,XMIN,XMAX,NY,YMIN,YMAX,ETMIN,ZMAX,PLTITL,
     &     XLAB,YLAB,ZLAB,ARRAY,IARRAY,XPMUO,
     &     NXMIN,NYMIN,NXG,NYG,N,ZSCAL,NMARK)
C Store Number of marks
      NTYP(3)=NMARK(1)
      NTYP(4)=NMARK(2)
      NTYP(7)=NMARK(3) 
      NTYP(8)=NMARK(4)
C
C ****  Drawing the legend
C
      WRITE(MESS1,200) ETMIN
  200 FORMAT('PHYDIS ETMIN= ',F6.2)
      CALL PCTEXT(1,MESS1)
      CALL LEGEND_DST_LEGO(NTYP)
  999 RETURN
      END
C =================================================== end  pcdst_lego
