      INTEGER FUNCTION DENSITY_OF_TRD_HITS(HIT_WIRE,NA,layer)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return number of hits in a region of the TRD
C-                         when THIT bank has been defined
C-
C-   Returned value  : number of hits (layers 0+1+2) in the region around
C-                     the hit wires (+ or - ncell arond the hit cells )
C-   Inputs  : hit_wire: hit cell numbers
C-             na      :nb. of hot cells
C-   Outputs : none
C-   Controls: none
C-
C-   Created  25-MAY-1993   Alain PLUQUET
C-   Updated   6-JAN-1994   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INTEGER layer,VCENTRAL_WIRE,TRD_PREVIOUS,TRD_NEXT,WIRE,HIT,JBYT,II
      INTEGER IUCOMP,NA,W1,tchnb
      INTEGER NCELL,HIT_WIRE(NA)
      PARAMETER( NCELL =  4)
      INTEGER WM1,WM2,WM3,WM4,WM5,WM6,WM7,WM8
      INTEGER WP1,WP2,WP3,WP4,WP5,WP6,WP7,WP8
      INTEGER LTHIT,GZTHIT,OFFSET,POLARITY
      INTEGER TOTAL_NUMBER_OF_HITS,NUMBER_OF_HITS_IN_REGION
      INTEGER NUMBER_OF_CLUSTERS,WMIN,WMAX
      INTEGER LOUT,TRUNIT
      REAL PHI
      LOGICAL ONE_MORE,FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        LOUT=TRUNIT()
        FIRST=.FALSE.
      END IF
      IF(NA.EQ.1)THEN
        WMIN=HIT_WIRE(1)
        WMAX=HIT_WIRE(1)
      ELSE
        WMIN=1000
        WMAX=0.
        DO HIT=1,NA
          WMIN=MIN0(HIT_WIRE(HIT),WMIN)
          WMAX=MAX0(HIT_WIRE(HIT),WMAX)
        END DO
C          write(lout,*)' wmin wmax',wmin,wmax
      ENDIF
      W1=WMIN
      NUMBER_OF_HITS_IN_REGION=0
      DO HIT=1,NCELL
        W1=TRD_PREVIOUS(W1,LAYER)
        if(twcod(tchnb(w1,layer)))NUMBER_OF_HITS_IN_REGION=
     &    NUMBER_OF_HITS_IN_REGION+1
      END DO
      W1=WMAX
      DO HIT=1,NCELL
        W1=TRD_NEXT(W1,LAYER)
        if(twcod(tchnb(w1,layer)))NUMBER_OF_HITS_IN_REGION=
     &    NUMBER_OF_HITS_IN_REGION+1
      END DO
      DENSITY_OF_TRD_HITS=NUMBER_OF_HITS_IN_REGION
  999 RETURN
      END
