      SUBROUTINE FCHECK_DL(HALF,LAYER,SEG,NDELAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check the number of delay line hits associated
C-                         with a segment.
C-
C-   Inputs  : HALF
C-             LAYER
C-             SEG = Segment number in layer
C-   Outputs : NDELAY = number of associated delay line hits
C-   Controls:
C-
C-   Created  30-MAY-1991   Susan K. Blessing
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,LAYER,SEG
      INTEGER NDELAY
      INTEGER LSEG,LOC
      INTEGER GZFSEG,LZFIND
C
C----------------------------------------------------------------------
C
      NDELAY = 0
C
      IF (LAYER.EQ.2) GO TO 999
      IF (SEG.LE.0) GO TO 999
C
      LSEG = GZFSEG(HALF,LAYER)
      IF (LSEG.LE.0) GO TO 999
      LOC = LZFIND(IXCOM,LSEG,SEG,-5)
      IF (LOC.LE.0) GO TO 999
C
      IF (BTEST(IQ(LOC),IDELAY)) THEN
        IF (BTEST(IQ(LOC),INUMDEL)) THEN
          NDELAY = 2
        ELSE
          NDELAY = 1
        END IF
      ELSE
        NDELAY = 0
      END IF
C
  999 RETURN
      END
