      SUBROUTINE LDTSEG(HALF,QUAD,SECTOR,NHIT,LRWIR,IHIT,
     &  PHI,THETA,RESID,SLOPE,INTER,CHISQ,AVE_ION,NDELAY,
     &  ERROR, DL_DIST, DL_ERROR)
C-----------------------------------------------------------------------
C
C    Purpose and Methods : Load Theta track segment to Zebra bank FSG0,
C                          FSG1, FSG3, or FSG4 
C                                                      FSG0 FSG1 FSG3 FSG4
C                          for segment in Theta Layer = 0    1    0    1  
C                                         HALF          0    0    1    1
C                           
C  Input:  HALF,QUAD,SECTOR = Logical location of segment's sector
C          NHIT          = Number of hits in segment
C          LRWIR(1:NHIT) = WIRE*2+LR (LR = 0/1 for phi(hit) >/< phi(wire))
C          IHIT (1:NHIT) = pointer to hit in bank FTSC
C          PHI           = arctan(dy/dx) for the segment 
C                          (approximated by phi of wire)    
C          THETA         = theta angle of segment to x,y,z = 0.
C          RESID(1:NHIT) = Residuals of hits on segment
C-         SLOPE         = Slope of segment fit
C-         INTER         = Intercept of segment fit
C-         CHISQ         = CHISQ/DOF of segment fit
C-         AVE_ION       = Average IONIZATION/HIT of segment
C          NDELAY        = Number of delay line hits used for segment (0,1,2)
C          ERROR         = Error on drift distance measurements
C          DL_DIST       = Delay line measurement
C          DL_ERROR      = Delay line measurement error
C
C-   Created  xx-DEC-1988   Daria Zieminska 
C-   Updated  27-FEB-1990   Jeffrey Bantly  use logical format 
C-   Updated  21-MAR-1990   Jeffrey Bantly  add residuals 
C-   Updated   3-MAY-1990   Jeffrey Bantly  use IXCOM not 0 in MZ calls 
C-   Updated  26-APR-1991   Jeffrey Bantly  mark hits used on segment 
C-   Updated   9-MAY-1991   Susan K. Blessing  Remove booking of FSGx bank 
C-   Updated  17-JUN-1991   Susan K. Blessing  Add SLOPE,INTER,CHISQ of 
C-    segment fit.  Add AVE_ION for the average ionization/hit of the segment.
C-   Updated  17-OCT-1991   Robert E. Avery  Add Position Error,
C-    Delay Line Position, and Delay Line Error to segement bank.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS/LIST'
C
      INTEGER HALF,QUAD,SECTOR,MODULE,LAYER,NHIT,LRWIR(*),IHIT(*)
      INTEGER LSEG(0:5),LSEGM,ID,NSEGM
      INTEGER LKFTRH,LOGCHA,ISECT,IHITD
      INTEGER GZFTRH,GZFSEG,NZBANK
      INTEGER NDELAY
      INTEGER STAT
C
      REAL PHI,THETA,RESID(*)
      REAL SLOPE,INTER,CHISQ,AVE_ION
      REAL ERROR, DL_DIST, DL_ERROR
C
C-----------------------------------------------------------------------
C
      LAYER = 0
      IF (QUAD.GE.4) LAYER = 1
      MODULE = LAYER+3*HALF
C
C  Book bank for new segment
C
      CALL BKFSEG(HALF,LAYER,LSEGM)
C
      LKFTRH = GZFTRH(0)
      LSEG(MODULE) = GZFSEG(HALF,LAYER)
      NSEGM = NZBANK(IXCOM,LSEG(MODULE))
      ISECT = SECTOR                      ! take care of cross-sector segs
      IHITD = SECTOR/1000
      SECTOR = ABS(SECTOR-1000*IHITD)
      CALL FCODER(LOGCHA,HALF,0,QUAD,SECTOR,0,0,2)
C
C  Store segment values
C
C Delay line information
      IF (NDELAY.GT.0) THEN
        STAT = 0
        IQ(LSEGM) = IBSET(STAT,IDELAY)
        IF (NDELAY.EQ.2) THEN
          STAT = IQ(LSEGM)
          IQ(LSEGM) = IBSET(STAT,INUMDEL)
        END IF
      END IF
C
      IQ(LSEGM-5) = NSEGM 
      IQ(LSEGM+1) = ISECT
      IQ(LSEGM+2) = LOGCHA          
      IQ(LSEGM+3) = NHIT                  ! number of hits on track segment
      DO 200 ID = 1,NHIT        
        IQ(LSEGM+3+ID) = LRWIR(ID)
        IQ(LSEGM+11+ID) = IHIT(ID) - 1
         Q(LSEGM+21+ID) = RESID(ID)
 200  CONTINUE
      DO 300 ID = NHIT+1,8                ! dummy remaining spaces 
        IQ(LSEGM+3+ID) = 18
        IQ(LSEGM+11+ID) = 999
         Q(LSEGM+21+ID) = 9999.
 300  CONTINUE               
      Q(LSEGM+20) = PHI   
      Q(LSEGM+21) = THETA 
      Q(LSEGM+30) = SLOPE
      Q(LSEGM+31) = INTER
      Q(LSEGM+32) = CHISQ
      Q(LSEGM+33) = AVE_ION
      Q(LSEGM+34) = ERROR         
      Q(LSEGM+35) = DL_DIST       
      Q(LSEGM+36) = DL_ERROR      
      Q(LSEGM+37) = 0.                  ! extra word
C
C  Mark hits in hit banks with segment number and LR ambiguity used.
C
      CALL FMARK_SEGHITS(MODULE,NSEGM,NSEGM,1)
C
C------------------------------------------------------------------------
  999 RETURN
      END
