      SUBROUTINE LDPSEG(HALF,SECTOR,NHIT,LRWIR,IHIT,PHI,DR1,Z1,RESID,
     &  SLOPE,INTER,CHISQ,AVE_ION,ERROR)
C-----------------------------------------------------------------------
C
C    Purpose and Methods : Load track segment found in FDC PHI unit  
C                          to Zebra bank :                 FSG2  FSG5      
C                          for segment in Phi unit in HALF = 0     1     
C 
C  Input:  HALF,SECTOR = Logical location of segment's sector
C          NHIT          = Number of hits in segment
C          LRWIR(1:NHIT) = WIRE*2+LR (LR = 0/1 for phi(hit) >/< phi(wire))
C          IHIT (1:NHIT) = pointer to hit in bank FPSC
C          PHI           = arctan(dy/dx) for the segment 
C                          (approximated by phi of wire)    
C          DR1           = drift distance for first hit
C          Z1            = z of first wire on segment
C-         SLOPE         = Slope of segment fit
C-         INTER         = Intercept of segment fit
C-         CHISQ         = CHISQ/DOF of segment fit
C-         AVE_ION       = Average ionization/hit of segment
C          ERROR         = Error on drift distance measurements
C
C-   Created  xx-DEC-1988   Daria Zieminska 
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format & path dep 
C-   Updated  21-MAR-1990   Jeffrey Bantly  add hit residuals to bank 
C-   Updated   3-MAY-1990   Jeffrey Bantly  use IXCOM not 0 in MZ calls 
C-   Updated  26-APR-1991   Jeffrey Bantly  mark hits used on segments 
C-   Updated   9-MAY-1991   Susan K. Blessing  Remove booking of FSGx bank 
C-   Updated  17-JUN-1991   Susan K. Blessing  Add SLOPE,INTER,CHISQ of 
C-    segment fit.  Add AVE_ION for the average ionization/hit of the segment.
C-   Updated  17-OCT-1991   Robert E. Avery  Add Position Error
C-    to segement bank.
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER LOGCHA
      INTEGER HALF,SECTOR,MODULE,LAYER,NHIT,LRWIR(*),IHIT(*)
      INTEGER LSEG(0:5),LSEGM,ID,NSEGM
      INTEGER LKFTRH,ISECT,IHITD
      INTEGER GZFTRH,GZFSEG,LZFIND,NZBANK
C
      REAL PHI,DR1,Z1,RESID(*)
      REAL SLOPE,INTER,CHISQ,AVE_ION,ERROR
C
      SAVE LAYER
      DATA LAYER/2/
C
C-----------------------------------------------------------------------
C
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
      CALL FCODER(LOGCHA,HALF,1,0,SECTOR,0,0,2)
C
C  Store segment values
C
      IQ(LSEGM-5) = NSEGM 
      IQ(LSEGM+1) = ISECT
      IQ(LSEGM+2) = LOGCHA
      IQ(LSEGM+3) = NHIT                  ! number of hits on track segment
      DO 200 ID = 1,NHIT        
        IQ(LSEGM+3+ID) = LRWIR(ID)
        IQ(LSEGM+19+ID) = IHIT(ID) - 1
         Q(LSEGM+38+ID) = RESID(ID)
 200  CONTINUE
      DO 300 ID = NHIT+1,16               ! dummy remaining spaces
        IQ(LSEGM+19+ID) = 999
         Q(LSEGM+38+ID) = 9999.
 300  CONTINUE               
      Q(LSEGM+36) = PHI   
      Q(LSEGM+37) = DR1 
      Q(LSEGM+38) = Z1 
      Q(LSEGM+55) = SLOPE
      Q(LSEGM+56) = INTER
      Q(LSEGM+57) = CHISQ
      Q(LSEGM+58) = AVE_ION
      Q(LSEGM+59) = ERROR         
C
C Fill in extra words with zeros. 
      DO ID = 1, 3
        Q(LSEGM+59+ID) = 0.
      END DO
C
C  Mark hits in hit banks with segment number and LR ambiguity used.
C
      CALL FMARK_SEGHITS(MODULE,NSEGM,NSEGM,1)
C
C------------------------------------------------------------------------
  999 RETURN
      END
