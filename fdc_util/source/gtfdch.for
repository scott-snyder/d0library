      SUBROUTINE GTFDCH(NHIT)
C-----------------------------------------------------------------  
C          
C  Fetch contents of Zebra bank FDCH 
C
C  Output: NHIT  = number of hits in Forward Drift Chamber   
C                     
C-   Created  ??-DEC-1988   Daria Zieminska
C-   Updated  26-FEB-1990   Jeffrey Bantly  check link value 
C
C-----------------------------------------------------------------
      IMPLICIT NONE           
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INTEGER NHIT,LFDCH
      INTEGER GZFDCH
C-----------------------------------------------------------------
      NHIT=0
      LFDCH=GZFDCH()
      IF(LFDCH.NE.0) NHIT=IQ(LFDCH+1)
C-----------------------------------------------------------------
      RETURN
      END
