      INTEGER FUNCTION GZZTRH
C-------------------------------------------------------------------
C
C  Returns pointer to Zebra bank ZTRH ( header for central tracks)
C
C  Daria Zieminska May 1987                         
C                  updated Oct 1988               
C------------------------------------------------------------------
      IMPLICIT NONE                           
      INTEGER LPROC,GZPROC 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZZTRH.LINK/LIST'
      GZZTRH=0
      LPROC = GZPROC()
      IF (LPROC.GT.0) GZZTRH=LQ(LPROC-IZZTRH)
      RETURN
      END   
