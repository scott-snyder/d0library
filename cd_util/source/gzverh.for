      INTEGER FUNCTION GZVERH
C-------------------------------------------------------------------
C
C  Returns pointer to Zebra bank VERH ( header for central vertices)
C
C  Daria Zieminska Nov 1988                         
C------------------------------------------------------------------
      IMPLICIT NONE                           
      INTEGER LPROC,GZPROC 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZVERH.LINK/LIST'
      GZVERH=0
      LPROC = GZPROC() 
      IF (LPROC.GT.0) GZVERH=LQ(LPROC-IZVERH)
 1000 RETURN
      END   
