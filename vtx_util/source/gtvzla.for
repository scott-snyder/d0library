      SUBROUTINE GTVZLA(LAYER,OPT,IHIT,NSTRIP,NWORDS,CONT)
C------------------------------------------------------------------------  
C
C  Fetch contents of Zebra bank VZLA (bank of hits in a VTX z-strip layer)
C  Input:  LAYER
C          OPT    ='LAY'   fetch number of hits in layer
C          OPT    ='HIT'   fetch hit 
C          OPT    ='ALL'   fetch all hits 
C          IHIT   = hit to be fetched (dummy for OPT.NE.'HIT') 
C                     
C  Output: if OPT='LAY' CONT(1)    = number of hits in layer           
C          if OPT='HIT' CONT(1:NWORDS) = hit paramereters 
C                     
C    Daria Zieminska   MAY 1987
C
C------------------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER LAYER,IHIT,JLOC,NHIT,NSTRIP,NWORDS
      REAL CONT(*)
      CHARACTER*(*) OPT
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'                             
C                      
      IF (OPT.EQ.'LAY') THEN
        NHIT=IQ(LVZLA(LAYER)+1)
        CONT(1)=FLOAT(NHIT)
      END IF
      NSTRIP=IQ(LVZLA(LAYER)+2)
      NWORDS=IQ(LVZLA(LAYER)+3)
      IF (OPT.EQ.'HIT') THEN
        JLOC=4+NWORDS*(IHIT-1)
        CALL UCOPY(Q(LVZLA(LAYER)+JLOC),CONT,NWORDS)
      END IF
      IF (OPT.EQ.'ALL') THEN
        CALL UCOPY(Q(LVZLA(LAYER)+4),CONT,NWORDS*IQ(LVZLA(LAYER)+1))
      END IF
 1000 RETURN                              
      END
