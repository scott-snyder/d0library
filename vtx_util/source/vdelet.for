      SUBROUTINE VDELET(LAYER,SECTOR)
C------------------------------------------------------------------
C
C  Delete used links (and mirrors) in VTX cell (LAYER,SECTOR)
C
C  Input LAYER,SECTOR
C
C    D.Z. JAN.,1987                                               
C             
C------------------------------------------------------------------
      IMPLICIT NONE                           
      INTEGER LAYER,SECTOR,LLINK,NLINK,ILINK,LOC,I1,I2,IBSET 
      INTEGER IQHIT(18),NEL,NWORDS
      REAL QHIT(18)
      EQUIVALENCE (IQHIT,QHIT)
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'                             
      INTEGER LUSER 
      INTEGER LZFIND
      LUSER=LQ(LHEAD-IZUSER) 
      LLINK=LQ(LUSER-1)             
      NLINK=IQ(LUSER+1)
      DO 200 ILINK=1,NLINK        ! loop over links
        LOC=LZFIND(0,LLINK,ILINK,-5)                
        IF (LOC.LE.0) GO TO 200
        I1=IQ(LOC+1)/2            ! pointer to 1-st hit on link ILINK
        CALL GTVSEC(LAYER,SECTOR,'HIT',I1,NEL,NWORDS,QHIT)
        I1=IQHIT(10)
        IF (BTEST(I1,2)) THEN     ! check if 1-st hit used
          CALL MZDROP(IXCOM,LOC,' ')    
          GO TO 200
        END IF
        I2=IQ(LOC+2)/2            ! pointer to 2-nd hit on link ILINK
        CALL GTVSEC(LAYER,SECTOR,'HIT',I2,NEL,NWORDS,QHIT)
        I2=IQHIT(10)
        IF (BTEST(I2,2)) CALL MZDROP(IXCOM,LOC,' ')    
  200   CONTINUE
      RETURN    
      END    
