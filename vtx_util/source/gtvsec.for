      SUBROUTINE GTVSEC(LAYER,SECTOR,OPT,JLOC,NEL,NWORDS,CONT)
C------------------------------------------------------------------------  
C
C  Fetch contents of Zebra bank VSEC (bank of hits in a VTX sector)
C  Input:  LAYER,SECTOR
C          OPT    ='SEC'   fetch number of hits in sector
C          OPT    ='WIR'   fetch number of hits and pointer for each wire
C          OPT    ='HIT'   fetch hit 
C          OPT    ='ALL'   fetch all hits 
C          JLOC   = pointer to hit to be fetched (dummy for OPT.NE.'HIT') 
C                     
C  Output: NEL                      = number of sense wires
C          NWORDS                   = number of words per hit
C          OPT='SEC' CONT(1)        = number of hits in sector           
C          OPT='WIR' CONT(1:8)      = number of hits on each wire        
C                    CONT(9:16)     = pointer for 1-st hit on each wire  
C          OPT='HIT' CONT(1:NWORDS) = hit paramereters 
C          OPT='ALL' CONT(1:NWORDS,1:NHIT) = hit paramereters for all hits
C                     
C  Daria Zieminska MAR., 1987
C-   Updated  26-SEP-1991   Daria Zieminska  check link value 
C
C------------------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER LAYER,SECTOR,JLOC,NHIT,NEL,NWORDS,JJ,LOC,GZVSEC
      REAL CONT(*)
      CHARACTER*(*) OPT
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      LOC=GZVSEC(LAYER,SECTOR)
      IF (LOC.LE.0) THEN
        CONT(1)=0
        GO TO 1000
      END IF
      NEL=IQ(LOC+2)
      NWORDS=IQ(LOC+3)
      IF (OPT.EQ.'SEC') THEN
        CALL UCOPY_i(IQ(LOC+1),CONT,1)
        GO TO 1000
      END IF
      IF (OPT.EQ.'WIR') THEN  
        CALL UCOPY_i(IQ(LOC+4),CONT(1),NEL)      
        CALL UCOPY_i(IQ(LOC+4+NEL),CONT(1+NEL),NEL)      
        GO TO 1000
      END IF  
      IF (OPT.EQ.'HIT') THEN
        CALL UCOPY(Q(LOC+JLOC),CONT,NWORDS)
        GO TO 1000
      END IF  
      IF (OPT.EQ.'ALL') THEN
        NHIT=IQ(LOC+1)
        JJ=2*NEL+4
        CALL UCOPY(Q(LOC+JJ),CONT,NWORDS*NHIT)
      END IF
 1000 RETURN
      END
