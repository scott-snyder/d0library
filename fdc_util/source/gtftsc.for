      SUBROUTINE GTFTSC(HALF,QUAD,SECTOR,OPT,JLOC,NEL,NWORDS,CONT)
C------------------------------------------------------------------------
C
C  Fetch contents of Zebra bank FTSC (bank of hits in one FDC THETA sector)
C  Input:  HALF,QUAD,SECTOR
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
C-   Created   x-DEC-1988   Daria Zieminska 
C-   Updated   5-FEB-1990   Jeffrey Bantly  uses GZFTSC, all paths
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format 
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER HALF,QUAD,SECTOR,JLOC,NHIT,NEL,NWORDS,JJ,LKFTSC
      INTEGER GZFTSC
C
      REAL CONT(*)
C
      CHARACTER*(*) OPT
C------------------------------------------------------------------------
      LKFTSC=GZFTSC(HALF,QUAD,SECTOR)
      IF(LKFTSC.EQ.0) THEN
        CONT(1)=0
        GOTO 1000
      ENDIF
      NEL=IQ(LKFTSC+2)
      NWORDS=IQ(LKFTSC+3)
      IF (OPT.EQ.'SEC') THEN
        CALL UCOPY(IQ(LKFTSC+1),CONT,1)
        GO TO 1000
      END IF
      IF (OPT.EQ.'WIR') THEN
        CALL UCOPY(IQ(LKFTSC+4),CONT(1),NEL)
        CALL UCOPY(IQ(LKFTSC+4+NEL),CONT(1+NEL),
     X             NEL)
        GO TO 1000
      END IF
      IF (OPT.EQ.'HIT') THEN
        CALL UCOPY(Q(LKFTSC+JLOC+1),CONT,NWORDS)
        GO TO 1000
      END IF
      IF (OPT.EQ.'ALL') THEN
        NHIT=IQ(LKFTSC+1)
        JJ=2*NEL+4
        CALL UCOPY(Q(LKFTSC+JJ),CONT,NWORDS*NHIT)
      END IF
C--------------------------------------------------------------------------
 1000 RETURN
      END
