      SUBROUTINE GTFPDA(HALF,SECTOR,OPT,JLOC,NEL,NWORDS,CONT)
C------------------------------------------------------------------------
C
C  Fetch contents of Zebra bank FPDA (bank of hit data in one FDC PHI sector)
C  Input:  HALF,SECTOR
C          OPT    ='SEC'   fetch number of hits in sector
C          OPT    ='WIR'   fetch number of hits and pointer for each wire
C          OPT    ='HIT'   fetch hit
C          OPT    ='ALL'   fetch all hits
C          JLOC   = pointer to hit to be fetched (dummy for OPT.NE.'HIT')
C
C  Output: NEL                      = number of sense wires
C          NWORDS                   = number of words per hit
C          OPT='SEC' CONT(1)        = number of hits in sector
C          OPT='WIR' CONT(1:16)      = number of hits on each wire
C                    CONT(17:32)     = pointer for 1-st hit on each wire
C          OPT='HIT' CONT(1:NWORDS) = hit paramereters
C          OPT='ALL' CONT(1:NWORDS,1:NHIT) = hit paramereters for all hits
C
C    Created  16-MAY-1989   Jeffrey Bantly
C-   Updated   5-FEB-1990   Jeffrey Bantly  uses GZFPDA, all paths
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format 
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER HALF,SECTOR,JLOC,NHIT,NEL,NWORDS,JJ,LKFPDA
      INTEGER GZFPDA
C
      REAL CONT(*)
C
      CHARACTER*(*) OPT
C------------------------------------------------------------------------
      LKFPDA=GZFPDA(HALF,SECTOR)
      IF(LKFPDA.EQ.0) THEN
        CONT(1)=0
        GOTO 1000
      ENDIF
      NEL=IQ(LKFPDA+2)
      NWORDS=IQ(LKFPDA+3)
      IF (OPT.EQ.'SEC') THEN
        CALL UCOPY_i(IQ(LKFPDA+1),CONT,1)
        GO TO 1000
      END IF
      IF (OPT.EQ.'WIR') THEN
        CALL UCOPY_i(IQ(LKFPDA+4),CONT(1),NEL)
        CALL UCOPY_i(IQ(LKFPDA+4+NEL),CONT(1+NEL),
     X             NEL)
        GO TO 1000
      END IF
      IF (OPT.EQ.'HIT') THEN
        CALL UCOPY(Q(LKFPDA+JLOC),CONT,NWORDS)
        GO TO 1000
      END IF
      IF (OPT.EQ.'ALL') THEN
        NHIT=IQ(LKFPDA+1)
        JJ=2*NEL+4
        CALL UCOPY(Q(LKFPDA+JJ),CONT,NWORDS*NHIT)
      END IF
C-----------------------------------------------------------------------
 1000 RETURN
      END
