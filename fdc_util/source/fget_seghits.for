C VAX/DEC CMS REPLACEMENT HISTORY, Element FGET_SEGHITS.FOR
C *1     4-NOV-1993 10:56:59 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FGET_SEGHITS.FOR
      SUBROUTINE FGET_SEGHITS(HALF,LAYER,SEG,NHIT,PTR_SC,PTR_DA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a segment (HALF,LAYER,SEG), 
C-              return the number of hits and pointers to hits. 
C-              Pointers to delay lines in DA are saved in 
C-              PTR_DA(NHIT+1) and PTR_DA(NHIT+2)
C-
C-   Inputs  :  HALF, LAYER, SEG
C-   Outputs :  NHIT        Number of hits.
C-              PTR_SC(16)  Pointers to FTSC or FPSC banks
C-              PTR_DA(16)  Pointers to FTDA or FPDA banks
C-
C-   Created   8-OCT-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER MAX_SEGHIT
      PARAMETER( MAX_SEGHIT = 16 )
C
C  INPUT:
      INTEGER HALF,LAYER,SEG
C  OUTPUT:
      INTEGER PTR_SC(MAX_SEGHIT),PTR_DA(MAX_SEGHIT),NHIT
C
C  LOCAL:
      INTEGER LSEGM,LZFIND,GZFSEG
      INTEGER LFXSC,LFXDA
      INTEGER GZFXSC,GZFXDA
      INTEGER IADD       
      INTEGER HA,UNIT,QDRT,SCTR,WIRE,UBIT
      INTEGER HIT,XHIT,DL
      INTEGER NSEN(0:1)
      CHARACTER*4 PATH
C
      DATA NSEN /8,16/
C----------------------------------------------------------------------
C
      CALL VZERO(PTR_SC,MAX_SEGHIT)
      CALL VZERO(PTR_DA,MAX_SEGHIT)
C
      LSEGM = GZFSEG(HALF,LAYER)
      IF (LSEGM .LE. 0) GOTO 999
      LSEGM = LZFIND(IXCOM,LSEGM,SEG,-5)
      IF (LSEGM .LE. 0) GOTO 999
C
      CALL PATHGT(PATH)
      XHIT = IQ(LSEGM+1)/1000
      IADD = IQ(LSEGM+2)
      NHIT = MIN(MAX_SEGHIT,IQ(LSEGM+3))
C
      CALL FCODER(IADD,HA,UNIT,QDRT,SCTR,WIRE,UBIT,1)
      LFXSC = GZFXSC(HALF,UNIT,QDRT,SCTR)
      LFXDA = GZFXDA(HALF,UNIT,QDRT,SCTR)
      DO HIT =  1, NHIT
        IF ( HIT .EQ. ABS(XHIT) ) THEN  ! Crosses sector
          SCTR = XHIT/ABS(XHIT) + SCTR
          LFXSC = GZFXSC(HALF,UNIT,QDRT,SCTR)
          LFXDA = GZFXDA(HALF,UNIT,QDRT,SCTR)
        ENDIF
        PTR_SC(HIT) = LFXSC + IQ(LSEGM+3+NSEN(UNIT)+HIT)
        IF (PATH.NE.'GEAN') THEN
          PTR_DA(HIT) = LFXDA + IQ(PTR_SC(HIT) +10)             
          IF(UNIT.EQ.0 .AND. IADD.EQ.IQ(PTR_DA(HIT)+1) ) THEN
            DL = IQ(PTR_SC(HIT)+11)
            IF (DL.NE.0) PTR_DA(NHIT+1) = LFXDA+DL
            DL = IQ(PTR_SC(HIT)+12)
            IF (DL.NE.0) PTR_DA(NHIT+2) = LFXDA+DL
          ENDIF
        ENDIF
      ENDDO
      GOTO 999
C
  999 RETURN
      END
