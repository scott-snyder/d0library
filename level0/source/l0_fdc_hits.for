      SUBROUTINE L0_FDC_HITS(NSECT,LOCATION,MINTRKS,AVGHITS,AVGTRKS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Predict the likely number of tracks passing 
C-                         through a level 0 scintillator pad.
C-
C-   Inputs  : NSECT
C-             LOCATION
C-   Outputs : NHITS
C-   Controls: none
C-
C-   Created   6-AUG-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NSECT
      INTEGER LOCATION(50,4)
      INTEGER HALF,UNIT,QUAD,SECT,WIRE
      INTEGER JLOC,NEL,NWORDS
      INTEGER CONT(50)
      INTEGER MINHITS,MINTRKS
      INTEGER TOTHITS(0:2),TOTTRKS
      INTEGER POSTRKS(0:2)
      INTEGER ISECT,LAYER
C
      REAL    AVGHITS(0:2),AVGTRKS
C
C----------------------------------------------------------------------
C
      CALL VZERO(TOTHITS(0),3)
      CALL VZERO(POSTRKS(0),3)
C
C  Accumulate the total hits per wire in the chosen sectors and the minimum
C  number of hits on a wire in the sector.
C
      DO ISECT=1,NSECT
        HALF=LOCATION(ISECT,1)
        UNIT=LOCATION(ISECT,2)
        QUAD=LOCATION(ISECT,3)
        SECT=LOCATION(ISECT,4)
        LAYER=0
        IF ( UNIT.EQ.0 .AND. QUAD.GE.4 ) LAYER=1
        IF ( UNIT.EQ.1 ) LAYER=2
        IF ( UNIT.EQ.0 ) THEN
          CALL GTFTSC(HALF,QUAD,SECT,'WIR',JLOC,NEL,NWORDS,CONT)
        ELSE
          CALL GTFPSC(HALF,SECT,'WIR',JLOC,NEL,NWORDS,CONT)
        ENDIF
        IF ( NEL.GT.0 ) THEN
          MINHITS=CONT(1)
          DO WIRE=1,NEL
            IF ( CONT(WIRE).LT.MINHITS ) MINHITS=CONT(WIRE)
            TOTHITS(LAYER)=TOTHITS(LAYER)+CONT(WIRE)
          ENDDO
          POSTRKS(LAYER)=POSTRKS(LAYER)+MINHITS
        ENDIF
      ENDDO
C
      MINTRKS=POSTRKS(0)
      TOTTRKS=0
C
C  Estimate the average number of hits per layer and average number of tracks
C  per layer.
C
C
      DO LAYER=0,2
        IF ( POSTRKS(LAYER).LT.MINTRKS ) MINTRKS=POSTRKS(LAYER)
        TOTTRKS=TOTTRKS+POSTRKS(LAYER)
        AVGHITS(LAYER)=FLOAT(TOTHITS(LAYER))/8.
        IF ( LAYER.EQ.2 ) AVGHITS(LAYER)=FLOAT(TOTHITS(LAYER))/16.
      ENDDO
      AVGTRKS=FLOAT(TOTTRKS)/3.
C
C----------------------------------------------------------------------
  999 RETURN
      END
