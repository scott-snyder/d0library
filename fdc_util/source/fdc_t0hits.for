C VAX/DEC CMS REPLACEMENT HISTORY, Element FDC_T0HITS.FOR
C *1     4-NOV-1993 10:52:53 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FDC_T0HITS.FOR
      SUBROUTINE FDC_T0HITS(TZERO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a first guess of the tzero for an 
C-   event by looking for the earliest drift time of a hit in 
C-   all populated cells (a cell with more than half of the wires hit).
C-   (Then subtract a little more, since first hit probably comes later 
C-   than t0, even for segments crossing the sense plane).
C-
C-   Inputs  : none 
C-   Outputs : TZERO  
C-
C-   Created  25-MAY-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
C  ouput:
      REAL    TZERO
C
C  local:
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
      INTEGER MAX_QUAD(0:1), MAX_SECT(0:1)
      INTEGER MAX_WIRE(0:1), MAX_WIRE_HT(0:1)
      INTEGER LFXDA,GZFXDA
      INTEGER LFTSE,GZFTSE
      INTEGER IPTR, LHIT, NWIRE_HIT 
      INTEGER DUM,NEL,NWORDS
C
      REAL    MTIME_SEC, TIME
      REAL    TZERO_OFFSET
      PARAMETER( TZERO_OFFSET = 20 )    ! ns
C
      REAL    FWIRE(0:31)
      INTEGER NWIRE(0:31)
      EQUIVALENCE(NWIRE,FWIRE)
C
      DATA MAX_QUAD, MAX_SECT, MAX_WIRE, MAX_WIRE_HT 
     &  / MXQUAD, 0, MXSECT, MXSECP, MXWIRT, MXWIRP, 5, 9/
C----------------------------------------------------------------------
      TZERO = 5000.
      DO HALF = 0,1
        DO UNIT = 0,1
          DO QUAD =  0,MAX_QUAD(UNIT)
            DO  SECTOR =  0, MAX_SECT(UNIT)
              LFXDA=GZFXDA(HALF,UNIT,QUAD,SECTOR)
              IF(LFXDA.GT.0) THEN
                IF ( UNIT.EQ.0 ) THEN
                  CALL GTFTDA(HALF,QUAD,SECTOR,
     &              'WIR',DUM,NEL,NWORDS,FWIRE)
                ELSE
                  CALL GTFPDA(HALF,SECTOR,
     &              'WIR',DUM,NEL,NWORDS,FWIRE)
                ENDIF
C
                MTIME_SEC = 5000.
                NWIRE_HIT = 0
                DO WIRE =  0, MAX_WIRE(UNIT)
                  IF (NWIRE(WIRE).GT.0  ) THEN
C first hit:
                    LHIT = LFXDA + NWIRE(WIRE+NEL) - 1
                    NWIRE_HIT = NWIRE_HIT + 1
                    TIME = Q(LHIT+2)
                    MTIME_SEC = MIN(TIME,MTIME_SEC )
                  ENDIF
                ENDDO
C
                IF ( NWIRE_HIT.GT.MAX_WIRE_HT(UNIT) ) THEN
                  TZERO = MIN(TZERO ,MTIME_SEC )
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
      IF ( TZERO.LT.5000. ) THEN
        TZERO = TZERO - TZERO_OFFSET
        CALL FDC_T0SHIFT( TZERO, 1 )
      ELSE
        TZERO = 0.
      ENDIF
  999 RETURN
      END
