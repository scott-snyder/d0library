      SUBROUTINE FRHITS_GEAN(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To mark hit banks on roads in geant
C-      and possibly do simple hit smearing, etc.
C-
C-   Inputs  : ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX
C-   Outputs : 
C-   Controls: 
C-
C-   Created  13-OCT-1993   Robert E. Avery
C-   Updated  17-AUG-1994   Susan K. Blessing  Remove call to FLFSEC 
C-    (done in FDROAD), replace ON array with call to FSECTOR_IN_ROAD.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C Input
      REAL ZVTX
      REAL PHIMIN,PHIMAX,THEMIN,THEMAX
C Local:
      INTEGER HALF,UNIT,QUAD,SECTOR
      INTEGER MAX_QUAD(0:1), MAX_SECT(0:1)
      INTEGER LFXSC,GZFXSC
      INTEGER STAT
      INTEGER NCHAN ,NWORDS ,NHIT_W0 ,PTR_W0 ,IHIT
C
      LOGICAL FULL,DONE
      LOGICAL FSECTOR_IN_ROAD
C
      CHARACTER*4 PATH
C
      SAVE MAX_QUAD, MAX_SECT 
      DATA MAX_QUAD / MXQUAD, 0 /
      DATA MAX_SECT / MXSECT, MXSECP /
C----------------------------------------------------------------------
      CALL PATHGT(PATH)
      IF ( PATH .NE. 'GEAN' ) GOTO 999  
C
C  Find sectors on the road
C
      IF ( PHIMIN.NE.PHIMAX ) THEN 
        FULL=.FALSE.
      ELSE
        FULL=.TRUE.
      ENDIF
C
      DO HALF = 0,1
        DO UNIT = 0,1
          DO QUAD =  0,MAX_QUAD(UNIT)
            DO  SECTOR =  0, MAX_SECT(UNIT)
              IF (FULL.OR.FSECTOR_IN_ROAD(HALF,UNIT,QUAD,SECTOR)) THEN
C
C Mark hitfinding 'done'
C
                CALL FHITCHK(HALF,UNIT,QUAD,SECTOR,1,DONE)
                LFXSC=GZFXSC(HALF,UNIT,QUAD,SECTOR)
                IF (LFXSC.GT.0) THEN
C
C Flag sectors on roads
C
                  STAT = IQ(LFXSC)               
                  IQ(LFXSC) = IBSET(STAT,ION)
C
C  Mark DL hits:
C
                  IF ( UNIT.EQ.0 ) THEN
                    NCHAN = IQ(LFXSC+2)
                    NWORDS = IQ(LFXSC+3)
                    NHIT_W0 = IQ(LFXSC+4)
                    PTR_W0 = LFXSC + IQ(LFXSC+NCHAN+4) - 1
                    DO IHIT=1,NHIT_W0
                      IF ( Q(PTR_W0+4).NE.0.0 ) THEN
                        IQ(PTR_W0+9) = IBSET(IQ(PTR_W0+9),0)
                        IQ(PTR_W0+9) = IBSET(IQ(PTR_W0+9),1)
                      ENDIF
                      PTR_W0 = PTR_W0 + NWORDS 
                    ENDDO
                  ENDIF
C
C  One can include smearing routines (e.g. SMFPSC,SMFTSC) here.
C
                ENDIF
              END IF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
  999 RETURN
      END
