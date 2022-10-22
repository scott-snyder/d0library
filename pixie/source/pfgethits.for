      FUNCTION PFGETHITS(HALF,UNIT,QUAD,SECTOR,
     &                   N_HITS,DR_DIST,DL_OK,ON_SEG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For a given sector (phi or theta), return
C-      unpacked information concerning all of the hits in that 
C-      sector. Use FXSC bank if available, else look for compressed
C-      hits bank, FHIT.
C-
C-   Returned value  : .TRUE. if hits found in sector.
C-   Inputs  : HALF,UNIT,QUAD,SECTOR
C-   Outputs :  N_HITS          number hits on each wire
C-              DR_DIST         Drift position (left & right) for each hit
C-              DL_OK           True if DL postition exists for hit
C-              ON_SEG          True if hit on segment
C-
C-   Created   8-OCT-1991   Robert E. Avery
C-   Updated  24-JAN-1992   Lupe Howell  Remove machine code
C-   Updated  20-OCT-1993   Robert E. Avery   Allow for new version of 
C-                                              FHIT bank.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      LOGICAL PFGETHITS
C  Input:
      INTEGER HALF,UNIT,QUAD,SECTOR
C  Output:
      INTEGER N_HITS(0:MXWIRP)
      REAL    DR_DIST(0:1, MX_HIT_WIRE, 0:MXWIRP)
      LOGICAL DL_OK(MX_HIT_WIRE, 0:MXWIRP)
      LOGICAL ON_SEG(MX_HIT_WIRE, 0:MXWIRP)
C
C  Local:
C
      INTEGER STATWORD, IHIT, FIRST_HIT
      INTEGER FIRST_DL,N_DL
      INTEGER NSW, WDPRHT, NHIT 
      INTEGER LWIRE, LHIT, LBANK
      INTEGER GZFXSC
      INTEGER IQFHIT(2)
      LOGICAL SECT_HITS_FOUND  
C
C FHIT_DECODE Arguments:
C
      INTEGER H,U,QD,S,WIRE
      LOGICAL DL
      INTEGER LR
      LOGICAL ON_SEGMENT
      INTEGER TRK_FDCT,TRK_ZTRK
      REAL    DRIFTD, DRIFTD_MIRROR
      REAL    Z_POS
      REAL    IONIZATION
C----------------------------------------------------------------------
      SECT_HITS_FOUND  = .FALSE.
      CALL VZERO(N_HITS,(MXWIRP+1))
C        
C  First Look in FXSC bank
C
      LBANK = GZFXSC(HALF,UNIT,QUAD,SECTOR)
      IF(LBANK.GT.5) THEN
        NSW  = IQ( LBANK+2 )
        WDPRHT  = IQ( LBANK+3 )
        DO WIRE = 0, NSW-1
          N_HITS(WIRE) = IQ( LBANK + 4 + WIRE )
          LWIRE = IQ( LBANK + 4 + NSW + WIRE )
          DO IHIT = 1, N_HITS(WIRE) 
            LHIT = LBANK + LWIRE + (IHIT-1)*WDPRHT - 1 
            STATWORD = IQ(LHIT+9)
            ON_SEG(IHIT,WIRE) = BTEST(STATWORD,2) 
            DL_OK(IHIT,WIRE) = 
     &        ( (IAND(STATWORD,3).GT.0) .AND. (UNIT.EQ.0) )
            DR_DIST(0,IHIT,WIRE) = Q(LHIT+2)
            DR_DIST(1,IHIT,WIRE) = Q(LHIT+3)
            SECT_HITS_FOUND  = .TRUE.
          ENDDO
        ENDDO
      ELSE
C        
C  Else Look in FHIT bank
C
        CALL GTFHIT(0,NHIT)
        IF ( NHIT .GT. 0 ) THEN
          CALL FHITPT(HALF,UNIT,QUAD,SECTOR,
     &      FIRST_HIT,NHIT,FIRST_DL,N_DL)
          DO IHIT = FIRST_HIT, NHIT+FIRST_HIT-1
C
            CALL GTFHIT(IHIT,IQFHIT(1))
            CALL FHIT_DECODE( 
     &          IQFHIT,
     &          H,U,QD,S,WIRE,
     &          DL,LR,ON_SEGMENT,TRK_FDCT,TRK_ZTRK,
     &          DRIFTD, DRIFTD_MIRROR, Z_POS, IONIZATION )
C
            SECT_HITS_FOUND  = .TRUE.
            N_HITS(WIRE) = N_HITS(WIRE) +1
C
            ON_SEG(N_HITS(WIRE),WIRE) = ON_SEGMENT
            DL_OK(N_HITS(WIRE),WIRE) = DL
            DR_DIST(LR,N_HITS(WIRE),WIRE) = DRIFTD
            DR_DIST((1-LR),N_HITS(WIRE),WIRE) = DRIFTD_MIRROR
C
          ENDDO
        ENDIF
      ENDIF
  900 CONTINUE
      PFGETHITS = SECT_HITS_FOUND  
  999 RETURN
      END
