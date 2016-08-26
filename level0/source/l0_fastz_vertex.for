      SUBROUTINE L0_FASTZ_VERTEX( ZPOS, GOOD )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack L0 vertex for use in L2
C-
C-   Inputs  : VERTEX via ZEBCOM
C-   Outputs : ZPOS [F] = Z position of FASTZ vertex in cm.,=0.0 if GOOD=F
C-             GOOD [L] = TRUE if good vertex bit set.
C-   Controls: none
C-
C-   For details see D0 Note 967
C-   Vertex is a five bit two's compliment integer
C-   The next significant bit is a good bit
C-   The last two bits in byte 525 are unused
C-
C-   Modified  7-JUL-1992   Jeffrey Bantly  from S.L.Linn routine
C-                                          L2_L0_VERTEX.FOR
C-   Updated   2-JUN-1993   Jeffrey Bantly  fixed bug, initialize FULL_*
C-                                          variables 
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ZBIN
      INTEGER LTRGR
      INTEGER LCRATE
      INTEGER GZTRGR, GZFIND_CRATE
      INTEGER OFFSET, IOFFSET
      INTEGER  VERTEX, CBUNCH
      INTEGER MASK, IAND, NOT, ERR
      logical btest
      PARAMETER ( OFFSET=269, MASK=15 )
      INTEGER IDATA_WORDS(20)
      INTEGER VBOARD, VERTEX_DATA(52)
      INTEGER FVTX
      INTEGER LINK, GZPLV0, GZL0AD, GZL0VX_BUNCH
      EXTERNAL GZPLV0,GZL0AD,GZL0VX_BUNCH
C
      REAL ZPOS
      REAL FASTZ_MULTIPLIER, FASTZ_OFFSET
      REAL DATA_WORDS(20)
      EQUIVALENCE ( DATA_WORDS, IDATA_WORDS )
C
      LOGICAL FULL_PLV0, FULL_L0VX
      LOGICAL GOOD
      LOGICAL SIGN
      LOGICAL FIRST, EZERROR
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','L0_FASTZ_VERTEX',
     &                              'LEVEL0_RCP not found.','W')
          FASTZ_MULTIPLIER = 6.25
          FASTZ_OFFSET = 0.00
        ELSE
          CALL EZGET('FASTZ_MULTIPLIER',FASTZ_MULTIPLIER,ERR)
          IF ( ERR.NE.0 ) FASTZ_MULTIPLIER = 6.25
          CALL EZGET('FASTZ_OFFSET',FASTZ_OFFSET,ERR)
          IF ( ERR.NE.0 ) FASTZ_OFFSET = 0.00
          CALL EZRSET
        ENDIF
C
        FIRST = .FALSE.
      ENDIF
C
      ZPOS = 0.0
      GOOD = .FALSE.
      FULL_PLV0=.FALSE.
      FULL_L0VX=.FALSE.
C
C  Check if data in PLV0 bank.
C
      LINK=GZPLV0()
      IF ( LINK.GT.0 ) THEN
        IF ( IBITS(IQ(LINK+1),7,3).GT.0 ) THEN
          CALL GTPLV0(IDATA_WORDS)
          ZPOS=DATA_WORDS(2)
          GOOD=BTEST(IDATA_WORDS(1),0)
          FULL_PLV0=.TRUE.
          GOTO 999
        ENDIF
      ENDIF
C
C  If not, fetch the correct bunch number and FASTZ data in L0VX bank.
C
      CALL L0_COR_BUNCH(CBUNCH)
C
      LINK=GZL0VX_BUNCH(CBUNCH)
      IF ( LINK.GT.0 ) THEN
        CALL GTL0VX(CBUNCH,VERTEX_DATA)
        VBOARD=VERTEX_DATA(3)
        IF ( VBOARD.EQ.2 ) THEN
          IOFFSET=0
        ELSE
          IOFFSET=26
        ENDIF
        IF ( .NOT.FULL_PLV0 ) THEN
          FVTX = VERTEX_DATA(IOFFSET+20)
          SIGN = IAND(FVTX,'10'X)/'10'X
          GOOD = IAND(FVTX,'20'X)/'20'X
          IF ( .not.SIGN ) THEN
            ZPOS = FASTZ_MULTIPLIER*IAND(FVTX,'F'X) + FASTZ_OFFSET
          ELSE
            ZPOS = -FASTZ_MULTIPLIER*IAND('10'X-FVTX,'F'X) +
     &                               FASTZ_OFFSET
          ENDIF
          FULL_L0VX=.TRUE.
          GOTO 999
        ENDIF
      ENDIF
C
C  If not, fetch Zebra location in TRGR bank of Level 0 data crate, crate 01.
C
      LTRGR = GZTRGR()
      LCRATE = GZFIND_CRATE( 'TRGR', LTRGR, 11 )
      IF ( LCRATE.GT.0 ) THEN
        VERTEX = IQ( LCRATE + OFFSET )
      ELSE
        VERTEX = 0
      ENDIF
      GOOD = BTEST( VERTEX, 5 )
      IF ( GOOD ) THEN
        SIGN = BTEST( VERTEX, 4)
        IF( SIGN )THEN
          VERTEX = VERTEX - 1
          VERTEX = NOT( VERTEX )
          ZBIN = -IAND( VERTEX, MASK )
        ELSE
          ZBIN =  IAND( VERTEX, MASK )
        ENDIF
      ELSE
        ZBIN = 0
      ENDIF
C
C  Z position is ZBIN * ( 6.25cm / bin )
C
      ZPOS = FLOAT(ZBIN)*FASTZ_MULTIPLIER + FASTZ_OFFSET
C
C  Done.
C
C----------------------------------------------------------------------
  999 RETURN
      END
