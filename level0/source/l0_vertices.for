      SUBROUTINE L0_VERTICES( FZPOS, FGOOD, SZPOS, SGOOD, MI_FLAG )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack L0 vertex for use in L2
C-
C-   Inputs  : VERTEX via ZEBCOM
C-   Outputs : FZPOS [F] = Z position of FASTZ vertex in cm.,=0.0 if FGOOD=F
C-             FGOOD [L] = TRUE if good FASTZ vertex bit set.
C-             SZPOS [F] = Z position of Slow Z vertex in cm.,=0.0 if SGOOD=F
C-             SGOOD [L] = TRUE if good SLOW vertex bit set.
C-             MI_FLAG [I] = Multiple Interaction Flag 1-4
C-   Controls: none
C-
C-   For details see D0 Note 967
C-   FASTZ Vertex is a five bit two's compliment integer
C-   The next significant bit is a good bit
C-   The last two bits in byte 525 are unused
C-
C-   Modified   7-JUL-1992   Jeffrey Bantly   from S.L.Linn routine
C-                                            L2_L0_VERTEX.FOR
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ZBIN, SVTX
      INTEGER GZTRGR, GZFIND_CRATE
      EXTERNAL GZTRGR, GZFIND_CRATE
      INTEGER FOFFSET, SOFFSET, SFOFFSET
      INTEGER FOFFSETA, SOFFSETA, SFOFFSETA
      INTEGER FOFFSETB, SOFFSETB, SFOFFSETB
      INTEGER HALF_BYTE, ONE_BYTE
      PARAMETER ( FOFFSETA=269, SOFFSETA=750, SFOFFSETA=352 )
      PARAMETER ( FOFFSETB=269, SOFFSETB=806, SFOFFSETB=352 )
      PARAMETER ( HALF_BYTE=15 , ONE_BYTE = 255 )
      INTEGER VERTEX, SVERTEX
      INTEGER MASK, IAND, NOT, IBITS
      logical btest
      PARAMETER ( MASK=15 )
      INTEGER CBUNCH
      INTEGER LTRGR,LCRATE11,LCRATE01
      INTEGER IB,BUNCH_BIT(32)
      INTEGER MI_FLAG
      INTEGER HEAD_LEN, NBLOCKS, VERSION
C
      REAL FZPOS, SZPOS
c      REAL FASTZ_MULTIPLIER, FASTZ_OFFSET
      REAL SLOW_Z_MULTIPLIER, SLOW_OFFSET
C
      LOGICAL FGOOD, SGOOD
      LOGICAL SIGN
C
c      DATA FASTZ_MULTIPLIER, FASTZ_OFFSET / 6.25, 0.00 /
      DATA SLOW_Z_MULTIPLIER, SLOW_OFFSET / 0.75, 0.00 /
C----------------------------------------------------------------------
      FGOOD = .FALSE.
      FZPOS = 0.0
      SGOOD = .FALSE.
      SZPOS = 0.0
      MI_FLAG = 0
C
C  Preset offset parameters
C
      FOFFSET = FOFFSETA
      SOFFSET = SOFFSETA
      SFOFFSET = SFOFFSETA
C
C  Fetch Zebra location in TRGR bank of Level 1 data crate, crate 11.
C
      LTRGR = GZTRGR()
      IF (LTRGR .EQ. 0) GOTO 999
      LCRATE11 = GZFIND_CRATE( 'TRGR', LTRGR, 11 )
C
C  Fetch and decode FASTZ vertex position.
C
      IF ( LCRATE11.GT.0 ) THEN
        VERTEX = IQ( LCRATE11 + FOFFSET )
      ELSE
        VERTEX = 0
      ENDIF
      FGOOD = BTEST( VERTEX, 5 )
      IF ( FGOOD ) THEN
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
      FZPOS = FLOAT(ZBIN)*6.25
C
C  Fetch the correct bunch.
C
      CBUNCH = -1
      DO IB=0,5
        BUNCH_BIT(IB+1) = IBITS(IQ(LCRATE11+195+2),IB,1)
        IF ( BUNCH_BIT(IB+1).EQ.1 ) THEN
          IF ( CBUNCH.EQ.-1 ) THEN
            CBUNCH=IB+1
          ELSE
C
          ENDIF
        ENDIF
      ENDDO
C
C  Fetch Zebra location in TRGR bank of Level 0 data crate, crate 01.
C
      LCRATE01 = GZFIND_CRATE('TRGR',LTRGR,1)
      IF (LCRATE01 .LE. 0) GOTO 999
      HEAD_LEN = IQ(LCRATE01)
      NBLOCKS = IQ(LCRATE01+2)
      NBLOCKS = IBITS(NBLOCKS,16,8)
      VERSION = IQ(LCRATE01+3)
      VERSION = IBITS(VERSION,0,16)
      IF ( HEAD_LEN.NE.4 ) THEN
c        ERRVAL=7
c        CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
      ENDIF
      IF ( VERSION.EQ.2 ) THEN
        IF ( NBLOCKS.EQ.25 ) SOFFSET = SOFFSETA
        IF ( NBLOCKS.EQ.29 ) SOFFSET = SOFFSETB
      ELSEIF ( VERSION.EQ.1 ) THEN
        SOFFSET = SOFFSETA
      ELSE
c        ERRVAL=8
c        CALL ERRMSG(MSG1(ERRVAL),'L2_L0_VERTEX',MSG2(ERRVAL),'W')
        GOTO 999
      ENDIF
C
C  Fetch the Level 0 Slow Z vertex position and Multiple Interaction Flag.
C
      IF ( CBUNCH.GE.1 .AND. CBUNCH.LE.6 ) THEN
        SVERTEX = IQ(LCRATE01+SOFFSET+(CBUNCH*6))
      ELSE
        SVERTEX = 0
      ENDIF
      SGOOD = BTEST(SVERTEX,5)
      SZPOS = 0.0
      IF ( SGOOD ) THEN
        SVTX = IBITS(SVERTEX,16,16)
        SIGN = IAND(SVTX,'100'X)/'100'X
        IF ( .not.SIGN ) THEN
          SZPOS = SLOW_Z_MULTIPLIER*IAND(SVTX,'FF'X)+SLOW_OFFSET
        ELSE
          SZPOS = -SLOW_Z_MULTIPLIER*IAND('100'X-SVTX,'FF'X)
     &      + SLOW_OFFSET
        ENDIF
        IF ( IBITS(SVERTEX,0,1).EQ.1 ) THEN
          MI_FLAG=1
        ELSEIF ( IBITS(SVERTEX,1,1).EQ.1 ) THEN
          MI_FLAG=2
        ELSEIF ( IBITS(SVERTEX,3,1).EQ.1 ) THEN
          MI_FLAG=4
        ELSEIF ( IBITS(SVERTEX,2,1).EQ.1 ) THEN
          MI_FLAG=3
        ELSE
          SGOOD = .FALSE.
c          CALL ERRMSG('LEVEL0-NO-MIFLAG-LV','L0_VERTICES',
c     &                'NO MIFLAG FD','W')
        ENDIF
      ENDIF
C
C  If bad FASTZ from Level 1, check the Slow Z board value of FASTZ.
C
      IF ( .NOT.FGOOD ) THEN
        IF ( CBUNCH.GE.1 .AND. CBUNCH.LE.6 ) THEN
          VERTEX = IQ(LCRATE01+SFOFFSET+(CBUNCH*6))
          VERTEX = IBITS(VERTEX,8,8)
        ELSE
          VERTEX = 0
        ENDIF
        FGOOD = BTEST( VERTEX, 5 )
        IF ( FGOOD ) THEN
          SIGN = BTEST( VERTEX, 4)
          IF( SIGN )THEN
            VERTEX = VERTEX - 1
            VERTEX = NOT( VERTEX )
            ZBIN = -IAND( VERTEX, MASK )
          ELSE
            ZBIN =  IAND( VERTEX, MASK )
          ENDIF
c          CALL ERRMSG('LEVEL0-NO-FASTZ-LV','L0_VERTICES',
c     &                'NO FASTZ L1 FD','W')
        ELSE
          ZBIN = 0
        ENDIF
C
        FZPOS = FLOAT(ZBIN)*6.25
C
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
