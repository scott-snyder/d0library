      SUBROUTINE L0_ANDOR_CHECK(ANDOR1,ANDOR2,NERR,ANDOR_ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check the Level 1 ANDOR bits against the
C-                         Level 1 FASTZ vertex word and Level 0 Vertex
C-                         boards output.
C-
C-   Inputs  : none
C-   Outputs : ANDOR words containing the bits concerning FASTZ
C-   Controls: none
C-
C-   Created  15-JUL-1992   Jeffrey Bantly
C-   Updated  14-SEP-1992   Jeffrey Bantly  add in check with Slow Z
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER ZBIN
      INTEGER ANDOR1, ANDOR2, L1VTX, L0SVTX
      INTEGER LTRGR, LCRATE, GZTRGR, GZFIND_CRATE, VERTEX
      INTEGER IAND, NOT
      logical btest
      INTEGER CBUNCH, WORDS(52)
      INTEGER OFFSET, MASK
      PARAMETER ( OFFSET=269, MASK=15 )
      INTEGER NERR, ANDOR_ERR(12)
      INTEGER INTERACTION,MI_FLAG
      INTEGER SVERTEX
      INTEGER L0_SLOW_INTER,L0_SLOW_Z_GOOD,L0_SLOW_CENTER
      INTEGER L0_MI_FLAG_0,L0_MI_FLAG_1,L0_MI_FLAG_2,L0_MI_FLAG_3
      INTEGER SZBIT(32),I,J,SANDOR
C
      REAL SLOW_Z,FULL_Z,MI_QUALITY
      REAL CHAN_EFF(72)
C
      LOGICAL GOODSZ
      LOGICAL GOOD
      LOGICAL SIGN
      LOGICAL FIRST
      LOGICAL L1BIT_PASSED, L2BIT_PASSED
      LOGICAL PRODUC, PRODFL
      EXTERNAL PRODUC
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        PRODFL = PRODUC()
        FIRST=.FALSE.
      ENDIF
C
      ANDOR1 = 0
      ANDOR2 = 0
      NERR = 0
      CALL VZERO(ANDOR_ERR,12)
C
      LTRGR = GZTRGR()
      LCRATE = GZFIND_CRATE( 'TRGR', LTRGR, 11 )
      IF ( LCRATE.LE.0 ) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-no-crt11-his',
     &          'L0_ANDOR_CHECK','Crate 11 data not found','W')
        GOTO 999
      ENDIF
C
      VERTEX = IQ( LCRATE + OFFSET )
      GOOD = BTEST( VERTEX, 5 )
      L1VTX = VERTEX - 128
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
      SVERTEX = IQ( LCRATE + 195 )
      L0_SLOW_INTER = IBITS(SVERTEX,7,1)
      L0_SLOW_Z_GOOD = IBITS(SVERTEX,16,1)
      L0_MI_FLAG_0 = IBITS(SVERTEX,17,1)
      L0_MI_FLAG_1 = IBITS(SVERTEX,18,1)
      L0_MI_FLAG_2 = IBITS(SVERTEX,19,1)
      L0_MI_FLAG_3 = IBITS(SVERTEX,20,1)
      L0_SLOW_CENTER = IBITS(SVERTEX,21,1)
C
      ANDOR1 = IAND(IQ(LCRATE + 195),127)
      ANDOR2 = IAND(IQ(LCRATE + 211),127)
      IF (ANDOR1.NE.ANDOR2) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-andor12-mismatch',
     &     'L0_ANDOR_CHECK','AND-OR Terms Disagree Level 1 crate','W')
        NERR = NERR + 1
        ANDOR_ERR(NERR) = 1
      ENDIF
C
      IF (.NOT.PRODFL) GOTO 100
      IF ( L1BIT_PASSED(1) ) GOTO 100
      IF ( L2BIT_PASSED(1) ) GOTO 100
      IF ((L1VTX.EQ.'20'X).AND.(ANDOR1.EQ.'5F'X)) GOTO 100
      IF ((L1VTX.EQ.'21'X).AND.(ANDOR1.EQ.'1F'X)) GOTO 100
      IF ((L1VTX.GE.'22'X).AND.(L1VTX.LE.'25'X).AND.(ANDOR1.EQ.'1D'X))
     &    GOTO 100
      IF ((L1VTX.GE.'26'X).AND.(L1VTX.LE.'29'X).AND.(ANDOR1.EQ.'19'X))
     &    GOTO 100
      IF ((L1VTX.GE.'2A'X).AND.(L1VTX.LE.'2F'X).AND.(ANDOR1.EQ.'11'X))
     &    GOTO 100
      IF ((L1VTX.GE.'31'X).AND.(L1VTX.LE.'36'X).AND.(ANDOR1.EQ.'31'X))
     &    GOTO 100
      IF ((L1VTX.GE.'37'X).AND.(L1VTX.LE.'3A'X).AND.(ANDOR1.EQ.'39'X))
     &    GOTO 100
      IF ((L1VTX.GE.'3B'X).AND.(L1VTX.LE.'3E'X).AND.(ANDOR1.EQ.'3D'X))
     &    GOTO 100
      IF ((L1VTX.EQ.'3F'X).AND.(ANDOR1.EQ.'3F'X)) GOTO 100
      CALL ERRMSG('LEVEL0-andor-fastz-mismatch',
     &    'L0_ANDOR_CHECK','Level 1 ANDOR bits dont map to FASTZ bits',
     &    'W')
      NERR = NERR + 1
      ANDOR_ERR(NERR) = 2
C
  100 CONTINUE
C
      CALL L0_COR_BUNCH(CBUNCH)
      CALL GTL0VX(CBUNCH,WORDS)
      IF ( WORDS(2) .EQ. 0 ) THEN
        L0SVTX = WORDS(20)
      ELSE
        L0SVTX = WORDS(46)
      ENDIF
      IF ( L0SVTX.GE.128 ) L0SVTX = L0SVTX - 128
      IF (L1VTX.NE.L0SVTX) THEN
        IF (.NOT.PRODFL) THEN
          CALL ERRMSG('LEVEL0-L1L0-mismatch',
     &      'L0_ANDOR_CHECK',
     &      'Vertex board Fastz Disagrees Level 1 crate','W')
          NERR = NERR + 1
          ANDOR_ERR(NERR) = 3
        ENDIF
      ENDIF
C
      IF (.NOT.PRODFL) GOTO 200
      IF ( L1BIT_PASSED(1) ) GOTO 200
      IF ( L2BIT_PASSED(1) ) GOTO 200
      IF ((L0SVTX.EQ.'20'X).AND.(ANDOR1.EQ.'5F'X)) GOTO 200
      IF ((L0SVTX.EQ.'21'X).AND.(ANDOR1.EQ.'1F'X)) GOTO 200
      IF ((L0SVTX.GE.'22'X).AND.(L0SVTX.LE.'25'X).AND.(ANDOR1.EQ.'1D'X))
     &    GOTO 200
      IF ((L0SVTX.GE.'26'X).AND.(L0SVTX.LE.'29'X).AND.(ANDOR1.EQ.'19'X))
     &    GOTO 200
      IF ((L0SVTX.GE.'2A'X).AND.(L0SVTX.LE.'2F'X).AND.(ANDOR1.EQ.'11'X))
     &    GOTO 200
      IF ((L0SVTX.GE.'31'X).AND.(L0SVTX.LE.'36'X).AND.(ANDOR1.EQ.'31'X))
     &    GOTO 200
      IF ((L0SVTX.GE.'37'X).AND.(L0SVTX.LE.'3A'X).AND.(ANDOR1.EQ.'39'X))
     &    GOTO 200
      IF ((L0SVTX.GE.'3B'X).AND.(L0SVTX.LE.'3E'X).AND.(ANDOR1.EQ.'3D'X))
     &    GOTO 200
      IF ((L0SVTX.EQ.'3F'X).AND.(ANDOR1.EQ.'3F'X)) GOTO 200
      CALL ERRMSG('LEVEL0-andor-verbrd-fastz-mismatch',
     &    'L0_ANDOR_CHECK',
     &    'Level 1 ANDOR bits dont map to vertex board FASTZ bits',
     &    'W')
      NERR = NERR + 1
      ANDOR_ERR(NERR) = 4
C
  200 CONTINUE
C
      CALL L0_SLOW_VERTEX(SLOW_Z,MI_FLAG,MI_QUALITY,
     &               INTERACTION,GOODSZ,FULL_Z,CHAN_EFF)
C
      IF ( L0_SLOW_INTER.NE.INTERACTION ) THEN
        CALL ERRMSG('LEVEL0-andor-verbrd-INTER-mismatch',
     &    'L0_ANDOR_CHECK',
     &  'Level 1 ANDOR bits dont map to vertex board interaction bit',
     &    'W')
        NERR = NERR + 1
        ANDOR_ERR(NERR) = 5
      ENDIF
      IF ( INTERACTION.EQ.1 ) THEN
        IF ( GOODSZ ) THEN
          IF ( L0_SLOW_Z_GOOD.NE.1 ) THEN
            CALL ERRMSG('LEVEL0-andor-verbrd-goodsz-mismatch1',
     &        'L0_ANDOR_CHECK',
     &     'Level 1 ANDOR bits dont map to vertex board good slowz bit',
     &        'W')
            NERR = NERR + 1
            ANDOR_ERR(NERR) = 6
          ENDIF
        ELSE
          IF ( L0_SLOW_Z_GOOD.EQ.1 ) THEN
            CALL ERRMSG('LEVEL0-andor-verbrd-goodsz-mismatch2',
     &        'L0_ANDOR_CHECK',
     &     'Level 1 ANDOR bits dont map to vertex board good slowz bit',
     &        'W')
            NERR = NERR + 1
            ANDOR_ERR(NERR) = 7
          ENDIF
        ENDIF
        IF ( L0_MI_FLAG_0.EQ.1 ) THEN
          IF ( MI_FLAG.NE.1 ) THEN
            CALL ERRMSG('LEVEL0-andor-verbrd-MIFLAG0-mismatch',
     &        'L0_ANDOR_CHECK',
     &      'Level 1 ANDOR bits dont map to vertex board MI FLAG 0 bit',
     &        'W')
            NERR = NERR + 1
            ANDOR_ERR(NERR) = 8
          ENDIF
        ELSEIF ( L0_MI_FLAG_1.EQ.1 ) THEN
          IF ( MI_FLAG.NE.2 ) THEN
            CALL ERRMSG('LEVEL0-andor-verbrd-MIFLAG1-mismatch',
     &        'L0_ANDOR_CHECK',
     &      'Level 1 ANDOR bits dont map to vertex board MI FLAG 1 bit',
     &        'W')
            NERR = NERR + 1
            ANDOR_ERR(NERR) = 9
          ENDIF
        ENDIF
        IF ( L0_MI_FLAG_3.EQ.1 ) THEN
          IF ( MI_FLAG.NE.4 ) THEN
            CALL ERRMSG('LEVEL0-andor-verbrd-MIFLAG3-mismatch',
     &        'L0_ANDOR_CHECK',
     &      'Level 1 ANDOR bits dont map to vertex board MI FLAG 3 bit',
     &        'W')
            NERR = NERR + 1
            ANDOR_ERR(NERR) = 11
          ENDIF
        ELSEIF ( L0_MI_FLAG_2.EQ.1 ) THEN
          IF ( MI_FLAG.NE.3 ) THEN
            CALL ERRMSG('LEVEL0-andor-verbrd-MIFLAG2-mismatch',
     &        'L0_ANDOR_CHECK',
     &      'Level 1 ANDOR bits dont map to vertex board MI FLAG 2 bit',
     &        'W')
            NERR = NERR + 1
            ANDOR_ERR(NERR) = 10
          ENDIF
        ENDIF
        IF ( L0_SLOW_CENTER.EQ.1 .AND. ABS(SLOW_Z).LE. 10.5 ) THEN
          IF ( L0_MI_FLAG_0.EQ.1 .AND. MI_FLAG.NE.1 ) THEN
            CALL ERRMSG('LEVEL0-andor-verbrd-SLOWZCTR-mismatch',
     &        'L0_ANDOR_CHECK',
     &      'Level 1 ANDOR bits dont map to vertex board slowz ctr bit',
     &        'W')
            NERR = NERR + 1
            ANDOR_ERR(NERR) = 12
          ENDIF
        ENDIF
      ENDIF
C
  300 CONTINUE
C
C------------------------------------------------------------------------
  999 RETURN
      END
