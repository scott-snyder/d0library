      SUBROUTINE LV0DIA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Cross-checks various LEVEL0 quantities.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  15-DEC-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INTERACTION,MI_FLAG
      INTEGER PMI_FLAG
      INTEGER MIF
      INTEGER FGFILT,SGFILT
      INTEGER ANDOR1,ANDOR2,ANDOR_ERR(12),NERR
      INTEGER I
      REAL FASTZ,SLOW_Z,FULL_Z
      REAL MI_QUALITY
      REAL CHAN_EFF(72)
      REAL PFZPOS, PSZPOS
      REAL FZPOS, SZPOS
      REAL FZFILT,SZFILT
      LOGICAL GOODFZ, GOODSZ
      LOGICAL PFGOOD, PSGOOD
      LOGICAL FGOOD, SGOOD
      LOGICAL OKFILT
      LOGICAL ZEROB
      LOGICAL L1BIT_PASSED, L2BIT_PASSED
      EXTERNAL L1BIT_PASSED, L2BIT_PASSED
C----------------------------------------------------------------------
C
      ZEROB=.FALSE.
      IF ( L1BIT_PASSED(1) ) THEN
        IF ( L2BIT_PASSED(1) ) ZEROB=.TRUE.
      ENDIF
C
      CALL L0_FASTZ_VERTEX(FASTZ,GOODFZ)
      CALL L0_SLOW_VERTEX(SLOW_Z,MI_FLAG,MI_QUALITY,
     &               INTERACTION,GOODSZ,FULL_Z,CHAN_EFF)
C
      CALL GTPLV0_ZONLY(PFZPOS,PFGOOD,PSZPOS,PSGOOD,PMI_FLAG)
      IF ( PFZPOS.NE.FASTZ ) THEN
        CALL ERRMSG('LEVEL0-BAD-FPLV0','LV0HIS','FPLV0','W')
      ENDIF
      IF ( PFGOOD.NE.GOODFZ ) THEN
        CALL ERRMSG('LEVEL0-BAD-GDFPLV0','LV0HIS','GDFPLV0','W')
      ENDIF
      IF ( PSZPOS.NE.SLOW_Z ) THEN
        CALL ERRMSG('LEVEL0-BAD-SPLV0','LV0HIS','SPLV0','W')
      ENDIF
      IF ( PSGOOD.NE.GOODSZ ) THEN
        CALL ERRMSG('LEVEL0-BAD-GDSPLV0','LV0HIS','GDSPLV0','W')
      ENDIF
      IF ( PMI_FLAG.NE.MI_FLAG ) THEN
        CALL ERRMSG('LEVEL0-BAD-MIFPLV0','LV0HIS','MIFPLV0','W')
      ENDIF
C
      CALL L0_VERTICES( FZPOS, FGOOD, SZPOS, SGOOD, MIF )
      IF ( FZPOS.NE.FASTZ ) THEN
        CALL ERRMSG('LEVEL0-BAD-FVERTICES','LV0HIS','FVERT','W')
      ENDIF
      IF ( FGOOD.NE.GOODFZ ) THEN
        CALL ERRMSG('LEVEL0-BAD-GDFVERTICES','LV0HIS','GDFVERT','W')
      ENDIF
      IF ( SZPOS.NE.SLOW_Z ) THEN
        CALL ERRMSG('LEVEL0-BAD-SVERTICES','LV0HIS','SVERT','W')
      ENDIF
      IF ( SGOOD.NE.GOODSZ ) THEN
        IF ( .NOT.ZEROB ) THEN
          CALL ERRMSG('LEVEL0-BAD-GDSVERTICES','LV0HIS','GDSVERT','W')
        ENDIF
      ENDIF
      IF ( MIF.NE.MI_FLAG ) THEN
        IF ( .NOT.ZEROB ) THEN
          CALL ERRMSG('LEVEL0-BAD-MIFVERTICES','LV0HIS','MIFVERT','W')
        ENDIF
      ENDIF
C
      CALL GTL0VT(FZFILT,FGFILT,SZFILT,SGFILT,OKFILT)
      IF ( OKFILT ) THEN
        IF ( FZFILT.NE.FASTZ ) THEN
          IF ( .NOT.ZEROB ) THEN
            CALL ERRMSG('LEVEL0-BAD-FZFILT','LV0HIS','FVERT','W')
          ENDIF
        ENDIF
        IF ( GOODFZ ) THEN
          IF ( FGFILT.NE.1 ) THEN
            CALL ERRMSG('LEVEL0-BAD-FGFILT-NOT1','LV0HIS','GDFVERT','W')
          ENDIF
        ELSE
          IF ( FGFILT.NE.2 ) THEN
            CALL ERRMSG('LEVEL0-BAD-FGFILT-NOT2','LV0HIS','GDFVERT','W')
          ENDIF
        ENDIF
        IF ( SZFILT.NE.SLOW_Z ) THEN
          CALL ERRMSG('LEVEL0-BAD-SZFILT','LV0HIS','SVERT','W')
        ENDIF
        IF ( GOODSZ ) THEN
          IF ( SGFILT.NE.MI_FLAG ) THEN
            IF ( SGFILT.NE.2 .OR. MI_FLAG.NE.1 ) THEN
              CALL ERRMSG('LEVEL0-BAD-MIF-SGFILT','LV0HIS',
     &                                            'MIFVERT','W')
            ENDIF
          ENDIF
        ELSE
          IF ( SGFILT.NE.0 ) THEN
            CALL ERRMSG('LEVEL0-BAD-SGFILT','LV0HIS','GDSVERT','W')
          ENDIF
        ENDIF
      ELSE
        CALL ERRMSG('LEVEL0-L0FILT-FAILED','LV0HIS','MIFVERT','W')
      ENDIF
C
      CALL L0_ANDOR_CHECK(ANDOR1,ANDOR2,NERR,ANDOR_ERR)
      IF ( NERR.GT.0 ) THEN
        CALL ERRMSG('LEVEL0-ANDOR-CHECK-FAILED','LV0HIS','ANDORS','W')
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
