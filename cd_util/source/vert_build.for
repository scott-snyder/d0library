      SUBROUTINE VERT_BUILD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : build VERT bank if don't want to use the data
C-                         to build VERT
C-
C-   Inputs  : none
C-   Outputs : none, VERT bank will be built if VERTEX_TYPE in VERTEX.RCP
C-             is non-zero
C-
C-   Created  24-NOV-1993   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPLV0.LINK'
      INTEGER VERTEX_TYPE, IER
      INTEGER LISV1, GZISV1, LPROC, GZPROC, LPLV0
      REAL    ZVERTX, ZERROR, ZVERTX_NORM,ZERROR_NORM
      REAL    ZERROR_ISV1, ZERROR_SLV0, ZERROR_FLV0
      REAL    FZPOS, SZPOS
      LOGICAL FIRST, EZERROR, FGOOD, SGOOD
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('VERTEX','VERT_BUILD',
     &      'Unable to find bank VERTEX_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('VERTEX_TYPE',VERTEX_TYPE,IER)
        CALL EZGET('ZVERTX_NORMINAL',ZVERTX_NORM,IER)
        CALL EZGET('ZERROR_NORMINAL',ZERROR_NORM,IER)
        IF (IER .EQ. 0) ZERROR_NORM = 0.001
        CALL EZGET('ZERROR_ISAJET',ZERROR_ISV1,IER)
        IF (IER .EQ. 0) ZERROR_ISV1 = 0.01
        CALL EZGET('ZERROR_SLV0',ZERROR_SLV0,IER)
        IF (IER .EQ. 0) ZERROR_SLV0 = 7.0
        CALL EZGET('ZERROR_FLV0',ZERROR_FLV0,IER)
        IF (IER .EQ. 0) ZERROR_FLV0 = 15.0
        CALL EZRSET
      ENDIF
C
      IF (VERTEX_TYPE .LE. 0) GOTO 999
      IF (VERTEX_TYPE .LE. 2) THEN
C
C   use norminal value from VERTEX_RCP to build VERT
C
        ZVERTX = ZVERTX_NORM
        ZERROR = ZERROR_NORM
        IF (VERTEX_TYPE .EQ. 2) THEN
          LISV1 = GZISV1()
          IF (LISV1 .LE. 0) THEN
            CALL ERRMSG('ZTRAKS','VERTEX',
     &        'no ISAJET info, ZVERTX_NORMINAL is used','W')
          ELSE
C
C   use ISAJET information to build VERT
C
            ZVERTX = Q(LISV1+9)
            ZERROR = ZERROR_ISV1
          ENDIF
        ENDIF
      ELSE
C
C   use Level0 information to build VERT
C
        LPROC = GZPROC()
        LPLV0 = LQ(LPROC - IZPLV0)
        IF (LPLV0 .LE. 0) THEN
          CALL ERRMSG('VERTEX','VERT_BUILD',
     &        'PLV0 missing - no VERT','W')
          GOTO 999
        ELSE
          IF (BTEST(IQ(LPLV0+1),5)) THEN  ! GOOD SLOW Z
            ZVERTX = Q(LPLV0+3)  !slow Z
            ZERROR = 7.0        !Jeff Bantley's guess 10/22/92
          ELSE                       !fast Z
            ZVERTX = Q(LPLV0+2)
            ZERROR = 15.0
          END IF
        ENDIF
      ENDIF
      CALL ZVERTFL_CDC(ZVERTX,ZERROR,0,0)
C
  999 RETURN
      END
