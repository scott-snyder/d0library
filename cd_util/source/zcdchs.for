      SUBROUTINE ZCDCHS(ZVERTX,ZERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill hitogram for the Z along beam line
C-                         and find the VERTEX's Z position (using
C-                         CDC information only)
C-
C-   Inputs  : none
C-   Outputs : ZVERTX: vertex's Z position
C-             ZERROR: error on vertex's Z position
C-
C-   Created  27-FEB-1990   Qizhong Li-Demarteau
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDRFT.LINK'
      INTEGER NOENT, NTRK, I, ILO, IHI, ERR, IER
      INTEGER LCDCH, GZCDCH, GZDGEH, LDRFT
      INTEGER GZISV1, LISV1
      REAL    ZISAJT, ZDIF
      REAL    ZVERTX, ZERROR
      REAL    ZSIGMA, ITRLMT, BIGSGM, SGMFCT, SGMFC2, CDCRES, R1, R2
      REAL    MEAN, SIGMA, OLDMEAN, OLDSGM, ZLO, ZHI, ZCUT, NENTRY
      REAL    CONTEN(100)
      REAL    HSTATI, HI
      LOGICAL FIRST, SNGLE1, HSTFLG
      LOGICAL EZERROR
      SAVE FIRST
      DATA    FIRST/.TRUE./, SNGLE1/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZCDCHS',
     &    'Unable to find bank VERTEX_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('ZSIGMA',ZSIGMA,ERR)
        CALL EZGET('SGMFCT',SGMFCT,ERR)
        CALL EZGET('ITRLMT',ITRLMT,ERR)
        CALL EZGET('BIGSGM',BIGSGM,ERR)
        CALL EZGET('SGMFC2',SGMFC2,ERR)
        CALL EZGET('CDCRES',CDCRES,ERR)
        CALL EZGET('HSTFLG',HSTFLG,ERR)
        CALL EZRSET
        LDGEH = GZDGEH()
C
C         Create/Set HBOOK directory for VERTEX
C
        CALL DHDIR('VERTEX_RCP','HBOOK_DIRECTORY',IER,' ')
        IF (IER.NE.0) THEN
          CALL ERRMSG('VERTEX','ZCDCHS',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
        CALL HBOOK1(1099,' VERTEX IN Z$',100,-90.,90.,0.)
        IF (HSTFLG) 
     &    CALL HBOOK1(1098,' Z(ISAJET) - Z(CDC) $',25,-25.,25.,0.)
        CALL HIDOPT(0,'STAT')     !  get the statistics for all histograms
      ENDIF
      ZVERTX = 9999.0
      ZERROR = 9999.0
C
C get Z information from CDC hits and fill Z into a histigram
C
      CALL ZCDCGZ
C
      OLDSGM = 0.0
      CALL HNOENT(1099,NOENT)
      IF (NOENT .EQ. 0) GOTO 106
      NENTRY = 0.0
      IF (NOENT .EQ. 1) GOTO 105
      OLDMEAN = HSTATI(1099,1,'HIST',0)
 104  MEAN = HSTATI(1099,1,'HIST',0)
      SIGMA = HSTATI(1099,2,'HIST',0)
      IF (SIGMA .LE. ZSIGMA) THEN
        GOTO 105
      ELSE
        IF (ABS(OLDSGM - SIGMA) .LE. ITRLMT) THEN
          IF (SGMFCT .EQ. SGMFC2) GOTO 105
          SGMFCT = SGMFC2
        ENDIF
      ENDIF
      OLDSGM = SIGMA
      IF (SIGMA .LE. BIGSGM) THEN
        ZCUT = SGMFCT * SIGMA
      ELSE
        ZCUT = SIGMA
      ENDIF
      ZLO = MEAN - ZCUT
      ZHI = MEAN + ZCUT
      CALL HXI(1099,ZLO,ILO)
      CALL HXI(1099,ZHI,IHI)
      DO 102 I = ILO-1, 1, -1
        IF (HI(1099,I) .EQ. 0.0) GOTO 103
 102  CONTINUE
 103  ILO = I+1
      DO 112 I = IHI+1, 100
        IF (HI(1099,I) .EQ. 0.0) GOTO 113
 112  CONTINUE
 113  IHI = I-1
      CALL HUNPAK(1099,CONTEN,' ',1)
      NENTRY = 0.0
      DO 101 I = 1, 100
        IF (I .LT. ILO .OR. I .GT. IHI) CONTEN(I) = 0.0
        NENTRY = NENTRY + CONTEN(I)
 101  CONTINUE
      CALL HPAK(1099,CONTEN)
      GOTO 104
C
 105  ZVERTX = HSTATI(1099,1,'HIST',0)
      IF (ZVERTX .GT. C(LDGEH + 19)) THEN
        ZERROR = 9999.9
        GOTO 106
      ELSE
        IF (NENTRY .GT. 1) THEN
          ZERROR = HSTATI(1099,2,'HIST',0)
          ZERROR = ZERROR / SQRT(NENTRY)
        ELSE
          IF (SNGLE1) THEN
            LDRFT = LC(LDGEH - IZDRFT)
            IF (LDGEH .GT. 0 .AND. LDRFT .GT. 0) THEN
              R1 = ((C(LDRFT+17) + C(LDRFT+25)) - 
     &        (C(LDRFT+11) + C(LDRFT+19))) / 2
              R2 = C(LDGEH+10) + (C(LDGEH+12) - C(LDGEH+10)) / 2
            ENDIF
            SNGLE1 = .FALSE.
          ENDIF
          IF (R1 .LE. 0 .OR. R2 .LE. 0) THEN
            ZERROR = 1.4
          ELSE
            ZERROR = CDCRES * R2 / R1
          ENDIF
        ENDIF
      ENDIF
C
C   get isajet track information for debug purpose, 
C   it should be removed later
C
      IF (HSTFLG) THEN
        LISV1 = GZISV1()
        IF (LISV1 .GT. 0) THEN
          ZISAJT = Q(LISV1+9)
          ZDIF = ZISAJT - ZVERTX
          CALL HF1(1098,ZDIF,1.)
        ENDIF
      ENDIF
C
  106 CALL HRESET(1099,' ')
      LCDCH = GZCDCH()
      IF (LCDCH .GT. 0) CALL MZDROP(IXCOM,LCDCH,' ')
C
  999 RETURN
      END
