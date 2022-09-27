      SUBROUTINE ZVERTX_ACCURATE(ZVERTX,ZERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reconstructe the Z position of the primary 
C-                         vertex more accurately by using reconstructed 
C-                         track information
C-
C-   Inputs  : none
C-   Outputs : ZVERTX: vertex's Z position
C-             ZERROR: error on vertex's Z position
C-
C-   Created  14-SEP-1990   Qizhong Li-Demarteau
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated   2-DEC-1991   Qizhong Li-Demarteau  added a check on edge tracks
C-                               don't use e4dge tracks for the vertex finding
C-   Updated  21-FEB-1992   Qizhong Li-Demarteau  removed machine_block 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDRFT.LINK'
      INTEGER NOENT, I, ILO, IHI, ERR, IER
      INTEGER GZDGEH, LDRFT
      INTEGER GZISV1, LISV1
      INTEGER LDTRH, LDTRK, GZDTRH, IPATH
      CHARACTER*4 DPATH
      EQUIVALENCE (IPATH, DPATH)
      REAL    ZPOSIT, THETA
      REAL    ZISAJT, ZDIF
      REAL    ZVERTX, ZERROR
      REAL    ZSIGMA, ITRLMT, BIGSGM, SGMFCT, SGMFC2, CDCRES, R1, R2
      REAL    MEAN, SIGMA, OLDMEAN, OLDSGM, ZLO, ZHI, ZCUT, NENTRY
      REAL    CONTEN(100)
      REAL    HSTATI, HI
      LOGICAL EZERROR
      LOGICAL FIRST, SNGLE1, HSTFLG
      LOGICAL BTEST
C
      SAVE FIRST
      DATA    FIRST/.TRUE./, SNGLE1/.TRUE./
C----------------------------------------------------------------------
C
C         Create/Set HBOOK directory for VERTEX
C
      CALL DHDIR('VERTEX_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER.NE.0) THEN
        CALL ERRMSG('VERTEX','ZVERT_ACCURATE',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZVERTX_ACCURATE',
     &    'Unable to find bank VERTEX_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('ZSIGMA',ZSIGMA,ERR)
        CALL EZGET('SGMFCT',SGMFCT,ERR)
        CALL EZGET('ITRLMT',ITRLMT,ERR)
        CALL EZGET('BIGSGM',BIGSGM,ERR)
        CALL EZGET('SGMFC2',SGMFC2,ERR)
        CALL EZGET('CDCRES',CDCRES,ERR)
        CALL EZGET_l('HSTFLG',HSTFLG,ERR)
        CALL EZRSET
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','ZVERTX_ACCURATE',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('DPATH',IPATH,ERR)
        CALL EZRSET        
        LDGEH = GZDGEH()
        CALL HBOOK1(1099,' VERTEX IN Z$',100,-90.,90.,0.)
        IF (HSTFLG) 
     &    CALL HBOOK1(1098,' Z(ISAJET) - Z(CDC) $',100,-15.,15.,0.)
        CALL HIDOPT(0,'STAT')     !  get the statistics for all histograms
      ENDIF
      ZVERTX = 9999.0
      ZERROR = 9999.0
C
C get Z information from CDC TRACKS and fill Z into a histigram
C
      CALL PATHST(DPATH)
      LDTRH = GZDTRH()
      IF (LDTRH .LE. 0) RETURN
      LDTRK = LQ(LDTRH - 1)
 100  CONTINUE
      IF (LDTRK .GT. 0) THEN
        IF (.NOT.BTEST(IQ(LDTRK+1),0)) THEN
          THETA = Q(LDTRK+9)
          IF (THETA .NE. 0) THEN
            ZPOSIT = Q(LDTRK+11) - Q(LDTRK+10) / TAN(THETA)
            CALL HF1(1099,ZPOSIT,1.)
          ENDIF
        ENDIF
        LDTRK = LQ(LDTRK)
        GOTO 100
      ENDIF
      CALL PATHRS
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
      IF (ABS(ZVERTX) .GT. C(LDGEH + 19)) THEN
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
C      
  999 RETURN
      END
