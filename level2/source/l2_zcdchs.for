      SUBROUTINE L2_ZCDCHS(ZVERTX,ZERROR)
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
C-   Updated  13-APR-1993   D Claes remove call to HIDOPT and decrease number
C-                                  of parameters in HSTATI call to make
C-                                  compatible with L2HBOOK versions
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDRFT.LINK'
      INTEGER NOENT, I, ILO, IHI 
C      INTEGER IER
      INTEGER LCDCH, GZCDCH, GZDGEH, LDRFT
      INTEGER GZISV1, LISV1
      REAL    ZISAJT, ZDIF
      REAL    ZVERTX, ZERROR, TOLDST
      REAL    ZSIGMA, ITRLMT, BIGSGM, SGMFCT, SGMFC2, CDCRES, R1, R2
      REAL    MEAN, SIGMA, OLDSGM, ZLO, ZHI, ZCUT, NENTRY
C     REAl    OLDMEAN
      REAL    CONTEN(100)
      REAL    L2_HSTATI, L2_HI
      REAL    ZCERMX
      LOGICAL FIRST, SNGLE1
C     LOGICAL HSTFLG, EZERROR
      LOGICAL MORETK
C
      COMMON/VERTEX_CUTS/ BIGSGM, CDCRES, ITRLMT, MORETK,
     &  SGMFCT, SGMFC2, TOLDST, ZCERMX, ZSIGMA
C
      SAVE FIRST
      DATA    FIRST/.TRUE./, SNGLE1/.TRUE./
C----------------------------------------------------------------------
C
C         Create/Set HBOOK directory for VERTEX
C
C*DC  No such references in L2HBOOK or L2CALIB code.  Can I skip w/o 
C*DC  consequences if using only L2HBOOK routines?
C*DC
C*DC*        CALL DHDIR('VERTEX_RCP','HBOOK_DIRECTORY',IER,' ')
C*DC*        IF (IER.NE.0) THEN
C*DC*          CALL ERRMSG('VERTEX','ZCDCHS',
C*DC*     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
C*DC*        ENDIF
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C*DC* Need to be passed ZSIGMA, SGMFCT, ITRLMT, BIGSGM, SGMFC2, CDCRES
C*DC* from L2_VERTEX_CDC_PARAMETERS
C
        LDGEH = GZDGEH()
        CALL L2_HBOOK1(1,' VERTEX IN Z$',100,-90.,90.,0.)
C*DC*        IF (HSTFLG)
C*DC*     &    CALL L2_HBOOK1(1098,' Z(ISAJET) - Z(CDC) $',25,-25.,25.,0.)
C*DC*   CALL HIDOPT(0,'STAT')     !  get the statistics for all histograms
      ENDIF
C
      ZVERTX = 9999.0
      ZERROR = 9999.0
C
C get Z information from CDC hits and fill Z into a histigram
C
      CALL L2_ZCDCGZ
C
      OLDSGM = 0.0
      CALL L2_HNOENT(1,NOENT)
      IF (NOENT .EQ. 0) GOTO 106
      NENTRY = 0.0
      IF (NOENT .EQ. 1) GOTO 105
C*DC*      OLDMEAN = L2_HSTATI(1099,1)
C*DC* 104  MEAN = HSTATI(1099,1,'HIST',0)
 104  MEAN = L2_HSTATI(1,1)
C*DC*      SIGMA = HSTATI(1099,2,'HIST',0)
      SIGMA = L2_HSTATI(1,2)
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
      CALL L2_HXI(1,ZLO,ILO)
      CALL L2_HXI(1,ZHI,IHI)
      DO 102 I = ILO-1, 1, -1
        IF (L2_HI(1,I) .EQ. 0.0) GOTO 103
 102  CONTINUE
 103  ILO = I+1
      DO 112 I = IHI+1, 100
        IF (L2_HI(1,I) .EQ. 0.0) GOTO 113
 112  CONTINUE
 113  IHI = I-1
      CALL L2_HUNPAK(1,CONTEN)
      NENTRY = 0.0
      DO 101 I = 1, 100
        IF (I .LT. ILO .OR. I .GT. IHI) CONTEN(I) = 0.0
        NENTRY = NENTRY + CONTEN(I)
 101  CONTINUE
      CALL L2_HPAK(1,CONTEN)
      GOTO 104
C
C*DC* 105  ZVERTX = HSTATI(1099,1,'HIST',0)
 105  ZVERTX = L2_HSTATI(1,1)
      IF (ZVERTX .GT. C(LDGEH + 19)) THEN
        ZERROR = 9999.9
        GOTO 106
      ELSE
        IF (NENTRY .GT. 1) THEN
C*DC*          ZERROR = HSTATI(1099,2,'HIST',0)
          ZERROR = L2_HSTATI(1,2)
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
C      IF (HSTFLG) THEN
C        LISV1 = GZISV1()
C        IF (LISV1 .GT. 0) THEN
C          ZISAJT = Q(LISV1+9)
C          ZDIF = ZISAJT - ZVERTX
C          CALL L2_HFILL(1098,ZDIF,0.,1.)
C        ENDIF
C      ENDIF
C
  106 CALL L2_HRESET(1,' ')
      LCDCH = GZCDCH()
      IF (LCDCH .GT. 0) CALL MZDROP(IXCOM,LCDCH,' ')
C
  999 RETURN
      END
