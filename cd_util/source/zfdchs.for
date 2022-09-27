      SUBROUTINE ZFDCHS(ZVERTX,ZERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill hitogram for the Z along beam line
C-                         and find the VERTEX's Z position (using
C-                         FDC information only)
C-
C-   Inputs  : none
C-   Outputs : ZVERTX: vertex's Z position
C-             ZERROR: error on vertex's Z position
C-
C-   Created  13-SEP-1990   Jeffrey Bantly
C-   Updated  14-AUG-1991   Susan K. Blessing  Add EZRSET, remove reference
C-    to FDSPEC.INC and include ONEZ in the call to ZFDCGZ.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFDRT.LINK'
C
      INTEGER NOENT, NTRK, I, ILO, IHI, ERR, IER
      INTEGER LKFDCH, GZFDCH, GZFGEH, LKFGEH, FRATIO
      INTEGER GZISV1, LISV1, HALF, ZFDCEN(0:1), MAXHLF, TOTZ, TFRACT
      INTEGER ONEZ(0:1)
C
      REAL    ZISAJT, ZDIF, XC, YC, ZIN, ZOUT, FNUMER
      REAL    FVERTX(0:1), FERROR(0:1), ZVERTX, ZERROR, ZFDIF, ZFERR
      REAL    ZSIGMA, ITRLMT, BIGSGM, SGMFCT, SGMFC2, FDCRES, R1, R2
      REAL    MEAN, SIGMA, OLDMEAN, OLDSGM, ZLO, ZHI, ZCUT, NENTRY
      REAL    CONTEN(100), ZFDCUT
      REAL    HSTATI, HI
C
      LOGICAL FIRST, SNGLE1, HSTFLG, FHALF
C
      DATA FIRST/.TRUE./
      DATA SNGLE1/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')       ! Get FDC vertex-finding params
        CALL EZGET('FZSIGMA',ZSIGMA,ERR)
        CALL EZGET('FSGMFCT',SGMFCT,ERR)
        CALL EZGET('FITRLMT',ITRLMT,ERR)
        CALL EZGET('FBIGSGM',BIGSGM,ERR)
        CALL EZGET('FSGMFC2',SGMFC2,ERR)
        CALL EZGET('FDCRES', FDCRES,ERR)
        CALL EZGET('ZFDCUT', ZFDCUT,ERR)
        CALL EZGET_i('FRATIO', FRATIO,ERR)
        CALL EZGET_i('TFRACT', TFRACT,ERR)
        CALL EZGET('FNUMER', FNUMER,ERR)
        CALL EZGET_l('FHALF',FHALF,ERR)
        CALL EZGET_l('FHSTFLG',HSTFLG,ERR)
        CALL EZRSET
        LKFGEH = GZFGEH()
C
C         Create/Set HBOOK directory for VERTEX
C
        CALL DHDIR('VERTEX_RCP','HBOOK_DIRECTORY',IER,' ')
        IF (IER.NE.0) THEN
          CALL ERRMSG('VERTEX','ZFDCHS',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
        CALL HBOOK1(1096,' VERTEX IN Z, FDC HALF 0$',100,-90.,90.,0.)
        CALL HBOOK1(1097,' VERTEX IN Z, FDC HALF 1$',100,-90.,90.,0.)
        IF (HSTFLG) THEN
          CALL HBOOK1(1095,' Z(ISAJET) - Z(FDC) $',25,-25.,25.,0.)
        ENDIF
        CALL HIDOPT(0,'STAT')     !  get the statistics for all histograms
      ENDIF
      ZVERTX = 9999.0
      ZERROR = 9999.0
      FVERTX(0) = 9999.0
      FERROR(0) = 9999.0
      FVERTX(1) = 9999.0
      FERROR(1) = 9999.0
C
C get Z information from FDC hits and fill Z into a histogram
C
      CALL ZFDCGZ(ONEZ)
C
      MAXHLF = 0
      IF(FHALF) MAXHLF = 1
      DO 100 HALF = 0, MAXHLF
        OLDSGM = 0.0
        CALL HNOENT(1096+HALF,NOENT)
        IF (NOENT .EQ. 0) GOTO 106
        NENTRY = 0.0
        IF (NOENT .EQ. 1) GOTO 105
        OLDMEAN = HSTATI(1096+HALF,1,'HIST',1)
  104   MEAN = HSTATI(1096+HALF,1,'HIST',1)
        SIGMA = HSTATI(1096+HALF,2,'HIST',1)
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
        CALL HXI(1096+HALF,ZLO,ILO)
        CALL HXI(1096+HALF,ZHI,IHI)
        DO 102 I = ILO-1, 1, -1
          IF (HI(1096+HALF,I) .EQ. 0.0) GOTO 103
  102   CONTINUE
  103   ILO = I+1
        DO 112 I = IHI+1, 100
          IF (HI(1096+HALF,I) .EQ. 0.0) GOTO 113
  112   CONTINUE
  113   IHI = I-1
        CALL HUNPAK(1096+HALF,CONTEN,' ',1)
        NENTRY = 0.0
        DO 101 I = 1, 100
          IF (I .LT. ILO .OR. I .GT. IHI) CONTEN(I) = 0.0
          NENTRY = NENTRY + CONTEN(I)
  101   CONTINUE
        ZFDCEN(HALF) = NENTRY
        CALL HPAK(1096+HALF,CONTEN)
        GOTO 104
C
  105   FVERTX(HALF) = HSTATI(1096+HALF,1,'HIST',1)
        IF ( ABS(FVERTX(HALF)) .GT. (C(LKFGEH + 8)-C(LKFGEH + 5)) ) THEN
          FERROR(HALF) = 9999.9
          GOTO 106
        ELSE
          IF (NENTRY .GT. 1) THEN
            FERROR(HALF) = HSTATI(1096+HALF,2,'HIST',1)
            FERROR(HALF) = FERROR(HALF) / SQRT(NENTRY)
          ELSE
            IF (SNGLE1) THEN
              CALL GTFALH(0,0,0,0,0,XC,YC,ZIN)
              CALL GTFALH(0,0,4,0,0,XC,YC,ZOUT)
              SNGLE1 = .FALSE.
            ENDIF
            IF (ZIN .LE. 1.0 .OR. ZOUT .LE. 1.0) THEN
              FERROR(HALF) = 1.4
            ELSE
              FERROR(HALF) = FDCRES * ZOUT / ZIN
            ENDIF
          ENDIF
        ENDIF
C
  100 CONTINUE                          ! End of HALF loop.
      ZFDIF = ABS(FVERTX(0) - FVERTX(1))
      ZFERR = (FERROR(0) + FERROR(1))/2.
      IF(FHALF) THEN
        IF( ZFDIF .LT. ZFDCUT ) THEN
          ZVERTX = (FVERTX(0) + FVERTX(1)) / 2.
          ZERROR = ZFERR
        ELSEIF( ZFDCEN(0) .GE. ZFDCEN(1) ) THEN
          ZVERTX = FVERTX(0)
          ZERROR = FERROR(0)
        ELSEIF( ZFDCEN(0) .LT. ZFDCEN(1) ) THEN
          ZVERTX = FVERTX(1)
          ZERROR = FERROR(1)
        ELSE
          ZVERTX = 9999.0
          ZERROR = 9999.0
        ENDIF
      ELSE
        ZVERTX = FVERTX(0)
        ZERROR = FERROR(0)
        IF(ABS(ZVERTX).GT.ZFDCUT) THEN
          TOTZ = (ONEZ(0) + ONEZ(1)) / TFRACT
          IF( ONEZ(0) .LE. 0 ) THEN
            IF( ONEZ(1).NE.0 ) ZVERTX = ZVERTX * (FNUMER/FLOAT(ONEZ(1)))
          ELSEIF( (ONEZ(1)/ONEZ(0)).GE.FRATIO) THEN
            ZVERTX = ZVERTX*FLOAT((ONEZ(0)+TOTZ))/FLOAT((ONEZ(1)+TOTZ))
          ENDIF
          IF( ONEZ(1) .LE. 0 ) THEN
            IF( ONEZ(0).NE.0 ) ZVERTX = ZVERTX * (FNUMER/FLOAT(ONEZ(0)))
          ELSEIF( (ONEZ(0)/ONEZ(1)).GE.FRATIO) THEN
            ZVERTX = ZVERTX*(FLOAT(ONEZ(1)+TOTZ))/FLOAT((ONEZ(0)+TOTZ))
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
          CALL HF1(1095,ZDIF,1.)
        ENDIF
      ENDIF
C
  106 CALL HRESET(1096,' ')
      CALL HRESET(1097,' ')
      LKFDCH = GZFDCH()
      IF (LKFDCH .GT. 0) CALL MZDROP(IXCOM,LKFDCH,' ')
C---------------------------------------------------------------------------
  999 RETURN
      END
