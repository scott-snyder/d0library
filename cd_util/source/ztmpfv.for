      SUBROUTINE ZTMPFV(PHIMIN,PHIMAX,NZ1,NZTMP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : build ZTMP banks by FDC+VTX or FDC tracks,
C-                         which go through TRD
C-
C-   Inputs  : 
C-     PHIMIN:  minimum phi
C-     PHIMAX:  maximum phi
C-     NZ1:     # of ZTMP (built by CDC+VTX or CDC) exist already
C-   Outputs : ZTMP banks are built
C-             NZTMP: number of ZTMP tracks built by FDC+VTX or FDC only
C-
C-   Created  01-FEB-1992   Qizhong Li-Demarteau
C-   Updated  17-NOV-1992   Qizhong Li-Demarteau  added ZGF and ZGV as 
C-                                        arguments for the call to FVMTCH
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:ZELCLK.INC/LIST'                             
      INCLUDE 'D0$INC:PI.DEF'                             
      INCLUDE 'D0$LINKS:IZZTMP.LINK'                             
      INCLUDE 'D0$LINKS:IZFDCT.LINK'                             
      INCLUDE 'D0$LINKS:IZVTXT.LINK'                             
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
C
      INTEGER NZFV, NZFV2, NZTMP
      INTEGER LFTRH, LVTRH, LZTRH
      INTEGER STAT, IER, ITRK
      INTEGER GZFTRH, GZVTRH, GZVTXT, GZZTRH, JBIT, BITMT2
      INTEGER LVERH, LVERT, GZVERH, TLVTXT
      INTEGER NZ, NF, NZ1, IPATH, JPATH
      INTEGER IVMTCH, MAXMTCH, ITMP, BESTMT
      PARAMETER( MAXMTCH = 5 )
      INTEGER TMPV(MAXMTCH)
      REAL    TMPDPH(MAXMTCH)
      REAL    PHIMIN, PHIMAX, QUAL2D, TQUALT
      REAL    PHIF, EPHIF, THETAF, PHIV, EPHIV, THETAV
      REAL    XGF, YGF, ZGF, XGV, YGV, ZGV
      REAL    ETHETF, ETHETV, QUALIT, PT, ERRTHE
      CHARACTER*4 FPATH, VPATH
      EQUIVALENCE (IPATH, FPATH)
      EQUIVALENCE (JPATH, VPATH)
      LOGICAL FDONLY, PASSTRD
      LOGICAL EZERROR
      LOGICAL FIRST, MATCH, ENDSEG, MATCH2, UNIQUE
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTMPFV',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 400
        ENDIF
        CALL EZGET('ERRTHE',ERRTHE,IER)
        CALL EZGET_l('FDONLY',FDONLY,IER)
        CALL EZRSET
        CALL EZPICK('FTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTMPFV',
     &      'Unable to find bank FTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('FPATH',IPATH,IER)
        CALL EZRSET
        CALL EZPICK('VTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTMPFV',
     &    'Unable to find bank VTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('VPATH',JPATH,IER)
        CALL EZRSET
      END IF
C
      NZFV = 0
      NZFV2 = 0
      NZ = 0
      NF = 0
      PT = 1.0
      CALL PATHST(VPATH)
      LVTRH = GZVTRH()
      CALL PATHRS
      CALL PATHST(FPATH)
      LFTRH = GZFTRH()
      CALL PATHRS
      IF (LFTRH .LE. 0) GOTO 400
      LFDCT = LQ(LFTRH - IZFDCT)      
 100  IF (LFDCT .EQ. 0) GOTO 400
C
C   loop on FDC tracks in the phi road and check if it goes through TRD
C
      PHIF = Q(LFDCT + 6)
      IF (PHIF .GE. PHIMIN .AND. PHIF .LE. PHIMAX) THEN
        CALL ZFTTRD(LFDCT,PASSTRD)
        IF (.NOT. PASSTRD) GOTO 401
        THETAF = Q(LFDCT+22)
        EPHIF = Q(LFDCT+23)
        ETHETF = Q(LFDCT+24)
        XGF = Q(LFDCT+4)
        YGF = Q(LFDCT+5)
        ITRK = IQ(LFDCT-5)
        CALL FGETZ0(ITRK,ZGF)
        QUALIT = 9999.9
        QUAL2D = 9999.9
        UNIQUE = .TRUE.
        IVMTCH = 0
        CALL VZERO_i(TMPV,5)
        CALL VZERO(TMPDPH,5)
C
C   loop on VTX tracks in the phi road
C
        IF (LVTRH .LE. 0) GOTO 200
        LVTXT = LQ(LVTRH - IZVTXT)
 301    IF (LVTXT .LE. 0) GOTO 302
        PHIV = Q(LVTXT+6)
        IF (PHIV .GE. PHIMIN .AND. PHIV .LE. PHIMAX) THEN
          STAT = IQ(LVTXT)
          IF (JBIT(STAT,JUSED3).EQ.1) GO TO 300
          EPHIV = Q(LVTXT+16)
          THETAV = Q(LVTXT+9)
          ETHETV = Q(LVTXT+18)
          XGV = Q(LVTXT+7)
          YGV = Q(LVTXT+8)
          ZGV = Q(LVTXT+11)
C
C  If both FDC track and VTX track have good theta value, the matching
C  is done in 3-D. If one of the track has bad theta or no theta,
C  try to match them in R-PHI plane only. 
C  The difference in theta between FDC track and VTX track is stored in 
C  Q(LZTMP + 8). If Q(LZTMP + 8) greater than or equal to 9.99, it means
C  the match was done in R-PHI only due to no enough theta information.
C
          IF (ETHETF .LE. ERRTHE .AND. ETHETV .LE. ERRTHE) THEN
            CALL FVMTCH(PHIV,PHIF,THETAV,THETAF,ETHETV,ETHETF,
     &                PT,XGF,YGF,ZGF,XGV,YGV,ZGV,MATCH)
            IF (MATCH) THEN   
              IVMTCH = IVMTCH + 1
              IF (IVMTCH .GT. MAXMTCH) GOTO 200
              TMPV(IVMTCH) = LVTXT
              TMPDPH(IVMTCH) = ABS(PHIF - PHIV)
              IF (ABS(TMPDPH(IVMTCH)-TWOPI) .LT. HALFPI) THEN
                TMPDPH(IVMTCH) = ABS(TMPDPH(IVMTCH)-TWOPI)
              ENDIF
            END IF
          ELSE 
            CALL FVRPHI(PHIV,PHIF,PT,XGF,YGF,XGV,YGV,MATCH2)
            IF (MATCH2) THEN
              TQUALT = ABS(PHIF-PHIV)  
              IF (ABS(TQUALT-TWOPI) .LT. HALFPI) THEN
                TQUALT = ABS(TQUALT-TWOPI)
              ENDIF
              BITMT2 = JBIT(IQ(LFDCT),JUSED2)
              IF (BITMT2 .NE. 1) THEN
                QUAL2D = TQUALT
                TLVTXT = LVTXT
                CALL MZFLAG(0,LFDCT,JUSED2,' ')
              ELSE
                IF (TQUALT .LT. QUAL2D) THEN
                  QUAL2D = TQUALT
                  TLVTXT = LVTXT
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C
  300   LVTXT = LQ(LVTXT)
        GOTO 301
  302   IF (IVMTCH .GT. 0) THEN          
C
C   it is a 3D match ...
C
          IF (IVMTCH .GT. 1) THEN
C
C  IVMTCH > 1 means there are 2 VTX tracks matching to this FDC track.
C  Choose the best matching in PHI to make the matching unique
C            
            BESTMT = 0
            DO 205 ITMP = 1, IVMTCH
              IF (TMPDPH(ITMP) .LT. QUALIT) THEN
                QUALIT = TMPDPH(ITMP)
                BESTMT = ITMP
              ENDIF
  205       CONTINUE
          ELSE
            BESTMT = IVMTCH
          ENDIF
          IF (BESTMT .LE. 0) GOTO 200
          LVTXT = TMPV(BESTMT)
          CALL MZFLAG(0,LFDCT,JUSED3,' ')
          CALL MZFLAG(0,LVTXT,JUSED3,' ')
          NZFV = NZFV + 1
          NZ = NZ1 + NZFV + NZFV2 + NF 
          CALL BKZTMP(LZTMP(NZ))
          IQ(LZTMP(NZ)) = IBSET(IQ(LZTMP(NZ)),11) ! mark it as 3D match
          QUALIT = ABS(Q(LFDCT+6) - Q(LVTXT+6))
          LQ(LZTMP(NZ) - 6) = LVTXT
          LQ(LZTMP(NZ) - 8) = LFDCT
          IQ(LZTMP(NZ) - 5) = NZ
          IQ(LZTMP(NZ) + 3) = IQ(LVTXT - 5)
          IQ(LZTMP(NZ) + 5) = IQ(LFDCT - 5)
          Q(LZTMP(NZ) + 6) = QUALIT 
          Q(LZTMP(NZ) + 8) = ABS(Q(LFDCT+22) - Q(LVTXT+9))
        ELSE
          IF (QUAL2D .LT. 9999.0) THEN
C
C      it is a 2D match ...
C
            LVTXT = TLVTXT
            CALL MZFLAG(0,LVTXT,JUSED2,' ')
            NZFV2 = NZFV2 + 1
            NZ = NZ1 + NZFV + NZFV2 + NF 
            CALL BKZTMP(LZTMP(NZ))
            LQ(LZTMP(NZ) - 6) = LVTXT
            LQ(LZTMP(NZ) - 8) = LFDCT
            IQ(LZTMP(NZ) - 5) = NZ
            IQ(LZTMP(NZ) + 3) = IQ(LVTXT - 5)
            IQ(LZTMP(NZ) + 5) = IQ(LFDCT - 5)
            Q(LZTMP(NZ) + 6) = QUAL2D 
            Q(LZTMP(NZ) + 8) = 9.99         ! no theta match
          ENDIF
        ENDIF
C
C  store the non-matched FDC track into ZTMP if requested
C
  200   IF (FDONLY) THEN
          STAT = IQ(LFDCT)
          IF (JBIT(STAT,JUSED3).NE.1 .AND. JBIT(STAT,JUSED2).NE.1) THEN
            NF = NF + 1
            NZ = NZ1 + NZFV + NZFV2 + NF 
            CALL BKZTMP(LZTMP(NZ))
            QUALIT = 0.0     ! Quality of track (temp. def.)
            LQ(LZTMP(NZ) - 8) = LFDCT
            IQ(LZTMP(NZ) - 5) = NZ
            IQ(LZTMP(NZ) + 5) = IQ(LFDCT - 5)
            Q(LZTMP(NZ) + 6) = QUALIT 
            Q(LZTMP(NZ) + 8) = 0.0
          ENDIF
        ENDIF
      ENDIF
  401 LFDCT = LQ(LFDCT)
      GOTO 100
  400 NZTMP = NZFV + NZFV2 + NF 
C
  999 RETURN
      END
