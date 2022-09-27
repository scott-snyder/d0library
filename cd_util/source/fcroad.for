      SUBROUTINE FCROAD(NF,IDF,NC,IDC,PT,NZ,NZFC)
C------------------------------------------------------------------------
C
C  Make Central Detector (CD) track by linking FDC and CDC tracks.
C  Loop over FDC tracks and match CDC tracks by comparing their parameters
C  (phi and ends in x,y view and theta); flag used tracks.
C  Store CD tracks in banks ZTRK. Increment number of CD tracks in bank
C  ZTRH (header for CD tracks).
C
C-   Inputs  : NF:     number of FDC tracks in the road
C-             IDF(I): ID number of FDC tracks in the road
C-             NC:     number of CDC tracks in the road
C-             IDC(I): ID number of CDC tracks in the road
C-             PT:     transverse momentum of a track
C-             NZ:     number of existing central tracks ZTRK in the road
C-   Outputs : NZFC:   number of central tracks ZTRK in the FC road
C
C-   Created  13-DEC-1990   Jeffrey Bantly   copied from FVROAD.FOR
C-   Updated   1-AUG-1991   Susan K. Blessing  Put EZRSETs in. 
C-   Updated   5-SEP-1991   Qizhong Li-Demarteau   use bits definition from
C-                                                params file instead of RCP 
C-   Updated  17-SEP-1991   Susan K. Blessing  Use correct value for the
C-    errors on phi and theta for the FDC (EPHIF, ETHETF)
C-   Updated  10-OCT-1991   Qizhong Li-Demarteau  make sure match is unique
C-   Updated  10-NOV-1991   Qizhong Li-Demarteau  fill ref. link to DTRK and
C-                                                FDCT
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZTRLNK.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
C
      INTEGER NZ, NZFC, NC, IC, NF, IF, IDF(*), IDC(*), NZTRAK
      INTEGER STAT,IBIT,JBIT,LOCF,LOCC, IER
      INTEGER LZTRK, LZTRH, GZZTRH, GZFDCT, GZDTRK
      INTEGER NTOT
      INTEGER IDX,LENGTH
      PARAMETER( IDX = 1 )
      INTEGER ICMTCH, MAXMTCH, ITMP, BESTMT, IDDTRK
      PARAMETER( MAXMTCH = 5 )
      INTEGER TMPC(MAXMTCH)
      REAL    TMPDPH(MAXMTCH)
C
      REAL PHIF,EPHIF,THETAF,PHIC,EPHIC,THETAC,XGF,YGF,XGC,YGC
      REAL ETHETF, ETHETC, QUALIT, PT, ERRPHI
C
      CHARACTER*4 FPATH,DPATH
C
      LOGICAL FIRST,MATCH,ENDSEG
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGETS('FPATH',IDX,FPATH,LENGTH,IER)
        CALL EZRSET
        CALL EZPICK('DTRAKS_RCP')
        CALL EZGETS('DPATH',IDX,DPATH,LENGTH,IER)
        CALL EZRSET
      END IF
      NZFC = 0
      LZTRH = GZZTRH()
      IF (LZTRH .LE. 0) GOTO 999
      NZTRAK = IQ(LZTRH+2)
C
      DO 200 IF=1,NF
        CALL PATHST(FPATH)
        LOCF = GZFDCT(IDF(IF))
        CALL PATHRS
        IF (LOCF .LE. 0) GOTO 200
        STAT = IQ(LOCF)
        IF (JBIT(STAT,IUSED).EQ.1) GO TO 200
        PHIF=Q(LOCF+6)
        THETAF = Q(LOCF+22)
        EPHIF = Q(LOCF+23)
        ETHETF=Q(LOCF+24)
        XGF=Q(LOCF+4)
        YGF=Q(LOCF+5)
        QUALIT = 9999.9
        ICMTCH = 0
        CALL VZERO_i(TMPC,5)
        CALL VZERO(TMPDPH,5)
        DO 300 IC=1,NC
          CALL PATHST(DPATH)
          LOCC = GZDTRK(IDC(IC))
          CALL PATHRS
          IF (LOCC .LE. 0) GOTO 300
          STAT = IQ(LOCC)
          IF (JBIT(STAT,IUSED).EQ.1) GO TO 300
          PHIC=Q(LOCC+6)
          EPHIC=Q(LOCC+16)
          THETAC=Q(LOCC+9)
          ETHETC=Q(LOCC+18)
          XGC=Q(LOCC+7)
          YGC=Q(LOCC+8)
C
          CALL FCMTCH(PHIC,PHIF,THETAC,THETAF,ETHETC,ETHETF,
     &                PT,XGF,YGF,XGC,YGC,MATCH)
          IF (MATCH) THEN
            ICMTCH = ICMTCH + 1
            IF (ICMTCH .GT. MAXMTCH) GOTO 200
            TMPC(ICMTCH) = LOCC
            TMPDPH(ICMTCH) = ABS(PHIF - PHIC)
          ENDIF
  300   CONTINUE
        IF (ICMTCH .GT. 1) THEN
C
C  ICMTCH > 1 means there are 2 CDC tracks matching to this FDC track.
C  Choose the best matching in PHI to make the matching unique
C            
          BESTMT = 0
          DO 205 ITMP = 1, ICMTCH
            IF (TMPDPH(ITMP) .LT. QUALIT) THEN
              QUALIT = TMPDPH(ITMP)
              BESTMT = ITMP
            ENDIF
  205     CONTINUE
        ELSE
          BESTMT = ICMTCH
        ENDIF
        IF (BESTMT .LE. 0) GOTO 200
        LOCC = TMPC(BESTMT)
        IDDTRK = IQ(LOCC - 5)
        THETAC = Q(LOCC + 9)
        PHIC = Q(LOCC + 6)
C
C Ok, make the matching to a ZTRK
C
        CALL MZFLAG(IXCOM,LOCF,IUSED,' ')
        CALL MZFLAG(IXCOM,LOCC,IUSED,' ')
        NZFC = NZFC+1
        CALL BKZTRK(LZTRK)
        IQ(LZTRK) = IBSET(IQ(LZTRK),11) ! mark it as 3D match
        IF (QUALIT .GE. 999.0) THEN
          QUALIT = ABS(PHIF - PHIC)    ! Quality of track (temp. def.)
        ENDIF
        LZTRH = GZZTRH()
        IQ(LZTRH+2)=IQ(LZTRH+2)+1
        CALL PATHST(FPATH)
        LOCF = GZFDCT(IDF(IF))
        CALL PATHRS
        CALL PATHST(DPATH)
        LOCC = GZDTRK(IDDTRK)
        CALL PATHRS
        LQ(LOCC - 2) = LZTRK
        LQ(LOCF - 2) = LZTRK
        LQ(LZTRK - 7) = LOCC
        LQ(LZTRK - 8) = LOCF
        IQ(LZTRK - 5) = NZTRAK + NZFC
        IQ(LZTRK+3)=IQ(LOCC - 5)
        IQ(LZTRK+4)=IQ(LOCF - 5)
        Q(LZTRK + 6) = QUALIT
        IF (THETAF*THETAC .NE. 0) Q(LZTRK+8) = ABS(THETAF-THETAC)
        NTOT = NZ + NZFC
        NTOT = MIN(NTOT,ZMAX)
        ZLINKS(NTOT) = LZTRK
        GOTO 200
  200 CONTINUE
C------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
