      SUBROUTINE CVROAD(NC,IDC,NV,IDV,PT,NZ,NZCV)
C------------------------------------------------------------------------
C 
C  Make Central Detector (CD) tracks by linking CDC and VTX tracks.
C  Loop over CDC tracks and match VTX tracks by comparing their parameters 
C  (phi and ends in x,y view and theta); flag matched tracks.
C  Store CD tracks in banks ZTRK. Increment number of CD tracks in bank 
C  ZTRH (header for CD tracks).
C                               
C-   Inputs  : NC:     number of CDC tracks in the road
C-             IDC(I): ID number of CDC tracks in the road
C-             NV:     number of VTX tracks in the road
C-             IDV(I): ID number of VTX tracks in the road
C-             PT:     transverse momentum of a track
C-             NZ:     number of existing central tracks ZTRK in the road
C-   Outputs : NZCV:   number of central tracks ZTRK in the CV road
C
C  Daria Zieminska  Nov 1988
C-   Updated  06-NOV-1989   Qizhong Li-Demarteau   change ZTRK bank structure
C-                                                 to use reference link 
C-   Updated  15-DEC-1989   Qizhong Li-Demarteau   deal hight PT and low
C-                                                 PT tracks differently 
C-   Updated  10-FEB-1990   Qizhong Li-Demarteau   new theta cut
C-   Updated  21-JAN-1991   Qizhong Li-Demarteau   make 2-D match for the 
C-                                                 non-theta tracks
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau   added EZRSET and EZERROR
C-   Updated   5-SEP-1991   Qizhong Li-Demarteau   use bits definition from
C-                                                params file instead of RCP 
C-   Updated  10-OCT-1991   Qizhong Li-Demarteau  make sure match is unique
C-   Updated   9-NOV-1991   Qizhong Li-Demarteau  added PATHST for VTX and
C-                                        fill ref. link for DTRK and VTXT
C-   Updated  17-NOV-1992   Qizhong Li-Demarteau  added ZGC and ZGV as 
C-                                        arguments for the call to CVMTCH
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:ZTRLNK.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
C
      INTEGER NZ, NZCV, NV, IV, NC, IC, IDC(*), IDV(*), NZTRAK
      INTEGER STAT,IBIT,JBIT,LOCC,LOCV, IER, IPATH, JPATH
      INTEGER LZTRK, LZTRH, GZZTRH, GZDTRK, GZVTXT, LVTXT2
      INTEGER BITMT2, NZCV2, IV2
      INTEGER NTOT
      INTEGER IVMTCH, MAXMTCH, ITMP, BESTMT, IDVTXT
      PARAMETER( MAXMTCH = 5 )
      INTEGER TMPV(MAXMTCH)
      REAL    TMPDPH(MAXMTCH)
      REAL PHIC, EPHIC, THETAC, PHIV, EPHIV, THETAV
      REAL    XGC, YGC, ZGC, XGV, YGV, ZGV
      REAL ETHETC, ETHETV, QUALIT, PT, QUAL2D, TQUALT, ERRTHE
      CHARACTER*4 DPATH, VPATH
      EQUIVALENCE (IPATH, DPATH)
      EQUIVALENCE (JPATH, VPATH)
      LOGICAL FIRST, MATCH, ENDSEG, MATCH2, UNIQUE, MATCH2D
      LOGICAL EZERROR
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','CVROAD',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('ERRTHE',ERRTHE,IER)
        CALL EZGET('MATCH2D',MATCH2D,IER)
        CALL EZRSET
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','CVROAD',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('DPATH',IPATH,IER)
        CALL EZRSET
        CALL EZPICK('VTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','CVROAD',
     &    'Unable to find bank VTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('VPATH',JPATH,IER)
        CALL EZRSET
      END IF
      NZCV = 0
      NZCV2 = 0
      LZTRH = GZZTRH()
      IF (LZTRH .LE. 0) GOTO 999
      NZTRAK = IQ(LZTRH+2)      
C
      DO 200 IC=1,NC
        IF (IDC(IC) .LE. 0) GOTO 200
        CALL PATHST(DPATH)
        LOCC = GZDTRK(IDC(IC))
        CALL PATHRS
        IF (LOCC .LE. 0) GOTO 200
        STAT = IQ(LOCC)
        IF (JBIT(STAT,IUSED).EQ.1) GO TO 200
        PHIC=Q(LOCC+6)
        THETAC=Q(LOCC+9)
        EPHIC=Q(LOCC+16)
        ETHETC=Q(LOCC+18)
        XGC=Q(LOCC+7)
        YGC=Q(LOCC+8)
        ZGC=Q(LOCC+11)
        QUALIT = 9999.9
        QUAL2D = 9999.9
        UNIQUE = .TRUE.
        IVMTCH = 0
        CALL VZERO(TMPV,5)
        CALL VZERO(TMPDPH,5)
        DO 300 IV=1,NV
          IF (IDV(IV) .LE. 0) GOTO 300
          CALL PATHST(VPATH)
          LOCV = GZVTXT(IDV(IV))
          CALL PATHRS
          IF (LOCV .LE. 0) GOTO 300
          STAT = IQ(LOCV)
          IF (JBIT(STAT,IUSED).EQ.1) GO TO 300
          PHIV=Q(LOCV+6)
          EPHIV=Q(LOCV+16)
          THETAV=Q(LOCV+9)
          ETHETV=Q(LOCV+18)
          XGV=Q(LOCV+7)
          YGV=Q(LOCV+8)
          ZGV=Q(LOCV+11)
C
C  If both CDC track and VTX track have good theta value, the matching
C  is done in 3-D. If one of the track has bad theta or no theta,
C  try to match them in R-PHI plane only (when MATCH2D switch is on). 
C  The difference in theta between CDC track and VTX track is stored in 
C  Q(LZTRK + 8). If Q(LZTRK + 8) greater than or equal to 9.99, it means
C  the match was done in R-PHI only due to no enough theta information.
C
          IF (ETHETC .LE. ERRTHE .AND. ETHETV .LE. ERRTHE) THEN
            CALL CVMTCH(PHIV,PHIC,THETAV,THETAC,ETHETV,ETHETC,
     &                PT,XGC,YGC,ZGC,XGV,YGV,ZGV,MATCH)
            IF (MATCH) THEN   
              IVMTCH = IVMTCH + 1
              IF (IVMTCH .GT. MAXMTCH) GOTO 200
              TMPV(IVMTCH) = LOCV
              TMPDPH(IVMTCH) = ABS(PHIC - PHIV)
            ENDIF
          ELSE 
            IF (.NOT.MATCH2D) GOTO 300
            IF (ETHETC .GT. ERRTHE .AND. ETHETV .GT. ERRTHE) 
     &        GOTO 300
            CALL CVRPHI(PHIV,PHIC,PT,XGC,YGC,XGV,YGV,MATCH2)
            IF (MATCH2) THEN
              TQUALT = ABS(PHIC-PHIV)     ! Quality of track (temp. def.)
              BITMT2 = JBIT(IQ(LOCC),IUSED2)
              IF (BITMT2 .NE. 1) THEN
                QUAL2D = TQUALT
                LVTXT2 = LOCV
                IV2 = IV
                CALL MZFLAG(0,LOCC,IUSED2,' ')
              ELSE
                IF (TQUALT .LT. QUAL2D) THEN
                  UNIQUE = .FALSE.
                  QUAL2D = TQUALT
                  LVTXT2 = LOCV
                  IV2 = IV
                ENDIF
              ENDIF
            ENDIF
          ENDIF
  300   CONTINUE
        IF (IVMTCH .GT. 0) THEN          
C
C   it is a 3D matching ...
C
          IF (IVMTCH .GT. 1) THEN
C
C  IVMTCH > 1 means there are 2 VTX tracks matching to this CDC track.
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
          LOCV = TMPV(BESTMT)
          IDVTXT = IQ(LOCV - 5)
          THETAV = Q(LOCV + 9)
          PHIV = Q(LOCV + 6)
C
C Ok, make the matching to a ZTRK
C
          CALL MZFLAG(0,LOCC,IUSED,' ')
          CALL MZFLAG(0,LOCV,IUSED,' ')
          NZCV = NZCV + 1
          CALL BKZTRK(LZTRK)
          IQ(LZTRK) = IBSET(IQ(LZTRK),11) ! mark it as 3D match
          IF (QUALIT .GE. 999.0) THEN
            QUALIT = PHIC - PHIV    ! Quality of track (temp. def.)
          ENDIF
          LZTRH = GZZTRH()
          IQ(LZTRH+2) = IQ(LZTRH+2) + 1
          CALL PATHST(DPATH)
          LOCC = GZDTRK(IDC(IC))
          CALL PATHRS
          CALL PATHST(VPATH)
          LOCV = GZVTXT(IDVTXT)
          CALL PATHRS
          LQ(LOCV - 2) = LZTRK
          LQ(LOCC - 2) = LZTRK
          LQ(LZTRK - 6) = LOCV
          LQ(LZTRK - 7) = LOCC
          IQ(LZTRK - 5) = NZTRAK + NZCV + NZCV2
          IQ(LZTRK + 2) = IQ(LOCV - 5)
          IQ(LZTRK + 3) = IQ(LOCC - 5)
          Q(LZTRK + 6) = QUALIT 
          Q(LZTRK + 8) = THETAC - THETAV
          NTOT = NZ + NZCV + NZCV2
          NTOT = MIN(NTOT,ZMAX)
          ZLINKS(NTOT) = LZTRK
          GOTO 200
        ELSE
C
C When no 3D matching is found, check if a 2D matching is requested and
C is found
C
          IF (QUAL2D .LT. 9999.0) THEN
            CALL MZFLAG(0,LOCC,IUSED,' ')
            CALL MZFLAG(0,LVTXT2,IUSED2,' ')
            NZCV2 = NZCV2 + 1
            CALL BKZTRK(LZTRK)
            LZTRH = GZZTRH()
            IQ(LZTRH + 2) = IQ(LZTRH + 2) + 1
            CALL PATHST(DPATH)
            LOCC = GZDTRK(IDC(IC))
            CALL PATHRS
            CALL PATHST(VPATH)
            LOCV = GZVTXT(IDV(IV2))
            CALL PATHRS
            LQ(LOCV - 2) = LZTRK
            LQ(LOCC - 2) = LZTRK
            LQ(LZTRK - 6) = LOCV
            LQ(LZTRK - 7) = LOCC
            IQ(LZTRK - 5) = NZTRAK + NZCV + NZCV2
            IQ(LZTRK + 2) = IQ(LOCV - 5)
            IQ(LZTRK + 3) = IQ(LOCC - 5)
            Q(LZTRK + 6) = Q(LOCC+6) - Q(LOCV+6)
            Q(LZTRK + 8) = 9.9999         ! no theta match
            IF (UNIQUE) IQ(LZTRK) = IBSET(IQ(LZTRK),12) 
C                                       ! mark it as unique 2D match
            IF (Q(LOCV + 18) .GT. ERRTHE) THEN
              IQ(LZTRK) = IBSET(IQ(LZTRK),10) 
C                                  ! mark it no theta from VTX track
            ELSE
              IQ(LZTRK) = IBSET(IQ(LZTRK),9) 
            ENDIF
            NTOT = NZ + NZCV + NZCV2
            NTOT = MIN(NTOT,ZMAX)
            ZLINKS(NTOT) = LZTRK
          ENDIF
        ENDIF
  200 CONTINUE
      NZCV = NZCV + NZCV2
C
  999 RETURN
      END
