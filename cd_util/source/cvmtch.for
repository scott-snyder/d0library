      SUBROUTINE CVMTCH
     &  (PHIV,PHIC,THEV,THEC,ETHV,ETHC,PT,XC,YC,ZC,XV,YV,ZV,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Match tracks between VTX and CDC 
C-
C-   Inputs  : 
C-            PHIV : Phi of VTX track
C-            PHIC : Phi of CDC track
C-            THEV : Theta of VTX track
C-            THEC : Theta of CDC track
C-            ERRV : error on theta of VTX track
C-            ERRC : error on theta of CDC track
C-            PT   : transverse momentum of a track
C-            XC,YC,ZC: center of gravity of CDC track
C-            XV,YV,ZV: center of gravity of VTX track
C-   Outputs :OK   : true for matched tracks, 
C-                   false for non-matched tracks
C-
C-   Created  16-FEB-1990   Qizhong Li-Demarteau
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  10-OCT-1991   Qizhong Li-Demarteau  allow crosses X axis matching
C-   Updated  17-NOV-1992   Qizhong Li-Demarteau  added ZC and ZV as input 
C-                                                arguments 
C-   Updated  21-MAY-1993   Ed Oltman  Replace DIFTHE with theta-dependant VTX
C-                                  theta resolution
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IER
      REAL    PHIV, PHIC, THEV, THEC, ETHV, ETHC, PT, PTOLD
      REAL    XC, YC, XV, YV, RC, ZC, RV, ZV
      REAL    CVRESP, CVTMCS, SGMFCT, DSTFCT, PTLIMT, TOLPH2, TOLDI2
      REAL    DIFTHE, ERRTHE, TOLPHI, TOLDIS, DELPHI, DELTHE, THEERR
      REAL    DELPH2, TOLDSZ, TOLZMN
      LOGICAL OK, ENDSEG, FIRST, OK_XY, OK_RZ, ZDISCT
      LOGICAL EZERROR
      LOGICAL ENDSEG_CV,VTXTHR
      SAVE FIRST, PTOLD
      DATA FIRST/.TRUE./
      DATA PTOLD/9999.9/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','CVMTCH',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('CVRESP',CVRESP,IER)
        CALL EZGET('CVTMCS',CVTMCS,IER)
        CALL EZGET('SGMFCT',SGMFCT,IER)
        CALL EZGET('DSTFCT',DSTFCT,IER)
        CALL EZGET('PTLIMT',PTLIMT,IER)
        CALL EZGET('TOLPH2',TOLPH2,IER)
        CALL EZGET('TOLDI2',TOLDI2,IER)
        CALL EZGET('TOLZMN',TOLZMN,IER)
        CALL EZGET('ERRTHE',ERRTHE,IER)
        CALL EZRSET
      END IF
C
      IF (PT .NE. PTOLD) THEN
C
C  use tighter cut for higher PT tracks in matching CDC tracks with VTX tracks
C  use loose cut in the matching, when PT of the track is lower than the 
C  PT limit. The PT limit and the cuts are defined in ZTRAKS.RCP
C
        IF (PT .GE. PTLIMT) THEN
          TOLPHI = SGMFCT * SQRT(CVRESP**2 + (CVTMCS / PT)**2)
          TOLDIS = TOLPHI * DSTFCT      
        ELSE
          TOLPHI = TOLPH2
          TOLDIS = TOLDI2
        ENDIF
        PTOLD = PT
      ENDIF
C            
C   phi cut for matching          
C            
      DELPHI = ABS(PHIC - PHIV)
      DELPH2 = TWOPI - DELPHI
      IF (DELPHI .GT. TOLPHI .AND. DELPH2 .GT. TOLPHI) GO TO 300
C            
C   theta cut for matching        
C
      DELTHE = ABS(THEC - THEV)
      IF (VTXTHR(1,THEC,DELTHE)) THEN
        THEERR = SQRT(ETHC**2 + ETHV**2)
        IF (THEERR .GT. ERRTHE) GOTO 300
      ELSE
        GOTO 300
      ENDIF
C            
C   cut on the distance between VTX and CDC tracks for matching
C
      TOLDSZ = DELTHE * DSTFCT + TOLZMN
      OK_XY = ENDSEG(XC,YC,PHIC,XV,YV,PHIV,TOLDIS) 
      OK_RZ = .TRUE.
      IF (OK_XY) THEN
        RC = SQRT(XC**2 + YC**2)
        RV = SQRT(XV**2 + YV**2)
        OK_RZ = ENDSEG(ZC,RC,THEC,ZV,RV,THEV,TOLDSZ) 
      ENDIF
      OK = OK_XY .AND. OK_RZ
C
      GOTO 999
  300 OK = .FALSE.
  999 RETURN
      END
