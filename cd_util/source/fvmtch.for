      SUBROUTINE FVMTCH
     &  (PHIV,PHIF,THEV,THEF,ETHV,ETHF,PT,XF,YF,ZF,XV,YV,ZV,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Match tracks between VTX and FDC 
C-
C-   Inputs  : 
C-            PHIV : Phi of VTX track
C-            PHIF : Phi of FDC track
C-            THEV : Theta of VTX track
C-            THEF : Theta of FDC track
C-            ERRV : error on theta of VTX track
C-            ERRF : error on theta of FDC track
C-            PT   : transverse momentum of a track
C-            XF,YF,ZF: the (X,Y,0) point on the FDC track
C-            XV,YV,ZV: center of gravity of VTX track
C-   Outputs :OK   : true for matched tracks, 
C-                   false for non-matched tracks
C-
C-   Created  03-APR-1990   Qizhong Li-Demarteau
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  10-OCT-1991   Qizhong Li-Demarteau  allow crosses X axis matching
C-   Updated  17-NOV-1992   Qizhong Li-Demarteau  added ZF and ZV as input 
C-                                                arguments 
C-   Updated  21-MAY-1993   Ed Oltman  Replace DIFTHE with theta-dependant VTX
C-                                  theta resolution
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IER
      REAL    PHIV, PHIF, THEV, THEF, ETHV, ETHF, PT, PTOLD
      REAL    XF, YF, XV, YV, RF, ZF, RV, ZV
      REAL    FVRESP, FVTMCS, SGMFCT, DSTFC2, PTLIMT, TOLPHF, TOLDIF
      REAL    DIFTHE, ERRTHE, TOLPHI, TOLDIS, DELPHI, DELTHE, THEERR
      REAL    DELPH2, TOLDSZ, TOLZMN
      LOGICAL OK, ENDSEG, FIRST, OK_XY, OK_RZ
      LOGICAL EZERROR
      LOGICAL ENDSEG_FV,VTXTHR
      SAVE FIRST, PTOLD
      DATA FIRST/.TRUE./
      DATA PTOLD/9999.9/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','FVMTCH',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('FVRESP',FVRESP,IER)
        CALL EZGET('FVTMCS',FVTMCS,IER)
        CALL EZGET('SGMFCT',SGMFCT,IER)
        CALL EZGET('DSTFC2',DSTFC2,IER)
        CALL EZGET('PTLIMT',PTLIMT,IER)
        CALL EZGET('TOLPHF',TOLPHF,IER)
        CALL EZGET('TOLDIF',TOLDIF,IER)
        CALL EZGET('TOLZMN',TOLZMN,IER)
        CALL EZGET('ERRTHE',ERRTHE,IER)
        CALL EZRSET
      END IF
C
      IF (PT .NE. PTOLD) THEN
C
C  use tighter cut for higher PT tracks in matching FDC tracks with VTX tracks
C  use loose cut in the matching, when PT of the track is lower than the 
C  PT limit. The PT limit and the cuts are defined in ZTRAKS.RCP
C
        IF (PT .GE. PTLIMT) THEN
          TOLPHI = SGMFCT * SQRT(FVRESP**2 + (FVTMCS / PT)**2)
          TOLDIS = TOLPHI * DSTFC2      
        ELSE
          TOLPHI = TOLPHF
          TOLDIS = TOLDIF
        ENDIF
        PTOLD = PT
      ENDIF
C            
C   phi cut for matching          
C            
      DELPHI = ABS(PHIF - PHIV)
      DELPH2 = TWOPI - DELPHI
      IF (DELPHI .GT. TOLPHI .AND. DELPH2 .GT. TOLPHI) GO TO 300
C            
C   theta cut for matching        
C
      DELTHE = ABS(THEF - THEV)
      IF (VTXTHR(1,THEF,DELTHE)) THEN
        THEERR = SQRT(ETHF**2 + ETHV**2)
        IF (THEERR .GT. ERRTHE) GOTO 300
      ELSE
        GOTO 300
      ENDIF
C            
C   cut on the distance between VTX and FDC tracks for matching
C
      TOLDSZ = DELTHE * DSTFC2 + TOLZMN
      OK_XY = ENDSEG(XF,YF,PHIF,XV,YV,PHIV,TOLDIS) 
      OK_RZ = .TRUE.
      IF (OK_XY) THEN
        RF = SQRT(XF**2 + YF**2)
        RV = SQRT(XV**2 + YV**2)
        OK_RZ = ENDSEG(ZF,RF,THEF,ZV,RV,THEV,TOLDSZ) 
      ENDIF
      OK = OK_XY .AND. OK_RZ
C
      GOTO 999
  300 OK = .FALSE.
  999 RETURN
      END
