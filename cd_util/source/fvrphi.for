      SUBROUTINE FVRPHI(PHIV,PHIF,PT,XF,YF,XV,YV,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Match tracks between VTX and FDC 
C-
C-   Inputs  : 
C-            PHIV : Phi of VTX track
C-            PHIF : Phi of FDC track
C-            PT   : transverse momentum of a track
C-            XF,YF: the (X,Y,0) point on the FDC track
C-            XV,YV: center of gravity of VTX track
C-   Outputs :OK   : true for matched tracks, 
C-                   false for non-matched tracks
C-
C-   Created  03-APR-1990   Qizhong Li-Demarteau
C    Modified May 5, 1990 D.Zieminska: 
C    remove theta match and change name FVMTCH-->FVRPHI     
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  10-OCT-1991   Qizhong Li-Demarteau  allow crosses X axis matching
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IER
      REAL    PHIV, PHIF, PT, PTOLD
      REAL    XF, YF, XV, YV
      REAL    FVRESP, FVTMCS, SGMFCT, DSTFC2, PTLIMT, TOLPHF, TOLDIF
      REAL    TOLPHI, TOLDIS, DELPHI 
      REAL    DELPH2
      LOGICAL EZERROR
      LOGICAL OK, ENDSEG, FIRST
      SAVE FIRST, PTOLD
      DATA FIRST/.TRUE./
      DATA PTOLD/9999.9/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','FVRPHI',
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
C   cut on the distance between VTX and FDC tracks for matching
C
      OK = ENDSEG(XF,YF,PHIF,XV,YV,PHIV,TOLDIS) 
C
      GOTO 999
  300 OK = .FALSE.
  999 RETURN
      END
