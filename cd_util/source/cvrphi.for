      SUBROUTINE CVRPHI(PHIV,PHIC,PT,XC,YC,XV,YV,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Match tracks between VTX and CDC 
C-
C-   Inputs  : 
C-            PHIV : Phi of VTX track
C-            PHIC : Phi of CDC track
C-            XC,YC: center of gravity of CDC track
C-            XV,YV: center of gravity of VTX track
C-   Outputs :OK   : true for matched tracks, 
C-                   false for non-matched tracks
C-
C-   Created  16-FEB-1990   Qizhong Li-Demarteau
C    Modified May 5, 1990 D.Zieminska: 
C    remove theta match and change name CVMTCH-->CVRPHI 
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  10-OCT-1991   Qizhong Li-Demarteau  allow crosses X axis matching
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IER
      REAL    PHIV, PHIC, PT, PTOLD
      REAL    XC, YC, XV, YV
      REAL    CVRESP, CVTMCS, SGMFCT, DSTFCT, PTLIMT, TOLPH2, TOLDI2
      REAL    TOLPHI, TOLDIS, DELPHI
      REAL    DELPH2
      LOGICAL OK, ENDSEG, FIRST
      LOGICAL EZERROR
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA PTOLD/9999.9/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','CVRPHI',
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
C   cut on the distance between VTX and CDC tracks for matching
C
      OK = ENDSEG(XC,YC,PHIC,XV,YV,PHIV,TOLDIS) 
C
      GOTO 999
  300 OK = .FALSE.
  999 RETURN
      END
