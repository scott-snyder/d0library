      SUBROUTINE ZTRELC(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,PT,NZTR,ZLINKR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : this is a special version of ZTRAKS used for
C-              electron analysis only. Find Central Detector tracks (ZTRKs) 
C-              in a road and build ZTMP banks for TRD analysis.
C-
C-   Inputs  : 
C-     ZVTX   = vertex position in Z
C-     PHIMIN   = minimum phi
C-     PHIMAX   = maximum phi
C-     THEMIN   = minimum theta
C-     THEMAX   = maximum theta
C-     PT       = transverse momentum of muon or electron candidate
C-
C-   Outputs : 
C-     NZTR:    number of Central Detector tracks in the road 
C-     ZLINKR(I): link of Central Detector track in the road
C-     
C-
C-   Created  19-NOV-1990   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZELCLK.INC/LIST'
      INTEGER NZTR, ZLINKR(ZTMAX)
      REAL    PHIMIN, PHIMAX, THEMIN, THEMAX, PT, ZVTX
C
C----------------------------------------------------------------------
C
C     Initialize a temporary LINK area
C
      IF (ZELCLK(1) .EQ. 0) 
     &  CALL MZLINT(IXCOM,'/ZELCLK/',ZELCLK,ZLINKT(ZTMAX),ZELCLK)
C
      CALL ZTRAKS(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,PT,NZTR,ZLINKT)
      IF (NZTR .LE. 0) GOTO 999
      CALL ZTRTMP(ZVTX,PHIMIN,PHIMAX)
C
      CALL UCOPY(ZLINKT(1),ZLINKR(1),NZTR)
C
C  deactivate the temporary link area
C
      ZELCLK(1) = 0
C
  999 RETURN
      END
