      SUBROUTINE DTRK_FIX(THC,SZC,STC,STZC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FIX UP DTRK SCREWUPS MADE IN CDXYZL.  For that rare
C-   case where THC = pi/2, default values are returned. 
C-
C-   Inputs  : THC = Theta value                (unchanged)
C-             SZC = error on Z0
C-             STC = error on theta
C-             STZC= covariance term theta-z0
C-   Outputs : SZC,STC,STZC -- corrected values..
C-   Controls: 
C-
C-   Created  21-JUL-1993   Ed Oltman
C-   Updated   1-NOV-1993   Ed Oltman  Protect divide, sqrt 
C-   Updated  17-NOV-1993   Stefano Lami   Check DTRK bank version 
C-   Updated  29-NOV-1993   Qizhong Li-Demarteau  fixed bug on getting 
C-              DTRK bank version, changed GZDTRK from REAL to INTEGER
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      REAL THC,SZC,STC,STZC
      REAL STC1,SN,CS,TMP
      REAL STC_90,SZC_90,STZC_90
      INTEGER NVERS,LDTRK
      INTEGER GZDTRK
      PARAMETER (STC_90 = 0.040)     ! These default values are based on
      PARAMETER (SZC_90 = 0.250)     ! reco-V11.17 CDC tracks..
      PARAMETER (STZC_90= 0.000)     ! Remember -- there use is RARE!!
C----------------------------------------------------------------------
      NVERS=0
      LDTRK = GZDTRK(0)
      IF (LDTRK .LE. 0) GOTO 999
      NVERS=IBITS(IQ(LDTRK),13,5)
      IF(NVERS.EQ.1) GOTO 999     ! check bank version, =1 since reco V12
      SN = SIN(THC)
      CS = COS(THC)
      IF (CS .NE. 0.) THEN
        STC = AMAX1(STC,1.E-06)
        STC1 = STC*ABS( SN/CS )**1.5
        TMP = STZC/STC**2
C       SZC = SQRT(SZC**2 - (STC*TMP)**2 + (STC1*TMP)**2)
        SZC = SZC**2 - (1.-ABS(SN/CS)**3)*(STZC/STC)**2
        IF (SZC .GE. 0.) THEN
          SZC = SQRT(SZC)
        ELSE
          SZC =SZC_90
        ENDIF
        STZC= STC1**2*TMP
        STC = STC1
      ELSE
        CALL ERRMSG('DTRK has THETA = pi/2',
     &              'DTRK_FIX',
     &              'Unable to recover r-z errors: defaults used',
     &              'W')
        SZC = SZC_90
        STC = STC_90
        STZC= STZC_90
      ENDIF
  999 RETURN
      END
