      FUNCTION QCD_UPK_NUT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get the information for Missing Et from 3
C-                         PNUT banks from MDST
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   20-DEC-1992   Andrew G. Brandt
C-   Updated   24-SEP-1993   Keep 2 NUT Banks (2,3) or (4,5)
C-   Updated   16-NOV-1993   Protect against 2 or 3 when expect 4 or 5
C-   Updated   22-FEB-1994   Keep 2 NUT banks 2 and 4
C-   Updated   02-NOV-1994   update for CW  NUT_NUM out of common
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_NUT.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
C
      INTEGER I,NUT_NUM
      LOGICAL QCD_UPK_NUT
C
      REAL    ENUT(4),ET,TH,ETA,PHI,SIG(3)
      REAL    SCET,SIGEZ,CORR1,CORR2,CORR3
      INTEGER IER,INUT

C----------------------------------------------------------------------
C
C        missing ET PNUT banks
C
      QCD_UPK_NUT=.TRUE.
C
C  Initialize
C
      DO I=1,2
        NUT_E(I)    =-999.
        NUT_ET(I)   =-999.
        NUT_SGX(I)  =-999.
        NUT_SGY(I)  =-999.
        NUT_SGET(I) =-999.
        NUT_ETA(I)  =-999.
        NUT_PHI(I)  =-999.
        NUT_SCET(I) =-999.
      END DO
C
C Get Number of Nut banks
C
      CALL GTPNUT_TOTAL(NUT_NUM,IER)
      IF(IER.NE.0.OR.NUT_NUM.LE.0) RETURN
C
C Save PNUT 2+4
C
      IF(ESCALE.AND.NUT_NUM.LT.4) THEN
        CALL ERRMSG('QCD_UPK_NUT ',' QCD_UPK_NUT ',
     +              ' PNUT4 not filled ','W')
      END IF
      INUT=0
      DO I=1,2
        INUT=INUT+2
        IF(INUT.GT.NUT_NUM) GO TO 999
        CALL GTPNUT(INUT,ENUT,ET,TH,ETA,PHI,SIG,IER)
        NUT_E(I)   =ENUT(4)
        NUT_ET(I)  =ET
        NUT_SGX(I) =SIG(1)
        NUT_SGY(I) =SIG(2)
        NUT_SGET(I)=SIG(3)
        NUT_ETA(I) =ETA
        NUT_PHI(I) =PHI
        CALL GTPNUT_2(INUT,SCET,SIGEZ,CORR1,CORR2,CORR3,IER)
        NUT_SCET(I)=SCET
  100 ENDDO
C
  999 RETURN
      END
