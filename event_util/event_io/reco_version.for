      SUBROUTINE RECO_VERSION(VERSION,PASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns RECO version and pass number 
C-
C-   Inputs  : 
C-   Outputs : VERSION   I  RECO version number
C-             PASS      I  RECO pass number
C-   Controls: 
C-
C-   Created  25-MAY-1993   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LHSTR,GZHSTR
      INTEGER VERSION,PASS
      CHARACTER*40 PROD_NAME
C----------------------------------------------------------------------
      VERSION = 0
      PASS    = 0
      IF(LHEAD.NE.0) THEN
        LHSTR = GZHSTR()
        DO WHILE (LHSTR.GT.0)
          CALL UHTOC(IQ(LHSTR+7),40,PROD_NAME,40)
          IF(PROD_NAME.EQ.'FULL_D0RECO') THEN
            VERSION = IQ(LHSTR+3)
            PASS    = IQ(LHSTR+4)
            GOTO 999          !exit if found 
          ENDIF
          LHSTR = LQ(LHSTR)
        ENDDO
      ENDIF
  999 RETURN
      END
