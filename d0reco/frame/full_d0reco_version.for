      SUBROUTINE FULL_D0RECO_VERSION(VERSION,PASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      return version and pass number of FULL_D0RECO version
C-      used to reconstruct
C-   Outputs : 
C-     VERSION= version # =0  if no FULL_D0RECO HSTR found
C-     PASS   = pass #    =0          "
C-
C-   Created  17-MAR-1994   Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER VERSION,PASS
      INTEGER LHSTR,GZHSTR
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*40 PROD_NAME
C----------------------------------------------------------------------
      CALL PATHST('RECO')  ! set path to RECO
      LHSTR=GZHSTR()
      VERSION=0
      PASS=0
      DO WHILE (LHSTR.NE.0)
        CALL UHTOC(IQ(LHSTR+7),40,PROD_NAME,40)
        IF (PROD_NAME(1:11).EQ.'FULL_D0RECO') THEN
          VERSION=IQ(LHSTR+3)
          PASS=IQ(LHSTR+4)
          LHSTR=0
        ENDIF
        IF(LHSTR.GT.0) LHSTR=LQ(LHSTR)
      ENDDO
      CALL PATHRS          ! reset path to default
  999 RETURN
      END
