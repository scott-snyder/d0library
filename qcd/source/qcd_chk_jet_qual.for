      LOGICAL FUNCTION QCD_CHK_JET_QUAL(EMFR,CHFR,HCFR,ETA,ZVERT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Apply Basic Quality cuts to jets
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-DEC-1995   Bob Hirosky
C-   Updated  21-JAN-1996   Bob Hirosky  ADD ICR DEPENDENCE ON EMFR CUT 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL EMFR,CHFR,HCFR,ETA,ZVERT
      REAL PETA_TO_DETA, DETA
      REAL CH_FRACTION_MAX, EM_FRACTION_MIN, EM_FRACTION_MAX
      REAL HOT_RATIO_MAX
      PARAMETER( CH_FRACTION_MAX = .4 )
      PARAMETER( EM_FRACTION_MIN = .05 )
      PARAMETER( EM_FRACTION_MAX = .95)
      PARAMETER( HOT_RATIO_MAX = 10.0 )
C----------------------------------------------------------------------
C
C: Apply CH AND HOT CELL CUT
      QCD_CHK_JET_QUAL = (CHFR.LE.CH_FRACTION_MAX) .AND.
     &  (HCFR.LE.HOT_RATIO_MAX)
      IF (.NOT. QCD_CHK_JET_QUAL) GOTO 999
C
C: Change Physics eta to Detector eta
      DETA=PETA_TO_DETA(ETA,ZVERT)
C
C: Apply EM cut(drop low EMF cut in ICR)
      IF ((ABS(DETA).GE.1.0).AND.(ABS(DETA).LE.1.6)) THEN
        QCD_CHK_JET_QUAL = (QCD_CHK_JET_QUAL) .AND.
     &    (EMFR.LT.EM_FRACTION_MAX)
      ELSE
        QCD_CHK_JET_QUAL = (QCD_CHK_JET_QUAL) .AND.
     &    (EMFR.LE.EM_FRACTION_MAX) .AND. (EMFR.GE.EM_FRACTION_MIN)
      ENDIF
C
  999 RETURN
      END
