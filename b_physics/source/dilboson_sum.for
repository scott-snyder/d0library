       FUNCTION DILBOSON_SUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : END OF JOB WORK ON HISTOS
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-MAR-1992   c.r.murphy
C-   Updated  17-MAR-1992   Daria Zieminska  : DILEPTON -> DILBOSON 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,NDILEP,NX,NY,NWT,LOC,IER
      REAL CONTEN1(20),CONTEN2(20)
      REAL XMI,XMA,YMI,YMA
      CHARACTER*80 TITL
      LOGICAL DILBOSON_SUM,OK
C----------------------------------------------------------------------
C Create/set HBOOK directory DILBOSON
C
      CALL DHDIR('DILBOSON_RCP','HBOOK_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('DILBOSON','DILBOSON',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
C     CALL HCDIR('//PAWC',' ')
c      CALL NTUPLE_CLOSE('NTUPLE',IER)
c      CALL NTUPLE_END
      DILBOSON_SUM=.TRUE.
C
  999 RETURN
      END
