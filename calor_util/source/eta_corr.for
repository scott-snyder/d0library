      FUNCTION ETA_CORR(DETA,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Correction due to the eta dependance observed
C-                        in test beam data(CCEM). Obtained from 100 GeV data.
C-
C-   Inputs  : DETA   (R) detector eta.
C-   Outputs : IER  (I) 0 if OK
C-   Controls: 
C-   Returns : Returns the correction factor. Observed energy should be 
C-              multiply by this number.
C-
C-   Created   16-NOV-1992   W.G.D.Dharmaratna
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL E_OBS,ETA_CORR,DETA
      REAL POLY(7),NORM,FUNC
      LOGICAL FIRST
      INTEGER IER,I
      DATA FIRST/.TRUE./
      DATA POLY/1.0459,-0.0085213,-1.1280,4.9434,
     &   -8.3856,6.1921,-1.6584/
!      DATA NORM/0.994/   This is the original number
      DATA NORM/0.9793/ !this is renormalized to remove the 1.5% discrepancy
C----------------------------------------------------------------------
      ETA_CORR = 1.0
      FUNC       = 0.0
C
      IF (ABS(DETA) .LE.1.0) THEN
        DO I = 1,7
          FUNC = FUNC+POLY(I)*(ABS(DETA))**(I-1)
        END DO
        IF (FUNC.LT.1.5 .AND. FUNC.GT.0.5) THEN
          ETA_CORR = NORM*FUNC
        ELSE
          CALL ERRMSG('ETA CORRECTION','ETA_CORR',
     &    ' UNEXPECTED CORRECTION','W')
          IER = -1      
        ENDIF
      ENDIF
  999 RETURN
      END
