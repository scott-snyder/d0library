      SUBROUTINE VTHROW(XHIT,YHIT,WT,NHIT,AL,XG,YG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop over hits on segment and remove (by setting 
C-           wt=0) that hit with the largest chi2 contribution
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-MAR-1993   Ed Oltman
C-   Updated   2-APR-1993   Ed Oltman  Use more efficient residual algorithm 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c I/O:
      INTEGER NHIT
      REAL XHIT(NHIT),YHIT(NHIT),WT(NHIT),XG,YG,AL
c Locals:
      REAL SINA,COSA,RESID,CHIMAX
      INTEGER I,WORST
C----------------------------------------------------------------------
      CHIMAX = 0.
      SINA = SIN(AL)
      COSA = COS(AL)
      DO I = 1,NHIT
        IF (WT(I) .GT. 0) THEN
          RESID = WT(I)*( (YHIT(I)-YG)*COSA - (XHIT(I)-XG)*SINA )**2
          IF (RESID .GT. CHIMAX) THEN
            CHIMAX = RESID
            WORST = I
          ENDIF
        ENDIF
      ENDDO
      WT(WORST) = 0.
  999 RETURN
      END
