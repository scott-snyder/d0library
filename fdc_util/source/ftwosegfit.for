      SUBROUTINE FTWOSEGFIT(XVAL,YVAL,NHIT,MHIT,
     &  A,B,DISP,DISP_ERR,CHISQ,RESID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fit 2 pieces of track segment, allow
C-   a degree of freedom for displacement at sector boundary,
C-
C-
C-   Inputs  :  XVAL,YVAL       ! Points to be fit
C-              NHIT            ! Num. points in fit
C-              MHIT            ! Num. points in first piece.
C-   Outputs :  A,B,Disp        ! Fit parameters
C-              CHISQ,RESID
C-   Controls:
C-
C-   Created   9-NOV-1990   Robert E. Avery
C-   Updated   5-JUL-1991   Robert E. Avery  Clean up 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NUM_PARAM
      PARAMETER( NUM_PARAM = 3 )
      INTEGER MAX_POINTS
      PARAMETER( MAX_POINTS = 16 )
C  Input:
      REAL    XVAL(MAX_POINTS) ,YVAL(MAX_POINTS)       ! Points to be fit
      INTEGER NHIT            ! Num. points in fit
      INTEGER MHIT            ! Num. points in first piece.
C  Output:
      REAL    A,B,DISP        ! Fit parameters
      REAL    DISP_ERR        
      REAL    CHISQ,RESID(MAX_POINTS)
C  Local:
      REAL AA(MAX_POINTS,NUM_PARAM)
      DOUBLE PRECISION COV(NUM_PARAM,NUM_PARAM)
      DOUBLE PRECISION DET,SUM(NUM_PARAM),PAR(NUM_PARAM)
      INTEGER IPOINT
      INTEGER IPAR1,IPAR2
      INTEGER IFAIL,IER
      INTEGER INDEX(NUM_PARAM)
C
      REAL    ERR_SQ
      PARAMETER(  ERR_SQ =  0.0004 )
C----------------------------------------------------------------------
      CALL VZERO(PAR,2*NUM_PARAM)
      CALL VZERO(SUM,2*NUM_PARAM)
      CALL VZERO(COV,2*NUM_PARAM*NUM_PARAM)
      DO IPOINT=1,NHIT
        AA(IPOINT,1)= 1.0
        AA(IPOINT,2)= XVAL(IPOINT)
        IF ( IPOINT .GT. MHIT ) THEN
          AA(IPOINT,3)= 0.5
        ELSE
          AA(IPOINT,3)= -0.5
        ENDIF
        DO IPAR1=1,NUM_PARAM
          SUM(IPAR1)=SUM(IPAR1)
     &         + AA(IPOINT,IPAR1) * YVAL(IPOINT)
          DO IPAR2=IPAR1,NUM_PARAM
            COV(IPAR1,IPAR2)=COV(IPAR1,IPAR2)
     &            + AA(IPOINT,IPAR1)*AA(IPOINT,IPAR2)
            COV(IPAR2,IPAR1)=COV(IPAR1,IPAR2)
          ENDDO
        ENDDO
      ENDDO
C
      CALL DINV(NUM_PARAM,COV,NUM_PARAM,INDEX,IFAIL)
C
      DO IPAR1=1,NUM_PARAM
        DO IPAR2=1,NUM_PARAM
          PAR(IPAR1)=PAR(IPAR1)+COV(IPAR1,IPAR2)*SUM(IPAR2)
        ENDDO
      ENDDO
      A = PAR(1)
      B = PAR(2)
      DISP  = PAR(3)
      DISP_ERR = SQRT(COV(3,3)*ERR_SQ)
C
      CHISQ=0
      DO IPOINT = 1,NHIT
        RESID(IPOINT) = YVAL(IPOINT)
        DO IPAR1=1,NUM_PARAM
          RESID(IPOINT) = RESID(IPOINT)
     &      - AA(IPOINT,IPAR1)*PAR(IPAR1)
        ENDDO
        CHISQ=CHISQ + RESID(IPOINT)**2.
      ENDDO
      CHISQ= CHISQ / ( FLOAT( NHIT-NUM_PARAM) * ERR_SQ)
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
