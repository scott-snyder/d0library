      SUBROUTINE CAL_SURVEY_TRANSF(XNOM, DELX, RINV, XCEN, XSURV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To transform the nominal Xnom to the
C-                         Survey corrected Xsurv.  DELX, RINV, and
C-                         XCEN are parameters determined by fitting
C-                         to the survey measurments.  These parameters
C-                         are stored in the CLIN banks in the (survey
C-                         modified) CAL_STPfile.  
C-
C-   Inputs  :      XNOM     Nominal positon vector
C-                  DELX     Survey translation vector parameters
C-                  RINV     Survey rotation matrix parameters (inverse
C-                             matrix is stored.  Actual rotation is
C-                             the transpose.
C-                  XCEN     Positon around which the rotation is made.
C-                             Usually chosen as the module center.
C-   Outputs :      XSURV    Survey corrected position vector.
C-   Controls: 
C-   Alternate Entry:  CAL_SURVEY_INV_TRANSF
C-
C-   Created  15-OCT-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    XNOM(3), DELX(3), RINV(3, 3), XCEN(3), XSURV(3)
      INTEGER I, J, K
C
      DO 100 I = 1, 3
        XSURV(I) = XCEN(I) - DELX(I)
        DO 100 J = 1, 3
          XSURV(I) = XSURV(I) + (XNOM(J)-XCEN(J))*RINV(J, I)
  100 CONTINUE
      RETURN
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      ENTRY CAL_SURVEY_INV_TRANSF(XSURV, DELX, RINV, XCEN, XNOM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-OCT-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      DO 200 I = 1, 3
        XNOM(I) = XCEN(I)
        DO 200 J = 1, 3
          XNOM(I) = XNOM(I) + RINV(I, J)*(XSURV(J)-XCEN(J)+DELX(J))
  200 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
