      SUBROUTINE VCAL_GNPACK(AVE_ABSG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-FEB-1994   Ed Oltman
C-   Updated   2-MAR-1994   Ed Oltman  Added dummy args to HFITHN for UNIX 
C-   Updated   3-JUN-1994   Ed Oltman  Eliminate sensitivity to initial
C-                             fit parameters; check if fit parameters are
C-                             sensible.
C-   Updated  30-AUG-1994   Liang-ping Chen remove a CALL ERRMSG  
C-   Updated  28-NOV-1994   Ed Oltman  Eliminate dependance on MINUIT, use 
C-                          truncated average.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
c I/O:
      REAL AVE_ABSG(0:2)
c Locals:
      INTEGER LAY,ITER,LVGNL,SEC,WIR,NSEC(0:2)
      REAL    DEN,AVE,GN
c Externals:
      INTEGER GZVGNL
c Data:
      DATA NSEC/15,31,31/
C----------------------------------------------------------------------
      DO LAY = 0,2
       LVGNL = GZVGNL(LAY)
        DO ITER = 1,3
          AVE_ABSG(LAY) = 0.
          DEN = 0.
          DO SEC = 0,NSEC(LAY)
            DO WIR = 0,7
              GN = C(LVGNL + 16*SEC + 2*WIR + 3*41 + 6)
              IF ( (ITER .EQ. 1 .AND. ABS(GN-1.) .LT. 1.) .OR.
     &             (ITER .GT. 1 .AND. ABS(GN-AVE) .LT. 0.3*AVE) ) THEN
                DEN = DEN + 1
                AVE_ABSG(LAY) = AVE_ABSG(LAY) + GN
              ENDIF
            ENDDO
          ENDDO
          AVE_ABSG(LAY) = AVE_ABSG(LAY)/DEN
          AVE = AVE_ABSG(LAY)
        ENDDO
      ENDDO
  999 RETURN
      END
