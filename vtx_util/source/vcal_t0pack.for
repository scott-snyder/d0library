      SUBROUTINE VCAL_T0PACK(T0_LC,T0_OFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find packing parameter for T0's
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-FEB-1994   Ed Oltman
C-   Updated   2-MAR-1994   Ed Oltman  Added dummy args to HFITHN for UNIX 
C-   Updated  30-NOV-1994   Ed Oltman  Eliminate dependance on MINUIT, use
C-                                     truncated average.
C-                                                       
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:VCAL.PARAMS'
c I/O:
      REAL    T0_LC,T0_OFF
c Locals:
      INTEGER LVTMW,LAY,SEC,WIR,END,NSEC(0:2),PT,ITER
      REAL    TMIN,TMAX,T,DEF_LC,MAX,AVE,SPAN,MEAN,DEN
c Externals:
      INTEGER GZVTMW
c Data:
      PARAMETER (DEF_LC=0.6)
      DATA NSEC/15,31,31/
C----------------------------------------------------------------------
      TMIN = 999999.
      TMAX =-999999.
      MAX = 2**T0_BITS - 1
      DO LAY = 0,2
        LVTMW = GZVTMW(LAY)
        DO SEC = 0,NSEC(LAY)
          DO WIR = 0,7
            DO END = 0,1
              PT = LVTMW + (8*SEC+WIR)*IC(LVTMW+3) + 5
              TMIN = AMIN1(TMIN,C(PT+1+2*END))
              TMAX = AMAX1(TMAX,C(PT+1+2*END))
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      IF (TMAX .LT. TMIN + DEF_LC*MAX) THEN
        T0_OFF= TMIN
        T0_LC = (TMAX-TMIN)/MAX
      ELSE
        AVE = 0.5*(TMAX+TMIN)
        SPAN = 0.5*DEF_LC*MAX
        DO ITER = 1,3
          MEAN = 0.
          DEN = 0.
          DO LAY = 0,2
            LVTMW = GZVTMW(LAY)
            DO SEC = 0,NSEC(LAY)
              DO WIR = 0,7
                DO END = 0,1
                  PT = LVTMW + (8*SEC+WIR)*IC(LVTMW+3) + 5
                  T = C(PT+1+2*END)
                  IF (ABS(T-AVE) .LT. SPAN) THEN
                    DEN = DEN + 1.
                    MEAN = MEAN + T
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
          ENDDO
          AVE = MEAN/DEN
        ENDDO
        T0_LC = DEF_LC
        T0_OFF= AVE - SPAN
      ENDIF
  999 RETURN
      END
