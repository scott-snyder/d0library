      LOGICAL FUNCTION BOKCAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book Calorimeter Histograms
C-                          Use Histogram #'s 3000-3999 UCAL
C-                          Use Histogram #'s 4000-4999 ECAL
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  23-JUL-1987   A.M.Jonckkhere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      BOKCAL = .TRUE.
      IF ( DCAL.LT.2 ) GOTO 999
C
C        CALL HBOOK1(3999,'ENERGY FROM ISA$')
C        CALL HBOOK1(4000,'ENERGY IN UCAL$')!,100,0.,1000.)
C        CALL HBOOK1(4001,'ENERGY IN ECAL$')!,100,0.,1000.)
C        CALL HBOOK1(4002,'ENERGY IN CRACK$')!,100,0.,100.)
C        CALL HBOOK1(4003,'TOTAL UCAL+ECAL+CRACK$')!,100,0.,1000.)
C        CALL HBOOK1(4004,'PERCENT CRACK/TOTAL$',100,0.,1.0)
C
C        CALL HBOOK2(4010,'PHOTON ETA VS TIME/GEV$',
C     +                    100,-10.,10.,100,0.,10.,0.0)
C        CALL HBOOK2(4011,'PHOTON PHI VS TIME/GEV$',
C     +                    100,0.,6.3,100,0.,10.,0.0)
C        CALL HBOOK2(4012,'PHOTON LOG(E) VS TIME/GEV$',
C     +                    60,-3.,3.,100,0.,10.,0.0)
C
C        CALL HBOOK2(5010,'ETA VS TIME/GEV$',100,-10.,10.,100,0.,10.,0.0)
C        CALL HBOOK2(5011,'PHI VS TIME/GEV$',100,0.,6.3,100,0.,10.,0.0)
C        CALL HBOOK2(5012,'LOG(E) VS TIME/GEV$',60,-3.,3.,100,0.,10.,0.0)
  999 RETURN
      END
