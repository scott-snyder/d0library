      SUBROUTINE TRISIS(NGOOD,ISV,ISP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-NOV-1989   J.Fr. Glicenstein
C-   Updated  17-NOV-1989   A. Zylberstejn   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NGOOD,ISV,ISP,IDENT   
      REAL PHI,PMOM,VIN(6)
      IF(TYPPRO.EQ.0 )RETURN
          PHI=Q(ISP+7)
          PMOM=Q(ISP+5)
        IF(PMOM.LT.3.)GO TO 999      !MOMENTUM CUT
          IDENT=0
          IF(Q(ISP+6).LT.0.1)IDENT=1
          VIN(1)=Q(ISV+7)
          VIN(2)=Q(ISV+8)
          VIN(3)=Q(ISV+9)
          VIN(4)=Q(ISP+2)/PMOM
          VIN(5)=Q(ISP+3)/PMOM
          VIN(6)=Q(ISP+4)/PMOM       
       IF (TYPPRO.EQ.1) THEN
        CALL TRDFIL(NGOOD,VIN,PHI,IDENT)
       ENDIF
  999 RETURN
      END
