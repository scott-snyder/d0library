      SUBROUTINE PFPHI 
C---------------------------------------------------------------------
C
C  Purpose and Method: Action routine to display end view of Phi.  
C  
C  Inputs:  none
C  Outputs: draws phi
C
C  Created 17-FEB-1989   Lupe Rosas
C
C---------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
      REAL    INR,OUTR     ! Inner and Outer radius of a PHI chamber
      INTEGER MODULE,ISECTR
      REAL    DIMENS(6),SIZ
      LOGICAL WAIMEN,TWAIMEN
      DATA MODULE /2/
      DATA TWAIMEN / .TRUE. /
C---------------------------------------------------------------------
      CALL PUMESS('FDC PHI END VIEW')
      CALL PUGETV('WAIT FOR MENU',WAIMEN)
      CALL PUSETV('WAIT FOR MENU',TWAIMEN)
      CALL GTFWAL(MODULE,DIMENS)
      IF( DIMENS(1).LT. 0.0) GOTO 999
      INR  = DIMENS(1)
      OUTR = DIMENS(2)
C
C  Cell By Cell Loop Process
C
      CALL PUOPEN 
      DO 100 ISECTR=0,35 
        CALL PXCOLR('GRE')
        CALL PXCOLFILL('GRE')
        IF(ISECTR.EQ.0)THEN    ! Drawing inner radius
          CALL JCIRCL(0.,0.,0.,INR-2.,0)
          CALL JCIRCL(0.,0.,0.,INR,0)
        ENDIF
        CALL PFPSEC(ISECTR,36,INR,OUTR)   ! Drawing the sector
  100 CONTINUE
      CALL JPINTR(0)
      CALL JCIRCL(0.,0.,0.,OUTR+1.,0)       
      CALL JCIRCL(0.,0.,0.,OUTR,0)       ! Drawing the outer most circle
      CALL JRCLOS
C-----------------------------------------------------------------------
  999 CONTINUE
      CALL PUSETV('WAIT FOR MENU',WAIMEN)
      RETURN
      END 
