      SUBROUTINE ETOETA(EIN,PHI,THETA,ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-        Calculate from E-vector phi,theta and eta
C-        To be used with calorimeter hits or towers
C- 
C-  WARNING: any modification to this subroutine should be carefully
C-           checked for protection against bad input.
C-
C-   Inputs  :
C-     EIN(4) = Ex, Ey, Ez and Etot
C-   Outputs :
C-      PHI  = phi (azimuth)
C-      THETA= theta (polar angle)
C-      ETA  = eta (pseudo-rapidity)
C-
C-   Created  26-APR-1989   Serban D. Protopopescu
C-   Updated  30-Oct-1989   Serban D. Protopopescu
C-   Updated  20-APR-1993   Serban Protopopescu   take care of negative E
C-   Updated  21-JAN-1994   Orin Dahl  Avoid roundoff for small Pperp
C-   Updated  22-JAN-1994   Qizhong Li-Demarteau  Avoid deviding by zero 
C-   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      REAL    EIN(4),PHI,THETA,ETA,PPERP,PTOTAL
      REAL    SMALL,E(4)
      PARAMETER( SMALL = 1.0E-5 )
C----------------------------------------------------------------------
C
      IF ( EIN(4).LT.0 ) THEN ! check for negative energy
        E(1)=-EIN(1)
        E(2)=-EIN(2)
        E(3)=-EIN(3)
      ELSE
        E(1)=EIN(1)
        E(2)=EIN(2)
        E(3)=EIN(3)
      ENDIF
C
C ****  Calculate phi,theta and eta
C
      PHI=ATAN2(E(2),E(1)+SMALL)
      IF(PHI.LT.0) PHI=PHI+TWOPI

      PPERP  = SQRT( E(1)**2 + E(2)**2 ) + SMALL
      PTOTAL = SQRT( E(1)**2 + E(2)**2 + E(3)**2 ) + SMALL

      THETA = ATAN2( PPERP , E(3) )
      IF ( E(3) .GT. 0. )  THEN
         ETA = ALOG( (PTOTAL+E(3)) / PPERP )
      ELSE
         ETA = ALOG( PPERP / (PTOTAL-E(3)) )
      ENDIF

  999 RETURN
      END
