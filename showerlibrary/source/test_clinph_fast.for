      SUBROUTINE TEST_CLINPH_FAST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test CLINPH_FAST
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAY-1990   Rajendran Raja
C-   Updated  24-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    VTX(3),PINT(3),DIR(3)
      REAL    DD,THETA,ETA
      INTEGER I,IETAC,ARGSOK
C----------------------------------------------------------------------
      write (*,*),' TYPE IN VERTEX Z '
C&IF LINUX
C&      read (*,*),VTX(3)
C&ELSE
      ACCEPT *,VTX(3)
C&ENDIF
      VTX(1) = 0.
      VTX(2) = 0.
      write (*,*),' TYPE IN CO-ORDINATE OF INTERSECTION POINT '
C&IF LINUX
C&      read (*,*), pint
C&ELSE
      ACCEPT *,PINT
C&ENDIF
C
      DD = 0.                           
      DO I = 1,3
        DD = DD + (PINT(I) - VTX(I))**2
      ENDDO
      DD = SQRT(DD)
      DO I = 1,3
        DIR(I) = (PINT(I)-VTX(I))/DD
      ENDDO
C
      THETA = ATAN2(SQRT(DIR(1)**2+DIR(2)**2),DIR(3))
      ETA = ALOG(TAN(THETA/2.0))
C
      CALL CLINPH_FAST(VTX,DIR,IETAC,ARGSOK)
      write (*,*),' IETC = ',IETAC
  999 RETURN
      END
