      SUBROUTINE JWINDO(X1,X2,Y1,Y2)
C  OPEN VIEWPLANE WINDOW
C-   Updated  24-MAR-2004   sss - compile with g77.
      INCLUDE 'D0$INC:DI3INC.INC'
      REAL*4 X1,X2,Y1,Y2
      IF(IFSPAC.EQ.0) CALL JVSPAC(-1.,1.,-1.,1.)
      UMIN=X1
      UMAX=X2
      VMIN=Y1
      VMAX=Y2
C  BUILD IT ALL
      CALL J_DEVMAT
      IF(IDEBUG.LT.3) RETURN
      write (*,444)
  444 FORMAT(' *****AT END OF JWINDO*****')
      CALL J_DUMP
      RETURN
      END
