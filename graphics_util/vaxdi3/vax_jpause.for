C-   Updated  24-MAR-2004   sss - compile with g77.
      SUBROUTINE JPAUSE(I)
C      TYPE 5
C    5 FORMAT(' JPAUSE',$)
C&IF LINUX
C&      read (*,*) ii
C&ELSE
      ACCEPT 10,II
   10 FORMAT(O)
C&ENDIF
C  DO THE MOUSE MENU
C      CALL J_RZP3D
      IF(II.EQ.1)STOP
      END
