C-   Updated  24-MAR-2004   sss - compile with g77.
      SUBROUTINE JDEVVP(IDEV,DXMIN,DXMAX,DYMIN,DYMAX)
      INCLUDE 'D0$INC:DI3INC.INC'
      IF(IDEBUG.GT.5)write (*,2222),IDEV,DXMIN,DXMAX,DYMIN,DYMAX
C&IF LINUX
C& 2222 FORMAT(' JDEVVP:',I7,4F6.2)
C&ELSE
 2222 FORMAT(' JDEVVP:',I,4F)
C&ENDIF
      END