      SUBROUTINE JFSOPN(CODE,DSPDEV,LUN,FILNAM)
      INTEGER CODE,DSPDEV,LUN
      CHARACTER*(*) FILNAM
      INCLUDE 'D0$INC:DI3INC.INC'
      IF(DSPDEV.NE.2.OR.CODE.NE.3)RETURN
      IUNIT=LUN
      IF(LUN.EQ.0) IUNIT=33
      FILENAM=FILNAM
      END
