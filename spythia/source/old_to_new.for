C****************************************************************
      subroutine OLD_TO_NEW
      implicit none
      include 'D0$SPYTHIA$INC:LUDAT2.INC'
      include 'D0$SPYTHIA$INC:LUDAT3.INC'
      integer KH(4),LH(4)
      integer ihcheck,ichannel
      data KH/25,35,36,37/
      data LH/72,73,74,75/
      integer I,IH,J,IL
C.....
      DO I=1,4
       IH=KH(I)
       IL=LH(I)
       DO J=1,3
        mdcy(IH,J)=mdcy(IL,J)
       ENDDO
       PMAS(IH,2)=PMAS(IL,2)
       PMAS(IH,3)=PMAS(IL,3)
      ENDDO
      return
      end

