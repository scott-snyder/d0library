      subroutine print_setup
      implicit none
      include 'D0$SPYTHIA$INC:LUDAT1.INC'
      include 'D0$SPYTHIA$INC:LUDAT2.INC'
      include 'D0$SPYTHIA$INC:LUDAT3.INC'
      include 'D0$SPYTHIA$INC:LUDAT4.INC'
      include 'D0$SPYTHIA$INC:PYPARS.INC'
      include 'D0$SPYTHIA$INC:SUSYPAR.INC'
      include 'D0$SPYTHIA$INC:PARSUSY.INC'
      include 'D0$SPYTHIA$INC:SPYTHIA.INC'
      integer I
      integer IMIN,IMAX
      data IMIN,IMAX /41,71/
C
      write(*,*) 'MODEL NUMBER = ',psusy(1)
      DO I=imin,imax
       write(*,*) ' pmas(',I,',2) = ',pmas(i,2)
      ENDDO
      RETURN
      END
