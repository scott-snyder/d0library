      SUBROUTINE PTECUT(ENRG,KCOLOR)
C===================================================================
C
C   Description: It will assign a color according to the energy input.
C   ============
C                  0 > ENRG <= 3000  CYAN
C               3000 > ENRG <= 6000  BLUE
C               6000 > ENRG <= 10000 PINK
C              10000 > ENRG <= 99999 RED
C    Also the in the ENTRY PCLCOL a label with the above information 
C    will be displayed at the bottom of the end view of the TRD.
C
C   Input:   ENRG   - [F] Energy 
C  
C   Output:  KCOLOR - [C*3] Three letter character giving the color chosen
C
C   Author:  Guadalupe Rosas
C
C   Revision History:
C   -----------------
C     Updated 10-JAN-1990 Lupe Howell Implementing color table
C-   Updated   5-JUN-1990   Norman A. Graf   
C-   Updated   4-FEB-1994   A. Zylberstejn:change limits 
C===================================================================
      IMPLICIT NONE
      CHARACTER*(*) KCOLOR      ! Output parameter corresponding color
      CHARACTER*3 ICOLOR(4)     ! Array containing the colors 
      INTEGER I
      REAL ENRG                 ! Energy that will determine color
      REAL ENGMIN(4)            ! Energy minimum cut off points
     X    ,ENGMAX(4)            ! Energy maximum cut off points
C===================================================================
      DATA ICOLOR/'BLU','CYA','MAG','RED'/ 
      DATA ENGMIN/0.,3000.,6000.,9000./
      DATA ENGMAX/3000.,6000.,10000.,9999./

C===================================================================
      IF (ENRG.LE.ENGMIN(2)) THEN  
        KCOLOR =ICOLOR(1)    ! CYAN
      ELSEIF(ENRG.LE.ENGMAX(2)) THEN
        KCOLOR = ICOLOR(2)   ! BLUE
      ELSEIF(ENRG.LE.ENGMAX(3)) THEN
        KCOLOR=ICOLOR(3)     ! PINK 
      ELSE
        KCOLOR= ICOLOR(4)    ! RED
      ENDIF
      RETURN
      END
