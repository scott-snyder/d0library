      SUBROUTINE PTLCOL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : It displays the following label 
C-
C                  0 > ENRG <= 3000  CYAN
C               3000 > ENRG <= 6000  BLUE
C               6000 > ENRG <= 10000 PINK
C              10000 > ENRG <= 99999 RED
C-      for the end view of the TRD.
C-
C-   Inputs  : PARCOL - Color that the label will be printed
C-
C-   Created   3-FEB-1989   LUPE ROSAS
C-   Updated  10-JAN-1990   LUPE HOWELL Implementing Color table
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C      INTEGER PARCOL            ! Label color
      INTEGER ICOLOR(4)         ! Array containing the colors 
      INTEGER I
      INTEGER WIRE              
      CHARACTER*3 KCOLOR
      REAL ENG
      REAL XLAB1,XLAB2,YLAB1,YLAB2   ! X AND Y COORDINATES FOR THE LABELS
      REAL ENGMIN(4)            ! Energy minimum cut off points
     X    ,ENGMAX(4)            ! Energy maximum cut off points
      REAL PTGEGT
      REAL XSIZE,YSIZE
      CHARACTER*6 CENMIN(4)    ! Enrg min in charcter form
     x           ,CENMAX(4)    ! enrg max in charcter form
     x           ,CX
      CHARACTER*8 CX2
      CHARACTER*7 ELAB          ! Energy label
      CHARACTER*28 MESS         ! Complete message to be display in labels
      CHARACTER*3 CI
      CHARACTER*6 CENG
      CHARACTER*4 CWIRE
      CHARACTER*3 LAB1
      CHARACTER*8 LAB2 
      CHARACTER*4 LAB3
C----------------------------------------------------------------------
      DATA ICOLOR/4,6,5,1/
      DATA XLAB2 /48./       ! X COORDINATE FOR LABELS
      DATA XLAB1/55./
      DATA ENGMIN/0.,3000.,6000.,9000./
      DATA ENGMAX/3000.,6000.,10000.,9999./
      DATA ELAB/'<ENRG<='/
      DATA LAB1/'LAY'/
      DATA LAB2/'MAXENRG='/     
      DATA LAB3/'WIR '/
      data cenmin/'0','3','6','10'/
      data cenmax/'3MIPS ','6MIPS ','10MIPS',' '/
C----------------------------------------------------------------------
c      DO 200 I=1,4              ! Converting min and maxs in charcter 
c        WRITE(CX,110)ENGMIN(I)  !   to print them.
c  110   FORMAT(F6.0)
c        READ(CX,120)CENMIN(I)
c  120   FORMAT(A6)
c        WRITE(CX,110)ENGMAX(I)
c        READ(CX,120)CENMAX(I)
c  200 CONTINUE
C-  Drawing Labels
      YLAB1=48.     ! Y coordinate for labels
      DO 300 I=1,4
        CALL JJUST(2,2)
        CALL JSIZE(1.5,4.0)
        CALL PTECUT(ENGMAX(I),KCOLOR)
        CALL PXCOLR(KCOLOR)
        MESS=CENMIN(I)//ELAB//CENMAX(I) ! Setting up the messasge
        CALL JMOVE(XLAB1,YLAB1)
        CALL J1STRG(MESS)
        YLAB1=YLAB1-3.0                   ! Calculating new Y for next label
 300  CONTINUE      
  999 RETURN
      END
