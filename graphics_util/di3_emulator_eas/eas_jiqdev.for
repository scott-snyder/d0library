      SUBROUTINE JIQDEV(DSPDV, CODE, LIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
CD   This module returns a particular characteristic of the display
CD   device. The parameters are display device (DSPDV), item 
CD   characteristic number to return (CODE), and returns the value as an 
CD   integer list (LIST).
C-
C-   Inputs  : DSPDV, CODE
C-   Outputs : LIST
C-   Controls: 
C-
C-   Created   09-NOV-1988   A. VIRGO
C-   UPDATED   19-JUL-1990   S. ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DSPDV, CODE, LIST(1)
C
      GOTO (  10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     +       110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     +       210, 220, 230, 240, 250, 260, 270, 280, 290, 300,
     +       310, 320, 330, 340, 350, 360, 370, 380, 390, 400,
     +       410, 420, 430, 440, 450, 460, 470), CODE
   10 CONTINUE
C.N.O.      
      LIST(1) = 17
C.N.O.
      GOTO 9900
   20 CONTINUE
      GOTO 9900
   30 CONTINUE
      GOTO 9900
   40 CONTINUE
      GOTO 9900
   50 CONTINUE
      GOTO 9900
   60 CONTINUE
      GOTO 9900
   70 CONTINUE
      GOTO 9900
   80 CONTINUE
      GOTO 9900
   90 CONTINUE
      GOTO 9900
  100 CONTINUE
      GOTO 9900
  110 CONTINUE
      GOTO 9900
  120 CONTINUE
      GOTO 9900
  130 CONTINUE
      LIST(1) = 1
      GOTO 9900
  140 CONTINUE
      GOTO 9900
  150 CONTINUE
      GOTO 9900
  160 CONTINUE
      GOTO 9900
  170 CONTINUE
      LIST(1) = 1
      GOTO 9900
  180 CONTINUE
      GOTO 9900
  190 CONTINUE
      GOTO 9900
  200 CONTINUE
      GOTO 9900
  210 CONTINUE
      GOTO 9900
  220 CONTINUE
      GOTO 9900
  230 CONTINUE
      GOTO 9900
  240 CONTINUE
      GOTO 9900
  250 CONTINUE
      GOTO 9900
  260 CONTINUE
      GOTO 9900
  270 CONTINUE
      GOTO 9900
  280 CONTINUE
      GOTO 9900
  290 CONTINUE
      GOTO 9900
  300 CONTINUE
      GOTO 9900
  310 CONTINUE
      GOTO 9900
  320 CONTINUE
      GOTO 9900
  330 CONTINUE
      GOTO 9900
  340 CONTINUE
      GOTO 9900
  350 CONTINUE
      GOTO 9900
  360 CONTINUE
      GOTO 9900
  370 CONTINUE
      GOTO 9900
  380 CONTINUE
      GOTO 9900
  390 CONTINUE
      GOTO 9900
  400 CONTINUE
      GOTO 9900
  410 CONTINUE
      LIST(1) = 1
      GOTO 9900
  420 CONTINUE
      GOTO 9900
  430 CONTINUE
      LIST(1) = 3
      GOTO 9900
  440 CONTINUE
      GOTO 9900
  450 CONTINUE
      GOTO 9900
  460 CONTINUE
      LIST(1) = 2
      GOTO 9900
  470 CONTINUE
      GOTO 9900
C
C   Common exit point.
C
 9900 CONTINUE
      RETURN
      END
