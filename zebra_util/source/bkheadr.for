      SUBROUTINE BKHEADR (NRUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book begin run head bank 
C-
C-   Inputs  : NRUN [I]  RUN NUMBER
C-   Outputs : none
C-   Controls: none
C-
C-   Created  28-JUN-1990   Chip Stewart
C-   Updated  26-JUN-1991   James Richardson  , Chip Stewart - write to lheadr
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NRUN
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$ZEB$RUN_HEAD:RUN_HEAD.ZEB/LIST'
      INTEGER IOHEAD,IUH,RUNNO
      CALL MZFORM('HEAD','1I 2H 11I',IOHEAD)
      CALL MZBOOK(IXDVR,LHEADR,LHEADR,1,
     $            'HEAD',18,18,14,IOHEAD,0)
C  identify this as an isajet initial record
      IQ(LHEADR+1)= 1
      CALL UCTOH('DATA',IUH,4,4)
      IQ(LHEADR+2)=IUH
      CALL UCTOH('BEG ',IUH,4,4)
      IQ(LHEADR+3)=IUH
      IQ(LHEADR+6)=NRUN
      IQ(LHEADR+14)=1
C----------------------------------------------------------------------
  999 RETURN
      END
