      SUBROUTINE MTC_TEST_IWHERE()
C----------------------------------------------------------------------
C- MTC_TEST_IWHERE: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : print the results of MTC_IWHERE to see
C-   if it is working properly
C-
C-   Inputs  : none
C-   Outputs : print statements to sys$output
C-
C-   Created   4-JUN-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA,IPHI,ILYR,IHERE,MTC_IWHERE,ISIGN
      CHARACTER*1 A(-37:37,-17:17)
      CHARACTER*1 AIHERE(0:10)
C----------------------------------------------------------------------
C- initialize here 
      do 9 ieta=-37,37
        do 8 ilyr=-17,17
          A(ieta,ilyr) = ' '
    8   continue
    9 continue
      AIHERE(0)  = '.'
      AIHERE(1)  = '1'
      AIHERE(2)  = '2'
      AIHERE(3)  = '3'
      AIHERE(4)  = '4'
      AIHERE(5)  = '5'
      AIHERE(6)  = '6'
      AIHERE(7)  = '7'
      AIHERE(8)  = '8'
      AIHERE(9)  = '9'
      AIHERE(10) = '0'
C----------------------------------------------------------------------
      DO 10 IPHI=18,50,32               ! 2 opposite slices in phi
        ISIGN = 1
        IF(IPHI.EQ.50) ISIGN = -1
        DO 20 IETA=-37,37
          DO 30 ILYR=1,17
            IHERE               = MTC_IWHERE(IETA,IPHI,ILYR)
            A(IETA,ISIGN*ILYR)  = AIHERE(IHERE)
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
C- print out results ...
      DO 40 IETA=37,-37,-1
        WRITE(6,80) (A(IETA,ILYR),ILYR=-17,17)
   40 CONTINUE
   80 FORMAT(1X,35A1)
C----------------------------------------------------------------------
  999 RETURN
      END
