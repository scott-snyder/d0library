      SUBROUTINE MTC_TEST_LONGZ()
C----------------------------------------------------------------------
C- MTC_TEST_LONGZ: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : test MTC_GET_LONGZ by printing the
C-      longitudinal cell size of all cal cells
C-
C-   Inputs  : none
C-   Outputs : print statements to sys$output
C-
C-   Created   8-SEPT-1993      Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MTC_ETOWERS.INC'
      INTEGER IETA,IPHI,ILYR,IHERE,OK
      INTEGER IETAIN,IPHIIN
      CHARACTER*5 A(-37:37,-17:17)
      REAL ZLONG
C- functns ...
      INTEGER MTC_IWHERE,MTC_IWHLAYER
C----------------------------------------------------------------------
C- initialize 
      IETAIN = 0
      IPHIIN = 0
      do 9 ieta=-37,37
        do 8 ilyr=-17,17
          A(ieta,ilyr) = '-----'
    8   continue
    9 continue
C----------------------------------------------------------------------
      IPHI = 1
      DO 20 IETA=-37,37
        IF(IETA.EQ.0) GO TO 20
        DO 30 ILYR=-17,17
          IF(ILYR.EQ.0) GO TO 30
          IF(ILYR.LT.0) IPHI = 63
C- where am I in the calorimeter ?
          IHERE = MTC_IWHERE(IETA,IPHI,ABS(ILYR))
          IF(IHERE.EQ.0) GO TO 30
C- Get the longitudinal cell size ...
          IETATOWER(ABS(ILYR)) = IETA
          IPHITOWER(ABS(ILYR)) = IPHI
          CALL MTC_GET_LONGZ(IETAIN,IPHIIN,ABS(ILYR), ZLONG, OK)
          IF(OK.NE.0) ZLONG = -1.
          WRITE(A(IETA,ILYR),88) ZLONG
   30   CONTINUE
   20 CONTINUE
   88 FORMAT(F5.1)
C- print out results ...
      WRITE(6,*) ' longitudinal calorimeter cell sizes by layer, eta '
      DO 40 IETA=37,-37,-1
        WRITE(6,80) (A(IETA,ILYR),ILYR=1,17)
   40 CONTINUE
      WRITE(6,*) ' negative layer numbers'
      DO 41 IETA=37,-37,-1
        WRITE(6,80) (A(IETA,ILYR),ILYR=17,1,-1)
   41 CONTINUE
   80 FORMAT(1X,17A5)
C----------------------------------------------------------------------
  999 RETURN
      END
