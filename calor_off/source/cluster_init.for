      SUBROUTINE CLUSTER_INIT(IHAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : initialize CATE words CLASS and NEXT for
C-                         Cluster finding
C-
C-   Inputs  :IHAD =1 , EM ONLY IHAD = 2 EM+HAD
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CLUPAR.INC'
      INTEGER CLASS,NEXT,CATW
      INTEGER IHAD,J
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  defining statement functions class,next
C
      CATW(J) = (J-1)*NREP+LCATE
C
C NREP,LCATE are the rep. num + addr of the CATE bank and will be
C defined later.
C
      CLASS(J) = CATW(J)+ICLASS
C
C gives the class number address of  tower J
C
      NEXT(J) = CATW(J)+INEXT
C
C gives the connection to tower J
C
C ****  statement function definitions complete
C
C------------------------------------------------------------------------
C
C ****  ILO,IHI ARE THE LIMITS OF THE TOWER INDICES
C
      IF(FIRST)THEN
        CALL EZPICK('CAPHEL_RCP')
        CALL GTSRCP('CLUSTER_CLASS_OFFSET',ICLASS,1)
        CALL GTSRCP('CLUSTER_NEXT_OFFSET',INEXT,1)
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
      NEMTWR = IQ(LCATE+3)/CATENM       ! Number of EM Towers
      NTOWER = IQ(LCATE+3)-CATENM*NEMTWR! Total number of towers
      NREP   = IQ(LCATE+2)            ! Repetition number
C
C ****  WE ASSUME THAT IN CATE THERE ARE NEMTWR EM TOWERS FOLLOWED
C ****  BY NTOWER-NEMTWR HADRONIC TOWERS. I.E. MIXING IS NOT ALLOWED.
C ****  THIS ASSUMPTION IS FOR SPEED.
C
      ILO =1
      IHI = NEMTWR
      IF(IHAD.EQ.2)THEN
        ILO = NEMTWR+1
        IHI = NTOWER
      ENDIF
C
      DO J = ILO, IHI
        IQ(CLASS(J)) = J
        IQ(NEXT(J)) = J
      ENDDO
  999 RETURN
      END
