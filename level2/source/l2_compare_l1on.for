      SUBROUTINE L2_COMPARE_L1ON(IWORD,L1ON,FLTON)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        set logicals for L1 TRIGGER bits 
C-   Inputs  : IWORD   the trigger word 
C-   Outputs : L1ON    logical array 
C-             FLTON    logical array to control L2_COMPARE
C-   Controls: 
C-
C-   Created   19-Dec-1993   Andrzej Zieminski 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IWORD,I,J
      LOGICAL L1ON(32), FLTON(5) 
      INTEGER BJETS(20),BL2EM(20),BMUOT(20),BPNUT(20)
      INTEGER NBJETS,NBL2EM,NBMUOT,NBPNUT
C
      DATA NBJETS /15/
      DATA BJETS/2,3,9,14,15,16,17,18,21,22,23,24,25,26,27,5*0/
      DATA NBL2EM /7/
      DATA BL2EM/7,8,10,11,12,13,19,13*0/
      DATA NBMUOT /7/
      DATA BMUOT/2,3,4,5,6,13,14,13*0/
      DATA NBPNUT /4/
      DATA BPNUT/10,20,21,22,16*0/
C----------------------------------------------------------------------
C
      DO I = 0, 31
        IF (BTEST(IWORD,I)) THEN
          L1ON(I+1)=.TRUE.
        ELSE
          L1ON(I+1)=.FALSE.
        ENDIF
      ENDDO
C
      FLTON(1)=.FALSE.
      DO I=1,NBJETS
        J=BJETS(I)
        IF(L1ON(J)) THEN
          FLTON(1)=.TRUE.
          GOTO 20
        ENDIF
      ENDDO
C
 20   FLTON(2)=.FALSE.
      DO I=1,NBL2EM
        J=BL2EM(I)
        IF(L1ON(J)) THEN
          FLTON(2)=.TRUE.
          GOTO 30
        ENDIF
      ENDDO
C
 30   FLTON(3)=.FALSE.
      DO I=1,NBMUOT 
        J = BMUOT(I)
        IF(L1ON(J)) THEN
          FLTON(3)=.TRUE.
          GOTO 40
        ENDIF
      ENDDO
C
 40   FLTON(4)=.FALSE.
      DO I=1,NBPNUT
        J= BPNUT(I)
        IF(L1ON(J)) THEN
          FLTON(4)=.TRUE.
          GOTO 50
        ENDIF
      ENDDO
C
 50   FLTON(5)=.FALSE.
      RETURN
      END
