      SUBROUTINE UDST_FIX_VTXT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : drop empty and duplicated VTXT banks
C-
C-   Created   6-OCT-1994   Sailesh Chopra
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER LVTXT,LVTXT1,LDROP,ID,IEQ,GZVTXT,I
      EQUIVALENCE(CRFLNK(90),LVTXT)
      EQUIVALENCE(CRFLNK(91),LVTXT1)
      LOGICAL QDROP
C----------------------------------------------------------------------
      LVTXT = GZVTXT(0)
      IF(LVTXT.GT.0)THEN
        ID = IQ(LVTXT-1)
        DO WHILE(LVTXT .GT. 0)
          QDROP=.FALSE.
          IEQ = 0
          DO I = 1,ID
            IF(Q(LVTXT+I).EQ.0)IEQ = IEQ+1
          ENDDO
          IF(IEQ.EQ.ID)THEN
            QDROP=.TRUE.
          ELSE
            IF(LQ(LVTXT-2).EQ.0)THEN  ! no link to ZTRK -> drop if duplicate
              LVTXT1 = GZVTXT(0)
              DO WHILE (LVTXT1.GT.0.AND..NOT.QDROP)
                IF(LVTXT1.NE.LVTXT)THEN
                  IEQ = 0
                  DO I = 1,ID
                    IF(Q(LVTXT+I).EQ.Q(LVTXT1+I))IEQ = IEQ+1
                  ENDDO
                  IF(IEQ.EQ.ID)QDROP=.TRUE.
                ENDIF
                LVTXT1=LQ(LVTXT1)
              ENDDO
            ENDIF
          ENDIF
          LDROP = LVTXT
          LVTXT = LQ(LVTXT)
          IF(QDROP)CALL MZDROP(IXCOM,LDROP,' ')
        ENDDO
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
