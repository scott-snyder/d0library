      SUBROUTINE UDST_FIX_PTAU(LPTAU)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reconstruct full set of kinematic quantities
C-                         from Et, eta, phi.
C-
C-   Inputs  : LPTAU - pointer to ptau
C-
C-   Created   5-JAN-1994   Ulrich Heintz
C-   Updated  18-NOV-1995   Ulrich Heintz  delete words that were not present 
C-                                         for older versions 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LPTAU,LJETS,I
      IF(LPTAU.EQ.0)THEN
        CALL ERRMSG('LPTAU=0','UDST_FIX_PTAU','called with LPTAU=0','W')
        GOTO 999
      ENDIF
      LJETS=LQ(LPTAU-2)
      IF(LJETS.EQ.0)THEN
        CALL ERRMSG('LJETS=0','UDST_FIX_PTAU','no link to JETS','W')
        GOTO 999
      ENDIF
      DO I=3,10
        Q(LPTAU+I) = Q(LJETS+I-1)
      ENDDO
      IF(IQ(LPTAU+1).LT.3)THEN
        DO I=12,IQ(LPTAU-1) ! these are not present for version < 3
          Q(LPTAU+I)=0.
        ENDDO
      ELSEIF(IQ(LPTAU+1).LT.4)THEN
        DO I=19,IQ(LPTAU-1) ! these are not present for version < 4
          Q(LPTAU+I)=0.
        ENDDO
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
