      SUBROUTINE LSQ_COLLECT_GARBAGE(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CLEANS UP THE AREA /LSQ_MATRIX/
C-   ALLOWS MORE MATRICES TO BE BOOKED. CALL THIS AFTER A NUMBER OF
C-   MANIPULATIONS WHERE MATRICES HAVE BEEN CREATED, INVERTED AND DELETED.
C-
C-   Inputs  : 
C-   Outputs : IER = NON ZERO . ERROR DURING GARBAGE COLLECTION.
C-   Controls: 
C-
C-   Created  21-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER INDEX,IER
      INTEGER I,J
C----------------------------------------------------------------------
      IER = 0
      I = 1
      DO WHILE (I.LE.NMAT)
        IF(M_DELETE(I).NE.0)THEN
          J = I
          DO WHILE (J.LT.NMAT)
            M_NAME(J) = M_NAME(J+1)
            M_ROWS(J) = M_ROWS(J+1)
            M_COLS(J) = M_COLS(J+1)
            M_DELETE(J) = M_DELETE(J+1)
            LSTLNK(J) = LSTLNK(J+1)
            J = J + 1
          ENDDO
          NMAT = NMAT -1
        ENDIF
        I = I + 1
      ENDDO
  999 RETURN
      END
