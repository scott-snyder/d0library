      SUBROUTINE BKISP2(LISV2, LISP2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book ISP2 bank (Secondary particles from 
C-                         long-lived vertices)
C-
C-   Inputs: : LISV2 = Pointer to supporting bank.
C-   Outputs : LISP2 = Pointer to booked bank.
C-
C-   Created  19-Apr-1993   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISP2.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LISV2, LISP2
      INTEGER IOISP2
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN 
        CALL MZFORM('ISP2','1I 8F',IOISP2)   ! format for ISP2
        FIRST=.FALSE.
      ENDIF
      CALL MZBOOK(IXMAIN, LISP2, LISV2, -IZISP2, 'ISP2', 4, 1, 9,
     &  IOISP2, -1)
  999 RETURN
      END
