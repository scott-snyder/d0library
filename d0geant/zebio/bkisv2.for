      SUBROUTINE BKISV2(LISV2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book ISV2 bank (long-lived vertex)
C-
C-   Inputs:
C-   Outputs : LISV2 = Pointer to booked bank.
C-
C-   Created  19-Apr-1993   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISV2.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LISAE, LISV2
      INTEGER IOISV2
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN 
        CALL MZFORM('ISV2','1I 8F 1I',IOISV2)   ! format for ISV2
        FIRST=.FALSE.
      ENDIF
      LISAE = LQ(LHEAD-IZISAE)
      CALL MZBOOK(IXMAIN, LISV2, LISAE, -IZISV2, 'ISV2', 3, 1, 10,
     &  IOISV2, -1)
  999 RETURN
      END
