      SUBROUTINE BKISV1(NVERTX,LISV1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Book ISV1 bank (ISAJET vertices)
C-   Input:
C-     NVERTX = vertex number
C-   Outputs : 
C-     LISV1 = pointer to LISV1
C-
C-   Created  11-DEC-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ISALNK.INC'
C
      INTEGER NVERTX,LISV1
      INTEGER IOISV1
      INTEGER LISAE
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN 
        CALL MZFORM('ISV1','1I-F',IOISV1)   ! format for ISV1
        FIRST=.FALSE.
      ENDIF
      IF(NVERTX.EQ.1) THEN
        LISAE=LQ(LHEAD-IZISAE)
        CALL MZBOOK(IXMAIN,LVD,LISAE,-IZISV1,
     $              'ISV1',2,1,9,IOISV1,-1)
        LISV1=LVD(1)
      ELSE
        CALL MZBOOK(IXMAIN,LVD(NVERTX),LVD(NVERTX-1),0,
     $                  'ISV1',2,1,9,IOISV1,-1)
        LISV1=LVD(NVERTX)
      ENDIF
  999 RETURN
      END
