      SUBROUTINE BKISAQ(LISAQ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Book ISAQ banks
C-   Outputs :
C-     LISAQ = pointer to created bank
C-
C-   Created  11-DEC-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LISAQ
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISAQ.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NLINKS,NDATA
      PARAMETER( NLINKS = 2 )
      PARAMETER( NDATA = 9 )
      INTEGER GZISAQ,IOISAQ,LISAE,LNEXT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('ISAQ','1I-F',IOISAQ)
        FIRST=.FALSE.
      ENDIF
C
      LISAQ=GZISAQ()
C
      IF(LISAQ.EQ.0) THEN   ! first ISAQ bank
        LISAE=LQ(LHEAD-IZISAE)
        CALL MZBOOK(IXMAIN,LISAQ,LISAE,-IZISAQ,
     $                    'ISAQ',NLINKS,0,NDATA,IOISAQ,-1)
C
      ELSE                  ! find last ISAQ bank
    1   LNEXT=LQ(LISAQ)
        IF(LNEXT.EQ.0) THEN
          CALL MZBOOK(IXMAIN,LISAQ,LISAQ,0,
     $                    'ISAQ',NLINKS,0,NDATA,IOISAQ,-1)
        ELSE
          LISAQ=LNEXT
          GOTO 1
        ENDIF
      ENDIF
C
  999 RETURN
      END
