      SUBROUTINE BKISAJ(LISAJ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Book ISAJ banks
C-   Outputs :
C-     LISAJ = pointer to created bank
C-
C-   Created  11-DEC-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LISAJ
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISAJ.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NLINKS,NDATA
      PARAMETER( NLINKS = 1 )
      PARAMETER( NDATA = 9 )
      INTEGER GZISAJ,IOISAJ,LISAE,LNEXT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('ISAJ','1I-F',IOISAJ)
        FIRST=.FALSE.
      ENDIF
C
      LISAJ=GZISAJ()
C
      IF(LISAJ.EQ.0) THEN   ! first ISAJ bank
        LISAE=LQ(LHEAD-IZISAE)
        CALL MZBOOK(IXMAIN,LISAJ,LISAE,-IZISAJ,
     $                    'ISAJ',NLINKS,0,NDATA,IOISAJ,-1)
C
      ELSE                  ! find last ISAJ bank
    1   LNEXT=LQ(LISAJ)
        IF(LNEXT.EQ.0) THEN
          CALL MZBOOK(IXMAIN,LISAJ,LISAJ,0,
     $                    'ISAJ',NLINKS,0,NDATA,IOISAJ,-1)
        ELSE
          LISAJ=LNEXT
          GOTO 1
        ENDIF
      ENDIF
C
  999 RETURN
      END
