C DEC/CMS REPLACEMENT HISTORY, Element BKISJT.FOR
C *1    30-JAN-1990 17:43:15 SERBAN "book ISJT, jets from toy calorimeter"
C DEC/CMS REPLACEMENT HISTORY, Element BKISJT.FOR
      SUBROUTINE BKISJT(LISJT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Book ISJT banks (jets found with toy calorimeter)
C-   Output:
C-      LISJT = pointer to booked bank
C-
C-   Created  29-JAN-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LISJT,LISAC,GZISAC,LNEXT,GZISJT
      INCLUDE 'D0$LINKS:IZISJT.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NLINKS,NDATA
      PARAMETER( NLINKS = 1 )
      PARAMETER( NDATA  = 9 )
C----------------------------------------------------------------------
C
      LISJT=GZISJT()
C
      IF(LISJT.EQ.0) THEN   ! first ISJT bank
        LISAC=GZISAC()
        CALL MZBOOK(IXMAIN,LISJT,LISAC,-IZISJT,
     $                    'ISJT',NLINKS,0,NDATA,3,-1)
C
      ELSE                  ! find last ISJT bank
    1   LNEXT=LQ(LISJT)
        IF(LNEXT.EQ.0) THEN
          CALL MZBOOK(IXMAIN,LISJT,LISJT,0,
     $                    'ISJT',NLINKS,0,NDATA,3,-1)
        ELSE
          LISJT=LNEXT
          GOTO 1
        ENDIF
      ENDIF
  999 RETURN
      END
