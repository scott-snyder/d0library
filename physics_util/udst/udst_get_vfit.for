      SUBROUTINE UDST_GET_VFIT(LVFIT,XDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill VFIT banks into XDATA
C-
C-   Inputs  : LVFIT
C-   Outputs : XDATA
C-
C-   Created  17-OCT-1994   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER KVFIT,KVFIT1,ID_VFIT,I,LVFIT
      PARAMETER( KVFIT = 3 )
      REAL    XX(KVFIT),XDATA(KVFIT) 
      EQUIVALENCE(XX,ZVFIT)
      CHARACTER*8 VFIT_TAGS(KVFIT)
      DATA VFIT_TAGS/'ZVFIT','DZVFIT','X2VFIT'/
      REAL ZVFIT,DZVFIT,X2VFIT
      COMMON/VFIT_OBJECT/ ZVFIT,DZVFIT,X2VFIT
C----------------------------------------------------------------------
      ZVFIT  = Q(LVFIT+5)     ! z-position of vfitex
      DZVFIT = Q(LVFIT+8)     ! error
      X2VFIT = Q(LVFIT+9)     ! Global Chi Square of fit
      DO I=1,KVFIT
        XDATA(I)=XX(I)
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
C
      ENTRY UDST_VFIT_TAGS(KVFIT1,ID_VFIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book VFIT group
C-
C----------------------------------------------------------------------
      KVFIT1=KVFIT
      ID_VFIT=19
      CALL UDST_BOOK_GROUP(ID_VFIT,'VFIT',VFIT_TAGS,KVFIT)
C----------------------------------------------------------------------
      RETURN
      END
