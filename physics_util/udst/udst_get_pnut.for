      SUBROUTINE UDST_GET_PNUT(LPNUT,XDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns PNUT words to be written to UDST
C-
C-   Created  17-DEC-1993   Ian Adam
C-   Updated  23-JAN-1994   Ulrich Heintz  include z-component
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER       KPNUT,KPNUT1,ID_PNUT,I,LPNUT
      PARAMETER     (KPNUT=7)
      REAL          XDATA(KPNUT)
      CHARACTER*8   PNUT_TAGS(KPNUT)
      DATA PNUT_TAGS/'MET','METPHI','SUMET','METZ','VARX','VARY',
     &  'VARXY'/
C----------------------------------------------------------------------
      IF(LPNUT.GT.0) THEN
        XDATA(1)=Q(LPNUT+7)   ! missing Et
        XDATA(2)=Q(LPNUT+10)  ! phi of missing Et
        XDATA(3)=Q(LPNUT+14)  ! sum Et
        XDATA(4)=Q(LPNUT+5)   ! z component
        XDATA(5)=Q(LPNUT+11)  ! (sigEx)**2
        XDATA(6)=Q(LPNUT+12)  ! (sigEy)**2
        XDATA(7)=Q(LPNUT+16)  ! corr <dExdEy>
      ELSE
        CALL ERRMSG('LPNUT=0','UDST_GET_PNUT',' ','W')
        CALL VZERO(XDATA,KPNUT)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
C
      ENTRY UDST_PNUT_TAGS(KPNUT1,ID_PNUT)

      KPNUT1=KPNUT
      ID_PNUT=7
      CALL UDST_BOOK_GROUP(ID_PNUT,'PNUT',PNUT_TAGS,KPNUT)
      RETURN
      END
