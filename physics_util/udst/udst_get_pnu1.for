      SUBROUTINE UDST_GET_PNU1(LPNU1,I,XDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns PNU1 words to be written to UDST
C-
C-   INPUTS :     LPNU1  [I] - LINK TO PNU1
C-                I      [I] - index of object in PNU1 bank
C-
C-   Created  17-DEC-1993   Ian Adam
C-   Updated  23-JAN-1994   Ulrich Heintz  include z-component
C-   Updated   9-NOV-1995   Ian Adam  Modify PNUT->PNU1
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER       KPNU1,KPNU11,ID_PNU1,I,LPNU1,NREP
      PARAMETER     (KPNU1=7)
      REAL          XDATA(KPNU1)
      CHARACTER*8   PNU1_TAGS(KPNU1)
      DATA PNU1_TAGS/'METZV','MET','METPHI','SUMET','VARX'
     &  ,'VARY','VARXY'/
C----------------------------------------------------------------------
      CALL VZERO(XDATA,KPNU1)

      IF(LPNU1.GT.0) THEN
        IF (I.LE.IQ(LPNU1+3)) THEN
          NREP = IQ(LPNU1+2)
          XDATA(1)=Q(LPNU1+NREP*(I-1) + 4)  ! Z VERTEX USED
          XDATA(2)=Q(LPNU1+NREP*(I-1) + 5)  ! missing Et
          XDATA(3)=Q(LPNU1+NREP*(I-1) + 6)  ! phi of missing Et
          XDATA(4)=Q(LPNU1+NREP*(I-1) + 7)  ! sum Et
          XDATA(5)=Q(LPNU1+NREP*(I-1) + 9)  ! VAR EX
          XDATA(6)=Q(LPNU1+NREP*(I-1) + 10) ! VAR EY
          XDATA(7)=Q(LPNU1+NREP*(I-1) + 12) ! corr <dExdEy>
        ELSE
          CALL ERRMSG('ILLEGAL PNU1 INDEX','UDST_GET_PNU1',' ','W')
        ENDIF
      ELSE
        CALL ERRMSG('LPNU1=0','UDST_GET_PNU1',' ','W')
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
C
      ENTRY UDST_PNU1_TAGS(KPNU11,ID_PNU1)

      KPNU11=KPNU1
      ID_PNU1=21
      CALL UDST_BOOK_GROUP(ID_PNU1,'PNU1',PNU1_TAGS,KPNU1)
      RETURN
      END
