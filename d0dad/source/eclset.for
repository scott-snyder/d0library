      SUBROUTINE ECLSET(ILUN,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Set up the protected links for the catalog
C-     on ILUN.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER ILUN,IERR
      INTEGER LZFIND
      EXTERNAL LZFIND
C-----------------------------------------------------------------------
C
      IERR=0
      IF( ILUN.NE.IQ(LECHD+JLUN) ) THEN
         LECHD=LZFIND(IXDDAD,LQ(LDADH-JFEC),ILUN,JLUN)
         IF( LECHD.EQ.0 ) THEN
           IERR = -1
           GOTO 999
         ENDIF
         LRUNS=LQ(LECHD-LLRUNS)
         LPREC=LQ(LECHD-LLPREC)
         LPTAB=LQ(LECHD-LLPTAB)
         LRDAT=LQ(LECHD-LLRDAT)
         LDATA=LQ(LECHD-LLDATA)
         LORDER=LQ(LECHD-LLORDER)
         LRANK=LQ(LECHD-LLRANK)
         LISOLD=LQ(LECHD-LLISOLD)
         LDNEW=LQ(LECHD-LLDNEW)
         LFLOK=LQ(LECHD-LLFLOK)
      ENDIF
C
  999 RETURN
      END
