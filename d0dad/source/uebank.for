      SUBROUTINE UEBANK(IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Book a control bank for a UE file.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER IERR
C-----------------------------------------------------------------------
C
      CALL MZBOOK(IXDDAD,LUEHD,LDADH,-JFUE,'UEHD',0,0,NDUE+JUEHED,2,0)
      IF( LUEHD.GT.0 ) THEN
         IERR=0
         IQ(LDADH+JFUE)=IQ(LDADH+JFUE)+1
         IQ(LUEHD+JUEFN)=NFCFN
         IQ(LUEHD+JUEGN)=NFCGN
         IQ(LUEHD+JUETAP)=NFCTAP
         IQ(LUEHD+JUECOM)=NFCCOM
      ELSE
         IERR = -1
      ENDIF
C
C-----------------------------------------------------------------------
  999 RETURN
      END
