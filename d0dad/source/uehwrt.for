      SUBROUTINE UEHWRT(ILUN,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write a header to an unsorted d0dad
C-      event catalog.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER   ILUN,IERR
      CHARACTER CUEHED*132
      INTEGER LENOCC
      EXTERNAL LENOCC
C----------------------------------------------------------------------
C
C  Write the header
C
      WRITE(CUEHED,1101,ERR=997) D0DADBASE,'UE',
     +  100*VER_D0DAD_MAJOR+VER_D0DAD_MINOR,NFCFN,NFCGN,
     +  NFCTAP,NFCCOM
 1101 FORMAT(A5,' ',A2,4X,I4,4X,4I4)
      WRITE(ILUN,'(A)',ERR=998) CUEHED(1:LENOCC(CUEHED))
C
C  Copy it to header record
C
      IQ(LUEHD+NDUE+3)=100*VER_D0DAD_MAJOR+VER_D0DAD_MINOR
      IQ(LUEHD+JUEFN)=NFCFN
      IQ(LUEHD+JUEGN)=NFCGN
      IQ(LUEHD+JUETAP)=NFCTAP
      IQ(LUEHD+JUECOM)=NFCCOM
C
  999 CONTINUE
      IERR=0
      RETURN
C
 997  CONTINUE
      IERR = -1
      RETURN
C
 998  CONTINUE
      IERR = -2
      RETURN
      END
