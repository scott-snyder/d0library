      SUBROUTINE UEHRD(ILUN,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the header from an unsorted d0dad
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
      INTEGER ILUN,IERR
      CHARACTER CUEHED*132,CTEMP*8
C----------------------------------------------------------------------
C
      REWIND(ILUN,ERR=997)
      READ(ILUN,'(A)',ERR=998) CUEHED
      READ(CUEHED,1101,ERR=996) CTEMP,Q(LUEHD+NDUE+3),IQ(LUEHD+JUEFN),
     +  IQ(LUEHD+JUEGN),IQ(LUEHD+JUETAP),IQ(LUEHD+JUECOM)
 1101 FORMAT(A8,4X,F4.2,4X,4I4)
C
 999  CONTINUE
      IERR=0
      RETURN
C
 996  CONTINUE
      IERR = -3
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
