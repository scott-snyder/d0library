      SUBROUTINE UEPUT(ILUN,IRUN,IEVT,IMSK,IZRN,IZBO,CFNAM,CGNAM,CTAP,
     + CCOM,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write a data record to an unsorted d0dad
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
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER ILUN,IRUN,IEVT,IMSK(*),IZRN,IZBO,IERR
      CHARACTER*(*) CFNAM,CGNAM,CTAP,CCOM
      INTEGER NUEFN,NUEGN,NUETAP,NUECOM
      INTEGER LENOCC
      EXTERNAL LENOCC
C----------------------------------------------------------------------
C
      IERR=0
      CALL UELSET(ILUN,IERR)
      IF( IERR.NE.0 ) THEN
        IERR = -1
        GOTO 999
      ENDIF
C
      NUEFN=IQ(LUEHD+JUEFN)
      NUEGN=IQ(LUEHD+JUEGN)
      NUETAP=IQ(LUEHD+JUETAP)
      NUECOM=IQ(LUEHD+JUECOM)
      IF( IRUN.EQ.-1 ) THEN
*         WRITE(ILUN,ERR=998) IRUN,NUEFN,NUEGN,NUETAP,NUECOM,0
         WRITE(ILUN,9001,ERR=998) CFNAM(1:LENOCC(CFNAM)),
     +     CGNAM(1:LENOCC(CGNAM)),CTAP(1:LENOCC(CTAP)),
     +     CCOM(1:LENOCC(CCOM))
 9001    FORMAT(4('!',A,/))
      ELSE
         IQ(LUEHD+JUERUN)=IRUN
         WRITE(ILUN,*,ERR=998) IRUN,IEVT,IMSK(1),IMSK(2),IZRN,IZBO
      ENDIF
C
  999 CONTINUE
      RETURN

 998  CONTINUE
      IERR=-2
      RETURN
      END
