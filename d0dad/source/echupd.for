      SUBROUTINE ECHUPD(ILUN,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Update a header in a D0DAD event catalog.  
C-
C-   INPUTS  : 
C-   OUTPUTS : 
C-   CONTROLS: 
C-
C-   CREATED   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IERR,NW,IUNIT,IRECEC,IRPREC,ISWAP
C----------------------------------------------------------------------
C
C  The values are set,  just write...
C
      IUNIT=IQ(LECHD+JLUN)
      IRPREC=IQ(LECHD+NDEC+JEC_IRPREC)
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      NW=IQ(LECHD+NDEC+JEC_IECHED)
      ISWAP=IQ(LECHD+JECBS)
      CALL VZERO(IQ(LPREC+1),IRPREC*IRECEC)
      CALL UCOPY(IQ(LECHD+NDEC+1),IQ(LPREC+1),NW)
C
      CALL ECRWRT(IUNIT,1,IQ(LPREC+1),IRPREC,ISWAP,IERR)
      IF( IERR.NE.0 ) GOTO 998
C
  999 CONTINUE
      IERR=0
      RETURN
C
  998 CONTINUE
      IERR = -1
      RETURN
      END
