      SUBROUTINE ECLOAD(IARR,IPREC1,NLRECS,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Load the NLRECS logicals records starting with
C-     physical record IPREC1 into IARR
C-
C-   Inputs  :
C-   Outputs : IARR - The data
C-   Controls:
C-
C-   Created  13-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER ILUN,IARR(*),IPREC1,NLRECS,NWORDS,IERR,NPRECS
      INTEGER I,J,IRPREC,IRECEC
C-----------------------------------------------------------------------
C
      IERR=0
      ILUN=IQ(LECHD+JLUN)
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      IRPREC=IQ(LECHD+NDEC+JEC_IRPREC)
      NPRECS=NLRECS/IRPREC
      NWORDS=IRPREC*IRECEC
C
C  Load all completely filled physical records
C
      DO I=IPREC1,IPREC1+NPRECS-1
        J=NWORDS*(I-IPREC1)+1
        CALL ECRRD(ILUN,I,IARR(J),IRPREC*IRECEC,IQ(LECHD+JECBS),IERR)
        IF( IERR.NE.0 ) GOTO 998
      ENDDO
C
C  Read portion of last physical record
C
      IF( MOD(NLRECS,IRPREC).NE.0 ) THEN
        J=NWORDS*(I-IPREC1)+1
        NWORDS=MOD(NLRECS,IRPREC)*IRECEC
        CALL ECRRD(ILUN,I,IARR(J),NWORDS,IQ(LECHD+JECBS),IERR)
        IF( IERR.NE.0 ) GOTO 998
      ENDIF
C
  999 CONTINUE
      IERR=0
      RETURN
C
 998  CONTINUE
      IERR = -1
      RETURN
      END
