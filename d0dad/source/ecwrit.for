      SUBROUTINE ECWRIT(IARR,IPREC1,NLRECS,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods:  Write NLRECS logical records from IARR into
C-      physical records starting with IPREC1.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER ILUN,IARR(*),IPREC1,NLRECS,NWORDS,IERR
      INTEGER I,J,IRPREC,IRECEC,NPRECS,ISWAP
C-----------------------------------------------------------------------
C
      IERR=0
      ILUN=IQ(LECHD+JLUN)
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      IRPREC=IQ(LECHD+NDEC+JEC_IRPREC)
      ISWAP=IQ(LECHD+JECBS)
      NPRECS=NLRECS/IRPREC
      NWORDS=IRPREC*IRECEC
C
C  Write all completely filled physical records
C
      DO I=IPREC1,IPREC1+NPRECS-1
        J=NWORDS*(I-IPREC1)+1
        CALL ECRWRT(ILUN,I,IARR(J),IRPREC*IRECEC,ISWAP,IERR)
        IF( IERR.NE.0 ) GOTO 998
      ENDDO
C
C  Write portion of last physical record
C
      IF( MOD(NLRECS,IRPREC).NE.0 ) THEN
        J=NWORDS*(I-IPREC1)+1
        NWORDS=MOD(NLRECS,IRPREC)*IRECEC
        CALL ECRWRT(ILUN,I,IARR(J),NWORDS,ISWAP,IERR)
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
