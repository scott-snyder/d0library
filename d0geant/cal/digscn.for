      SUBROUTINE DIGSCN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DIGITIZE ICD'S
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:HCAL.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ISET,IDET
      INTEGER LICDH,GZICDH
C
      INTEGER NDET
      PARAMETER ( NDET = 2)
C
      INTEGER LSET(1000,NDET),IS,LEN3,IDT
      REAL ETOT(NDET)
      CHARACTER*11 NAMDET(NDET)
      DATA NAMDET/'IUSET_ICD+Z',
     &            'IUSET_ICD-Z'/
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(SCAL(1).NE.0)GO TO 999   !Don't digitize for AMJ  towers
C
      IF(FIRST)THEN
        DO 20 IDT = 1,NDET
          CALL GETDET(NAMDET(IDT),LSET(1,IDT))
   20   CONTINUE
        FIRST = .FALSE.
      ENDIF
C
      DO 30 IDT = 1,NDET
        ETOT(IDT) = 0.
        CALL DODIGI(LSET(1,IDT),ETOT(IDT))
   30 CONTINUE
C
      IF(DHIT.EQ.1)THEN
        DO 40 IDT = 1,NDET
          WRITE(LOUT,41)NAMDET(IDT),ETOT(IDT)
   41     FORMAT(' DETECTOR SET ',A32,2X,F15.5,' GEV ')
   40   CONTINUE
      ENDIF
C
  999 RETURN
      END
