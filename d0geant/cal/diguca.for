      SUBROUTINE DIGUCA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : does a GFHITS and GSDIGI and calculates
C-                         ETOT, the total energy deposited in a detector
C-                         set 'IUSET_CENTRAL_CALORIMETER'
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  11-OCT-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:HCAL.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
C
      INTEGER ISET,IDET
C
      INTEGER NDET
      PARAMETER ( NDET = 4)
C
      INTEGER LSET(1000,NDET),IS,LEN3,IDT
      REAL ETOT(NDET)
      CHARACTER*32 NAMDET(NDET)
      DATA NAMDET/'IUSET_CCEM',
     &            'IUSET_CCFH',
     &            'IUSET_CCCH',
     &            'IUSET_CC_MASSLESS_GAP'/
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
   30   CONTINUE
C
      IF(DHIT.EQ.1)THEN
        DO 40 IDT = 1,NDET
        WRITE(LOUT,41)NAMDET(IDT),ETOT(IDT)
   41   FORMAT(' DETECTOR SET ',A32,2X,F15.5,' GEV ')
   40   CONTINUE
      ENDIF
      DO 42 IDT = 1,NDET
      TOTUCA = TOTUCA+ETOT(IDT)
   42 CONTINUE
  999 RETURN
      END
