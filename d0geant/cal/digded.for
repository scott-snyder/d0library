      SUBROUTINE DIGDED
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : does a GFHITS and GSDIGI and calculates
C-                         ETOT, the total energy deposited in DEAD MATERIAL
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
      INTEGER IS,LEN3,IDT
C
      INTEGER NDETU
      PARAMETER ( NDETU = 14)
C
      INTEGER LSETU(1000,NDETU)
      REAL ETOTU(NDETU)
      CHARACTER*32 NAMDTU(NDETU)
C
      DATA NAMDTU/'IUSET_CC_CRY_WARM_TUBE+Z',
     &            'IUSET_CC_CRY_WARM_SIDE1+Z',
     &            'IUSET_CC_CRY_WARM_SIDE2+Z',
     &            'IUSET_CC_CRY_COLD_TUBE+Z',
     &            'IUSET_CC_CRY_COLD_SIDE1+Z',
     &            'IUSET_CC_CRY_COLD_SIDE2+Z',
     &            'IUSET_CC_CRY_WARM_TUBE-Z',
     &            'IUSET_CC_CRY_WARM_SIDE1-Z',
     &            'IUSET_CC_CRY_WARM_SIDE2-Z',
     &            'IUSET_CC_CRY_COLD_TUBE-Z',
     &            'IUSET_CC_CRY_COLD_SIDE1-Z',
     &            'IUSET_CC_CRY_COLD_SIDE2-Z',
     &            'IUSET_CC_CRACKS',
     &            'IUSET_CC_ENDPLATES'/
C
      INTEGER NDETE
      PARAMETER ( NDETE = 10)
C
      INTEGER LSETE(1000,NDETE)
      REAL ETOTE(NDETE)
      CHARACTER*32 NAMDTE(NDETE)
C
      DATA NAMDTE/'IUSET_EC_CRY_WARM+Z',
     &          'IUSET_EC_CRY_COLD+Z',
     &          'IUSET_EC_CRY_WARM-Z',
     &          'IUSET_EC_CRY_COLD-Z',
     &          'IUSET_END_MASSLESS_GAPS+Z',
     &          'IUSET_END_PLATES+Z',
     &          'IUSET_END_CRACKS+Z',
     &          'IUSET_END_MASSLESS_GAPS-Z',
     &          'IUSET_END_CRACKS-Z',
     &          'IUSET_END_PLATES-Z'/
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(SCAL(1).NE.0)GO TO 999   !Don't digitize for AMJ  towers
C
      IF(FIRST)THEN
        DO 20 IDT = 1,NDETU
          IF(DUCA.GT.2)CALL GETDET(NAMDTU(IDT),LSETU(1,IDT))
   20   CONTINUE
        DO 25 IDT = 1,NDETE
          IF(DECA.GT.2)CALL GETDET(NAMDTE(IDT),LSETE(1,IDT))
   25   CONTINUE
        FIRST = .FALSE.
      ENDIF
C
      IF(DUCA.GT.2)THEN
      DO 30 IDT = 1,NDETU
        ETOTU(IDT) = 0.
        CALL DODIGI(LSETU(1,IDT),ETOTU(IDT))
   30   CONTINUE
C
      IF(DHIT.EQ.1)THEN
        DO 40 IDT = 1,NDETU
        WRITE(LOUT,41)NAMDTU(IDT),ETOTU(IDT)
   41   FORMAT(' DETECTOR SET ',A32,2X,F15.5,' GEV ')
   40   CONTINUE
      ENDIF
      DO 42 IDT = 1,NDETU
      TOTCRK = TOTCRK+ETOTU(IDT)
   42 CONTINUE
      ENDIF
C
      IF(DECA.GT.2)THEN
      DO 50 IDT = 1,NDETE
        ETOTE(IDT) = 0.
        CALL DODIGI(LSETE(1,IDT),ETOTE(IDT))
   50   CONTINUE
C
      IF(DHIT.EQ.1)THEN
        DO 60 IDT = 1,NDETE
        WRITE(LOUT,41)NAMDTE(IDT),ETOTE(IDT)
   60   CONTINUE
      ENDIF
      DO 62 IDT = 1,NDETE
      TOTCRK = TOTCRK+ETOTE(IDT)
   62 CONTINUE
      ENDIF
C
  999 RETURN
      END
