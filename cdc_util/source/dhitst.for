      SUBROUTINE DHITST(LABEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : set bits in compressed hits bank DHIT for
C-                         the hits on track DTRK
C-
C-   Inputs  : LABEL(0:27): Label of the hits
C-   Outputs : none
C-
C-   Created  10-AUG-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LABEL(28)
      INTEGER LDHIT, GZDHIT, LAYER, SECTOR, NHIT, IHIT, IWIR, HLABEL, IP
      INTEGER BITPTN, IHSIDE, POINT
      INTEGER ONDTRK
      PARAMETER( ONDTRK = 2**22 )
      INTEGER HTSIDE
      PARAMETER( HTSIDE = 1 )
C----------------------------------------------------------------------
C
      LDHIT = GZDHIT()
      IF (LDHIT .LE. 0) GOTO 999
      DO 100 IWIR = 1, 28
        IF (LABEL(IWIR) .EQ. 0) GOTO 100
        LAYER = IBITS(LABEL(IWIR), 16, 2)
        SECTOR = IBITS(LABEL(IWIR), 11, 5)
        CALL DHITPT(LAYER,SECTOR,POINT,NHIT)
        IP = LDHIT + POINT
        DO 200 IHIT = 1, NHIT
          HLABEL = IBITS(IQ(IP+1),1,17)
          IF (LABEL(IWIR)/2 .EQ. HLABEL) THEN
C      bit 0: side;   bit 24: on DTRK
            IHSIDE = IAND(LABEL(IWIR),HTSIDE)
            BITPTN = ONDTRK + IHSIDE
            IQ(IP+1) = IOR(IQ(IP+1),BITPTN)     
            GOTO 100
          ENDIF
          IP = IP + IQ(LDHIT+3)
  200   CONTINUE
  100 CONTINUE
C
  999 RETURN
      END
