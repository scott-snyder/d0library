      SUBROUTINE MUIMP_BNB(LMUOT,DIMP_BEND,DIMP_NB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculate non-bend impact parameter
C-
C-   Inputs  : LMUOT = pointer to MUOT bank
C-   Outputs : DIMP_NB = impact parameter to vertex in non-bend view
C-   Controls: 
C-
C-   Created  14-DEC-1992   Darien R. Wood
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LMUOT,IQUAD,LVERT,ISTAT_VERT,IFW1
      REAL DIMP_BEND,DIMP_NB,ZVERT,XMAGC,YMAGC,ZMAGC,XV,ZV
      REAL XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,ZCOSOM
      INTEGER GZVERT
      EXTERNAL GZVERT
C----------------------------------------------------------------------
      DIMP_BEND = -9999.
      DIMP_NB = -9999.
      IF(LMUOT.GT.0) THEN
        IFW1 = IQ(LMUOT+4)
        IF (IFW1 .EQ. 5) GO TO 999 ! Return if A-stub track
        IQUAD = IQ(LMUOT+3)
        XMAGC = Q(LMUOT+11)
        YMAGC = Q(LMUOT+12)
        ZMAGC = Q(LMUOT+13)
        XCOSIM = Q(LMUOT+14)
        YCOSIM = Q(LMUOT+15)
        ZCOSIM = Q(LMUOT+16)
        XCOSOM = Q(LMUOT+17)
        YCOSOM = Q(LMUOT+18)
        ZCOSOM = Q(LMUOT+19)
C
        LVERT = GZVERT(1)
        IF (LVERT .NE. 0) THEN
          DO WHILE (LVERT.GT.0)  !-loop over all primary vertices
            ISTAT_VERT = IQ(LVERT+2)
            IF(BTEST(ISTAT_VERT,30) .OR. BTEST(ISTAT_VERT,31)) THEN
              ZVERT = Q(LVERT+5)
CC  PROJECT TO POINT (0,0,ZVERT) ZV IS BEND, XV IS NONBEND
              IF(IQUAD.EQ.1.OR.IQUAD.EQ.3) THEN
                XV=YMAGC-XMAGC*YCOSOM/XCOSOM
                ZV=ZMAGC-XMAGC*ZCOSIM/XCOSIM-ZVERT
              ELSE IF(IQUAD.EQ.2.OR.IQUAD.EQ.4) THEN
                XV=XMAGC-YMAGC*XCOSOM/YCOSOM
                ZV=ZMAGC-YMAGC*ZCOSIM/YCOSIM-ZVERT
              ELSE IF(IQUAD.EQ.5.OR.IQUAD.EQ.7.OR.
     &    IQUAD.EQ.9.OR.IQUAD.EQ.11) THEN
                XV=YMAGC+(ZVERT-ZMAGC)*YCOSOM/ZCOSOM
                ZV=XMAGC+(ZVERT-ZMAGC)*XCOSIM/ZCOSIM
              ELSE
                XV=XMAGC+(ZVERT-ZMAGC)*XCOSOM/ZCOSOM
                ZV=YMAGC+(ZVERT-ZMAGC)*YCOSIM/ZCOSIM
              ENDIF
              IF(ABS(ZV).LT.ABS(DIMP_BEND)) DIMP_BEND = ZV
              IF(ABS(XV).LT.ABS(DIMP_NB)) DIMP_NB = XV
            ENDIF
            LVERT = LQ(LVERT)
          ENDDO
        ENDIF
      ENDIF  
C----------------------------------------------------------------------
  999 RETURN
      END
