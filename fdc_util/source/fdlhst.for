      SUBROUTINE FDLHST(H,U,QD,S,W,IPTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill delay line histograms for hits
C-                         associated with a segment
C-
C-   Inputs  : IPTR pointer to theta wire 0 hit
C-   Outputs :
C-   Controls:
C-
C-   Created  22-FEB-1990   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER H,U,QD,S,W,DL
      INTEGER ID
      INTEGER LFXSC,LFXDA
      INTEGER IPTR,PTR,DPTR(8:9)
      INTEGER IER
      INTEGER GZFXSC,GZFXDA
C
      REAL DRIFTT0
      REAL INDEX
      REAL DELLEN(0:5,0:1),ZCOORD
      REAL PHV,PSIG,PTOT
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C GET DELAY LINE LENGTHS
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('DELLEN',DELLEN,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      LFXSC = GZFXSC(H,U,QD,S)
      LFXDA = GZFXDA(H,U,QD,S)
C
      IF (LFXSC.NE.0.AND.LFXDA.NE.0) THEN
C
C POINTER TO WIRE 0 HIT IN FXDA BANK
        PTR = IQ(LFXSC+IPTR+10)
        DRIFTT0 = Q(LFXDA+PTR+2)
C
C POINTERS TO WIRE 8 AND 9 HITS IN FXDA BANK
        DPTR(8) = IQ(LFXSC+IPTR+11)
        DPTR(9) = IQ(LFXSC+IPTR+12)
C
        ID = 100000*H + 10000*U + 1000*QD + 100*S
C
        IF (DPTR(8).NE.0.AND.DPTR(9).NE.0) THEN
C
C DELAY(EAST+WEST)
          PTOT = Q(LFXDA+DPTR(8)+2) + Q(LFXDA+DPTR(9)+2) - 2*DRIFTT0
          CALL HF1(40000000+ID,PTOT,1.)
C
C DELAY (EAST VS WEST)
          PHV = Q(LFXDA+DPTR(8)+2) - DRIFTT0
          PSIG = Q(LFXDA+DPTR(9)+2) - DRIFTT0
          CALL HF2(42000000+ID,PHV,PSIG,1.)
C
C DELAY Z (EAST - WEST)
          ZCOORD = (PHV-PSIG)*(DELLEN(S,MOD(QD,2))/2.) / PTOT
          CALL HF1(41000000+ID,ZCOORD,1.)
        END IF
C
        DO DL = 8,9
          IF (DPTR(DL).NE.0) THEN
C
            ID = 100000*H + 10000*U + 1000*QD + 100*S + DL
C
C DRIFT TIME
            CALL HF1(2000000+ID,Q(LFXDA+DPTR(DL)+2),1.)
C PHVAK FADC
            CALL HF1(8000000+ID,Q(LFXDA+DPTR(DL)+5),1.)
C PULSE HEIGHT
            CALL HF1(12000000+ID,Q(LFXDA+DPTR(DL)+3),1.)
C
C TOTAL NUMBER OF HITS ON SEGMENTS FOR THIS WIRE
            ID = 100000*H + 10000*U + 1000*QD
            INDEX = (10.)*S + DL
            CALL HF1(50000000+ID,INDEX,1.)
C
          END IF
        END DO
      END IF

C
  999 RETURN
      END
