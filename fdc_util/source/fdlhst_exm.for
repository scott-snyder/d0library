      SUBROUTINE FDLHST_EXM(H,U,QD,S,W,IPTR)
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
C-   Updated  22-MAR-1991   Susan K. Blessing  Modified Rob's modifications
C-    for use with Cosmic Ray Commissioning Examine2
C-   Updated  22-OCT-1993   Susan K. Blessing  Split sectors 0 and 1 from
C-    sectors 2-5 in PH histograms
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER H,U,QD,S,W,DL
      INTEGER ID,ID1
      INTEGER LFXSC,GZFXSC
      INTEGER LFXDA,GZFXDA
      INTEGER IPTR,PTR,DPTR(8:9)
      INTEGER IER
C
      REAL DRIFTT0
      REAL INDEX
      REAL PHV,PSIG,PTOT
C
C----------------------------------------------------------------------
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
        DO DL = 8,9
          IF (DPTR(DL).NE.0) THEN
C
            ID = 100*H + 10*U + QD
            ID1 = 100000*H + 10000*U + 1000*QD + 100*S + DL
C
C PEAK FADC
            CALL HF1(22000+ID,Q(LFXDA+DPTR(DL)+5),1.)
            CALL HF1(5000000+ID1,Q(LFXDA+DPTR(DL)+5),1.)
C
            INDEX = FLOAT( 6*2*QD + 2*S + DL-8)
            CALL HF1(101+H*10,INDEX,Q(LFXDA+DPTR(DL)+5))
C
C PULSE HEIGHT
            IF (S.LE.1) THEN
              CALL HF1(32000+ID,Q(LFXDA+DPTR(DL)+3),1.)
            ELSE
              CALL HF1(35000+ID,Q(LFXDA+DPTR(DL)+3),1.)
            END IF
            CALL HF1(6000000+ID1,Q(LFXDA+DPTR(DL)+3),1.)
C
          END IF
        END DO
      END IF
C
      GO TO 999
C
      ENTRY FDLHST_CD_EXM(H,U,QD,S,W,IPTR)
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
        DO DL = 8,9
          IF (DPTR(DL).NE.0) THEN
C
C TOTAL NUMBER OF HITS ON SEGMENTS FOR THIS WIRE 
            ID = 10*H
            INDEX = 2.*6.*QD + 2.*S + DL-8.
            CALL HF1(404+ID,INDEX,1.)
C
          END IF
        END DO
      END IF
C
  999 RETURN
      END
