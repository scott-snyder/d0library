      SUBROUTINE DTSG_Z(LAYER,IDTSG,ZPOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find Z information for a given segment
C-
C-   Inputs  : LAYER: layer number
C-             IDTSG: the segment number in this layer's DTSG bank
C-   Outputs : ZPOS: array contains the Z position of this segment 
C-                   from the two delay lines in this layer
C-                   (return 999.9 if no delay line information)
C-
C-   Created  23-SEP-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER  LAYER, IDTSG
      INTEGER  LDTSG, GZDTSG, LDHIT, GZDHIT, KPDSEC, GZDSEC
      INTEGER  IPDTSG, IPDHIT, IPHIT, POINT
      INTEGER  LAY, SEC, WIR, NUMHIT, LHIT, NFADC, NHIT, IHIT
      INTEGER  LABEL(2), HLABEL, I
      REAL     ZPOS(2)
C----------------------------------------------------------------------
C
      ZPOS(1) = 999.9
      ZPOS(2) = 999.9
      LDHIT = 0
      LDTSG = GZDTSG(LAYER)
      IF (LDTSG .LE. 0) GOTO 999
      IPDTSG = LDTSG + 2 + (IDTSG-1) * IQ(LDTSG+2)
      LABEL(1) = IQ(IPDTSG + 9)
      LABEL(2) = IQ(IPDTSG + 15)
      DO 100 I = 1, 2
        IF (LABEL(I) .GT. 0) THEN
          LAY = IBITS(LABEL(I), 16, 2)
          SEC = IBITS(LABEL(I), 11, 5)
          WIR = IBITS(LABEL(I),  8, 3)
          IF (WIR .NE. 0 .AND. WIR .NE. 6) GOTO 100
          NUMHIT = IBITS(LABEL(I),  1, 7)
          KPDSEC = GZDSEC(SEC, LAY)
C
C    if DSEC bank exist, get Z information from DSEC bank
C    if DSEC bank does not exist, get Z information from DHIT bank
C    if both DSEC and DHIT do not exist, too bad, no Z information
C          
          IF (KPDSEC .GT. 0) THEN     
            LHIT   = IQ(KPDSEC + 3)
            NFADC  = IQ(KPDSEC + 2)
            IPHIT  = IQ(KPDSEC + NFADC + 4 + WIR) + 
     &                     (NUMHIT - 1) * LHIT + KPDSEC
            IF (Q(IPHIT+6 ) .LT. 9999.) THEN
              ZPOS(I) = Q(IPHIT+4)
            ENDIF
          ELSE
            IF (LDHIT .LE. 0) LDHIT = GZDHIT()
            IF (LDHIT .LE. 0) GOTO 999
            CALL DHITPT(LAY,SEC,POINT,NHIT)
            IPDHIT = LDHIT + POINT
            DO 200 IHIT = 1, NHIT
              HLABEL = IBITS(IQ(IPDHIT+1),1,17)
              IF (LABEL(I)/2 .EQ. HLABEL) THEN
                ZPOS(I) = FLOAT(IBITS(IQ(IPDHIT+2),16,31)) / 100. 
C                                        ! convert it to cm
                GOTO 100
              ENDIF
              IPDHIT = IPDHIT + IQ(LDHIT+3)
  200       CONTINUE
          ENDIF
        ENDIF
  100 CONTINUE
      GOTO 999
C
  999 RETURN
      END
