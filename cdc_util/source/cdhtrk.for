      SUBROUTINE CDHTRK (LAYER,SECTOR,WIRE,IHIT,TRK,TRFLAG,SIDE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if a sense wire hit is on a segment(track)
C-                         Return TRFLAG = 1 if hit on full track (TRK=2)
C-                                           if hit on segment    (TRK=1)
C-   Inputs  : 
C-    LAYER,SECTOR,WIRE,IHIT: layer,sector,wire and hit number of the hit
C-    TRK : =1 check done for segment; =2 check done for track
C-   Outputs : 
C-         TRFLAG: =1 on track (or segment); =0 not
C-         SIDE:   of the hit on track or segment found
C-
C-   Created  12-JUL-1988   Domenico Pizzuto
C-   Updated  27-JUL-1989   Qizhong Li-Demarteau  use modified bank DTRH 
C-   Updated   2-JAN-1990   Qizhong Li-Demarteau  rewrite completely 
C-                                       (old one did not work correctly) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
C
      INTEGER LAYER, SECTOR, WIRE, IHIT, TRK, TRFLAG, SIDE
      INTEGER LABEL, JLABEL, KLABEL, NSEG, JSEG, NHIT, JHIT, IWIRE
      INTEGER XYHITS, HITFLG, GZDTRH, GZDTSG
      INTEGER PLDTRH, PLDTRK, PLDTTH, PLDTSG, IPOINT
C----------------------------------------------------------------------
C
      TRFLAG = 0
      SIDE = 0
      IF (WIRE .GT. MXSENS) GOTO 999
      KLABEL = LAYER*2**15 + SECTOR*2**10 + WIRE*2**7 + IHIT
C
      IF (TRK .EQ. 1) THEN
C
C      check if the hit is on a segment
C
        PLDTSG = GZDTSG(LAYER)
        NSEG = IQ(PLDTSG + 1)
        DO 100 JSEG = 1, NSEG
          IPOINT = PLDTSG + 2 + (JSEG - 1) * IQ(PLDTSG + 2) + 9 + WIRE
          LABEL = IQ(IPOINT)
          JLABEL = LABEL / 2
          IF (KLABEL .EQ. JLABEL) GOTO 101
  100   CONTINUE
        GOTO 999
  101   TRFLAG = 1
        SIDE = IBITS(LABEL,0,1)
C
      ELSE
        IF (TRK .EQ. 2) THEN
C
C      check if the hit is on a track
C
          PLDTRH = GZDTRH()
          IF (PLDTRH .LE. 0) GOTO 999
          PLDTRK = LQ(PLDTRH - 1)
          XYHITS = IQ(PLDTRK + 3)
          IWIRE = (LAYER - 1) * 7 + WIRE
          HITFLG = IBITS(XYHITS,IWIRE,1)
          IF (HITFLG .EQ. 1) THEN
            PLDTTH = LQ(PLDTRK - 1)
            NHIT = IQ(PLDTRK + 2)
            DO 200 JHIT = 1, NHIT
              IPOINT = PLDTTH + (JHIT - 1) * 2 + 1
              LABEL = IQ(IPOINT)
              JLABEL = LABEL / 2
              IF (KLABEL .EQ. JLABEL) GOTO 201
  200       CONTINUE
          ENDIF
          GOTO 999
  201     TRFLAG = 1
          SIDE = IBITS(LABEL,0,1)
        ENDIF
      ENDIF
C
  999 RETURN
      END
