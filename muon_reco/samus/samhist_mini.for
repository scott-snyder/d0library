      SUBROUTINE SAMHIST_MINI(IHOFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Minimum histograms for SAMUS_RECO.
C-
C-   Inputs  : IHOFF    I    Offset for histogram ID.
C-   Outputs :
C-   Controls:
C-
C-   Created  10-MAY-1991   O.Eroshin
C-   Updated   5-MAY-1994   Andrei  Mayorov   SAMUS quadrants were changed
C-   Updated  15-MAY-1994   Andrei  Mayorov   change logic of h. filling to
C-                                            skip WAMUS tracks
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUOT.LINK'
      INTEGER IHOFF,LMTRH,LTRK,NTR_N,NTR_S,GZMTRH
      REAL PT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
C
C......   Track histograms
C
        CALL HBOOK1(IHOFF+3,' TRACK/EVENT RECONSTRUCTED $',20,0.,20.,0.
     &    0)
        CALL HBOOK1(IHOFF+8,' TRACK/EVENT RECONSTRUCTED NORTH $',20,0.,
     &    20.,0.0)
        CALL HBOOK1(IHOFF+9,' TRACK/EVENT RECONSTRUCTED SOUTH $',20,0.,
     &    20.,0.0)
C
C......   Angle distributions
C
        CALL HBOOK1(IHOFF+44,' THETA RC $',20,  0., 20.,0.0)
        CALL HBOOK1(IHOFF+45,' THETA RC $',20,160.,180.,0.0)
        CALL HBOOK1(IHOFF+48,' PHI RECONSTRUCTED $',90,-180.,180.,0.0)
C
C......   Momentum distributions
C
        CALL HBOOK1(IHOFF+54,' MOMENTUM REC NORTH $',100,-125.,125.,0.0)
        CALL HBOOK1(IHOFF+55,' MOMENTUM REC SOUTH $',100,-125.,125.,0.0)
C
        FIRST=.FALSE.
      ENDIF
C
C  Fill Histograms.
C  ================
C
      LMTRH  = GZMTRH()
      IF (LMTRH.LE.0)                           RETURN
C
      NTR_N  = 0
      NTR_S  = 0
      LTRK   = LQ(LMTRH-IZMUOT)
    1 CONTINUE
      IF (LTRK.GT.0)                            THEN
        IF (IQ(LTRK+3).ne.13)  then
          IF (IQ(LTRK+3).ne.14)  then
            LTRK = LQ(LTRK)
            goto 1                        ! WAMUS track 
          else  
            NTR_S = NTR_S+1
            CALL HF1(IHOFF+55,Q(LTRK+23),1.)
          end if  
        ELSE
          NTR_N = NTR_N+1
          CALL HF1(IHOFF+54,Q(LTRK+23),1.)
        END IF

        PT = SQRT(Q(LTRK+14)**2+Q(LTRK+15)**2)
        CALL HF1(IHOFF+44,ATAN2(PT,Q(LTRK+16))*180/3.1415927,1.)
        CALL HF1(IHOFF+45,ATAN2(PT,Q(LTRK+16))*180/3.1415927,1.)
        CALL HF1(IHOFF+48,ATAN2(Q(LTRK+12),Q(LTRK+11))*180/3.1415927,1.)
        LTRK = LQ(LTRK)
        GO TO 1
      END IF
      CALL HF1(IHOFF+3,FLOAT(NTR_N+NTR_S),1.)
      CALL HF1(IHOFF+8,FLOAT(NTR_N),1.)
      CALL HF1(IHOFF+9,FLOAT(NTR_S),1.)
C
  999 RETURN
      END
