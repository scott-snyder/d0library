      SUBROUTINE DTRKHT(LDTRK,NHIT,HITX,HITY,HITZ,WR,WZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to find coordinates for all the hits on a
C-                         giving CDC track
C-
C-   Inputs  : LDTRK: the bank address of a CDC track DTRK
C-   Outputs : NHIT: number of hits on the track (NHIT=0 if no DTTH bank)
C-             HITX(I),HITY(I),HITZ(I): coordinates of the hits
C-             WR(I): weight for HITX(I) and HITY(I)
C-             WZ(I): weight for HITZ(I) 
C-
C-   Created  19-MAR-1990   Qizhong Li-Demarteau
C-   Modified 15-Jan-1994   C. Klopfenstein - option to work from DHIT bank.
C-   Updated  13-APR-1994   NORMAN A. GRAF  fixed argument list in
C-                                          DSEC_FROM_DHIT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LDTRK, NHIT, PLDTTH, LDRFT, LDALS
      INTEGER GZDRFT, GZDALS, GZDSEC
      INTEGER WRFLAG, I, LABEL, LAY, SEC, WIR, NUMHIT, ISIDE, DLFLAG
      INTEGER LHIT, NFADC, IPHIT, IPAL, KPDSEC
      REAL    HITX(28), HITY(28), HITZ(28), WR(28), WZ(28)
      REAL    YR, DY
      integer gzdhit, ldhit
      integer nhits, nhits1
C----------------------------------------------------------------------
C
      NHIT = 0
      CALL VZERO(HITX(1),28)
      CALL VZERO(HITY(1),28)
      CALL VZERO(HITZ(1),28)
      CALL VZERO(WR(1),28)
      CALL VZERO(WZ(1),28)
C
      IF (LDTRK .LE. 0) RETURN
      PLDTTH = LQ(LDTRK - 1)
      IF (PLDTTH .LE. 0) RETURN
      NHIT = IQ(LDTRK + 2)
      LDRFT = GZDRFT()
      DO 200 I = 0,27
        WRFLAG = IBITS(IQ(LDTRK+3),I,1)  ! XXflag=0 no hit on this wire;
        IF (WRFLAG.NE.0) THEN
          LABEL = IQ(PLDTTH+1)                ! get hit label
          PLDTTH = PLDTTH+2
          LAY = IBITS(LABEL, 16, 2)
          SEC = IBITS(LABEL, 11, 5)
          WIR = IBITS(LABEL,  8, 3)
          NUMHIT = IBITS(LABEL,  1, 7)
          ISIDE  = IBITS(LABEL,  0, 1)
          KPDSEC = GZDSEC(SEC, LAY)
C          IF (KPDSEC .LE. 0) GOTO 200
          if (kpdsec .le. 0) then        ! if no DSEC try to fill it from DHIT
            ldhit = gzdhit()
            if (ldhit .gt. 0) then
              call dsec_from_dhit(lay, sec, nhits, nhits1)
            endif
            kpdsec = gzdsec(sec, lay)
          endif
          if (kpdsec .le. 0) goto 200
          LHIT   = IQ(KPDSEC + 3)
          NFADC  = IQ(KPDSEC + 2)
          IPHIT  = IQ(KPDSEC + NFADC + 4 + WIR) +
     &                     (NUMHIT-1) * LHIT + KPDSEC
          YR = Q(IPHIT + ISIDE + 2) - C(LDRFT + 26 + WIR)
          DY = Q(IPHIT + 5)
          LDALS = GZDALS(LAY,SEC)
          IPAL  = LDALS + 6 + IC( LDALS+6 ) * WIR
          HITX(I+1) = C(IPAL+1) + YR * C(LDALS+3)
          HITY(I+1) = C(IPAL+2) + YR * C(LDALS+4)
          WR(I+1) = 1./ DY**2
          WZ(I+1) = 0.
          HITZ(I+1) = 0.
          DLFLAG = IBITS(IQ(LDTRK+4),I,1) 
          IF(Q(IPHIT+6) .LT. 9999.0 .AND. DLFLAG .NE. 0) THEN
            HITZ(I+1) = Q(IPHIT+4)
            WZ(I+1) = 1./Q(IPHIT+6) **2
          ENDIF
        ENDIF
 200    CONTINUE
C
  999   RETURN
        END
