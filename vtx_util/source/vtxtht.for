      SUBROUTINE VTXTHT(LVTXT,NHIT,HITX,HITY,HITZ,WR,WZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to find coordinates for all wire hits on a
C-                         given VTX track
C-
C-   Inputs  : LVTXT: the bank address of a VTX track VTXT
C-   Outputs : NHIT:  number of hits on the track
C-             HITX(I),HITY(I),HITZ(I): coordinates of the hits
C-             WR(I): weight for HITX(I) and HITY(I)
C-             WZ(I): weight for HITZ(I)
C-
C-   Created  21-MAR-1990   Peter Grudberg from DTRKHT
C-   Updated  16-AUG-1991   Qizhong Li-Demarteau  NHIT=0 when no hits 
C-                                                information available
C-   Updated   4-NOV-1991   Peter M. Grudberg  Use updated VTTH bank 
C-   Updated  15-NOV-1991   Peter M. Grudberg  Add fix to handle both versions
C-                                             of VTTH 
C-   Updated   2-APR-1993   Ed Oltman  include wire slopes and curvatures
C-   Updated   1-MAY-1993   Ed Oltman  ACCOMADATE CHANGE TO VTTH BANK 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LVTXT, NHIT, PLVTTH, LVRFT, LVALS
      INTEGER GZVRFT, GZVALS, GZVSEC, ISIDE, ZFLAG
      INTEGER WRFLAG, I, LABEL, LAY, SEC, WIR, HITNUM, IEND
      INTEGER LHIT, IPAL, KPVSEC, IPHIT, NWIRE
      REAL    HITX(24), HITY(24), HITZ(24), WR(24), WZ(24)
      REAL    YR, DY, DZ
      REAL    ZVTX,PHIR,DZDR,ZPRED
C----------------------------------------------------------------------
C
      NHIT = 0
      IF (LVTXT .LE. 0) GO TO 999
      CALL VRDGET(ZVTX,PHIR,DZDR)
      PLVTTH = LQ(LVTXT - 1)
      IF(PLVTTH.LE.0)GO TO 999
      NHIT = IQ(LVTXT + 2)
      LVRFT = GZVRFT()
      DO I = 0, 23
        WRFLAG = IBITS(IQ(LVTXT+3),I,1)  ! XXflag=0 no hit on this wire;
        IF (WRFLAG.NE.0) THEN
          LABEL = IQ(PLVTTH + 6)                ! get hit label
          LAY = IBITS(LABEL, 9, 2)
          SEC = IBITS(LABEL, 4, 5)
          WIR = IBITS(LABEL,  1, 3)
          HITNUM = IQ(PLVTTH + 7)
          PLVTTH = PLVTTH + 4
          ISIDE  = IBITS(LABEL,  0, 1)
          KPVSEC = GZVSEC(LAY, SEC)
          NWIRE  = IQ(KPVSEC + 2)
          LHIT   = IQ(KPVSEC + 3)
C
C ****  This is a kludge to make this routine work with the old version of VTTH.
C ****  VTXT was changed at the same time as VTTH: a reference link was added.
C ****  If this reference link is not there, then it must be the old VTTH.
C
          IF ( IQ(LVTXT-3) .GE. 2 ) THEN
            IPHIT  = KPVSEC + IQ(KPVSEC+4+NWIRE+WIR) + LHIT*(HITNUM-1)
          ELSE
            IPHIT  = KPVSEC + HITNUM ! Here, HITNUM is really a pointer
          ENDIF
          IF ( MOD(SEC,2) .EQ. 0 ) THEN
            YR = Q(IPHIT + ISIDE + 1) - C(LVRFT + 31 + WIR)
          ELSE
            YR = Q(IPHIT + ISIDE + 1) + C(LVRFT + 31 + WIR)
          ENDIF
          DY = Q(IPHIT + 4)
          LVALS = GZVALS(LAY, SEC)
          IPAL  = LVALS + 6 + IC( LVALS+6 ) * WIR
          HITX(I+1) = C(IPAL+1) + YR * C(LVALS+3)
          HITY(I+1) = C(IPAL+2) + YR * C(LVALS+4)
          IF (ABS(ZVTX) .LT. 999.) THEN
            ZPRED = ZVTX + SQRT(HITX(I+1)**2+HITY(I+1)**2)*DZDR
            HITX(I+1) = HITX(I+1) 
     &        + C(IPAL+4)*ZPRED + C(IPAL+6)*ZPRED**2
            HITY(I+1) = HITY(I+1)
     &        + C(IPAL+5)*ZPRED + C(IPAL+7)*ZPRED**2
          ENDIF
          IF ( DY .GT. 0. ) THEN
            WR(I+1) = 1. / (DY**2.)
          ENDIF
          WZ(I+1) = 0.
          HITZ(I+1) = 0.
          ZFLAG = IBITS(IQ(LVTXT+4),I,1)  ! ZFLAG=0 hit not used in z fit
          IF( ZFLAG .NE. 0 ) THEN
            HITZ(I+1) = Q(IPHIT + 3)
            DZ = Q(IPHIT+5)
            IF ( DZ .GT. 0. ) THEN
              WZ(I+1) =  1. / (DZ**2.)
            ENDIF
          ENDIF
        ELSE
          HITX(I+1) = 0.
          HITY(I+1) = 0.
          HITZ(I+1) = 0.
          WR(I+1) = 0.
          WZ(I+1) = 0.
        ENDIF
      ENDDO
C
  999 RETURN
      END
