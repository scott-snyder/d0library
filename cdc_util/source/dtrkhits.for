      SUBROUTINE DTRKHITS(LDTRK,NHIT,HITX,HITY,HITZ,WR,WZ,QUALITY)
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
C-             QUALITY(I): 
C-                      Bit  0       set if one -Z hit exists and no satuation
C-                                   and no overlap
C-                      Bit  1       set if one +Z hit exists and no satuation
C-                                   and no overlap
C-
C-   Created  26-JUL-1993    Qizhong Li-Demarteau   based on DTRKHT with more
C-                                                information on hit quality
C-   Updated  19-NOV-1993   Srini Rajagopalan  Modified to look into Ideal hit
C-                          banks. Also added DLRES to smear ideal hits.
C-   Updated  12-APR-1994   Srini Rajagopalan  Delay line quality flag is
C-                          now picked from DHIT bank instead of DSEC/DCDA. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LDTRK, NHIT, PLDTTH, LDRFT, LDALS
      INTEGER GZDRFT, GZDALS, GZDSEC, IER
      INTEGER WRFLAG, I, LABEL, LAY, SEC, WIR, NUMHIT, ISIDE, DLFLAG
      INTEGER LHIT, NFADC, IPHIT, IPAL, KPDSEC
      INTEGER QUALITY(28), STATUS, KPDCDA, GZDCDA, JHIT
      INTEGER NH,IIHIT,DHIT_LABEL
      INTEGER LDHIT,GZDHIT,JPOINT
      REAL    HITX(28), HITY(28), HITZ(28), WR(28), WZ(28)
      REAL    YR, DY, XR,DLRES
      PARAMETER (DLRES = 0.25)            ! DL hit resl. in cm.
      CHARACTER*4 PATH
C----------------------------------------------------------------------
C
      NHIT = 0
      CALL PATHGT(PATH)
      CALL VZERO(HITX(1),28)
      CALL VZERO(HITY(1),28)
      CALL VZERO(HITZ(1),28)
      CALL VZERO(WR(1),28)
      CALL VZERO(WZ(1),28)
      CALL VZERO(QUALITY(1),28)
C
      IF (LDTRK .LE. 0) RETURN
      PLDTTH = LQ(LDTRK - 1)
      IF (PLDTTH .LE. 0) RETURN
      NHIT = IQ(LDTRK + 2)
      LDRFT = GZDRFT()
      ldhit = gzdhit()
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
          IF (KPDSEC .LE. 0) GOTO 200
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
C
          IF(Q(IPHIT+6) .LT. 9999.0 .AND. DLFLAG .NE. 0) THEN
            HITZ(I+1) = Q(IPHIT+4)
            WZ(I+1) = 1./Q(IPHIT+6) **2
C
C
C get pointer to DHIT bank
C
            call dhitpt(LAY,SEC,JPOINT,NH)
C
C get quality of delay line hit.. Must compare hit label to ensure you
C are on the right hit.
C
            do 500 iihit = 1, nh
              dhit_label = ibits(iq(ldhit+jpoint+1),0,18)
              if (dhit_label .eq. label) then
                quality(i+1) = IBITS(IQ(ldhit+jpoint+1),20,2)
                goto 501
              endif
              jpoint = jpoint + iq(ldhit+3)
  500       continue
C
C            QUALITY(I+1) = IBITS(IQ(IPHIT+9),0,2)
  501       IF (PATH.EQ.'GEAN') THEN
              CALL NORRAN(XR)
              HITZ(I+1) = HITZ(I+1) + DLRES*XR
              WZ(I+1) = 1.0/(DLRES**2)
            ENDIF
          ENDIF
        ENDIF
  200 CONTINUE
C
  999 RETURN
      END
