      SUBROUTINE DTRK_TO_ZTRK(ZVTX,LZTRKI,LDTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert a track in CDC to a CD Track
C-
C-   Inputs  : ZVTX  = Vertex position being used
C-             LDTRK = Address of DTRK bank which is to be copied
C-             LZTRKI= Address of the booked ZTRK bank. 
C-                     If 0, a new ZTRK bank, will be booked but the address
C-                     of the new ZTRK will *NOT* be returned.
C-   Outputs : 
C-   Controls: 
C-
C-   Created   7-MAY-1995   Srini Rajagopalan
C-   Updated  17-OCT-1995   Srini Rajagopalan  Add pointer from DTRK->ZTRK 
C-   Updated  17-OCT-1995   Srini Rajagopalan  Do not return new ZTRK address 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
C
      INTEGER LZTRH,GZZTRH,LZTRK,LZTRKI,LDTRK,LZFIT
      INTEGER NZTRAK
      INTEGER STAT,JBIT
C
      REAL ZVTX,FITVTX(3),ERRVTX(3)
      REAL SINPHI,COSPHI,STHETA,CTHETA,TANPHI,TANTHE
      REAL XVERT,YVERT,ZVERT
C----------------------------------------------------------------------
C
      IF (LDTRK.LE.0) THEN
        CALL ERRMSG('No DTRK Bank','DTRK_TO_ZTRK',
     &              'ZTRK bank not filled','W')
        GO TO 999
      ENDIF
C
      LZTRH = GZZTRH()
      IF (LZTRH.LE.0) GO TO 999         ! Should never happen
      NZTRAK = IQ(LZTRH + 2)
C
C Check if it is a valid DTRK bank => SHould not already be used in
C a ZTRK
C
      STAT = IQ(LDTRK)
      IF (JBIT(STAT,IUSED).NE.0) THEN
        CALL ERRMSG('This bank already used to form ZTRK',
     &              'DTRK_TO_ZTRK','DTRK Not copied','W')
        GO TO 999
      ENDIF
C
C Book a ZTRK bank if necessary
C
      IF (LZTRKI.LE.0) THEN
        CALL BKZTRK(LZTRK)
      ELSE
        LZTRK = LZTRKI
      ENDIF
C
      LZFIT =  LQ(LZTRK - IZZFIT)
      IF (LZFIT.GT.0) RETURN              ! ZFIT Already exists!
      CALL BKZFIT(LZTRK,LZFIT)
C
      IQ(LZTRH + 2) = IQ(LZTRH + 2) + 1   ! Update number of Tracks in ZTRH
      CALL MZFLAG(0,LDTRK,IUSED,' ')      ! Set DTRK bank as being used
      LQ(LDTRK-2) = LZTRK                 ! pointer from DTRK to ZTRK
C
C Fill ZTRK Bank
C
      LQ(LZTRK - 7) = LDTRK             ! Set pointer to DTRK              
      IQ(LZTRK - 5) = NZTRAK + 1        ! Update ZTRK Track number by 1
      IQ(LZTRK + 3) = IQ(LDTRK - 5)     ! ID of CDC Track
      Q(LZTRK + 6) = 0.0                ! Set quality words to zero
      Q(LZTRK + 7) = 0.0      
      Q(LZTRK + 8) = 0.0      
C
C Fill ZFIT Bank from contents of DTRK bank
C
      IQ(LZFIT - 5) = IQ(LZTRK - 5)
      IQ(LZFIT + 1) = 0
      IQ(LZFIT + 2) = 0
      IQ(LZFIT + 3) = 0
      CALL MVBITS(IQ(LDTRK+2),0,16,IQ(LZFIT+4),0)
      CALL MVBITS(IQ(LDTRK+5),0,16,IQ(LZFIT+4),16)
      IQ(LZFIT + 5) = 0
      IQ(LZFIT + 6) = IQ(LDTRK+2)
      IQ(LZFIT + 7) = IQ(LDTRK+5)
C
      Q(LZFIT +  8)  = Q(  LDTRK+12)
      Q(LZFIT +  9)  = Q(  LDTRK+13)
      Q(LZFIT + 10)  = Q(  LDTRK+6 )
      Q(LZFIT + 11)  = Q(  LDTRK+7 )
      Q(LZFIT + 12)  = Q(  LDTRK+8 )
      Q(LZFIT + 13)  = Q(  LDTRK+9 )
      Q(LZFIT + 14)  = SQRT(Q(LZFIT+11)**2 + Q(LZFIT+12)**2)
      Q(LZFIT + 15)  = Q(  LDTRK+11)
      Q(LZFIT + 16)  = Q(  LDTRK+16)
      Q(LZFIT + 17)  = Q(  LDTRK+17)
      Q(LZFIT + 18)  = Q(  LDTRK+18)
      Q(LZFIT + 19)  = Q(  LDTRK+19)
C
      SINPHI = SIN(Q(LZFIT + 10))
      COSPHI = COS(Q(LZFIT + 10))
      STHETA = SIN(Q(LZFIT + 13))
      CTHETA = COS(Q(LZFIT + 13))
C
      Q(LZFIT+20) = STHETA * COSPHI
      Q(LZFIT+22) = STHETA * SINPHI
      Q(LZFIT+24) = CTHETA
      Q(LZFIT+21) = SQRT( COSPHI**2 * CTHETA**2 * Q(LZFIT+18)**2 +
     &                    SINPHI**2 * STHETA**2 * Q(LZFIT+16)**2)
      Q(LZFIT+23) = SQRT( SINPHI**2 * CTHETA**2 * Q(LZFIT+18)**2 +
     &                    COSPHI**2 * STHETA**2 * Q(LZFIT+16)**2)
      Q(LZFIT+25) = STHETA * Q(LZFIT+18)
C
      Q(LZFIT + 26) = Q(LDTRK + 20)     ! dE/dx
      Q(LZFIT + 27) = Q(LDTRK + 21)     ! Error
C     
      IQ(LZFIT + 28) = IQ(LZFIT + 6) - 2
      IQ(LZFIT + 29) = IQ(LZFIT + 7) - 2
C
      Q(LZFIT + 30) = 0.                 ! VTX dE/dx
      Q(LZFIT + 31) = 0.                 ! Error
C
C  calculate the impact parameter in X-Y plane and
C  calculate the distance between Vertex_Z and the track along Z axis
C
      CALL ZTRKVT(ZVTX,FITVTX,ERRVTX)
      XVERT = FITVTX(1)
      YVERT = FITVTX(2)
      ZVERT = FITVTX(3)
      TANPHI = TAN(Q(LZFIT + 10))
      TANTHE = TAN(Q(LZFIT + 13))
C
      Q(LZFIT + 32) = ABS(XVERT * TANPHI - YVERT + Q(LZFIT + 12) -
     &  Q(LZFIT + 11) * TANPHI) / SQRT(TANPHI**2 + 1)
      IF (TANTHE .NE. 0.0) THEN
        Q(LZFIT + 33) = Q(LZFIT + 15) - (Q(LZFIT + 14) / TANTHE) - ZVERT
      ELSE
        Q(LZFIT + 33) = 999.9
      ENDIF
C
      Q(LZFIT+34) = Q(LDTRK + 22)
C
  999 RETURN
      END
