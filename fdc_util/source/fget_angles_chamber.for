      SUBROUTINE FGET_ANGLES_CHAMBER(LZTRK,IVERT,PHI,THETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the pointer to a ZTRK bank for an FDC
C-     track, and given a vertex number, recomputes the PHI and THETA 
C-     of the track. The returned value of PHI and THETA are obtained
C-     by taking the the point on the FDCT track at Z = center of FDC
C-     (about +- 120. cm), and assuming that the track comes from the
C-     vertex. 
C-
C-   Inputs  :  LZTRK    = pointer to ZTRK bank (e.g. LQ(PELC-3) )
C-              IVERT    = Vertex number used (IVERT = 1 uses primary vertex).
C-   Outputs :  PHI      , recomputed using point of track at Z=+-120. cm
C-              THETA    ,     "
C-
C-   Created  12-MAR-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER LZTRK, IVERT
      REAL    PHI, THETA 
C
      INTEGER LFDCT
      INTEGER TRACK
      INTEGER NV
      INTEGER LADDER(0:2)
C
      REAL VERT(14)
      REAL XVER,YVER,ZVER
      REAL Z0
      REAL QTRAK(26),QHTRK(3,34)        
      REAL XPOS,YPOS,ZPOS,RPOS
      REAL ZFDC
C
      DATA ZFDC /120./
C
C----------------------------------------------------------------------
      PHI = 0.0
      THETA = 0.0
      LFDCT = LQ(LZTRK-8)
      IF (LFDCT.LE.0) GOTO 999
C
      CALL GTVERT(IVERT,VERT) 
      XVER = VERT(3)
      YVER = VERT(4)
      ZVER = VERT(5)
C
      TRACK = IQ(LFDCT-5)
      CALL GTFDCT(TRACK,QTRAK,QHTRK,LADDER)
      CALL FGETZ0(TRACK,Z0)
      XPOS = QTRAK(4) - XVER
      YPOS = QTRAK(5) - YVER
C
C Position at chamber center (z=+-120.cm)
C
      ZFDC = SIGN(ZFDC,Z0)
      XPOS = XPOS + (ZFDC-Z0)*QTRAK(7) 
      YPOS = YPOS + (ZFDC-Z0)*QTRAK(8) 
      ZPOS = ZFDC - ZVER
C
      RPOS = SQRT(XPOS**2 + YPOS**2)
      PHI = ATAN2(YPOS,XPOS)
      IF (PHI .LT.0.) PHI = PHI + TWOPI
      THETA = ATAN(RPOS/ZPOS)
      IF (THETA.LT.0) THETA = THETA + PI
C
  999 RETURN
      END
