      SUBROUTINE GTDTRK(ITRACK,QTRAK)
C-----------------------------------------------------------------------
C
C  Returns contents of a CDC track
C
C  Input:   ITRACK       track ID number
C
C  Output:  QTRAK(1:27)  contains information on the fitted track
C                        candidate
C
C             +1    B    Track Status word
C                        Bit  0: 1 if track goes through part of CDC
C                                  (edge track)
C                        Bit  1-2 : number of ZTRKs is built by this track
C                        Bit  3: not used
C                        Bit  4-10: segment # in layer 0 used for this track
C                        Bit 11-17: segment # in layer 1 used for this track
C                        Bit 18-24: segment # in layer 2 used for this track
C                        Bit 25-31: segment # in layer 3 used for this track
C             +2    I    Number of wires on track NHIT
C             +3    B    Bit pattern of XY used wires ( 0:27 )
C             +4    B    Bit pattern of RZ used wires ( 0:27 )
C             +5    I    Number of Z coordinates on the track NZ
C             +6    F    phi (angle in the x,y plane [0,2.pi] in D0 frame)
C             +7    F    X0   of xy center of gravity
C             +8    F    Y0   of xy center of gravity
C             +9    F    theta (angle in r,z plane [0,pi] with D0 convention)
C            +10    F    R0   ( R0 = SQRT(X0**2 + Y0**2) )
C            +11    F    Z0   at R0 point
C            +12    F    Chi square of xy fit
C            +13    F    Chi square of rz fit
C            +14    I    Total number of degrees of freedom ( NHIT + NZ - 4 )
C            +15    I    0   ( for VTX compatibility )
C            +16    F    Error on phi
C            +17    F    Error on xy center of gravity
C            +18    F    Error on theta
C            +19    F    Error on Z0
C            +20    F    Ionisation of the track, in MIP
C            +21    F    Error on the ionisation ( MIP )
C            +22    F    covariance term between theta and Z0
C
C   The following information are for vertex_Z constrained fit
C
C            +23    F    theta from vertex_z constrained fit
C            +24    F    error on above variable
C            +25    F    Z0 from vertex_z constrained fit
C            +26    F    error on above variable
C            +27    F    covariance term between constrain fitted theta and Z0
C
C  Daria Zieminska Feb 1989
C  Modified Dec 1989: call GTDTRK_LINK
C  Updated  28-NOV-1994   NORMAN A. GRAF  Returns additional DTRK words
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITRACK,LDTRK,GZDTRK,LOC
      REAL QTRAK(27)
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      LOC=GZDTRK(ITRACK)
      CALL GTDTRK_LINK(LOC,QTRAK)
 1000 RETURN
      END
