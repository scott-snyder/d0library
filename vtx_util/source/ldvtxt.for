      SUBROUTINE LDVTXT(QTRAK,QHSEC,SEGMP)
C-----------------------------------------------------------------------
C
C  Load a VTX track:
C  1/ Increment the number of VTX tracks in VTRH
C  2/ Store VTX track in Zebra bank VTXT
C  3/ Store the associated hits and fit residuals in VTTH
C
C  Input:  QTRAK(1:21)   contains information on the fitted track
C                        candidate
C           QTRAK(1)     status word (to be defined)
C           QTRAK(2)     number of wire hits on track
C           QTRAK(3)     bit pattern for used sense wires (0:23) in x-y
C           QTRAK(4)     bit pattern for used sense wires (0:23) in r-z
C           QTRAK(5)     total number of z hits related to track
C                        (ch.div + z-strip)
C           QTRAK(6)     phi =arctan(dy/dx)
C           QTRAK(7)     XG  x of center of gravity of x-y projection
C           QTRAK(8)     YG  y of center of gravity of x-y projection
C                           (y-YG)*cos(phi)=(x-XG)*sin(phi)
C           QTRAK(9)     theta=arctan(dr/dz)
C          QTRAK(10)     VZGTHETA covariance of ZG and THETA
C          QTRAK(11)     ZG  z of track at XG,YG
C          QTRAK(12)     chi squared from x-y fit
C          QTRAK(13)     chi squared from r-z fit
C          QTRAK(14)     DZDR OF ROAD CENTER
C          QTRAK(15)     ZVTX OF ROAD
C          QTRAK(16)     error of phi
C          QTRAK(17)     error of center of gravity x-y projection
C          QTRAK(18)     error of theta
C          QTRAK(19)     error of center of gravity of r-z projection
C          QTRAK(20)     ionization
C          QTRAK(21)     SIN(THETA)
C
C          QHSEC(1:4,IHSEC)  contains information on wire hits on track
C          QHSEC(1,IHSEC)    addres = LAYER*2**9+SECTOR*2**4+WIRE*2+LR
C          QHSEC(2,IHSEC)    hit# at this address
C          QHSEC(3,IHSEC)    fit residual in x-y view
C          QHSEC(4,IHSEC)    fit residual in r-z view
C          
C          SEGMP(1:3)       SEGMENT NUMBERS CORRESPONDING TO TRACK 
C
C  D.Zieminska May 1987
C-   Updated  24-OCT-1991   Peter M. Grudberg  Add reference link to VTXT
C-   Updated  27-APR-1992   Peter M. Grudberg  Remove LVTXT in call to BKVTTH
C-   Updated   5-OCT-1992   Peter M. Grudberg  Remove strips
C-   Updated  11-FEB-1993   Liang-ping Chen    update number of (STA) tracks 
C-                                             in VTRH   
C-   Updated   1-MAY-1993   Ed Oltman   Accomadate new VTTH bank
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LVTRH,GZVTRH,LVTXT,LVTTH
      INTEGER NHSEC,SEGMP(3)
      REAL QTRAK(21),QHSEC(4,24),FHSEC
      EQUIVALENCE (NHSEC,FHSEC)
C-------------------------------------------------------------------------
      LVTRH=GZVTRH()
      IF (LVTRH.LE.0) THEN
        CALL BKVTRH(LVTRH)
        IQ(LVTRH+1)=0 ! version number
      END IF
      IQ(LVTRH+2)=IQ(LVTRH+2)+1     ! increment number of tracks in VTX
      IQ(LVTRH+5)=IQ(LVTRH+5)+1     ! same as above but not counted down 
                                    ! from STA (if full tracking) to DST (road)
C
      CALL BKVTXT(LVTXT)
      IQ(LVTXT-5)=IQ(LVTRH+2)
      CALL UCOPY(QTRAK,Q(LVTXT+1),21)
      FHSEC=QTRAK(2)
      CALL BKVTTH(LVTTH)  ! Book VTTH under the just-created VTXT
      CALL UCOPY(SEGMP,IQ(LVTTH+1),3)
      CALL UCOPY(QHSEC,Q(LVTTH+6),4*NHSEC)
 1000 RETURN
      END
