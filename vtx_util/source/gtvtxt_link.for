      SUBROUTINE GTVTXT_LINK(LOC,QTRAK,QHSEC,QHZLA)
C-----------------------------------------------------------------------
C
C  Returns a VTX track and associated hits: 
C
C  Input:   LOC          track bank location
C
C  Output:  QTRAK(1:21)  contains information on the fitted track
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
C          QTRAK(10)     SG  s of center of gravity of r-z projection
C          QTRAK(11)     ZG  z of center of gravity of r-z projection
C                            s-SG=(z-ZG)*tg(theta)
C                            s defined as the distance from the point 
C                            (XG,YG) along the x,y projection of the track: 
C                            s=sqrt((y-YG)**2+(x-XG)**2)
C          QTRAK(12)     chi squared from x-y fit
C          QTRAK(13)     chi squared from r-z fit
C          QTRAK(14)     ROAD DZDR
C          QTRAK(15)     ROAD ZVTX
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
C          QHZLA(1:3,IHZLA)  not used; left in for compatibility
C
C  Daria Zieminska 
C  Dec. 1989: 
C   Updated 18-JUN-1991 A. Mayorov - do not destroy value of LOC
C-   Updated  29-OCT-1992   Peter M. Grudberg  remove strips 
C-----------------------------------------------------------------------
      IMPLICIT NONE           
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INTEGER LOC,LOCH,LZFIND,NHSEC,NHZLA
      REAL QTRAK(21),QHSEC(4,24),QHZLA(3,6)
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C------------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL VZERO(QHZLA,3*6)
      ENDIF
C                      
      IF (LOC.EQ.0) THEN           
        CALL VZERO(QTRAK,21) 
        CALL VZERO(QHSEC,96) 
        GO TO 1000
      END IF
      CALL UCOPY(Q(LOC+1),QTRAK,21)
      CALL UCOPY(QTRAK(2),NHSEC,1)
      LOCH=LQ(LOC-1)
      CALL UCOPY(Q(LOCH+1),QHSEC(1,1),4*NHSEC)
 1000 RETURN
      END
