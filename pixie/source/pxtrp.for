      SUBROUTINE PXTRP(XCENT,YCENT,ZCENT,ZANG,DXB,DXT,DY,DZ,NSEC)
C======================================================================
C
C  Description: Draws a trapezoid, with NSEC sections, in the spirit
C               of GEANT, using DI-3000. 
C
C  Parameter Declarations:
C  =======================
C  
C  XCENT,YCENT,ZCENT - Position in which one would like the trapeziod, in
C                      coordinates of the mother volume.
C
C  DXB,DXT - Half-lengths of the top and bottom of the trapezoid.
C  DY,DZ   - Half-lengths of the heigth and depth, respectively.
C  NSEC    - The number of sections one would like the trapezoid to 
C            be divided into.
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  March 18,1986 - Original Creation
C  Jan.  10,1990 - Updated Lupe Howell: Implementing Color Table
C
C======================================================================
C
C  
      IMPLICIT NONE
C
      INTEGER NSEC,I,J,K,L
      REAL DXB,DXT,DY,DZ,R,THET,XDIS,YDIS,XCENT,YCENT,ZCENT
      REAL XDARR(4),YDARR(4),DELX
      REAL XARR1(4),YARR1(4),ZARR1(4)
      REAL XARR2(4),YARR2(4),ZARR3(4),ZFRT,ZBCK,ZANG
      REAL XARR3(4),YARR3(4)

C
C  Executable code:
C  ================
C
      CALL PXCOLR('CYA')
      YDIS = 2*DY/NSEC
      DELX = DXT - DXB
      R = SQRT((DY)**2 + (DELX)**2)
      THET = ACOS(DELX/R)
      XDIS = R*COS(THET)/NSEC
      ZBCK = ZCENT - DZ
      ZFRT = ZCENT + DZ
      DO 27 I = 1,NSEC
           XDARR(1) = - DXB - XDIS*(I-1)
           YDARR(1) = - DY + YDIS*(I-1)
           ZARR1(1) = ZBCK
           XDARR(2) = - DXB - XDIS*(I)
           YDARR(2) = - DY + YDIS*(I)
           ZARR1(2) = ZBCK
           XDARR(3) = DXB + XDIS*(I)
           YDARR(3) = - DY + YDIS*(I)
           ZARR1(3) = ZBCK
           XDARR(4) = DXB + XDIS*(I-1)
           YDARR(4) = - DY + YDIS*(I-1)
           ZARR1(4) = ZBCK
           DO 40 K = 1,4
              XARR1(K)=XDARR(K)*COS(ZANG)+YDARR(K)*SIN(ZANG)
              YARR1(K)=XDARR(K)*(-SIN(ZANG))+YDARR(K)*COS(ZANG)
              XARR2(K) = XCENT + XARR1(K)
              YARR2(K) = YCENT + YARR1(K)
   40      CONTINUE
           CALL J3PLGN(XARR2,YARR2,ZARR1,4)
           DO 36 L = 1,4
              XARR3(L) = XARR2(L)
              YARR3(L) = YARR2(L)
              ZARR3(L) = ZFRT
              CALL J3MOVE(XARR2(L),YARR2(L),ZARR1(L))
              CALL J3DRAW(XARR3(L),YARR3(L),ZARR3(L))
   36      CONTINUE
           CALL J3PLGN(XARR3,YARR3,ZARR3,4)

   27 CONTINUE
      RETURN
      END
