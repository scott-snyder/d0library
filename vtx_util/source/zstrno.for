      SUBROUTINE ZSTRNO(ZCOORD,LAYER,ISECV,NZSTRP,IEND,ZSTRP)
C-----------------------------------------------------------------------
C-  Subroutine ZSTRNO converts a z coordinate in a given layer and sector
C-  into the number of the z strip whose pad spans this coordinate,
C-  the end which is read out and the floating z strip number corresponding
C-  to the z coordinate that would be obtained from z strip cluster finding.
C-  Inputs:
C-    ZCOORD is the z coordinate of the induced hit (same as sense wire z).
C-    LAYER is the z strip layer (0 - 5)
C-    ISECV is the sector number (0 -32)
C-  Outputs:
C-    NZSTRP is the number of the z strip whose pad spans ZCOORD.
C-    IEND is the z strip end read out (0 is -z end, 1 is +z end)
C-    ZSTRP is the floating z strip number of the z strip cluster center
C-
C-  Description of z-strip numbering:
C-     Z-layers 0(unused), 2 and 4 have pads connected into right-handed helixes
C-     while Z-layers 1(unused), 3 and 5 have pads connected into left-handed 
C-     helixes.
C-     Z-strips are numbered zero to NSTRPS(LAYER)-1 with strip numbers
C-     increasing as phi increases.  Strip zero is defined such that its
C-     pad number zero lies directly over the sense wire in sector zero.  Pads 
C-     are numbered from zero to NPADS(LAYER)-1 with pad number increasing as
C-     z increases.  Z-layer 2 is split at z=0 but the strips and pads in 
C-     this layer are numbered as if the strips were not split.  The two ends
C-     are distinguished by including an end bit (0 for -z end, 1 for +z end).
C-     The end bit is also present on unsplit channels and indicates which end
C-     of the strip is read out.
C-
C-  T. Trippe, 4 Jan. 1987
C-  P. Grudberg 14-DEC-1988  get constants from geometry banks
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZVZST.LINK/LIST'
      INTEGER LVZST, NWZLYR, I
      INTEGER LAYER,ISECV,NZSTRP,IEND,IPAD
      REAL ZCOORD,ZSTRP,HALFZL,ZIPAD,DZSTRN
C
C  Z-strip constants:
      INTEGER NPADS(0:5),NSTRPS(0:5),NSECS(0:5)        ! for z-layer 0 to 5
      REAL ZL(0:5), DZ(0:5)
C  Get constants from VZST
      LVZST = LC( LVGEH - IZVZST )
      NWZLYR = IC( LVZST + 2 )
      DO 10 I = 0 , 5
        NSTRPS(I) = IC( LVZST + 3 + I*NWZLYR )         ! number of strips
        NPADS(I)  = IC( LVZST + 5 + I*NWZLYR )         ! number of pads
        NSECS(I)  = IC( LVZST + 6 + I*NWZLYR )         ! number of phi-sectors
        ZL(I) = C( LVZST + 7 + I*NWZLYR )              ! z-length (cm)
        DZ(I) = C( LVZST + 8 + I*NWZLYR )              ! pad spacing
   10 CONTINUE
C
      HALFZL=ZL(LAYER)/2.
      ZIPAD=(HALFZL+ZCOORD)/DZ(LAYER)            
      IPAD=INT(ZIPAD)
      NZSTRP=MOD(ISECV*NSTRPS(LAYER)/NSECS(LAYER)-(-1)**LAYER*IPAD+
     + NSTRPS(LAYER),  NSTRPS(LAYER))
      DZSTRN=(ZIPAD-FLOAT(IPAD))
      ZSTRP=FLOAT(NZSTRP)+0.5+(-1)**LAYER*(0.5-DZSTRN)
C  Which end is read out?
      IF(LAYER.EQ.2) THEN           ! layer 2 is split at z=0.
        IF((2*IPAD).GE.NPADS(LAYER)) THEN 
          IEND=1                                  ! +z end (south).
        ELSE
          IEND=0                                  ! -z end (north).
        END IF
      ELSE IF((LAYER.EQ.3) .OR. (LAYER.EQ.4)) THEN
        IEND=MOD((NZSTRP+5)/96,2)      ! layer 3 same as 4 for now;
                                       ! change when real numbers known
      ELSE 
        IEND=MOD((NZSTRP+48)/32,2)     ! layer 5
      END IF
C
      RETURN
      END
