      INTEGER FUNCTION MUQUAD2(NMOD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC    DETERMINES WHAT QUADRANT MODULE MOD IS IN
CCC
CCC    12 quadrants 1-4 in central, 
CCC            5-8 north (+Z) end, 9-12 south (-Z) end
CCC   1 = +X
CCC   2 = +Y (CEILING)  split into -2,+2
CCC   3 = -X
CCC   4 = -Y (BOTTOM)   split into -4,+4
CCC
CCC   DH 9/92, 12/92 use lookup array
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER NMOD,PLNX(307)
      DATA PLNX/9*0,1,-2,2,3,0,4,-4,3*0,            ! 1-19
     A 1,-2,2,3,0,4,-4,3*0,1,-2,2,3,0,4,-4,13*0,     ! 20-49
     A 11*0,6,6,0,7,8,0,5,23*0,10,10,0,11,12,0,9,2*0,    ! 50-99
     A 1,-2,2,3,3,4,-4,1,2*0,1,-2,2,3,3,4,-4,1,2*0,  ! 100-119
     A 1,-2,2,3,3,4,-4,1,2*0,1,-2,2,3,3,4,-4,1,2*0,  ! 120-139
     A 1,-2,2,3,3,4,-4,1,2*0,5,0,0,7,6*0,5,6,6,7,7,8,8,5,2*0,  ! 140-169
     A 10*0,9,0,0,11,6*0,9,10,10,11,11,12,12,9,2*0,   ! 170-199
     A 1,-2,2,3,3,4,-4,1,2*0,1,-2,2,3,3,4,-4,1,2*0,  ! 200-219
     A 1,-2,2,3,3,4,-4,1,2*0,1,-2,2,3,3,4,-4,1,2*0,  ! 220-239
     A 1,-2,2,3,3,4,-4,1,2*0,5,6,0,7,0,8,4*0,5,6,6,7,7,8,8,5,2*0,  ! 240-269
     A 5,6,6,7,7,8,8,5,2*0,9,10,0,11,0,12,4*0,         ! 270-289
     A 9,10,10,11,11,12,12,9,2*0,9,10,10,11,11,12,12,9/ ! 290-307
C
      MUQUAD2=PLNX(NMOD) 
C      I=MOD(NMOD,10)
C      IF(I.EQ.7) I=-1
C      MUQUAD2=(I+3)/2
C      J=NMOD/10
C      J=MOD(J,10)
C      IF(J.GE.5.AND.J.LE.7) MUQUAD2=MUQUAD2+4
C      IF(J.GE.8.AND.J.LE.9) MUQUAD2=MUQUAD2+8
C      IF(NMOD.GE.300) MUQUAD2=MUQUAD2+8
C      IF(MUQUAD2.EQ.2.AND.I.EQ.1) MUQUAD2=-2
C      IF(MUQUAD2.EQ.4.AND.I.EQ.6) MUQUAD2=-4
      RETURN
      END