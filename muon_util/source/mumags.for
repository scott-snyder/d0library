      SUBROUTINE MUMAGS(NMOD,HSHAPE,NSPAR,SPAR,XPAR,ROTM
     +                 ,NBUF,IBUF)
C----------------------------------------------------------------
C     S/R MUMAGS returns geometrical parameters of iron toroids.
C  Input data for this is STP bank structure.
C
C  Input:
C     NMOD       Iron slub number
C     NBUF       maximum no. of user words (IBUF) to be filled
C                in this routine.
C     MMAH and MMAG banks in STP structure.
C
C  Output:
C     HSHAPE     volume shape.
C     NSPAR      number of shape parameters.  (=3)
C     SPAR       shape parameters.   (DX,DY,DZ,  half width)
C     XPAR       coordinaes of the center of the volume
C                in lab system.
C     ROTM       (3x3) rotation matrix.
C     NBUF       number of words (IBUF) filled in this routine.
C     IBUF       number of spacial parameters.  (not yet defined)
C
C     note: If the requested volume dose not exist, output 
C           parameters will be,
C                HSHAPE='    '     (i.e.  blank)
C                NSPAR =0
C
C  S.Kunori   30-APR-1987
C-----------------------------------------------------------------
      IMPLICIT NONE  
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZSTPO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMMAH.LINK/LIST'
C  variables in i/o arguments...
      INTEGER NMOD,NSPAR
      REAL    SPAR(3),XPAR(3),ROTM(3,3)
      CHARACTER*4 HSHAPE
      INTEGER IBUF,NBUF(*)
C  extenal function...
      INTEGER GZMMAH,GZMMAG
C  local variables...
      INTEGER LH,LM,I,J,K
C -- get pointer to MMAH bank...
      LH=GZMMAH(I)
C -- get pointer to MMAG bank...
      LM=GZMMAG(NMOD)
C -- check if the bank exist...
      IF(LM.EQ.0) THEN                                  
C     -- MMAG bank dose not exist...
         HSHAPE='    '
         NSPAR=0
         CALL VFILL(SPAR,3,10000.)
         CALL VFILL(XPAR,3,10000.)
         CALL VFILL(ROTM,9,10000.)
      ELSE
C     -- MMAG bank exists.   Now calculate parameters...   
C
C     -- store shape of volume and number of parameters...
C
         HSHAPE='BOX '
         NSPAR=3        
C
C     -- store rotation matrix.
C
         K=0   
         DO 100 I=1,3
         DO 110 J=1,3
         K=K+1
         ROTM(J,I)=C(LM+K+20) 
  110    CONTINUE
  100    CONTINUE 
C
C     -- shape parameters...  
C
         SPAR(1)=C(LM+15) 
         SPAR(2)=C(LM+16) 
         SPAR(3)=C(LM+17) 
C
C     -- position of volume...
C
         XPAR(1)=C(LM+18)
         XPAR(2)=C(LM+19)
         XPAR(3)=C(LM+20)
      ENDIF
C
      RETURN
      END
