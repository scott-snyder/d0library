      SUBROUTINE GTSMAG(NMAG,HSHAPE,NSPAR,SPAR,XPAR,ROTM
     +                 ,NBUF,IBUF)
C----------------------------------------------------------------
C     GTSMAG returns geometrical parameters for the SAMUS toroid.
C            The SAMUS is not modeled like CF and EF by slabs, but
C            has a hole cut out of the initial volume
C
C  Input data for this is STP bank structure.
C
C  Input:
C     NMAG       Iron slub number
C     NBUF       maximum no. of user words (IBUF) to be filled
C                in this routine.
C
C  Output:
C     HSHAPE     volume shape.
C     NSPAR      number of shape parameters.  (=6)
C     SPAR       samus and hole shape parameters. (DX,DY,DZ,  half width)
C     XPAR       coordinaes of the center of the volume
C                in lab system.
C     ROTM       (3x3) rotation matrix.
C     NBUF       number of words (IBUF) filled in this routine.
C     IBUF       number of spacial parameters.  (rotation flag)
C
C     note: If the requested volume dose not exist, output
C           parameters will be,
C                HSHAPE='    '     (i.e.  blank)
C                NSPAR =0
C
C  Alex Mesin   7/16/92
C-----------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZSTPO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMMAH.LINK/LIST'

      INTEGER NMAG,NSPAR,IBUF,NBUF(*),CHROT,GZSMAG,LM,I,J,K
      REAL    SPAR(6),XPAR(3),ROTM(3,3)
      CHARACTER*4 HSHAPE

      LM=GZSMAG(NMAG)   !Get address for the slab

      IF (LM .EQ. 0) THEN
        HSHAPE='    '
        NSPAR=0

      ELSE
        HSHAPE='BOX '
        NSPAR=6

C     -- store rotation matrix.
        K=0
        DO 100 I=1,3
          DO 110 J=1,3
            K=K+1
            ROTM(J,I)=C(LM+K+15)
  110     CONTINUE
  100   CONTINUE

C     -- shape parameters of samus ...
        SPAR(1)=C(LM+6)
        SPAR(2)=C(LM+7)
        SPAR(3)=C(LM+8)

C     -- hole parameters of samus ...
        SPAR(4)=C(LM+9)
        SPAR(5)=C(LM+10)
        SPAR(6)=C(LM+11)

C     -- position of volume...
        XPAR(1)=C(LM+12)
        XPAR(2)=C(LM+13)
        XPAR(3)=C(LM+14)

C     -- rotation flag ...
        IBUF=IC(LM+25)

      ENDIF
C
      RETURN
      END
