      REAL FUNCTION TRDTMIN(Y)
C----------------------------------------------------------------------
C-
C-   Purpose and method: ARRIVAL TIME OF FIRST ELECTRONS ON THE TRD ANODE
C-            computed by analysing the FADC distribution
C-
C-   Inputs  :
C-   Outputs :
C-
C-   CREATED : A LONG TIME AGO IN SACLAY
C-   Updated  17-MAR-1991   A. Zylberstejn
C-
C----------------------------------------------------------------------
C
C
C
      IMPLICIT NONE
      INTEGER I,IG,II,ITZER,J,LOUT,NDIM,NTOT,NTOTI,NTZER,NZERO
      INTEGER TRUNIT
      INCLUDE 'D0$INC:FADTRD.INC'
      REAL FSEUIL,Y(NBFAD),S,SURF
      LOGICAL FIRST
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
C      INCLUDE 'D0$INC:TRENER.INC'
      DATA FSEUIL/2.5/,SURF/10./
      DATA NZERO,NTOTI/0,0/
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
      END IF
      IG=0
      ITZER=0
C
      DO 40 I=1,NBFAD-20
        IF(Y(I).LT.FSEUIL)GO TO 40
        S=0.
        DO 36 II=1,4
          J=I+II
          IF(Y(J).LT.FSEUIL)GO TO 40 !n consecutive bins above threshold
          S=S+Y(J)
   36   CONTINUE
        IF(S.GT.4.*(Y(I)+.5))THEN
          ITZER=I
          GO TO 1000
        END IF
   40 CONTINUE
 1000 CONTINUE
      TRDTMIN=ITZER
      RETURN
      END
