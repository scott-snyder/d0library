      SUBROUTINE TCELLF (FLAGCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine if several tracks share
C-                         the same TRD cells
C-
C-   Inputs  : Table of hit cells
C-   Outputs : FLAGCH (I,J)=1 if track J has a companion in layer I
C-                          I=1 to 3 for anodes, 4 to 6 for cathodes
C-   Controls:
C-
C-   Created  30-NOV-1989   A. Zylberstejn
C-   Updated  15-JUL-1993   A. Zylberstejn  :replace NBPNT by NBHWIR 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRENER.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$INC:TRDWIR.INC'
c      INCLUDE 'D0$INC:TRTOBN.INC'
      INTEGER FLAGCH(6,NTOTTR)
      INTEGER I,ICH,ITR,IUCOMP,JI,JTR
C----------------------------------------------------------------------
      CALL VZERO( FLAGCH,6*NGOODT)
      IF(NGOODT.LE.1)RETURN
      DO 400 ITR=1,NGOODT
C  Look if cells are shared with another particle
        JI=ITR+1
        IF(JI.LT.NGOODT)THEN
          DO 40 JTR=JI,NGOODT
            DO 30 ICH=1,6
              IF(NBHWIR(ICH,ITR)*NBHWIR(ICH,JTR).LE.0)GO TO 30
              DO 20 I=1,NBHWIR(ICH,ITR) !Loop on hit wires for track ITR
                IF(IUCOMP(WIRNBH(I,ICH,ITR),WIRNBH(1,ICH,JTR),
     &            NBHWIR(ICH,ITR)).LE.0)GO TO 20
                FLAGCH(ICH,ITR)=1
                FLAGCH(ICH,JTR)=1
                GO TO 30
   20         CONTINUE
   30       CONTINUE
   40     CONTINUE
        END IF
  400 CONTINUE
  999 RETURN
      END
