      SUBROUTINE FAJXYZ(PTCUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FAKES JXYZ DRAWING BANKS FOR ALL ISAJET
C-                              TRACKS.
C-
C-   Inputs  : PTCUT    Minimum Pt (of track) to process
C-   Outputs :
C-   Controls:
C-
C-   Created  11-OCT-1988   R.RAJA
C-   MODIFIED to work when "DCEN 0" is chosen   S.ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCBANK.INC/LIST'
      INCLUDE 'D0$INC:GCLINK.INC/LIST'
      INCLUDE 'D0$INC:GCNUM.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:CENMV.INC/LIST'
      INCLUDE 'D0$INC:GDITRP.INC/LIST'
C
      REAL UBUF(5),A,B,C,RAD,PP,ALAM,MCEN3,PTCUT,PT,MC3
      INTEGER NUBUF,NVERT,I,J,K
      DATA NUBUF/5/     !FROM ISKINE
C
C----------------------------------------------------------------------
C
      DO 10 I = 1,10
   10 IDILST(I) = 0
C MAKE ALL CONTINUOUS LINES

      IF ( JXYZ.NE.0 ) CALL MZDROP(IXSTOR,JXYZ,' ')
C
      RAD = MCEN(2)       !tube of central detector mother volume
      MC3 = MCEN(3)
      IF(MCEN(2) .EQ. 0.0) RAD = 74.93
      IF(MCEN(3) .EQ. 0.0) MC3 = 143.0  ! Outside end of LV0
C
      DO 100 ITRA = 1,NTRACK  !loop over tracks.
        CALL GFKINE(ITRA,VERT,PVERT,IPART,NVERT,UBUF,NUBUF)
        PT = SQRT(PVERT(1)**2 + PVERT(2)**2)
        IF ( PT.LT.PTCUT ) GO TO 100
        VECT(1) = VERT(1)
        VECT(2) = VERT(2)
        VECT(3) = VERT(3)
        CALL GSXYZ
        PP = SQRT(PVERT(1)**2 + PVERT(2)**2 + PVERT(3)**2)
C
        DO 200 J = 1,3
  200   UBUF(J) = PVERT(J)/PP   !direction cosines
C
        B = 2.0*(UBUF(1)*VERT(1) + UBUF(2)*VERT(2))
        A = UBUF(1)*UBUF(1) + UBUF(2)*UBUF(2)
        C = VERT(1)*VERT(1) + VERT(2)*VERT(2) - RAD*RAD
        ALAM = (-B + SQRT(B*B - 4.0*A*C))/(2.0*A)
        DO 300 K = 1,3
  300   VECT(K) = VERT(K) + ALAM*UBUF(K)   !point of intersection with cylinder
        IF ( (ABS(VECT(3))-MC3).LT.0.0 ) GO TO 301 !Point of exit not past end
        MCEN3 = MC3*VECT(3)/ABS(VECT(3)) !signed quantity
        ALAM = ALAM*(MCEN3 - VERT(3))/(VECT(3) - VERT(3))
        DO 302 K = 1,3
  302   VECT(K) = VERT(K) + ALAM*UBUF(K)   !now end point correctly treated
C
  301   CONTINUE
        CALL GSXYZ
C
  100 CONTINUE
  999 RETURN
      END
