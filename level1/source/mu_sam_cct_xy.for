      SUBROUTINE MU_SAM_CCT_XY(SAMCC,MULT_CUT,QQ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : SAMCC = Coarse centroids from SAMUS MACs for 1 station
C-   Outputs : QQ    = X,Y coarse centroids satisfying XYU triplet
C-
C-   Controls: None
C-
C-   Created  25-APR-1992   Kamel A. Bazizi
C-   Update   26-Jan-1993   K. Bazizi and G. Lima 
C-                          (Turn off 2 overlap pixels because of hardware
C-                          limitations)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,II,M,N,IX,IY,IU,IU2
      INTEGER NOCT,IBIT,IMAT,LIM(4,2)
      INTEGER SWBITS(0:13,0:13,2),PIX1(0:31,0:31),PIX2(0:31,0:31)
      INTEGER SAMCC(0:15,6),MULT_CUT,QQ(0:15,6)
      INTEGER XC(0:31),YC(0:31),UC(0:31),SUMCC
      INTEGER PIX(0:26,0:26)
C<<
      INTEGER FIRST,IPRSWCCT
      DATA FIRST /1/
      DATA LIM/ 13,  0, 29, 16,
     &          16, 13,  0, 29/
C<<
      IF (FIRST.EQ.1) THEN
        FIRST = 0
        CALL MU_INIT_ARRAYS(PIX1,PIX2,SWBITS)
        PIX2(6,12)=-1   ! turn off     
        PIX2(22,28)=-1  ! turn off
      ENDIF
C<<
C.. Clear output arrays and initialize X,Y,U Centroid arrays
      DO I=0,15
C<<
        DO II=1,6
          QQ(I,II)=0
        ENDDO
C<<
        XC(I)    = SAMCC(I,1)
        XC(I+16) = SAMCC(I,2)
        YC(I)    = SAMCC(I,3)
        YC(I+16) = SAMCC(I,4)
        UC(I)    = SAMCC(I,5)
        UC(I+16) = SAMCC(I,6)
C<<
      ENDDO
C<<
C.. Count the number of coarse centroids
C   and VETO if it acceeds the CUT
      SUMCC=0
      DO I=0,31
        SUMCC=SUMCC + XC(I)
      ENDDO
      IF( SUMCC.GT. MULT_CUT) RETURN
C<<
C== Look for good triplets
C<<
C.. Loop over quadrants
      DO NOCT=1,4
        IMAT=NOCT
        IF(NOCT.GT.2) IMAT=NOCT-2   ! Quads (1,3) and (2,4) are equivalent
C<<
        DO 50 I=0,13                !  Loop over 14 XC bits in each quadrant
          IX=I+LIM(NOCT,1)
          IF(IX.GT.31) IX=IX-32
          IF(XC(IX).NE.0) THEN
            DO 40 II=0,13           !  Loop over 14 YC bits in each quadrant
              IY=II+LIM(NOCT,2)
              IF(IY.GT.31) IY=IY-32
              IF(YC(IY).NE.0) THEN
C<<
                IU = PIX1(IX,IY)    !  Get the U partner(s) for this
                IU2= PIX2(IX,IY)    !    X,Y combination
C<<
                IF(IU.EQ.-1) GOTO 40
                IF(UC(IU).NE.0) THEN              ! Good XYU combination
                  IBIT=SWBITS(I,II,IMAT)
                  IF(IBIT.NE.-1) QQ(IBIT,NOCT)=1  ! Set corresponding output bit
                ENDIF
C<<
                IF(IU2.EQ.-1) GOTO 40
                IF(UC(IU2).NE.0) THEN             ! Good XYU combination
                  IBIT=SWBITS(I,II,IMAT)
                  IF(IBIT.NE.-1) QQ(IBIT,NOCT)=1  ! Set corresponding output bit
                ENDIF
              ENDIF
C<<
   40       CONTINUE
          ENDIF
C<<
   50   CONTINUE
C<<
      ENDDO
C<<
      RETURN
      END
