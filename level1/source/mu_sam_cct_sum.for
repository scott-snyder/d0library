      SUBROUTINE MU_SAM_CCT_SUM(SCC,SAMBITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Pure SAMUS L1 trigger - Last stage of logic
C-
C-   Inputs  : SCC(0:15,6)   - Bit map with good (X,Y,U) roads found
C-
C-   Outputs : SAMBITS(0:28) - Bits sent to the CCT latch card.
C-               SAMBITS(0:1) form a 2-bit counter for the SAMUS triggers
C-
C-   Controls:
C-
C-   Original 17-JUN-1992   Kamel Bazizi
C-   Updated   6-OCT-1992   Guilherme Lima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER SCC(0:15,6)
      LOGICAL SAMBITS(0:11)
      INTEGER QQ(0:15,6),AREA(0:11)
      INTEGER PIX1(0:31,0:31),PIX2(0:31,0:31),SUMBITS(0:13,0:13,2)
      INTEGER XC(0:31),YC(0:31),UC(0:31)
      INTEGER I,II,M,N,IX,IY,IU,IU2,ICNT
      INTEGER IQUAD,IBIT,IMAT,LIM(4,2),ITEMP
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      DATA LIM/ 13,  0, 29, 16,
     &          16, 13,  0, 29/
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL MU_INIT_ARRAYS(PIX1,PIX2,SUMBITS)
        CALL MU_INIT_SUMBITS(SUMBITS)
      ENDIF

C.. Initialize arrays
      DO I=0,11
        SAMBITS(I)=.FALSE.
        AREA(I)=0
      ENDDO
      DO I=0,15
        XC(I)    = SCC(I,1)
        XC(I+16) = SCC(I,2)
        YC(I)    = SCC(I,3)
        YC(I+16) = SCC(I,4)
        UC(I)    = SCC(I,5)
        UC(I+16) = SCC(I,6)
      ENDDO

C== Look for good triplets

C.. Loop over quadrants
      DO IQUAD=1,4
        IMAT=IQUAD
        IF(IQUAD.GT.2) IMAT=IQUAD-2   ! Quads (1,3) and (2,4) are equivalent

        DO I=0,13                   !  Loop over 14 XC bits in each quadrant
          IX=I+LIM(IQUAD,1)
          IF(IX.GT.31) IX=IX-32
          IF(XC(IX).NE.0) THEN
            DO II=0,13              !  Loop over 14 YC bits in each quadrant
              IY=II+LIM(IQUAD,2)
              IF(IY.GT.31) IY=IY-32
              IF(YC(IY).NE.0) THEN

                IU = PIX1(IX,IY)    !  Get the U partner(s) for this
                IU2= PIX2(IX,IY)    !    X,Y combination

                IF(IU.EQ.-1) GOTO 40
                IF(UC(IU).NE.0) THEN              ! Good XYU combination
                  IBIT=SUMBITS(I,II,IMAT)
                  IF(IBIT.NE.-1) THEN
                    IF(IQUAD.GE.3) THEN
                      IBIT=IBIT+6
                      IF(IBIT.GE.12) IBIT=IBIT-12
                    ENDIF
                    AREA(IBIT)=1      ! Set the bit for the corresponding area
                  ENDIF
                ENDIF

                IF(IU2.EQ.-1) GOTO 40
                IF(UC(IU2).NE.0) THEN             ! Good XYU combination
                  IBIT=SUMBITS(I,II,IMAT)
                  IF(IBIT.NE.-1) THEN
                    IF(IQUAD.GE.3) THEN
                      IBIT=IBIT+6
                      IF(IBIT.GE.12) IBIT=IBIT-12
                    ENDIF
                    AREA(IBIT)=1      ! Set the bit for the corresponding area
                  ENDIF
                ENDIF
              ENDIF
   40         CONTINUE
            ENDDO
          ENDIF
   50     CONTINUE
        ENDDO

      ENDDO
C
      ICNT = 0
      DO I = 0,11
        ICNT = ICNT + AREA(I)
      ENDDO
C
      IF(ICNT.GT.3) ICNT=3
      IF(MOD(ICNT,2).EQ.1) SAMBITS(0)=.TRUE.
      IF(MOD(ICNT/2,2).EQ.1) SAMBITS(1)=.TRUE.
C
      DO I = 1,4
        ITEMP = AREA(3*I-3) + AREA(3*I-2) + AREA(3*I-1)
        IF(ITEMP.NE.0) SAMBITS(I+1) = .TRUE.
      ENDDO
C
      DO I=0,11
        IF(AREA(I).NE.0) CALL HFILL(660,FLOAT(I),0.,1.)
      ENDDO
C
      RETURN
      END
