      SUBROUTINE QTRGFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store highest 5 trigger towers and L0 vertex
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-JUN-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LQTRG,I,J,IE,IP,IETA(10),IPHI(10),ZBIN,NUM_TT,LTRGR,LCRATE
      INTEGER GZTRGR,GZFIND_CRATE,OFFSET,VERTEX,MASK,IAND,BTEST,NOT
      REAL EM_TT(-20:20,32),HD_TT(-20:20,32),HIGH(10)
      LOGICAL GOOD,NOT_DONE,SIGN
      PARAMETER ( OFFSET=269, MASK=15 )
C----------------------------------------------------------------------
C
      CALL BKQTRG(LQTRG)
C
      NUM_TT = IQ(LQTRG+2)
      DO I = 1, NUM_TT
        HIGH(I) = -99.0
      ENDDO
      CALL L1UTIL_TRGR_ADC_UNPACK(EM_TT,HD_TT)
      DO IE = -20, 20
        DO IP = 1,32
          IF (IE.NE.0) THEN
            I = 1
            NOT_DONE = .TRUE.
            DO WHILE (I.LE.NUM_TT .AND. NOT_DONE)
              IF ((EM_TT(IE,IP)+HD_TT(IE,IP)).GT.HIGH(I)) THEN
                NOT_DONE = .FALSE.
                DO J = NUM_TT-1, I, -1
                  HIGH(J+1) = HIGH(J)
                  IETA(J+1) = IETA(J)
                  IPHI(J+1) = IPHI(J)
                ENDDO
                HIGH(I) = EM_TT(IE,IP) + HD_TT(IE,IP)
                IETA(I) = IE
                IPHI(I) = IP
              ENDIF
              I = I + 1
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
C  GET L0 Z-VERTEX
C
      LTRGR = GZTRGR()
      LCRATE = GZFIND_CRATE( 'TRGR', LTRGR, 11 )
      IF ( LCRATE.GT.0 ) THEN
           VERTEX = IQ( LCRATE + OFFSET )
      ELSE
           VERTEX = 0
      ENDIF
      GOOD = BTEST( VERTEX, 5 )
      IF ( GOOD ) THEN
           SIGN = BTEST( VERTEX, 4)
           IF( SIGN )THEN
               VERTEX = VERTEX - 1
               VERTEX = NOT( VERTEX )
               ZBIN = -IAND( VERTEX, MASK )
           ELSE
               ZBIN =  IAND( VERTEX, MASK )
           ENDIF
      ELSE
           ZBIN = 0
      ENDIF
      IF (GOOD) THEN
        Q(LQTRG+5) = FLOAT(ZBIN)*6.25     ! FAST Z-VERTEX
      ELSE
        Q(LQTRG+5) = 0.
      ENDIF
C
      DO I = 1, NUM_TT
        IQ(LQTRG+ 6 + 3*(I-1)) = IETA(I)
        IQ(LQTRG+ 7 + 3*(I-1)) = IPHI(I)
        Q(LQTRG + 8 + 3*(I-1)) = HIGH(I)
      ENDDO
C
  999 RETURN
      END
