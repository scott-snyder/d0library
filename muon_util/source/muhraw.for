      SUBROUTINE MUHRAW(NMOD,NCEL,NLAT,IADC,IFLG,HIT)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Convert raw ADC counts to hits for MUOH
C-
C-    Input  :  NMOD   - Module ID
C-              NCEL   - Cell address
C-              NLAT   - Latch bits
C-              IADC(8)- Raw ADC counts
C-
C-    Output :  IFLG(3,2)- Flag words for MUOH; (m,1)=even, (m,2)=odd
C-                       (1) Cell Address
C-                       (2) Hit quality
C-                       (3) Number of drift times
C-              HIT(6,2)- Data for MUOH; (m,1)=even, (m,2)=odd
C-                       (1-2,n) Corrected drift times
C-                       (3-4,n) Corrected pad pulses
C-                       (5-6,n) Corrected delta times
C-
C-    Created :  2-SEP-93  M. Fortner
C-       7/94 DH allow hits missing times
C     11/94 DH allow hits missing pads. but cut out very low times
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMOD,NCEL,NLAT,IADC(8),IFLG(3,2)
      INTEGER IADD(2),IPAD(2),IWIR,NOUT
      INTEGER I,J,K
      REAL HIT(6,2)
      REAL PAD(4),WIRE(4)
C-------------------------------------------------------------------
C       Convert ADC counts into real signals
C-------------------------------------------------------------------
      IADD(1) = NMOD*256 + NCEL
      IADD(2) = NMOD*256 + NCEL + 4
      CALL MUHPAD(NMOD,NCEL,NLAT,IADC,IPAD,PAD)
      CALL MUHWIR(NMOD,NCEL,IPAD,IADC,IWIR,WIRE)
C-------------------------------------------------------------------
C       Fill hit array for single or double hits
C-------------------------------------------------------------------
      DO I=1,2
        IFLG(1,I)= IADD(I)
        IFLG(2,I)= IWIR+IPAD(I)
        IFLG(3,I)= 2
        HIT(1,I) = WIRE(1)
        HIT(2,I) = WIRE(2)
        HIT(3,I) = PAD(2*I-1)
        HIT(4,I) = PAD(2*I)
        HIT(5,I) = WIRE(3)
        HIT(6,I) = WIRE(4)
        IF(IAND(IWIR,8).EQ.0) THEN     ! One time
          IFLG(3,I) = 1
          HIT(2,I) = 0
          HIT(6,I) = 0
        ENDIF
        IF(IWIR.GE.512) THEN           ! No times
          IFLG(3,I) = 0
          HIT(1,I) = 0
          HIT(2,I) = 0
          HIT(5,I) = 0
          HIT(6,I) = 0
        ENDIF
CCCC   analyze pad hits
        IF(IPAD(1).EQ.3.AND.IPAD(2).EQ.3) THEN  ! neither had pad
          IF(WIRE(1).LT.-30..OR.IWIR.GE.512) THEN    ! no drift time
            IFLG(1,I) = 0
            IFLG(2,I) = 0
          ENDIF
        ELSE IF(IPAD(I).EQ.3) THEN ! one pad bad
          IFLG(1,I) = 0
          IFLG(2,I) = 0
        ENDIF
      ENDDO
      HIT(5,2) = -HIT(5,2)
      HIT(6,2) = -HIT(6,2)
C
      RETURN
      END
