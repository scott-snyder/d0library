      REAL FUNCTION CELVOL( JQCLYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO SUPPLY THE VOLUME OF A SUB-CELL
C-
C-   Inputs  :     JQCLYR    pointer to CLYR bank
C-   Outputs :     CELVOL    volumee of sub-cell
C-   Controls: 
C-
C-   Created  10-JAN-1989   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER  JQCLYR
      REAL AREA1, AREA2
      CHARACTER*4 CHAR41,CHAR42,CHAR43
      INTEGER ICHAR41,ICHAR42,ICHAR43
      EQUIVALENCE (ICHAR41,CHAR41)
      EQUIVALENCE (ICHAR42,CHAR42)
      EQUIVALENCE (ICHAR43,CHAR43)
      DATA     CHAR41 /'TRAP'/
      DATA     CHAR42 /'TUBS'/
      DATA     CHAR43 /'PCON'/
C 
      IF( IC(JQCLYR + ICSHAP) .EQ. ICHAR41) THEN       ! trapezoid volume
        AREA1 = 2.*C( JQCLYR + ICPAR4) * (C(JQCLYR+ICPAR5) +
     &    C(JQCLYR+ICPAR6))            ! 2*H1*(LH1+LB1) -- face 1 area
        AREA2 = 2.*C( JQCLYR + ICPAR8) * (C(JQCLYR+ICPAR9) +
     &    C(JQCLYR+ICPA10))            ! 2*H2*(LH2+LB2) -- face 2 area
        CELVOL = C( JQCLYR + ICPAR1) * ( AREA1 + AREA2)
      ELSE IF( IC(JQCLYR + ICSHAP) .EQ. ICHAR42) THEN    ! cylindrical
C                                        ! section volume
        CELVOL =  C( JQCLYR + ICPAR3) * ABS(C(JQCLYR+ICPAR2) -
     &    C(JQCLYR+ICPAR1)) * ABS(C(JQCLYR+ICPAR5) - C(JQCLYR+ICPAR4)) 
     &    * (C(JQCLYR+ICPAR1) + C(JQCLYR+ICPAR2)) * RADIAN
      ELSE IF( IC(JQCLYR + ICSHAP) .EQ. ICHAR43) THEN    ! special for
C                                        ! EC/OH
        CELVOL =  (C(JQCLYR + ICPA10) - C(JQCLYR + ICPAR4)) *
     +    (C(JQCLYR + ICPAR9) - C(JQCLYR + ICPAR8)) *
     +    (C(JQCLYR + ICPAR9) + C(JQCLYR + ICPAR8)) * 0.5 * RADIAN *
     +    (C(JQCLYR + ICPAR2) - C(JQCLYR + ICPAR1))
      ELSE
        WRITE( 6, 10)  IC(JQCLYR+ICSHAP)
   10   FORMAT(' CELVOL -- SHAPE NOT CODED: ',A4)
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
