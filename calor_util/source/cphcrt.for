      SUBROUTINE CPHCRT(IETA,IPHI,ILYR,ICRATE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FIND ADC CRATE FROM PHYSICS ADDRESS
C-
C-   Inputs  : IETA,IPHI,ILYR
C-   Outputs : ICRATE:   ADC CRATE NUMBER
C-   Controls: NONE
C-
C-   Created  25-JUN-1993   Joan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA,IPHI,ILYR,ICRATE
      INTEGER IQUAD,IPHI_QUAD
C----------------------------------------------------------------------
C
      IPHI_QUAD=(IPHI-1)/16
      IF (ABS(IETA).LE.12 .AND. (ILYR.LT.8.OR.ILYR.GT.10)) THEN  ! CC
        IF (IETA.LT.0) THEN
          IQUAD=IPHI_QUAD/3                                      ! NORTH
        ELSE
          IQUAD=1-IPHI_QUAD/3                                    ! SOUTH
        ENDIF
        IF (IPHI_QUAD.EQ.0) IQUAD=IQUAD-1*(ISIGN(1,IETA))
      ELSE                                                       ! EC
        IF (IETA.LT.0) THEN
          IQUAD=IPHI_QUAD+1+4*((3-IPHI_QUAD)/3)                  ! NORTH
        ELSE
          IQUAD=2+IPHI_QUAD+(-1)**(IPHI_QUAD/3)*2*(IPHI_QUAD-
     &      2*(IPHI_QUAD/2))                                     ! SOUTH
        ENDIF
      ENDIF
      IF (IETA.LT.0) THEN
        ICRATE=IQUAD*10+7
      ELSE
        ICRATE=IQUAD*10+8
      ENDIF
C
  999 RETURN
      END
