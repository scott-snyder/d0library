      SUBROUTINE TRD_COMBINE_ELEMENTS
     &  (REAL_WORD,INTEGER_WORD,NEW_CORRECTION,OLD_FADC,COLD,OKW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : recomputes TRD energies with old FADC and
C-                          new corrections
C-
C-   Inputs  : REAL_WORD,INTEGER_WORD,NEW_CORRECTION,OLD_FADC,COLD
C-   Outputs : REAL_WORD,INTEGER_WORD,OKW
C-   Controls: none
C-
C-   Created  24-JUN-1994   Alain PLUQUET
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      REAL NEW_CORRECTION(10,4),OLD_FADC(4),CNEW(4),COLD(4)
      LOGICAL OKW(4)
      INTEGER WIRE,NA,NC,NCLA,CLUSTER,I

      DO I=1,4
        OKW(I)=.TRUE.
      ENDDO
      NA=INTEGER_WORD(4)
      NC=INTEGER_WORD(5)
      NCLA=INTEGER_WORD(6)

      REAL_WORD(1)       = NEW_CORRECTION(2,1)  ! EPICOR
      REAL_WORD(2)       = NEW_CORRECTION(3,1)  ! APC
      REAL_WORD(44)      = NEW_CORRECTION(7,1)  ! hvt
      REAL_WORD(42)      = NEW_CORRECTION(8,1)  ! angle
      REAL_WORD(43)      = NEW_CORRECTION(9,1)  ! gas

      DO WIRE=1,NA
        REAL_WORD(28+WIRE) = NEW_CORRECTION(1,WIRE) ! electronic gain
        REAL_WORD(14+WIRE) = NEW_CORRECTION(4,WIRE)! pedestal
        REAL_WORD(20+WIRE) = NEW_CORRECTION(5,WIRE)! sector
        REAL_WORD(24+WIRE) = NEW_CORRECTION(6,WIRE)! wire

        IF (NEW_CORRECTION(2,WIRE).GT.0.) THEN
          CNEW(WIRE)
     &        =NEW_CORRECTION(1,WIRE)    ! electronic
     &        /NEW_CORRECTION(2,WIRE)    ! epicor
     &        *NEW_CORRECTION(5,WIRE)    ! sector
     &        *NEW_CORRECTION(6,WIRE)    ! wire
     &        *NEW_CORRECTION(7,WIRE)    ! hvt
     &        *NEW_CORRECTION(9,WIRE)    ! gas
     &        *NEW_CORRECTION(8,WIRE)    ! angle
          IF (CNEW(WIRE).GT.0.) THEN
            REAL_WORD(50+WIRE)=
     &      (OLD_FADC(WIRE)
     &      -128.*NEW_CORRECTION(4,WIRE)       ! pedestal
     &      +NEW_CORRECTION(3,WIRE))      ! APC
     &      *CNEW(WIRE)
             OKW(WIRE)=.TRUE.
          ELSE
            OKW(WIRE)=.FALSE.
          ENDIF
        ELSE
          OKW(WIRE)=.FALSE.
        ENDIF
      ENDDO

      IF (COLD(1).GT.0.) THEN
        DO CLUSTER=1,NCLA
          REAL_WORD(50+NA+NC+CLUSTER)=
     &    REAL_WORD(50+NA+NC+CLUSTER)*CNEW(1)/COLD(1)
        ENDDO
      ENDIF

      END
