      SUBROUTINE TRD_SEPARATE_ELEMENTS
     &  (REAL_WORD,INTEGER_WORD,CORRECTION,
     &  OLD_CORRECTION,OLD_FADC,COLD,OKW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : put in array old_elements the different pieces used
C-                         to compute corrected TRD energies
C-
C-   Inputs  : REAL_WORD,INTEGER_WORD,CORRECTION
C-   Outputs : OLD_CORRECTION,OLD_FADC,COLD,OKW
C-   Controls: none
C-
C-   Created  23-JUN-1994   Alain PLUQUET
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      REAL OLD_CORRECTION(10,4),OLD_FADC(4),COLD(4)
      CHARACTER*1 C1
      LOGICAL CORRECTION(10),OKW(4)
      INTEGER WIRE,NA,I
      NA=INTEGER_WORD(4)
      DO I=1,4
        OKW(I)=.FALSE.
      ENDDO
      IF(NA.GT.4)THEN
        WRITE(C1,'(i1)')NA
        CALL ERRMSG('TRD_SEPARATE_ELEMENTS','TRD_SEPARATE_ELEMENTS',
     &                  ' na='//C1,'W')
        GO TO 999
      END IF

      DO WIRE=1,NA
        CALL VFILL(OLD_CORRECTION(1,WIRE),10,0.)
        OLD_FADC(WIRE)=0.
        COLD(WIRE)=0.
        OLD_CORRECTION(1,WIRE)=1.
        OLD_CORRECTION(2,WIRE)=1.
        OLD_CORRECTION(3,WIRE)=0.
        OLD_CORRECTION(4,WIRE)=0.
        OLD_CORRECTION(5,WIRE)=1.
        OLD_CORRECTION(6,WIRE)=1.
        OLD_CORRECTION(7,WIRE)=1.
        OLD_CORRECTION(8,WIRE)=1.
        OLD_CORRECTION(9,WIRE)=1.
C
        IF (CORRECTION(1))
     +     OLD_CORRECTION(1,WIRE)=REAL_WORD(28+WIRE)! electronic gain
        IF (CORRECTION(2))
     +     OLD_CORRECTION(2,WIRE)=REAL_WORD(1)     ! EPICOR
        IF(REAL_WORD(1).LT.10.)THEN
          GO TO 999
        END IF
        IF (CORRECTION(3))
     +     OLD_CORRECTION(3,WIRE)=REAL_WORD(2)! APC
        IF (CORRECTION(4))
     +     OLD_CORRECTION(4,WIRE)=REAL_WORD(14+WIRE)! pedestal
        IF (CORRECTION(5))
     +     OLD_CORRECTION(5,WIRE)=REAL_WORD(20+WIRE)! sector
        IF (CORRECTION(6))
     +     OLD_CORRECTION(6,WIRE)=REAL_WORD(24+WIRE)! wire
        IF (CORRECTION(7))
     +     OLD_CORRECTION(7,WIRE)=REAL_WORD(44)! hvt
        IF (CORRECTION(8))
     +     OLD_CORRECTION(8,WIRE)=REAL_WORD(42)! angle
        IF (CORRECTION(9))
     +     OLD_CORRECTION(9,WIRE)=REAL_WORD(43)! gas
        IF (OLD_CORRECTION(2,WIRE).LE.0.) GO TO 20
        IF( OLD_CORRECTION(2,WIRE).LT.10.)THEN
          GO TO 999
        END IF
        COLD(WIRE)=
     &         OLD_CORRECTION(1,WIRE)    ! electronic
     &         /OLD_CORRECTION(2,WIRE)    ! epicor
     &         *OLD_CORRECTION(5,WIRE)    ! sector
     &         *OLD_CORRECTION(6,WIRE)    ! wire
     &         *OLD_CORRECTION(7,WIRE)    ! hvt
     &         *OLD_CORRECTION(9,WIRE)    ! gas
     &         *OLD_CORRECTION(8,WIRE)    ! angle
        IF (COLD(WIRE).GT.0.) THEN
          OLD_FADC(WIRE)=
     &         REAL_WORD(50+WIRE)/COLD(WIRE)
     &         +128.*OLD_CORRECTION(4,WIRE)    ! pedestal
     &         -OLD_CORRECTION(3,WIRE)    ! APC
        ENDIF
        OKW(WIRE)=.TRUE.
   20   CONTINUE
      ENDDO
  999 RETURN
      END
