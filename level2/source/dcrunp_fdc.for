      SUBROUTINE DCRUNP_FDC(STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack Crates and get pointers to Logical channels.
C-
C-   Inputs  : Minimum Phi supplied by Calorimeter
C-             Maximum Phi supplied by Calorimeter
C-   Outputs : none
C-   Controls: STATUS = Set false for bad data
C- 
C-   Created :  20-AUG-1990    Srini Rajagoplan
C-   Modified:  05-JUL-1991   Yi-Cheng Liu
C-                            ( for FDC )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCMAP.INC'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
C
      INTEGER ICRT,LENCRT
      INTEGER LCDD3,ADDR,CHNL,LENGTH
      INTEGER POINT,CRTID,MASK,START
      INTEGER PHIMIN,PHIMAX
      LOGICAL STATUS
      data MASK / z'FFFF' /
C
C----------------------------------------------------------------------
C
      STATUS = .TRUE.
      DO 5 ICRT = 0,11
        IMAP(ICRT) = .FALSE.
    5 CONTINUE
C
      IF (LHEAD.EQ.0) THEN              ! Check if Top bank exists
        STATUS = .FALSE.
        GO TO 999
      ENDIF
      LCDD3 = LQ(LHEAD-IZCDD3)
      IF (LCDD3.LE.0) THEN              ! Check if CDD3 bank exists
        STATUS = .FALSE.
        GO TO 999
      ENDIF
C
      POINT = LCDD3 + IQ(LCDD3-1) - 16      ! Last data word
C
C  Loop over all sense wire crates.
C
      DO 10 ICRT = 0,11
        IF (POINT.LE.IQ(LCDD3-1)+4) GO TO 999
        CRTID = IAND(IQ(POINT-2), MASK)
        IMAP((CRTID-5)/10) = .TRUE.     ! Crate data found
        LENCRT = IQ(POINT-3)
        START = POINT - LENCRT
        POINT = POINT - 4
        DO WHILE (POINT.GT.START+4)
          ADDR = POINT
          CHNL = IAND(ISHFT(IQ(POINT), -16), MASK)
C
C                                         ! Minimum expected LENGTH is 4
          LENGTH = IAND(IQ(POINT), MASK)  ! But crate problems may return 0
          IF (LENGTH.LT.4) GOTO 10        ! Skipping bad crates
C
          MAP(CHNL) = ADDR              ! Store pointer to channel address
          POINT = POINT - LENGTH/4
        ENDDO
        POINT = START
   10 CONTINUE

  999 RETURN
      END
