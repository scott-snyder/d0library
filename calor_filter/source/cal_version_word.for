      SUBROUTINE CAL_VERSION_WORD(MONTE,TBLOAD,SFTVSN,NOISE,PLATE,IWORD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : set the version word according to inputs
C-
C-   Inputs  : MONTE    .TRUE. if Monte Carlo
C-             TBLOAD   [I] 0 if not test beam; 1 if load 1; 2 if load 2
C-             SFTVSN   [I]    version number
C-             NOISE    [I]    2 bits of noise information
C-             PLATE    .TRUE. if plate sampling fraction setup needed
C-   Outputs : IWORD    32 bit version word
C-   Controls:
C-
C-   Created  27-APR-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MONTE,PLATE
      INTEGER TBLOAD,SFTVSN,NOISE,IWORD
      INTEGER USER_INDEX,DATA_TYPE
C
C----------------------------------------------------------------------
C
C ****  IS THIS GEANT TB or D0?
      USER_INDEX = ISHFT(NOISE,20)                 ! noise at bits 20,21
      DATA_TYPE = 0
      IF (MONTE) DATA_TYPE  = 2**29                ! assume MC
      IF (TBLOAD.NE.0) THEN
        IF (TBLOAD.EQ.1) THEN
          USER_INDEX = 2**16              ! TB90 LOAD 1
          DATA_TYPE  = DATA_TYPE + 2**30  ! TB90
        ELSE
          USER_INDEX = 2**17              ! TB90 LOAD 2
          DATA_TYPE  = DATA_TYPE + 2**30  ! TB90
        END IF
      END IF
      IF ( PLATE ) USER_INDEX = USER_INDEX + 2**18  ! Plate geometry
C
      IWORD = SFTVSN +            ! Version #
     &              USER_INDEX  +       ! CALVSN:L1,L2,PLT,MIX,NOISE
     &              DATA_TYPE           ! D0VSN: NWA/load1/load2
  999 RETURN
      END
