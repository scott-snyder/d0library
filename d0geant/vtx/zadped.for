      SUBROUTINE ZADPED( FLASH, LENGTH, PED, SIGMA, IFLASH )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add pedestals to one channel of fadc data.  On the
C-                         first call, fill array RANDOM with random numbers,
C-                         then cycle through those for all subsequent calls.
C-
C-   Inputs  : FLASH(1:*) [F] : real fadc data without pedestal
C-             LENGTH [I] : number of fadc bins
C-             PED [F] : pedestal value
C-             SIGMA [F] : fluctuation of PED
C-
C-   Outputs : IFLASH(1:*) [I] : integer data with pedestal added
C-
C-   Created   1-NOV-1989   Peter Grudberg
C-   Updated  30-OCT-1990   Peter Grudberg  Reset seed after initializing
C-                          random number array 
C-   Updated   6-MAY-1992   Alexandre Zinchenko - zeroed negative amplitudes
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCFLAG.INC'
C
      REAL FLASH(1:*), PED, SIGMA
      INTEGER LENGTH, IFLASH(1:*)
C
      INTEGER NMRAND, MAXCNT
      PARAMETER ( NMRAND = 2999 )
      PARAMETER ( MAXCNT = 255 )
      REAL RANDOM(0:NMRAND-1), FLUCT, VALPED
      REAL RNDM, DUMMY
      INTEGER POINT, I, NUM, COUNT, NRANDO
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        COUNT = 0
C
C ****  Get thr random number seed from RNDM
C
        CALL RDMOUT(NRANDO)
C
C ****  Fill the random number array:
C
        FIRST = .FALSE.
        DO I = 0, NMRAND-1
          CALL NORRAN(FLUCT)
          RANDOM(I) = FLUCT
        ENDDO
C
C ****  Now reset the random number seed to what it was before filling the
C ****  array
C
        CALL RDMIN(NRANDO)
C
      ENDIF
C
C ****  Form a random number between 1 and NMRAND-1 from RANDOM(COUNT):
C
      NUM = ABS( NINT( FLOAT(NMRAND-2) * RANDOM(COUNT) ) ) + 1
C
C ****  Roll one random number to replace RANDOM(COUNT):
C
      CALL NORRAN(FLUCT)
      RANDOM(COUNT) = FLUCT
      COUNT = COUNT + 1
      IF ( COUNT .GE. NMRAND ) COUNT = 0
C
C ****  add pedestals:
C
      DO I = 1, LENGTH
        POINT = MOD(I*NUM,NMRAND)
        VALPED = PED + RANDOM(POINT) * SIGMA
        IFLASH(I) = MIN( NINT( FLASH(I) + VALPED ), MAXCNT )
        IFLASH(I) = MAX0(IFLASH(I),0) 
      ENDDO
C
  999 RETURN
      END
