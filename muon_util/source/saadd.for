      SUBROUTINE SAADD(IADD,NMOD,NWIR,JERR)
C     ================================================
C     ROUTINE TO UNPACK SAMUS ADDRESS
C     INPUT IS IADD -- ADDRESS
C     OUTPUT IF MODULE,WIRE
C      JERR=0 THEN OK
C
C      O.Eroshin 20-MAR-1991
C     ================================================
      IMPLICIT NONE
      INTEGER I8,I10,IAND,ISHFT
      INTEGER IADD,NMOD,NWIR,JERR
C
      NWIR = MOD(IADD,2**8)
      NMOD = MOD(IADD/2**8,2**10)
      RETURN
      END
