      SUBROUTINE CDRESP ( CHARGE, IRESP, N )
C======================================================================
C
C   Purpose and Methods : Convert the charge distributed in each
C                         time slice of one FADC into FADC counts.
C
C   Inputs  :   CHARGE (I) = Charge in the time-slice i
C               N          = # of time-slices (256)
C   Outputs :   IRESP  (I) = FADC response
C
C-   Created  29-APR-1987   Ghita Rahal-Callot
C-   Updated  11-FEB-1988   Ghita Rahal-Callot Arrays begin now at 0   
C
C======================================================================
      IMPLICIT NONE
      REAL    CHARGE(0:*)
      INTEGER IRESP(0:*), N
C
      REAL BKPOIN, SLOP1, SLOP2, OFFS
      INTEGER ISAT, I
C
      DATA BKPOIN, OFFS / 64., 192. /
      DATA SLOP1, SLOP2 / 3., .3333333333 /
      DATA ISAT         / 255 /
C======================================================================
C
C ****  The FADC response is approximated by two line segments
C
      DO 100 I = 0, N-1
C
C ****  We assume a linear response for the moment; The bilinear response
C ****  must be checked with real data.
C
        IRESP(I) = NINT ( CHARGE(I) )
C        IF ( CHARGE (I) .LE. BKPOIN ) THEN
C          IRESP (I) =  NINT ( SLOP1 * CHARGE (I))
C        ELSE
C          IRESP (I) =  NINT ( SLOP2 * ( CHARGE (I) - BKPOIN ) + OFFS )
C        ENDIF
C
C ****  The FADC count can't be greater than 2**8-1
C
        IF ( IRESP (I) .GT. ISAT ) IRESP (I) = ISAT
  100 CONTINUE
  999 RETURN
      END
