      SUBROUTINE CDRIFT ( X , IADR , TIME , ERTIME)
C======================================================================
C
C   Purpose and Methods : Converts the distance in the drift direction
C                         to a drift time
C
C   Inputs  :  X    : local coordinate x of the hit
C              IADR : Layer, sector, Sense wire #
C   Outputs :  TIME : Drift time
C            ERTIME : error on the Drift time
C
C-   Created   5-MAR-1986    K. Ng
C              7-APR-1987    G. Rahal-Callot
C             30-APR-1987    G. Rahal : add spatial resolution as given by
C                                      K. Nishikawa
C-   Updated  11-FEB-1988   Ghita Rahal-Callot : add the error. The X
C-                          coordinate is already corrected from the alignment
C-                          of the wire
C-   Updated  23-JUN-1989   Qizhong Li-Demarteau  change error on drift time
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      REAL TIME, ERTIME, X, XXSS, EDUM, RESOL
      INTEGER IADR(*), LDTMW
C
C
      REAL DRFVEL, ERRX, OFFS
      DATA RESOL   / .01   /
      DATA ERRX   /.0160/
C======================================================================
      LDTMW = LC ( LDTMH - IADR(1) - 1 )
      IF ( LDTMW .LE. 0 ) THEN
        WRITE(LOUT,*)'**** CDRIFT: Bank DTMW not defined'
        CALL EXIT(1)
      ENDIF
C
C ****  Space resolution = Gaussian
C
      XXSS = X
C      CALL RANNOR ( XXSS, EDUM )
C      XXSS = X + XXSS * RESOL
C
C ****  Drift time and error
C
      LDTMW = LDTMW + (IADR(2)*IC(LDTMW+4) + IADR(3))*IC(LDTMW+3) + 4
      OFFS   = C ( LDTMW + 1 )
      DRFVEL = C ( LDTMW + 2 )
      TIME   = ABS ( XXSS ) / DRFVEL + OFFS
      ERTIME = ERRX         / DRFVEL
  999 RETURN
      END
