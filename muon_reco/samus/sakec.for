C#######################################################################
      SUBROUTINE SAKEC(PMOM,TRK,ET,ISIDE,NIN,NOUT,
     +		  Z,U,SI,CO,SIG,THET,ELOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Add (dummy for now) points corresponding EC
C-
C-   Inputs  :  ISIDE -     Side: 1=N, 2=S
C-              NIN   -     Number of points already in
C-              TRK(6)-     Track params
C-              PMOM  -     Estimation of mean momentum in CALOR
C-		ET    -	    Total loss of energy in EC
C-   Outputs :  NOUT  -     Total number of points, including dummies
C-   Controls:  Z(*)  -     Array of Z coordinates
C-              U(*)  -     Array of measurements
C-              SI(*),CO(*) - Array of plane slopes
C-              SIG(*)  -   Array of measurement errors (<=0 means zero)
C-              THET(*) -   Array of standard m.s. deviation angles
C-		ELOS(*)	-   Energy loses per plane
C-
C-   U(i) = X(i)*Co(i) + Y(i)*Si(i)
C-   Created  23-MAR-1994   Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL  AM
      PARAMETER       (AM  = 0.1056)
      REAL  PMOM,U(*),Z(*),SI(*),CO(*),SIG(*),THET(*),TRK(6),ET,
     +			  ELOS(*)
      INTEGER NIN,NOUT,ISIDE
      INTEGER I
      REAL	BETA
      INCLUDE 'D0$INC:SAKFIT.INC'
C----------------------------------------------------------------------
      DO I=1,EC_NMOD(ISIDE)
	BETA = PMOM/SQRT(PMOM**2 + AM**2)
        Z(I+NIN)    = EC_ZMED(I,ISIDE)
        SIG(I+NIN)  = -1.
        THET(I+NIN)  = 0.0141/(PMOM*BETA)*
     +		  SQRT(ABS(EC_LEN(I,ISIDE)/TRK(6)))
	ELOS(I+NIN) = ET*EC_LEN(I,ISIDE)/EC_TOTLEN(ISIDE)
	U(I+NIN) = 0.	! To make it not dummy only.
	SI(I+NIN) = 0.
	CO(I+NIN) = 0.
      END DO
      NOUT = NIN + EC_NMOD(ISIDE)
  999 RETURN
      END
