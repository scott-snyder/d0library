C----------------------------------------------------------------------
C-
C-   Created  24-MAR-1994   Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      INTEGER SAMAG_MAXPNT, EC_MAXPNT, MUKA_MAXPNT
      PARAMETER (SAMAG_MAXPNT=100,
     +		EC_MAXPNT=100,
     +		MUKA_MAXPNT=100)
      REAL  SAMAG_OUT_DZ, SAMAG_IN_DZ
      PARAMETER		(SAMAG_OUT_DZ=.5,
     +			SAMAG_IN_DZ=5.0)
C-
      COMMON  /MUKA_CONST/      MUKA_CONST_INIT,
     +				USELOS,
     +				USE_PREFIT_BC,
     +				USE_VERTEX,
     +				RUN_SAKFIT,
     +				PBIAS,
     +				SAMUS_SIGMA,
     +				VTX_SIGMA,
     +				VTY_SIGMA,
     +				VTZ_SIGMA,
     +                          EC_NMOD(2),
     +                          EC_ZMED(EC_MAXPNT,2),
     +                          EC_LEN(EC_MAXPNT,2),
     +				EC_TOTLEN(2),
     +                    SAMAG_NP(2),
     +			  SAMAG_NDIVZ(2),
     +			  SAMAG_ZMED(2),
     +                    SAMAG_Z(SAMAG_MAXPNT,2),
     +                    SAMAG_LEN(SAMAG_MAXPNT,2)
C-
	INTEGER MUKA_CONST_INIT,  ! Flag  'Initialized'
     +        EC_NMOD,          
     +        SAMAG_NP,         
     +	      SAMAG_NDIVZ,
     +	      RUN_SAKFIT
C-
	REAL	EC_ZMED,          
     +		EC_LEN,
     +		EC_TOTLEN,
     +		SAMAG_LEN,
     +		SAMAG_Z,
     +		SAMAG_ZMED,
     +		SAMUS_SIGMA,
     +		VTX_SIGMA,
     +		VTY_SIGMA,
     +		VTZ_SIGMA,
     +		PBIAS
C-
	LOGICAL	USELOS,
     +		USE_PREFIT_BC,
     +		USE_VERTEX
C----------------------------------------------------------------------
