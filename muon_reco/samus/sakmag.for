C#######################################################################
      SUBROUTINE SAKMAG(PIN,TRKI,TRKO,ELT,IDIR,NIN,NOUT,
     +				  Z,U,SI,CO,SIG,THET,EL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Add dummy magnet points
C-
C-   Inputs  :  ISIDE -     Side: 1=N, 2=S
C-		PIN  -	    Estimated momentum at entrance to the magnet.
C-              NIN   -     Number of points already in
C-              TRKI(6)-    Track params before magnet
C-              TRKO(6)-    Track params after magnet
C-		ELT   -	    Total energy loss
C-   Outputs :  NOUT  -     Total number of points, including dummies
C-   Controls:  Z(*)  -     Array of Z coordinates
C-              U(*)  -     Array of measurements
C-              SI(*),CO(*) - Array of plane slopes
C-              SIG(*)  -   Array of measurement errors (<=0 means zero)
C-              THET(*) -   Array of standard m.s. deviation angles
C-		EL(*)	-   Energy loss array
C-
C-   U(i) = X(i)*Co(i) + Y(i)*Si(i)
C-   Created  23-MAR-1994   Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      real am
      PARAMETER ( AM  = 0.1056)
      INCLUDE 'D0$INC:SAKFIT.INC'
	REAL	SIG(*),SI(*),CO(*),Z(*),
     +		TRKI(6),TRKO(6),
     +		THET(*),U(*),ELT,EL(*)
	INTEGER	NIN,NOUT
	REAL	PNT(3),DZ,AX,AY,AZ,BETA,ELO,ELI,DE
	REAL	PIN,POUT,PMED,P1,P2,P
	INTEGER	I,J,IDIR
C----------------------------------------------------------------------
	J = NIN + 1
C<<
	ELO = ELT/(1.+ABS(TRKO(6)/TRKI(6)))
	ELI = ELT-ELO
	PMED = PIN-ELI
	POUT = PIN-ELT
	P1 = SQRT(ABS(PIN*PMED))
	P2 = SQRT(ABS(PMED*POUT))
C<<
	ELO = ELO/(SAMAG_NDIVZ(IDIR)/2)
	ELI = ELI/(SAMAG_NDIVZ(IDIR)/2)
C<<
	DO I = 1,SAMAG_NP(IDIR)	    ! SAMAG_NP MUST BE EVEN !!!
	    DZ = SAMAG_Z(I,IDIR) - TRKI(3)
	    IF( ( IDIR.EQ.1 .AND.
     +		      SAMAG_Z(I,IDIR).GT.SAMAG_ZMED(IDIR) )
     +		.OR.
     +		( IDIR.EQ.2 .AND.
     +		      SAMAG_Z(I,IDIR).LT.SAMAG_ZMED(IDIR) )
     +	    ) THEN
		AX = TRKI(4)/TRKI(6)	    ! Dx/Dz
		AY = TRKI(5)/TRKI(6)	    ! Dy/Dz
		AZ = TRKI(6)		    ! Dz/Ds
		PNT(1) = TRKI(1) + AX*DZ
		PNT(2) = TRKI(2) + AY*DZ
		PNT(3) = SAMAG_Z(I,IDIR)
		DE     = ELI
		P = P1
	    ELSE
		AX = TRKO(4)/TRKO(6)
		AY = TRKO(5)/TRKO(6)
		AZ = TRKI(6)
		PNT(1) = TRKO(1) + AX*DZ
		PNT(2) = TRKO(2) + AY*DZ
		PNT(3) = SAMAG_Z(I,IDIR)
		DE     = ELO
		P = P2
	    END IF
c+old
c	    CALL    SAFLD(PNT,FLD)
c	    BFLD(1,J) = FLD(1)
c	    BFLD(2,J) = FLD(2)
c	    BFLD(3,J) = FLD(3)
c-old
	    Z(J) = PNT(3)
	    SIG(J) = -1.0
	    IF( SAMAG_LEN(I,IDIR).GT.0. ) THEN
		BETA = P/SQRT(P**2 + AM**2)
		THET(J) = 0.0136/(P*BETA)*
     +		      SQRT(ABS(SAMAG_LEN(I,IDIR)/AZ))
		EL(J) = DE
	    ELSE
		THET(J) = 0.
		EL(J) = 0.
	    END IF
	    SI(J) = 0.
	    CO(J) = 0.
	    U(J) = 0.	! To make it not-dummy.
	    J = J + 1
	END DO
	NOUT = NIN + SAMAG_NP(IDIR)
  999 RETURN
      END
