      SUBROUTINE MUKFFL(LMUKF,ISEG,NP,NH,Z,CHI2,BDL,FIT,ERRMTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books and fills MFIT bank
C-
C-   Inputs  : LMUKF        - MUKF bank to fill
C-	       IVERS	    - Version (0-epmty, 1-SAMUS KF)
C-             ISEG         - Number of fit segment to fill
C-             NP           - Number of fit parameters in the fit
C-                            NP=6 - MAX
C-             NH           - Number of fit points
C-             Z            - Coordinate the fit is at
C-             CHI2         - Non-normalized CHI2 for fit
C-             BDL          - Intergal of B*dL [Tesla*m]
C-             FIT(NP)      - Fit vector
C-             ERRMTX(NP,NP)
C-			    - Error martix of the fit
C-
C-   Outputs : (implicit)   - MUKF bank fit segment
C-
C-   Controls:
C-
C-   Created  29-DEC-1994   Igor V. Mandrichenko
C-   Updated  13-Jan-1995   Igor V. Mandrichenko Renamed from MUER,
C-                                          added CHI2
C-   Updated  23-Jan-1995   Igor V. Mandrichenko Add B*dL
C-   Updated  23-Jan-1995   Igor V. Mandrichenko Add Nhits
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUKF.LINK'
C------------------------------------------- Parameters ---------------
	INTEGER     LMUKF,ISEG,NP,NH
	REAL	    FIT(NP),ERRMTX(NP,NP),Z,CHI2,BDL
C------------------------------------------- Locals -------------------
	INTEGER	J
C<<
	!
	! Verify input parameters
	IF( LMUKF.LE.0 )	    GOTO 999
	IF( ISEG.LE.0 )		    GOTO 999
	IF( NP.LE.0 .OR. NP.GT.6 )  GOTO 999
	!
	! Verify the bank
	IF( IQ(LMUKF+2) .LT. ISEG ) GOTO 999	! Check space allocated
	IF( IQ(LMUKF-1) .LT. MUKF_HDRLEN+MUKF_SEGLEN*ISEG ) GOTO 999	
C<<
	J = LMUKF+1+MUKF_HDRLEN+MUKF_SEGLEN*(ISEG-1)
	
	IQ(J)  = NP
	IQ(J+1) = NH
	Q(J+2) = Z
	Q(J+3) = CHI2
	Q(J+4) = BDL
C<<
	CALL VZERO(Q(J+5),27)
	CALL UCOPY(FIT,Q(J+5),NP)
	CALL TRPCK(ERRMTX,Q(J+11),NP)
  999 RETURN
      END
