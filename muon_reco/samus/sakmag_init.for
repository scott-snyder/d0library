      SUBROUTINE SAKMAG_INIT(INIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  24-MAR-1994   Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SAKFIT.INC'
C
C..........   Fe parameters
C
c	PARAMETER       RHO = 7.87
c        PARAMETER       DEX = 1.48
        real lrd
        PARAMETER (LRD = 1.76)
C<<
      INTEGER INIT,INT
	LOGICAL	MCONST,SCONST
	INTEGER	IDIR
	CHARACTER*100	H
	REAL	XP(100),SP(3,100)
	REAL	ROT(3,3)
	INTEGER	IB(100)
	INTEGER	I,J,N,IERR
	INTEGER NDIVZ,NB
	REAL	DZ,Z
C----------------------------------------------------------------------
      IF( MUKA_CONST_INIT.NE.0 ) RETURN
      IF( INIT.NE.0 ) THEN
C
C ****  open SAMUS_RECO.RCP file
C
        CALL INRCP ('SAMUS_RECO_RCP',IERR)
        IF (IERR .NE. 0) STOP 'SAMUS_RECO_RCP ERROR'
C
C ****  read WAMUS constants
C
        CALL EZPICK ('MURECO_RCP')
        IF (.NOT.MCONST())  STOP ' WAMUS initialization failed'
        CALL EZRSET
C
C ****  read SAMUS constants
C
        CALL EZPICK ('SAMRECO_RCP')
        IF (.NOT.SCONST()) STOP ' SAMUS initialization failed'
        CALL  EZRSET
C
C ****  initialize magnetic field map
C
        CALL EZPICK ('MURECO_RCP')
        CALL SAFLD (XP, SP)	      ! Dummy call
        CALL EZRSET
      END IF
C<<
	DO IDIR = 1,2
	    CALL    GTSMAG(IDIR,H,N,SP,XP,ROT,NB,IB)
	    SAMAG_ZMED(IDIR) = XP(3)
	    !
	    ! Set two outer points
	    J = 1
	    SAMAG_Z(J,IDIR) = XP(3) - SP(3,1) - SAMAG_OUT_DZ
	    SAMAG_LEN(J,IDIR) = 0.
	    J = J + 1
	    !
	    ! And one on the magnet boundary
	    SAMAG_Z(J,IDIR) = XP(3) - SP(3,1)
	    SAMAG_LEN(J,IDIR) = 0.
	    J = J + 1
	    !
	    ! Set points inside magnet
	    NDIVZ = INT(SP(3,1)/SAMAG_IN_DZ) + 1
	    NDIVZ = NDIVZ+MOD(NDIVZ,2)	    ! MAKE IT EVEN
	    SAMAG_NDIVZ(IDIR) = NDIVZ
	    DZ = 2*SP(3,1)/NDIVZ
	    Z = XP(3) - SP(3,1)
	    DO I = 1,NDIVZ
		SAMAG_Z(J,IDIR) = Z + DZ/2
		SAMAG_LEN(J,IDIR) = DZ / LRD
		J = J + 1
		SAMAG_Z(J,IDIR) = Z + DZ
		SAMAG_LEN(J,IDIR) = 0.
		J = J + 1
		Z = Z + DZ
	    END DO	! Over Z divisions
	    !
	    ! Now Z == XP + SP -- just end of the magnet
	    ! Set up two outer points
	    SAMAG_Z(J,IDIR) = Z + SAMAG_OUT_DZ
	    SAMAG_LEN(J,IDIR) = 0.
	    J = J + 1
	    SAMAG_NP(IDIR) = J - 1
	END DO		! idir = 1,2
  999 RETURN
      END
