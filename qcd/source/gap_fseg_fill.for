      SUBROUTINE GAP_FSEG_FILL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Get good FDC segments and fill by eta
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-MAR-1996   Brent J. May
C-   Updated  10-MAR-1996   Andrew G. Brandt  Add GAP_FSEG.INC initialize
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZFSEG, I
C
      INCLUDE 'D0$INC:GAP_FSEG.INC'
C----------------------------------------------------------------------
C Format to get segment bank
C    LFSEG = GZFSEG(half,layer)
C    Half 0=N,1=S; Layer 0=Inner Theta,Layer 1=Outer Theta,Layer 2=Phi
C
C Ignore Phi segments for now
      DO I = 1,4
        NFSGIN(I) = -1
        NFSGON(I) = -1
        NFSGIS(I) = -1
        NFSGOS(I) = -1
      END DO
C North Inner theta layer
      CALL COUNT_FSEGS(GZFSEG(0,0),NFSGIN)
C North Outer theta layer
      CALL COUNT_FSEGS(GZFSEG(0,1),NFSGON)
C South Inner theta layer
      CALL COUNT_FSEGS(GZFSEG(1,0),NFSGIS)
C South Outer theta layer
      CALL COUNT_FSEGS(GZFSEG(1,1),NFSGOS)
  999 RETURN
      END


      SUBROUTINE COUNT_FSEGS(LFSEG,NSEGS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Get good FDC segments binned in eta
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-MAR-1996   Brent J. May
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER INDEX,LFSEG,NSEGS(4),IETA_BIN
      REAL AETA
C----------------------------------------------------------------------
      CALL VZERO(NSEGS,4)
      DO WHILE (LFSEG.GT.0)
        IF (Q(LFSEG+32).GT.2.0) GOTO 10   ! Bad chi2/dof
        IF (Q(LFSEG+21).EQ.0.0) GOTO 10   ! Bad theta (unlikely)
        AETA = ABS(LOG(TAN(Q(LFSEG+21)/2.)))  ! Get eta from theta
        INDEX = IETA_BIN(AETA)
        NSEGS(INDEX) = NSEGS(INDEX) + 1
  10    LFSEG = LQ(LFSEG) ! next link
      ENDDO
  999 RETURN
      END


      INTEGER FUNCTION IETA_BIN(ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Return index for forward eta bins
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-MAR-1996   Brent J. May
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ETA
C----------------------------------------------------------------------
        IF (ETA.GE.3.0) IETA_BIN = 1
        IF (3.0.GT.ETA.AND.ETA.GE.2.5) IETA_BIN = 2
        IF (2.5.GT.ETA.AND.ETA.GE.2.0) IETA_BIN = 3
        IF (2.0.GT.ETA.AND.ETA.GE.1.5) IETA_BIN = 4
  999 RETURN
      END

