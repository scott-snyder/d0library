      SUBROUTINE GTMUKF(LMUKF, IT, ISEG, VERS,NP,NH,Z,CHI2,BDL,FIT,MTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetches error matrix and other data from
C-                         MUKF bank
C-
C-   Inputs  : IT   --    Track no.
C-             ISEG --    Track segment no.
C-
C-   Outputs : VERS --    Version, 0 if no MUKF bank found
C-             Z    --    Z coordinate where the matrix is calculated
C-             NP   --    Number of fit parameters
C-             NH   --    Number of fit points
C-             CHI2 --    Chi-squared for the fit
C-             BDL  --    Integral of B*dL [Tesla*m]
C-             FIT(NP)
C-             MTX(NP,NP) --  Error matrix
C-
C-   Controls:
C-
C-   Created   5-MAY-1994   Igor V. Mandrichenko
C-   Updated  13-Jan-1995   I.Mandrichenko CHI2,MUKF
C-   Updated  23-Jan-1995   I.Mandrichenko B*dL
C-   Updated  27-Jan-1995   I.Mandrichenko NHits
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZMUKF.LINK'
      INTEGER IT,VERS,NP,LMUKF,ISEG,NH
      REAL  Z,MTX(*),FIT(*),CHI2,BDL
C----------------------------------------------------------------------
      INTEGER LM,GZMUKF,JM,NPAR

      IF( LMUKF.LE.0 ) THEN
        LM = GZMUKF(IT)
        IF( LM.LE.0 ) GOTO 999
        LMUKF = LM
      ELSE
        LM = LMUKF
      END IF


      VERS = IQ(LM+1)
      IF(VERS.LE.0) GOTO 999

      IF(ISEG.EQ.0) THEN
        ISEG = IQ(LM+2)
        GOTO 999
      END IF

      IF( ISEG.GT.IQ(LM+2) ) GOTO 999
      JM = LM + MUKF_HDRLEN + (ISEG-1)*MUKF_SEGLEN + 1

      NPAR = IQ(JM)
      IF( NPAR.LE.0 .OR. NPAR.GT.6 ) GOTO 999

      NH = IQ(JM+1)
      Z    = Q(JM+2)
      CHI2 = Q(JM+3)
      BDL  = Q(JM+4)

      CALL    UCOPY(Q(JM+5),  FIT, NPAR)
      CALL    TRUPCK(Q(JM+11), MTX, NPAR)
      NP = NPAR

  999 RETURN
      END
