      SUBROUTINE CHGDIV(VPLHT,ZMINUS,ZPLUS,VHTMIN,VHTPLU)
C-----------------------------------------------------------------------
C-   Purposes and Methods :
C-  CHGDIV computes pulse heights due to charge division.
C-   Inputs  : VPLHT  = pulse height (charge) deposited on wire
C-             ZPLUS  = distance to z+ end of wire (cm)
C-             ZMINUS = distance to z- end of wire (cm)
C-   Outputs : VHTPLU = pulse height (charge) seen at z+ end of wire
C-             VHTMIN = pulse height (charge) seen at z- end of wire
C-   Controls: none
C-
C-  T. Trippe, Dec. 1, 1985
C-  P. Grudberg  3-OCT-1989  Change resistance parameters
C-  Modified 12-NOV-1989 P. Grudberg - rearrange arguments
C-----------------------------------------------------------------------
      REAL VPLHT, ZPLUS, ZMINUS, VHTPLU, VHTMIN
      REAL RPERCM, RIN, GPLUS, GMINUS
      DATA RPERCM/18./   ! wire resistance/cm (ohms/cm)
      DATA RIN/400./     ! preamp input impedance (ohms)
C-----------------------------------------------------------------------
C
C **** Admittances for each direction
C
      GPLUS = 1. / (RIN + ZPLUS*RPERCM)
      GMINUS = 1. / (RIN + ZMINUS*RPERCM)
C
C **** Pulse heights at each end
C
      VHTPLU = VPLHT * GPLUS / (GPLUS + GMINUS)
      VHTMIN = VPLHT * GMINUS / (GPLUS + GMINUS)
C
  999 RETURN
      END

