      SUBROUTINE MUOTWS(LMUOT,PAR,CHISQ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Update the contents of MUOT bank 
C-   refitted in MUFITWS
C-
C-   Inputs  : LMUOT: track bank location
C-             PAR(1:4) fit parameters (see MUFITWS)
C-             CHISQ: chi_squared for the 3D fit
C-
C-   Outputs : updated MUOT bank
C-   Controls: 
C-
C-   Created  11-JUL-1991   Daria Zieminska
C-   Updated  28-JAN-1992   Daria Zieminska  call MUPQCK 
C-   Updated  18-FEB-1992   Daria Zieminska  fix ZA 
C-   Updated   2-JUN-1992   Daria Zieminska  add argument BDL in MUPQCK 
C-   12/93 Hedin. donot update chisq word. leave as wamus BC
C-   Updated  21-NOV-1994   Daria Zieminska  don't update MUOT if PAR unphysical
C     DH 3/95 MUOT delp word no longer defined as such
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LMUOT,QUAD
      REAL PAR(4),CHISQ,CONST,ZA,P,DP,EC,EF,BDL
      INCLUDE 'D0$INC:ZEBCOM.INC'
      IF (ABS(PAR(3)).LT.0.01.AND.ABS(PAR(4)).LT.0.01) THEN
        GO TO 999
      END IF
      CONST = SQRT(1.+PAR(3)**2+PAR(4)**2)
      ZA=430.-ABS(Q(LMUOT+13))
      IF (Q(LMUOT+13).LT.0.) THEN
        CONST=-CONST
        ZA=-ZA
      END IF
      Q(LMUOT+8) = PAR(1)+PAR(3)*ZA ! coordinates at A layer 
      Q(LMUOT+9) = PAR(2)+PAR(4)*ZA
      Q(LMUOT+10)=SIGN(430.,Q(LMUOT+13))
      Q(LMUOT+14) = PAR(3)/CONST  ! direction cosines inside the magnet
      Q(LMUOT+15) = PAR(4)/CONST
      Q(LMUOT+16) = 1./CONST
C      Q(LMUOT+20) = CHISQ 
      CALL MUPQCK(IQ(LMUOT+3),Q(LMUOT+8),Q(LMUOT+9),Q(LMUOT+10),
     1            Q(LMUOT+11),Q(LMUOT+12),Q(LMUOT+13),
     2            Q(LMUOT+14),Q(LMUOT+15),Q(LMUOT+16),
     3            Q(LMUOT+17),Q(LMUOT+18),Q(LMUOT+19),
     4            P,DP,EC,EF,BDL)
      QUAD=IQ(LMUOT+3)
      Q(LMUOT+22)=BDL
      Q(LMUOT+23)=P
      Q(LMUOT+25)=EC
      Q(LMUOT+26)=EF
  999 RETURN
      END
