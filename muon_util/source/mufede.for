C VAX/DEC CMS REPLACEMENT HISTORY, Element MUFEDE.FOR
C *1     2-AUG-1991 02:30:16 ABACHI "A. KLATCHKO: dE/dX in Iron"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MUFEDE.FOR
C =====================================================================
	FUNCTION MUFEDE(POLD)
C =====================================================================
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Energy loss in iron
C-
C-   Inputs  : POLD- The momentum before the energy loss upgrade
C-   Outputs : dP/ds [GeV/cm]
C-   Controls: 
C-
C-   Created   2-APR-1991  Don Franks
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
	INTEGER I,J,ICE
	REAL MUFEDE
	REAL POLD,PEKA,FERHO,UNICO,DRAG,UMASS,GONG,SING
	REAL PEGS(6),VAL(4),COPES(4,5)
	PARAMETER (FERHO=.007874)		!  gm/cm^3 x Mev/GeV
	PARAMETER (UNICO=0.14296)		!  Avo's,Me,Zfe,Afe,etc
	PARAMETER (UMASS=.105658)
C
	DATA PEGS/0.5,7.,25.,110.,270.,700./
C
	DATA COPES/.0252347, 1.16639,   13.6750, -0.00663505,
     &		   .4679890, 0.373112,  13.6274, -0.397351,
     &             .5167550,-0.00542436,14.1285, -1.08664,
     &             .5340920, 0.167590,  13.1160,  3.55653,
     &             .5579500,-0.272439,  13.681,  10.2975/
C
C
	ICE=0
	DRAG=0.0
C
	DO 10 I=1,6
   10	IF (POLD .GT. PEGS(I)) ICE=ICE+1
C
	IF (ICE.GT.5) THEN
	  MUFEDE= -1.0
	  GO TO 6000
	END IF
C
	IF (ICE.EQ.0) THEN
	  MUFEDE= 100.
	  GO TO 6000
	END IF
C
	PEKA=POLD/10.0
C
	VAL(1)=PEKA
	VAL(2)=LOG(PEKA)
	VAL(3)=1.0       
	VAL(4)=1.0/PEKA
C 
	DO 20 J=1,4
   20 	DRAG=DRAG+COPES(J,ICE)*VAL(J)
C
   22	DRAG=DRAG*UNICO
	MUFEDE=DRAG*FERHO
C
	IF (ICE.LE.2) THEN			! beta < 1 to 4 sig figs
	  SING=POLD/UMASS			! beta gamma
	  GONG=SQRT(1.0+SING*SING)		! gamma
	  MUFEDE=MUFEDE*GONG/SING		! dp/ds=(dE/ds)(dp/dE)
	END IF					! dp/dE=(dp/dh)/(dE/dh)
C						    ... h=arctanh(beta).
C
 6000	RETURN
C
	END
