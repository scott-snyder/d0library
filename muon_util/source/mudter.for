      REAL FUNCTION MUDTER(IPOINT,IDELT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURNS error (cm) for a given deltaT
C-   hit in the MUOH bank
C-   Inputs  : IPOINT = which MUOH hit
C-             IDELT  = which deltaT solution 
C-   Controls: 
C-
C-   Created  26-AUG-1992   David Hedin
C-     extremely preliminary version
C-   Revised  14-SEP-1993   Paul Quintas
C-     include module dependence and wire position dependence
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IPOINT,IDELT
      INTEGER IWADD,IFW1,IFW2,INRAW,IORIEN,NHWIR
      REAL CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1
      REAL DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR
C
      INTEGER NMOD,NPLN,NWIR,IERR,JERR
      REAL E_DRIFT,E_NONDRIFT
C
	REAL WLEN, VECT(3), VOFF
	INTEGER IORIENT, IOR, ORIEN
C
C parameters from fitting dtres by eighths - Paul Quintas 10-6-93
C
	real x,factor
	real p1,p2,p3,p4,p5,p6,p7
	data p1,p2,p3,p4,p5,p6,p7
     x	/-.57620,36.641,-231.58,674.60,-1017.4,771.02,-232.15/
C----------------------------------------------------------------------
      MUDTER=99999.
      IF(IDELT.LE.0.OR.IDELT.GE.3) GO TO 999
C
      CALL GTMUOH(IPOINT,IWADD,IFW1,IFW2,INRAW,IORIEN,NHWIR,
     &  CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,
     &  DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR)
C
      CALL MUADD(IWADD,NMOD,NPLN,NWIR,IERR)
      IF(IERR.EQ.0) THEN
        CALL MU_MOD_ERRORS(NMOD,E_DRIFT,E_NONDRIFT,JERR)
        IF(JERR.EQ.0) THEN
          MUDTER = MIN(MAX(E_NONDRIFT,10.),40.)
C
C CALCULATE CORRECTION FACTOR BASED ON POSITION WITHIN CHAMBER
C MIN VALUE CUT OFF AT 0.9 WHICH OCCURS WITHIN 0.06 OF X=0 AND X=1
C MAX VALUE IS ABOUT 1.53 AT X=0.18
C
          CALL MUGEOM(NMOD,NPLN,NWIR,VECT,WLEN,VOFF,IORIENT)
	  ORIEN = IABS(IORIENT)
	  IOR = IORIENT/ORIEN
	  IF (IDELT.EQ.1) THEN
		X = TDIV1*IOR/WLEN + 0.5
	  ELSE
		X = TDIV2*IOR/WLEN + 0.5
	  ENDIF
	  X = MAX(0.,MIN(1.,X))
C
C	  FACTOR = P1+P2*X+P3*X**2+P4*X**3+P5*X**4+P6*X**5+P7*X**6
	  FACTOR = P1+X*(P2+X*(P3+X*(P4+X*(P5+X*(P6+X*P7)))))
	  FACTOR = MAX(0.9,FACTOR)
	  MUDTER = FACTOR * MUDTER
	
        ENDIF
      ENDIF  
C----------------------------------------------------------------------
  999 RETURN
      END
