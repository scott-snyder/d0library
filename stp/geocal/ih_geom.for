C DEC/CMS REPLACEMENT HISTORY, Element IH_GEOM.FOR
C *2    30-DEC-1988 23:23:50 RAJA "GOT RID OF VPSP"
C *1     7-NOV-1988 23:06:15 RAJA "IH and MH"
C DEC/CMS REPLACEMENT HISTORY, Element IH_GEOM.FOR
      SUBROUTINE IH_GEOM(NAME,STRING,MOTHER,MATNO,RIN,ROUT,DELZ,ZCEN,
     &  IRTOFF,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Works out parameters for IH tubes in CGS
C-
C-   Inputs  : SRCP Survey parameters in CGS
C-             NAME = GEANT name of the module
C              STRING = \ARRAY name on SRCP output. OPTIONAL
C                       IF blank, name is manufactured
C-             MOTHER = Mother volume this hangs from
C-             MATNO = Material number
C-             RIN = Inner radius
C-             ROUT = Outer radius
C-             DELZ = Half width of module in Z direction.
C-             ZCEN = Z co-ordinate of center of module IN ITS MOTHER VOLUME
C-             IRTOFF = First Rotation matrix number
C-             IFL    =1 .NOOP
C-             IFL = 2 ADD mother volume to SRCP name
C-
C-   Outputs : SRCP GEANT parameters for EC IH module
C-   Controls: None
C-
C-   Created  25-OCT-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NPAR,IROT,ITYP,NSEG,NAME,MOTHER,MATNO,NLN,IRTOFF
      INTEGER IFL
      REAL CRACK,RIN,ROUT,ZCEN,DELZ,XX,YY,ZZ
      CHARACTER*(*) STRING
      CHARACTER*40 ARNAME
      CHARACTER*10 ARNAM
      DATA ARNAM/'\ARRAY EC_'/
      CHARACTER*14 ARNAM1
      CHARACTER*4 NAMEC
      CHARACTER*4 ACTION
C
      REAL PI,PAR(3)
      INTEGER ICOPY,ICPY2,KK,LNS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL GTSRCP('PI',PI,1)
      ENDIF
C
C
      PAR(1) = RIN
      PAR(2) = ROUT
      PAR(3) = DELZ
C
      NPAR = 3
      ICPY2=1
      IROT = IRTOFF
      XX = 0.0
      YY = 0.0
      ACTION = 'POS '
      LNS = LEN(STRING)
      CALL UHTOC(MOTHER,4,NAMEC,4)
      IF(STRING(1:LNS).EQ.' ')THEN
        IF(IFL.EQ.1)THEN
          CALL UHTOC(NAME,4,NAMEC,4)
          ARNAME = ARNAM//NAMEC
        ELSEIF(IFL.EQ.2)THEN
          ARNAM1 = ARNAM//NAMEC
          CALL UHTOC(NAME,4,NAMEC,4)
          ARNAME = ARNAM1//NAMEC
        ENDIF
      ELSE
        ARNAME = STRING(1:LNS)
      ENDIF
      WRITE(20,1)ARNAME,NAME,MATNO,
     &        MOTHER,ACTION,
     &        IROT,ICPY2,XX,YY,ZCEN,NPAR,
     &        (PAR(KK),KK=1,NPAR)
  100 CONTINUE
    1 FORMAT(A,/,
     &  2X,'''',A4,'''',5X,'''TUBE''',5X,I2,5X,
     &  '''',A4,'''',5X,'''',A4,'''',/,
     &  I7,2X,I5,3F10.4,2X,I5,/,
     &  2X,3F10.4,/,
     &  '\END')
      END
