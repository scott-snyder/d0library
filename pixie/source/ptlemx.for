      SUBROUTINE PTLEMX(ANODE,MXWIR,MXENRG,PARCOL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the wire with the maximum energy released 
C-             per layer and the energy in a TRD end view.
C-
C-   Input:  PARCOL - [C*3]  Color of the wire
C-
C-   Created   9-FEB-1989   LUPE ROSAS
C-   Updated  10-JAN-1990   Lupe Howell Implementing Color Table
C-   Updated  24-mar-1992   S. Hagopian add ANODE,MXWIR and MXENRG as arguments
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ANODE
      INTEGER MXWIR(3)
      REAL MXENRG(3)
      CHARACTER*(*) PARCOL
      CHARACTER*3 LAB1
      CHARACTER*8 LAB2 
      CHARACTER*4 LAB3
      CHARACTER*3 CI
      CHARACTER*6 CENG
      CHARACTER*4 CWIRE
      CHARACTER*28 MESS         ! Complete message to be display in labels
      CHARACTER*8 CX2
      CHARACTER*6 CX
      REAL XLAB,YLAB,ENG,PTGEGT
      INTEGER I,WIRE
C----------------------------------------------------------------------
      DATA XLAB /48./       ! X COORDINATE FOR LABELS
      DATA LAB1/'LAY'/
      DATA LAB2/'MAXENRG='/     
      DATA LAB3/'WIR '/
C----------------------------------------------------------------------
      CALL PXCOLR(PARCOL)
      YLAB=-43.5
      DO 400 I=1,3
        WIRE=MXWIR(I)
        ENG=MXENRG(I)
C-  Writing real and int var into char variables
        WRITE(CX,310)I
 310    FORMAT(I2)
        READ(CX,320)CI
 320    FORMAT(A2)
        WRITE(CX,325)WIRE
 325    FORMAT(I3)
        READ(CX,327)CWIRE
 327    FORMAT(A3)
        WRITE(CX2,330)ENG
 330    FORMAT(F8.0)
        READ(CX2,340)CENG
 340    FORMAT(A8)
        MESS=LAB1//CI//LAB2//CENG//LAB3//CWIRE 
        CALL JMOVE(XLAB,YLAB)
        CALL J1STRG(MESS)
        YLAB=YLAB-3.0
 400  CONTINUE
  999 RETURN
      END
