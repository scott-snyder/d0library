      SUBROUTINE MUTSWW(JQUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  reconstruct muon tracks in the overlap region
C-                          SAMUS-A, WAMUS-B, WAMUS-C
C-                          Uses results of WAMUS processing
C-
C-   Inputs  :              JQUAD  Quadrant number 17 is north, 18 is south
C-
C-   Outputs : 
C-   Controls: 
C-
C-   Created  13-DEC-1993   M. Fortner   from MUSANLZ
C-   Modified 10/94  MF  Add call to create SAHH,SAMH,SACL
C-  
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LMUOT,GZMUOT,ITRAK,NMUOT,NW,IFW1,IQUAD,JQUAD,IERR
      LOGICAL NORTH,SOUTH
C
C  Create working bank of SAMUS hits
C
      IQUAD = JQUAD - 16
      CALL SAHHFL(IQUAD,IERR)
      IF (IERR.NE.0) GO TO 999
C
C  Loop over WAMUS tracks
C
      LMUOT = GZMUOT(0)
      IF (LMUOT.EQ.0) GO TO 999
  10  CONTINUE
      NW = IQ(LMUOT+1)
      IQUAD = IQ(LMUOT+3)
      NORTH = IQUAD.GT.4.AND.IQUAD.LE.8.AND.JQUAD.EQ.17
      SOUTH = IQUAD.GT.8.AND.IQUAD.LE.12.AND.JQUAD.EQ.18
      IF (NW.GT.0.AND.(NORTH.OR.SOUTH)) THEN
          IFW1 = IQ(LMUOT+4)
          IF (IFW1.EQ.1.OR.IFW1.EQ.11) THEN     ! WAMUS track missing A layer
              ITRAK = IQ(LMUOT-5)
              CALL MUWTOS(ITRAK)                ! extrapolate it to SAMUS
              CALL MUIFW3(ITRAK)                ! Check for trigger tag
              CALL MUIFW4(ITRAK)                ! Redo IFW4 count
          END IF
      END IF
      LMUOT=LQ(LMUOT)
      IF(LMUOT.NE.0) GO TO 10
C
  999 RETURN
      END
