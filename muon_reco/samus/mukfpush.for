      SUBROUTINE MUKFPUSH(LMUKF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Squeeze MUKF bank releasing unused fit segments
C-
C-   Inputs  :  LMUKF   -   Address of the bank
C-   Outputs :
C-   Controls:
C-
C-   Created   1-FEB-1995   Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZMUKF.LINK'
C----------------------------------------------------------------------
      INTEGER LMUKF,ISEG,NSEG,ILAST,NSQ,J
C----------------------------------------------------------------------
      IF(LMUKF.LE.0) GOTO 999
      IF(IQ(LMUKF+1).NE.1) GOTO 999   ! Check version
      NSEG = IQ(LMUKF+2)
      IF(NSEG.LE.0) GOTO 999          ! Check number of segments allocated
C<<
      J = LMUKF + 1 + MUKF_HDRLEN
      ILAST = 0
      DO ISEG = 1,NSEG
        IF( IQ(J).GT.6 .OR. IQ(J).LT.0 ) GOTO 999   ! Error
        IF( IQ(J).GT.0 ) ILAST = ISEG
        J = J + MUKF_SEGLEN
      END DO
      NSQ = MUKF_SEGLEN * (NSEG-ILAST)
      IF( NSQ.GT.0 ) THEN
        CALL MZPUSH(IXMAIN, LMUKF, 0, -NSQ, 'IR')
        IQ(LMUKF+2) = ILAST
      END IF
  999 RETURN
      END
