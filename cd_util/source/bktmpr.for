      SUBROUTINE BkTmpr(idet, ltmpr)
C======================================================================
C
C   Purpose and Methods :  Book a standalone bank "TMPR" for my own devious
C                          purposes.
C
C-  Inputs : integer idet = detector id (1:VTX, 2:CDC, 3:FDC, 4:TRD)
C-  Output : integer ltmpr = pointer to TMPR bank
C-
C-  C. Klopfenstein June 1993
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER MPCDDN(5), ISETVN
      integer idet, dummy
      integer ltmpr
      character*4 cdet(4)
      data cdet / 'CDD1', 'CDD2', 'CDD3', 'CDD4' /
      DATA MPCDDN / 0, 0, 0, 10000, 1 /
C
C======================================================================
C
      CALL UCTOH( cdet(idet), MPCDDN, 4, 4 )
C
C ****  Book standalone bank. Note than bank name is CDDn.
C
C        CALL MZLIFT ( IXMAIN, Ltmpr, dummy, 2, MPCDDN, 3 )
        CALL MZLIFT ( 0, Ltmpr, dummy, 2, MPCDDN, 3 )
C
      IQ(Ltmpr) = ISETVN(IQ(Ltmpr),0)
  999 CONTINUE
      RETURN
      END
