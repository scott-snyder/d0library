      SUBROUTINE CL2_LINK_INI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize link STP and data Link Areas
C-              for CL2 unpacking routines
C-
C-   Inputs  : /CL2_LINK/ and /CL2_STP_LINK/
C-   Outputs : Zebra knows about them
C-   Controls:
C-
C-   Created  30-APR-1991   James T. Linnemann
C-   Updated  27-JUL-1992   James T. Linnemann  add FIRST protection 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
      INCLUDE 'D0$INC:CL2_LINK.INC'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
C
C...tell ZEBRA about CL2's private link areas
C...assume both event and STP links are all structural
C
C...STP links set in CL2_STPLNK
        CALL MZLINK(IXSTP,'/CL2_STP_LINK/',CL2_CLINK(1),
     &          CL2_CLINK(NCL2_CLINK), CL2_CLINK(1) )
C
C...CAEP and CAD links set in CL2_TTOW_ETNOM
        CALL MZLINK(IXCOM,'/CL2_LINK/',CL2SLNK(1),CL2SLNK(CL2NSLNK),
     &    CL2SLNK(1) )
      ENDIF
  999 RETURN
      END
