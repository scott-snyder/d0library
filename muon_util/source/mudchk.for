      SUBROUTINE MUDCHK(LCRATE,LNEXT,HCRID,IVRUN,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Diagnostic program for MUON DAQ
C-                         Finds crate header and trailer
C-                         Only checks one crate at a time
C-
C-   Inputs  : LCRATE - Pointer to a crate in MUD1
C-
C-   Outputs : LNEXT - Location of next crate (0 if end)
C-             ICONT - Crate controller word
C-             IVERS - Version word
C-             IERR - Non zero if an error was encountered
C-
C-   Created   9/91   M. Fortner
C-
C-   10/21/91 Fixed LTRAIL pointer J. Butler
C-    DH 11/91 skip out for VERS=1 (old Monte Carlo)
C-    DH 12/91 if squeezed, skip out
C-    MF 8/93  Add header output and use Run 1B tools
C-    DW 7/94  Relax header length requirement for squeezed data
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCRATE,LTRAIL,LNEXT,IERR
      INTEGER NHEAD,NDATA
      INTEGER IVERS,ICONT,IVRUN
      INTEGER HCOUNT,TCOUNT,HCRID,TCRID,HTRIG,TTRIG
      INTEGER IUS2,IUS3,IUS4,IUS5,IUS6,IUS7,IUS8
      LOGICAL IERRI,IERRT,IERRC,EVERR
      INTEGER ISQ
C----------------------------------------------------------------------
C               Set initial values
C----------------------------------------------------------------------
      IERR=0
      IERRC=.FALSE.
      IERRT=.FALSE.
      IERRI=.FALSE.
      EVERR=.FALSE.
C----------------------------------------------------------------------
C               Unpack crate header and skip nonstandard versions
C----------------------------------------------------------------------
      CALL GTMUD1(1,LCRATE,LTRAIL,ICONT,NHEAD,HTRIG,NDATA,IVERS
     &            ,IUS5,IUS6,IUS7,IUS8)
      IF (LTRAIL .LT. 0) THEN             ! No bank / No data
          IERR = LTRAIL
          GOTO 900
      ENDIF
      IF (LCRATE.EQ.0) THEN               ! Store version number
          CALL MUHTFL(10,IVERS,IUS2)
      ENDIF
      ISQ = IBITS(IVERS,21,1)
      IVRUN = IBITS(IVERS,20,1)
      IF(ISQ.EQ.0.AND.NHEAD.GT.96) THEN
          IERR = -3                       ! Protect against FANOUT
          GO TO 900                       ! mailbox shrapnel
      ELSE IF(ISQ.NE.0.AND.(NHEAD.GT.96.OR.NHEAD.LT.0)) THEN
          IERR = -3                       ! Protect against FANOUT
          GO TO 900                       ! mailbox shrapnel
      ENDIF
C----------------------------------------------------------------------
C               Unpack crate trailer and check with header
C----------------------------------------------------------------------
      CALL GTMUD1(5,LTRAIL,LNEXT,TCRID,TCOUNT,TTRIG,
     &            IUS3,IUS4,IUS5,IUS6,IUS7,IUS8)
C
      IF (HTRIG.NE.TTRIG) THEN
        IERRT=.TRUE.                    ! Sync word mismatch
        EVERR=.TRUE.
      END IF
C
      HCRID = IBITS(ICONT,24,8)
      IF (HCRID.NE.TCRID) THEN
        IERRI=.TRUE.                    ! Crate ID mismatch
        EVERR=.TRUE.
      END IF
C
      HCOUNT=NHEAD+NDATA+5
      IF (HCOUNT.NE.TCOUNT) THEN
        IERRC=.TRUE.                    ! Crate count mismatch
        EVERR=.TRUE.
      END IF
C----------------------------------------------------------------------
C               Output results
C----------------------------------------------------------------------
  900 IF (EVERR) THEN
        IERR=1
      END IF
  999 RETURN
      END
