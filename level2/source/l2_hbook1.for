      SUBROUTINE L2_HBOOK1(ID,TITLE,NBIN,LLIM,ULIM,VMX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book One dimensional Histogram.
C-
C-   Inputs  : ID = Histogram identifier. Non-zero      (I)
C-             TITLE = Histogram Title (not used currently) (A)
C-             NBIN  = Number of channels (I)
C-             LLIM  = Lower edge of first channel (F)
C-             ULIM  = Upper edge of last channel  (F)
C-             VMX   = 0. 32 bit words (use for Weighted//Real Histograms)
C-                     Non-zero = 16 bit Integers only.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  18-JAN-1990   Jan Guida, Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBHST.INC'
      INCLUDE 'D0$INC:L2CLBH.INC'
C
      INTEGER ID,NBIN,NIO(2),ND,IFL,IT,LTITL
      INTEGER ITITLE(20)
      REAL LLIM,ULIM,VMX
C
      CHARACTER*4 BANK(2),SHTITLE(20)
      CHARACTER*80 NEWTIT
      CHARACTER*(*) TITLE
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      DATA BANK /'CLID','CLRD'/
C
      EQUIVALENCE (ITITLE(1),SHTITLE(1),NEWTIT)
C----------------------------------------------------------------------
C
C  *** Check if initialization has been done. If not ABORT
C
      IF (IDVHST.EQ.0.OR.LCLBH.EQ.0) THEN
        CALL L2_HBERR('L2HBOOK1','/ZEBHST/ not initialized',ID)
        GO TO 999
      ENDIF
C
C  *** Check if ID is valid
C
      IF (ID.LE.0.OR.ID.GT.IH(LCLBH+3)) THEN
        CALL L2_HBERR('L2HBOOK1',
     &    'Attempt to book invalid ID, Aborted',ID)
        GO TO 999
      ENDIF
C
C  *** Form NIO of Bank, only on first call
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL MZFORM('CLID','5I -F',NIO(1))
        CALL MZFORM('CLRD','3I 5F -I',NIO(2))
      ENDIF
      IFL = 1
      IF (VMX.EQ.0.) IFL = 2
C
C  *** Check if Histogram has been booked. Abort if so.
C
      IF (LCHST(ID).NE.0) THEN
        CALL L2_HBERR('L2HBOOK1',
     &    'Attempt to rebook Histogram. Aborted',ID)
        GO TO 999
      ENDIF
C
C  *** Book Histogram bank
C

      IF (IFL.EQ.1) THEN
        ND = (NBIN+1)/2 + CHEAD                 ! data + header words
      ELSE
        ND = NBIN + CHEAD
      ENDIF
C
      CALL MZBOOK(IDVHST,LCHST(ID),LCLBH,-ID,BANK(IFL),1,1,
     &  ND,NIO(IFL),0)
C
      IF (TITLE.NE.' ') THEN
        NEWTIT = TITLE
        CALL MZBOOK(IDVHST,LTITL,LCHST(ID),0,'CTIT',1,1,20,2,0)
        DO IT=1,20
          IH(LTITL+IT) = ITITLE(IT)
        ENDDO
      ENDIF

      IH(LCLBH+4) = IH(LCLBH+4) + 1     ! # of Histograms
      IH(LCHST(ID)+1) = 0               ! Store Histogram Flag
      IH(LCHST(ID)+2) = NBIN            ! Number of channels
      IH(LCHST(ID)+3) = 0               ! TOTAL # of entries
      H(LCHST(ID)+4) = LLIM             ! Lower limit
      H(LCHST(ID)+5) = ULIM             ! Upper limit
      H(LCHST(ID)+6) = 0.               ! Weighted Valid entries
      H(LCHST(ID)+7) = 0.               ! Underflows - weighted
      H(LCHST(ID)+8) = 0.               ! Overflows - weighted
C
  999 RETURN
      END
