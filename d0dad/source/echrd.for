      SUBROUTINE ECHRD(ILUN,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Read the header from a d0dad event catalog.
C-
C-   INPUTS  : 
C-   OUTPUTS : 
C-   CONTROLS: 
C-
C-   CREATED   8-NOV-1993   John D Hobbs
C-   MODIFIED 21-DEC-1994   JDH - Add fixes to allow dynamic header
C-      version updating (Start w/d0dad version 104)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IERR,NRUNS,VMAJOR,VMINOR
      INTEGER ITEMP(KECHED),NW,NHSIZE,IRECEC,IECHED,IRPREC
      CHARACTER*8 CSTR
C----------------------------------------------------------------------
C
C  Read start of file in order to avoid needing defaults.  All config
C  parameters are read from the file...  Put this in the record buffer
C
      READ(ILUN,REC=1,ERR=996) ITEMP
      IF( ITEMP(JEC_BSWAP).NE.TESTBSWAP) THEN
        CALL VXINVB(ITEMP(3),KECHED-2)
        IF( ITEMP(JEC_BSWAP).NE.TESTBSWAP) GOTO 993
        IQ(LECHD+JECBS)=1
      ENDIF
      NW=ITEMP(JEC_IRPREC)*ITEMP(JEC_IRECEC)
      CALL MZBOOK(IXDDAD,LPREC,LECHD,-LLPREC,'PAGE',0,0,NW,2,0)
      IF( LPREC.LE.0 ) GOTO 995
      NHSIZE=MAX(KECHED,ITEMP(JEC_IECHED))+NDEC
      IF( NHSIZE.GT.IQ(LECHD-1) ) THEN
        NHSIZE=NHSIZE-IQ(LECHD-1)
        CALL MZPUSH(IXDDAD,LECHD,0,NHSIZE,'I')
      ENDIF
C
C  Read the header as packed in echwrt...
C
      CALL ECRRD(ILUN,1,IQ(LPREC+1),IQ(LPREC-1),0,IERR)
      IF( IQ(LECHD+JECBS).NE.0 ) THEN
        CALL VXINVB(IQ(LPREC+1),KECHED)    ! Byteswap header 
        CALL VXINVB(IQ(LPREC+1),2)         ! Undo swap of 'D0DAD EC' field
        CALL VXINVB(IQ(LPREC+7),5)         !   and tag field
      ENDIF
      IF( IERR.NE.0 ) GOTO 996
      CALL UCOPY(IQ(LPREC+1),IQ(LECHD+NDEC+1),ITEMP(JEC_IECHED))
C
      CALL UHTOC(IQ(LECHD+NDEC+JEC_CECTAG),4,CECTAG,20)
      CALL D0DAD_CPAD(CECTAG)
C
C  Allocate run section and page array and read run section...
C
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      IRPREC=IQ(LECHD+NDEC+JEC_IRPREC)
      NRUNS=IQ(LECHD+NDEC+JEC_IRUN1)-IQ(LECHD+NDEC+JEC_IRUN0)+1
      NRUNS=IRPREC*NRUNS
      NW=IRECEC*NRUNS
      CALL MZBOOK(IXDDAD,LRUNS,LECHD,-LLRUNS,'RUNS',0,0,NW,2,0)
      IF( LRUNS.LE.0 ) GOTO 997
      CALL ECLOAD(IQ(LRUNS+1),2,NRUNS,IERR)
      IF( IERR.NE.0 ) GOTO 994
C
C  Update header if it's too short.  This implies that header changes
C  are extensions only, and the the NULL (and NOT IMPLEMENTED) value is
C  zero. If not, the default must be put in here...
C
C  If this code is properly done, new versions of the catalog can be 
C  self updating from older versions.  Modifications to the run and page
C  (NYI) sections can also be made at this point in a trivial manner...
C
C
      IF( IQ(LECHD+NDEC+JEC_IECHED).LT.KECHED ) THEN
        IQ(LECHD+NDEC+JEC_IECHED)=KECHED
        VMAJOR=IQ(LECHD+NDEC+JEC_RECVER)/100
        VMINOR=MOD(IQ(LECHD+NDEC+JEC_RECVER),100)

C  Coversion to V1.04, add JEC_EXTRA to header.

        IF( VMAJOR.EQ.1 .AND. VMINOR.LE.3 ) THEN
          IQ(LECHD+NDEC+JEC_EXTRA)=0
          IQ(LECHD+NDEC+JEC_RECVER)=104
          VMAJOR=1
          VMINOR=4
        ENDIF

      ENDIF
C
C
C  Check for corrupted/dirty catalog...
C
      IF( IQ(LECHD+NDEC+JEC_ISDIRT).NE.0 ) GOTO 998
C
 999  CONTINUE
      IERR=0
      RETURN
C
 993  CONTINUE
      IERR = -6
      RETURN
C
 994  CONTINUE
      IERR = -5
      RETURN
C
 995  CONTINUE
      IERR = -1
      RETURN
C
 996  CONTINUE
      IERR = -2
      RETURN
C
 997  CONTINUE
      IERR = -3
      RETURN
C
 998  CONTINUE
      IERR = -4
      IF( LDDBG.GT.0 ) WRITE(*,1001 ) IQ(LECHD+NDEC+JEC_ISDIRT)
 1001 FORMAT(' ECHRD: Fatal Error (',I8,') Corrupted event catalog')
      RETURN
      END
