      SUBROUTINE UDST_OUT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : write uDST to output file
C-
C-   Created   6-APR-1993   Balamurali V.
C-   Updated  26-JAN-1994   Ulrich Heintz  add QOUT,QDROP switches 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZRECO.LINK'
      INCLUDE 'D0$LINKS:IZFILT.LINK'
      INCLUDE 'D0$LINKS:IZHSUM.LINK'
      INCLUDE 'D0$LINKS:IZESUM.LINK'
      INTEGER IUHEAD,OUNIT,ILEN,IER,NID1,NID,NIDMAX,IHEAD,LENGTH,LRECO
      INTEGER LFILT,LESUM,LHSUM
      PARAMETER     (NID1=2,NIDMAX=100)
      LOGICAL       OK,FIRST,QOPEN,QOUT,QDROP
      DATA          FIRST/.TRUE./
      CHARACTER*2  XMODE
      CHARACTER*80 OFNAME
      CHARACTER*4   CIDLIST2(NIDMAX),CHOPT
      INTEGER       IDLIST1(NID1),IDLIST2(NIDMAX)
      EQUIVALENCE ( IDLIST2, CIDLIST2 )
      DATA          IDLIST1 /'ANLS','UTAG'/
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL EZPICK('UDST_RCP')
        CALL EZGET('UDST_OUT',QOUT,IER)
        IF(QOUT)THEN
          IF(IER.EQ.0)CALL EZGETS('OUT_FILE',1,OFNAME,LENGTH,IER)
          IF(IER.EQ.0)CALL EZGETS('OUT_FILE',2,XMODE,LENGTH,IER)
          IF(IER.EQ.0)CALL EZ_GET_CHARS('LIST_OF_BANKS',NID,CIDLIST2,
     &      IER)
          IF(NID.GT.NIDMAX)CALL ERRMSG('NID>NIDMAX','UDST_OUT',
     &      'number of banks in list exceeds array dimension','F')
        ELSE
          IER=0 ! if UDST_OUT wasn't found assume it .FALSE. and reset error
        ENDIF
        IF(IER.EQ.0)CALL EZGET('DROP_RECO',QDROP,IER)
        IF(IER.NE.0)CALL ERRMSG('UDST_RCP','MAKE_UDST',
     &    'error getting RCP parameters','F')
        CALL EZRSET
      ENDIF
      IF(QOUT)THEN    ! if output through this routine was selected
        IF(.NOT.QOPEN)THEN
C
C... open uDST output file
C
          XMODE = XMODE(1:LENGTH)
          CHOPT = 'OU'
          IF(XMODE(1:1).EQ.'X') CHOPT='XO'
          CALL GTUNIT(88,OUNIT,IER)
          CALL D0OPEN(OUNIT,OFNAME,CHOPT,OK)
          CALL XZRECL(ILEN,CHOPT)
          CALL FZFILE(OUNIT,ILEN,CHOPT)
          QOPEN=.TRUE.
C
C... write begin-run-record with UTAG bank
C
          IHEAD=IQ(LHEAD+1)
          IQ(LHEAD+1)=1
          CALL MZMARK(IXMAIN,LHEAD,' ',NID1,IDLIST1)
          CALL FZOUT(OUNIT,IXMAIN,LHEAD,1,'M',2,0,IUHEAD)
          IQ(LHEAD+1)=IHEAD
        ENDIF
C
C... write event record
C
        CALL MZMARK(IXMAIN,LHEAD,' ',NID,IDLIST2)
        CALL FZOUT(OUNIT,IXMAIN,LHEAD,1,'M',2,0,IUHEAD)
      ENDIF
      IF(QDROP)THEN
          LRECO=LQ(LHEAD-IZRECO)
          CALL MZDROP(IXCOM,LRECO,'L')
          LFILT=LQ(LHEAD-IZFILT)
          CALL MZDROP(IXCOM,LFILT,'L')
          LHSUM=LQ(LHEAD-IZHSUM)
          LESUM=LQ(LHSUM-IZESUM)
          CALL MZDROP(IXCOM,LESUM,'L')
      ENDIF
      GOTO 999
C
      ENTRY UDST_OUT_CLOSE
C
      IF(QOPEN)THEN
        CALL FZENDO(OUNIT,'T')
        CALL RLUNIT(88,OUNIT,IER)
      ENDIF
C
  999 RETURN
      END
