      LOGICAL FUNCTION L2C2EM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create compressed C2EM bank from online
C-                         L2EM bank.  Called once per event in event loop.
C-                         For L2EM versions less than 3, certain elements of
C-                         the C2EM bank will be -1=INTEGER or -999999.0=FLOAT
C-                         since they did not exist in early L2EM versions.
C-                         Since it is possible that there are 128 L1 candidates
C-                         C2EM_ZLINKA common is 512 words just to be really safe
C-                         (no big deal right? its only 2k!)
C-
C-
C-           Original L2EM:
C-
C- (L1 cand.) *   1     2     3     1     3     1     2     3
C-                ------------------------------------------- ...
C-                |     |     |     |     |     |     |     |
C- (parset #)     1     1     1     2     2     3     3     3
C-
C- ------------>
C-
C-           C2EM repetition:
C-           (parset 1,2,3... represent non-repeated portions of L2EM here)
C-
C-  L1 cand.----> 3           2           1
C-                ------------------------- ...
C-   p |          |           |           |
C-   a |          - 1         - 1         - 1
C-   r |          |           |           |
C-   s |          - 2         - 3         - 2
C-   e |          |                       |
C-   t |          - 3                     - 3
C-     |
C-   # V
C-
C- See .ZEB files for detailed descriptions of L2EM and C2EM banks
C-
C-   Returned value  : .TRUE.
C-   Inputs  : None
C-   Outputs : C2EM bank
C-   Controls: None
C-
C-   Created  27-APR-1993   James T. McKinley
C-   Modified  6-JUN-1993   James T. McKinley - always return true, remove
C-                          call to ERRMAX, move push on FILT bank so that
C-                          it is not conditional on whether or not L2EM
C-                          banks exist (i.e. only one version of FILT)
C-   Modified 12-JAN-1994   James T. McKinley - Fix bug for number of
C-                          repetition banks exceeds booked bank size 
C-                          (TT ETA,PHI = 0), had a GT needed a GE.
C-                          Also split into 2 routines and renamed some
C-                          variables to make code easier for others to read.
C-                          Put bank filling into C2EMFL which is called by
C-                          L2C2EM and also uses link area which was put in
C-                          the include file C2EM_ZLINKA.INC along with some
C-                          parameters.
C-   Modified 10-MAY-1994   James T. McKinley - Clean up again, it seems
C-                          the wrong code got released and I can't find the
C-                          correct version.
C-   Modified 18-MAY-1994   James T. McKinley - Drop any C2EM banks that exist
C-                          if L2EM exists for rebuild so will work on data
C-                          with and without C2EM.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:C2EM_ZLINKA.INC'   !contains my reserved link area
      INTEGER NL1,L1PTR
      INTEGER LL2EM,LC2EM,C2EMFL,GZL2EM,GZC2EM
      INTEGER GZFILT,LFILT,UNUSED
      INTEGER TT(3,NL1MAX)      ! TT(1,*) = TT ETA, 
C                                 TT(2,*) = TT PHI,
C                                 TT(3,*) = PAR SET OFFSET, 
C                                 * = NUMBER OF L1 CANDIDATES
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      CHARACTER*1 CHOPT
      DATA CHOPT/'L'/
C----------------------------------------------------------------------
      L2C2EM = .TRUE.                     ! always true
      LC2EM = GZC2EM()
      LL2EM = GZL2EM()
      IF((LC2EM.GT.0).AND.(LL2EM.GT.0))THEN ! drop existing C2EM if there are
        CALL MZDROP(IXCOM,LC2EM,CHOPT)      ! L2EM from which to rebuild
      ENDIF
C
C initialize reserved link area, used in C2EMFL also
C
      IF(FIRST) THEN                      ! initialize my zebra link area
        CALL MZLINK(IXCOM,'/C2EM_ZLINKA/',C2EM_LSLINK,C2EM_LRLINK,
     &    C2EM_LRLINK(C2EM_NRLINK))
        FIRST=.FALSE.
      ENDIF
C
      LFILT = GZFILT()
      IF(LFILT.EQ.0)THEN
        CALL BKFILT(LFILT)                ! book FILT bank if not there
      ELSEIF(IQ(LFILT+1).LT.FILTVER)THEN  ! versions 5+ will have C2EM link
        CALL MZPUSH(IXMAIN,LFILT,1,0,' ') ! push to add link for C2EM (v<5)
        IQ(LFILT+1) = FILTVER             ! change version of existing bank
      ENDIF
      IF(LFILT.LE.0)THEN                  ! abort if no FILT bank
        CALL ERRMSG('L2C2EM','FILT BANK PROBLEM',
     &      'cannot make FILT bank!','W')
        GOTO 999
      ENDIF
C
      CALL VZERO(TT,3*NL1MAX)             ! initialize counters and indices
      NL1=0
C
      LL2EM = GZL2EM()
      IF(LL2EM.LE.0)THEN
        CALL ERRMSG('L2C2EM','NO L2EM BANK',
     &    'No L2EM bank on this event! Cannot make C2EM.','W')
        GOTO 999
      ENDIF
C
      DOWHILE (LL2EM.GT.0)                ! loop over all L2EM banks on event
C
C TT(1..3,*) and NL1 are changed only by C2EMFL in this loop
C LL2EM is only changed by L2C2EM in this loop
C
        C2EM_LRLINK(IL2EM) = LL2EM        ! save the link to L2EM
        CALL C2EMFL(LL2EM,TT,NL1)         ! copy L2EM info to C2EM banks
        LL2EM = LQ(LL2EM)                 ! get link to next L2EM bank
      ENDDO
C
C squeeze out unused repetition banks from all C2EM banks
C
      DO L1PTR = 1,NL1                    ! loop over all L1 candidates
        LC2EM = C2EM_LRLINK(L1PTR+1)      ! get link for this bank
        IQ(LC2EM+4) = TT(3,L1PTR)         ! number of repetition banks (p.s.)
        IF(TT(3,L1PTR).LT.NPMAX)THEN      ! did C2EMFL have to push?
          UNUSED = (NPMAX - TT(3,L1PTR))*NR   ! drop unused repetition banks
          CALL MZPUSH(IXMAIN,LC2EM,0,-UNUSED,' ')
        ENDIF
      ENDDO
C
  999 RETURN
      END
