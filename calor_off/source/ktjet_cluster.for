C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_CLUSTER_1.FOR
C *1     3-FEB-1994 14:35:51 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_CLUSTER_1.FOR
      SUBROUTINE KTJET_CLUSTER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Run the KT algorithm and use the IKUT set of
C-                         parameters
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: See parameters in KTJET.INC
C-
C-   Created   Kate C. Frame
C-   Modified  27-JUL-1992   Richard V. Astur
C-   Modified  12-JAN-1993   R. Astur "Change from arrays to zebra bank"
C-   Modified   3-FEB-1994   K. Frame "Modify to match Catani paper"
C-   Modified    4-MAY-1994   R. Astur "small changes"
C-   Modified  30-MAY-1994   R. Astur "Add KHIS bank to keep track of history"
C-   Modified  30-DEC-1994 R. Astur "Add SCALE=topnjets*c1 option"
C-   Modified  28-MAR-1995   R. Astur "speed up clustering"
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:KTJET.INC'            ! Parameter cuts
      INCLUDE 'D0$INC:KTJET_LINK.INC'
C      REAL  KT_ET_MIN                       ! Minimum ET for a  KTjet (low)
C      PARAMETER( KT_ET_MIN = 8.000 )
      INTEGER MAX_MASTERJETS
      PARAMETER( MAX_MASTERJETS = 20 )
c      INCLUDE 'D0$INC:MKL1.INC'             ! 4-vectors
      REAL KTETOT, YCUT
      INTEGER LDUM, IYM, JYM, KTP
      INTEGER NFIX, NCUT, I, J
      INTEGER NSCJETS
      INTEGER IOH_HIS
      SAVE    IOH_HIS, NFIX
      INTEGER IETMAX(MAX_MASTERJETS), NLOOP
      LOGICAL FIRST
      REAL TOPET(MAX_MASTERJETS)
      LOGICAL START
      INTEGER IJ9, POINT, IJ10
      REAL KTET
c      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C: Statement function
      KTET( IJ10 )    =   ( LKVEC + ( (IJ10)-1 )*IQ(LKVEC + 4 ) + 10 )
      POINT( IJ9 ) =   ( LKVEC + ( (IJ9)-1 )*IQ(LKVEC + 4 ) + 5 )
      KTP( IYM, JYM) = LKHIS + ( (JYM) - 1)*3   + ( IYM ) + 2
C----------------------------------------------------------------------
      LKMKL = 0                     ! Not used anymore
      NFIX = IQ( LKVEC + 3 )
      IF ( NFIX .LE. 0 ) THEN
        CALL ERRMSG('No 4vectors','KTJET_CLUSTER',
     &    ' No 4 vectors in this event?','W')
        GOTO 999
      ENDIF
C
C: Book the KHIS bank, 3 words per entry to keep track of merging history
C
        IF ( FIRST ) THEN
          FIRST = .FALSE.
          CALL MZFORM('KHIS', '2I -F', IOH_HIS)
        ENDIF
        CALL MZBOOK( IXMAIN, LKHIS, LDUM, 2, 'KHIS', 0, 0, 2+NFIX*3,
     &    IOH_HIS, 0 )
        IQ( LKHIS + 1 ) = 1               ! Version number
        IQ( LKHIS + 2 ) = NFIX            ! 3 words per cluster
C
C: Call klustering initialization routine. Pass it in the
C: variables it needs. ktclust expects to get zebra banks
C: starting at the word before the start of the iterated data.
C
        if (nfix .gt. 1400) then          ! May require too much memory
           call errmsg ('Too Many Preculsters', 'ktjet_cluster',
     $          'Program may quit without warning due to out of memory',
     $          'w')
        endif
        call ktclust_init( nfix, d_sep, iq(lkvec+4), iq(lkmap),
     &    iq(lkhis+2), ietdef, ietadef)
        call ktclust_fill()               ! fill internal arrays
C
C: Run clustering. It will fill/modify the KVEC and KHIS banks for us
C
        call ktclust_do()
C
C: Allow it to free the memory it allocated
C
        call ktclust_done()
C
C: Done with clustering. Now we look for jets that pass the fraction cut
C: (unless we are doing subjets)
C

C
C: Turn back 'ON'
C: all those final state jets that we turned off during the clustering
C: process
C
      NCUT  = NFIX
      DO I = 1, NCUT
 8003   format(' ',i7,i3,i5,i5,f10.2)
c        write(43,8003) iq(lhead+9),input_type, iq(ktp(1,i)),iq(ktp(2,
c     &    i)),
c     &    q(ktp(3,i))
        IF ( IQ( KTP(2,I) ) .EQ. -1 ) THEN
          J = IQ( KTP(1,I) )
          IQ( POINT(J) ) = ABS(IQ(POINT(J) ))        ! Activate it
        ENDIF
      ENDDO
C
C: Sort the jets by ET. Store the top ET results in TOPET, and the KEY
C: in IETMAX
C
      NSCJETS = 0                 ! Number of SCJETS above fraction cut
C                                 ! If doing SUBJETS, this is also the
C                                 ! number of Master jets
      START   = .TRUE.            ! Start sorting process
      DO I = 1, IQ( LKVEC + 3 )
        IF ( IQ( POINT(I) ) .GT. 0 ) THEN
          CALL MAKE_SORT_LIST( NSCJETS, TOPET, Q(KTET(I)),
     &      MAX_MASTERJETS,
     &      IETMAX, KT_ET_MIN, START ) ! All jets above .5 GeV
        ELSE
          CALL MAKE_SORT_LIST( NSCJETS, TOPET, KT_ET_MIN-1.,
     &      MAX_MASTERJETS, IETMAX, KT_ET_MIN, START )
        ENDIF
      ENDDO
C
C: Determine SCALE of the event. For example, IFSCALE=1 means use the leading
C: jet. IFSCALE=2 means add the top NSCALE jets and divide by 2.
C
      FSCALE  = -999.
      IF ( IFSCALE .EQ. 2 .AND. NSCALE .GT. MAX_MASTERJETS ) THEN
        CALL ERRMSG('NSCALE TOO BIG','KTJET_CLUSTER',
     &    'CHANGED TO MAX_MASTERJETS ',
     &    'W')
        NSCALE  = MAX_MASTERJETS
      ENDIF
      IF ( IFSCALE .LT. 1 .OR. IFSCALE .GT. 2 ) THEN
        CALL ERRMSG('IFSCALE INVALID','KTJET_CLUSTER','Use IFSCALE=2',
     &    'W')
        IFSCALE = 2
      ENDIF

      IF ( IFSCALE .EQ. 1 ) THEN
        FSCALE  = TOPET(1)        ! Leading jet ET
      ELSE
        FSCALE  = 0.0             ! 1/2 of top NSCALE jets
        DO I = 1, MIN( NSCALE, NSCJETS )
          FSCALE  = FSCALE + .5*TOPET(I)
        ENDDO
      ENDIF

C
C: Loop over all final state jets and turn off those that fail the fraction
C: cut. Change NSCJETS to reflect the number that pass.
C
      NSCJETS = 0
      DO I = 1, IQ( LKVEC + 3 )
        IF ( IQ(POINT(I)) .GT. 0 ) THEN
          IF ( Q( KTET(I) ) .LT. F_CUT * FSCALE .OR. Q(KTET(I)) .LT.
     &      KT_ET_MIN ) THEN
            IQ( POINT(I) ) = -IQ( POINT(I) )
          ELSE
            NSCJETS = NSCJETS + 1
          ENDIF
        ENDIF
      ENDDO

      IF ( NSCJETS .GE. MAX_MASTERJETS ) THEN
        CALL ERRMSG('Overflow',
     &    'KTJET_CLUSTER',' Possibly more master jets','W')
        IF ( DO_SUBJETS) NSCJETS = MAX_MASTERJETS
      ENDIF
C
C: If this is not a subjet analysis, store the results
C
      IF ( .NOT. DO_SUBJETS ) THEN
        CALL KTJET_STORE
        GOTO 999
      ELSE
C
C: Take the leading jets and look at their KT structure. Loop over
C: each jet individually.
C
        NLOOP = NJETS                   ! Number of leading jets we want
        NLOOP = MIN( NJETS, NSCJETS)    ! How many we have
        DO WHILE (NLOOP .GT. 0 )
          ISUBJET = NLOOP               ! Save number of jet
C
C: First deactivate all the jets except this one
C
          DO I = 1, IQ( LKVEC + 3)
            IF ( I .NE. IETMAX(NLOOP) ) IQ( POINT(I) ) = -ABS( IQ(
     &          POINT(I) ) )
          ENDDO
          IQ( POINT( IETMAX(NLOOP) ) ) = ABS( IQ( POINT( IETMAX(NLOOP)
     &        ) ) )       ! Activate this jet
C
C: Look at structure of this jet. Start with the highest resolution (ycut)
C: and work our way down, splitting up clusters as we do. Only split up
C: clusters of activated jets. (Our main jets and its products)
C
          NCUT    = 1                   ! Loop over merges
          KTETOT  = Q( KTET(IETMAX(NLOOP)) )
          DO IKTCUT =  N_KTCUTS+1, 1, -1  ! Loop over ycuts
            IF ( IKTCUT .EQ. N_KTCUTS+1 ) THEN
              YCUT  = D_SEP*KTETOT**2       ! Always store master jet
              KTCUT(N_KTCUTS+1) = 0.0
            ELSE
              YCUT    = KTCUT(IKTCUT) * KTETOT**2
            ENDIF
            IF ( NCUT .LE. IQ(LKVEC+3) ) THEN
              DO WHILE ( Q( KTP(3,NCUT) ) .GT. YCUT )
                IF (IQ(KTP(2,NCUT)) .GT. 0 .AND.
     &            IQ(POINT( IQ(KTP(1,NCUT)) ))
     &            .GT. 0 ) THEN
                  I = IQ(KTP(1,NCUT))
                  J = IQ(KTP(2,NCUT))
                  CALL KT_SPLIT_CELLS(I,J)
                ENDIF
                NCUT  = NCUT + 1
                IF ( NCUT .GT. IQ(LKVEC+3) ) GOTO 10
              ENDDO
   10         CONTINUE
            ENDIF
            IF ( DO_POSTCLUSTER) CALL KTJET_POSTCLUSTER    ! Do postclustering
            CALL KTJET_STORE          ! Store all jets now
            IF ( DO_POSTCLUSTER) CALL KTJET_UNDO_POSTCLUSTER ! Undo those
C                                                        ! effects
C                                                        !  so we can continue

          ENDDO
          NLOOP = NLOOP - 1             ! Do another jet
        ENDDO
      ENDIF
  999 RETURN

C
C: Done with clustering. Drop only the KHIS bank.
C: KMKL is done internally.
C
      ENTRY KTJET_CLUSTER_DONE
      CALL MZDROP( IXCOM, LKHIS, 'L')
      LKHIS = 0
      LKMKL = 0
      RETURN
      END
