      FUNCTION L2JETS_HOTFL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 1) Unpack hot tower candidates from TRGR and
C-                            put information in common block /L2JETS_HOT/
C-                         2) Set TRGR link
C-WARNING WARNING this whole routine depends on the VAX byte order of the
C-trigger block!!
C-
C-
C-   Returned value  : .TRUE. is successful. .FALSE. on any error
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-MAY-1990   Richard V. Astur
C-   Modified 30-APR-1991   R. Astur " Get Hot links from params file now"
C-   Modified  7-JUN-1992   R. Astur " Add jet list builder call "
C-   Modified 30-JAN-1992   R. Astur " Add ENTRY point to force list building"
C-   Modified 22-AUG-1993   R. Astur " Delete forcing of list building routine
C-        logic, as it will always be called now. Add large tile and l1.5
C-        list building.
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2JETS_HOTFL
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! zebra common
      INCLUDE 'D0$PARAMS:L2J_TRGRUNP.PARAMS'    ! TRGR parameters
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'  ! L1 indices
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS' ! JETS algorithm parameters
      INCLUDE 'D0$INC:L2JETS_CONT.INC'  ! Control common block
      INCLUDE 'D0$INC:L2JETS_CHARS.INC' ! L2JETS character strings
      INCLUDE 'D0$INC:L2JETS_HOT.INC'   ! FILL THIS COMMON
      INTEGER ICAND
      LOGICAL FIRST
      INTEGER NJET(2),I
      INTEGER GZTRGR, GZFIND_CRATE, LTRGR
      INTEGER ie, ip, ieave, ipave, l2j_get_pt_to_l1fadc, ioff
      REAL em, tt, etot, emtot
      LOGICAL COMP(2), ok
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C: Initialization of last event variable
C
      IF ( FIRST ) THEN
        FIRST    = .FALSE.
        IEVT_HOT = -9000
      ENDIF
      LTRGR = GZFIND_CRATE('TRGR', GZTRGR(), 11) ! Define link once
      LTRGR = LTRGR - 1
      NOWERRID     = 'L2JETS_HOTFL'     ! Name of this routine
      L2JETS_HOTFL = .TRUE.             ! Initialize as true

C---We only have to run this routine if we have not done it before.
      IF (IEVT_HOT .EQ. IQ(LHEAD+7)) GOTO 900

C---Set # of jets to 0 in case we cannot access TRGR.
      NJTHOT = 0
      NEMHOT = 0
      NJTTHOT= 0
      NJLTHOT= 0
      NJ15HOT= 0
C---Check TRGR link
      IF ( LTRGR .LE. 0) THEN         ! No TRGR bank!!!
        LTRGR = LTRGR + 1             ! Because I subtracted 1 from it above
        WRITE( NOWERRMESS, 1001) LTRGR
 1001   FORMAT(' GZFIND_CRATE returns ',I8)
        NOWSMESS   = ' No TRGR'
        NOWERR = ERR_NO_TRGR                     ! Severe error. Cannot recover.
        ERR_FLAG = 'E'
        GOTO 800
      END IF



C       ****** START UNPACKING HOT TOWER LISTS ********
C---Let's unpack the hot tower lists. DO two loops: one for EM one for TOTAL
C

C: First unpack Trigger tower candidates - both EM and total
      
      DO I = 1,2                        ! 1=EM 2=TOTAL
C
C---8/21/93 Call L1 List builder all the time for run 1B
C
        IF (I .EQ. 1) THEN
          CALL L1UTIL_JET_LIST_BUILDER( LTRGR+1, I-1, NLIST_MAX,
     &        NJET(I), IHOT_EM(1))
        ELSE
          CALL L1UTIL_JET_LIST_BUILDER( LTRGR+1, I-1, NJTT_MAX,
     &        NJET(I), IHOT_JT(1))
        END IF
C
C---Check to see that the list is complete. If it is not, bit 15 (msb of 2nd
C---byte) will be set.
C
        IF (NJET(I) .GT. NLIST_MAX) THEN
          COMP(I) = .FALSE.
          NJET(I) = NLIST_MAX
          IF ( I .EQ. 2 ) NJET(I) = NJTT_MAX
          CALL ERRMSG('list overflow','L2JETS_HOTFL',
     &        ' Jets list is incomplete: assume maximum candidates ',
     &        'I')
        END IF
        
      ENDDO
C
C---Finish up
C
      NEMHOT = NJET(1)
      NJTTHOT = NJET(2)
      EM_COMP = .FALSE.
      JT_COMP = .FALSE.
      IF (COMP(1) )  EM_COMP = .TRUE.
      IF (COMP(2) ) JT_COMP =  .TRUE.
C
C--- Find Large Tile objects
C
      CALL L1UTIL_JET_LIST_BUILDER( LTRGR+1, 2, NJLT_MAX,
     &        NJLTHOT, IHOT_JT( 2*NJTTHOT + 1 ))
      IF (NJLTHOT .GT. NJLT_MAX) THEN
        NJLTHOT = NJLT_MAX
        CALL ERRMSG('LT list overflow','L2JETS_HOTFL',
     &    ' Large Tile list is incomplete: assume maximum candidates ',
     &    'I')
      END IF
C
C: Loop over large tiles and give them a 'trigger tower seed' which is
C: the average over the entire large tile
      DO I = 1, NJLTHOT
        CALL CDBITT( IHOT_JT(2*(I+NJTTHOT)), IE, IP, OK )
        IF ( OK ) THEN
          IF ( IE .GT. 0 ) THEN
            IE = IE + 20
            CALL l2j_etaphi_ave( IE, IP, 0,  0, NTTLTETA-1,
     &        NTTLTPHI-1,
     &        Q( L2J_GET_PT_TO_L1FADC() ),
     &        IE, IP, EMTOT, ETOT )
            IE = IE - 20
          ELSE
            IE = IE + 21
            CALL l2j_etaphi_ave( IE, IP, 1-NTTLTETA,  0, 0,
     &        NTTLTPHI-1,
     &        Q( L2J_GET_PT_TO_L1FADC() ),
     &        IE, IP, EMTOT, ETOT )
            IE = IE - 21
          ENDIF
          CALL CTTDBI( IE, IP, IHOT_JT(2*(I+NJTTHOT)) )
        ENDIF
      ENDDO
C
C--- Get L1.5 jet objects
C
      NJ15HOT = 0
C
C--- Sum up all jet objects
C
      NJTHOT = NJ15HOT + NJLTHOT + NJTTHOT
C
C--- debug
C
C      do i = 1, njtthot
C        ioff = 0
C        call cdbitt( ihot_jt(2*i), ie, ip, ok )
C        call cl1phet( ip, ie, em, tt )
C        if ( ie .gt. 0 ) then
C          ie = ie + 20
C        else
C          ie = 21 + ie
C        endif
C      call l2j_etaphi_ave( ie, ip, ioff, ioff,
C     &  iq(l2j_get_pt_to_l1fadc() ), ieave, ipave, emtot, etot )
C      type *, em, emtot, tt, etot, ieave, ipave
C      enddo
C
C---Flag event number so we dont do this again this event.
C
      IEVT_HOT = IQ(LHEAD+7)                 ! Update event number


      GOTO 900
  800 CONTINUE    !   *** ERROR CONDITION ***

      L2JETS_HOTFL = .FALSE.            ! error
      CALL ERRMSG('L2JETS_HOTFL',NOWSMESS, NOWERRMESS,'E')
      NOWERR       = ERR_UTILITY         ! Error has been issued already
  900 CONTINUE    !   *** NORMAL COMPLETION ****
  999 RETURN

C
C: Entry points to return MASK and ADDRESS of jet list candidates
C
      ENTRY IHOT_MSK_JT( ICAND )
      IHOT_MSK_JT = IHOT_JT( 2*ICAND-1 )
      RETURN

      ENTRY IHOT_ADR_JT( ICAND )
      IHOT_ADR_JT = IHOT_JT( 2*ICAND )
      RETURN

      ENTRY IHOT_MSK_EM( ICAND )
      IHOT_MSK_EM = IHOT_EM( 2*ICAND-1 )
      RETURN

      ENTRY IHOT_ADR_EM( ICAND )
      IHOT_ADR_EM = IHOT_EM( 2*ICAND )
      RETURN

      END
