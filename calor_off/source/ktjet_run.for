C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_RUN.FOR
C *1     2-FEB-1994 16:58:53 ASTUR "CATANI ALGORITHM"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_RUN.FOR
      FUNCTION KTJET_RUN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Run the KT jet finding algorithm
C-        1) Construct zebra banks to hold 4 vectors of objects to be
C-      clustered.
C-        2) Precluster based on angular separation to mitigate hadronization
C-      and showering effects.
C-        3) Apply "Kt" algorithm for all other y values.
C-
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-JAN-1993   Richard V. Astur
C-   Updated   3-NOV-1995   Dhiman Chakraborty   
C-                          Comment out CALL BKCAPH(LCAPH) for D0FIX
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      INCLUDE 'D0$INC:zebcom.INC'
      CHARACTER*4 PATH
      INTEGER LCAPH
      INTEGER I, ITIME, IALGO
      SAVE    IALGO
      LOGICAL KTJET_RUN, REUSE_BANKS
      LOGICAL KTJET_BOOK_UTIL       ! Book utility banks
      LOGICAL KTJET_DROP_UTIL, KILL_BEAM
      LOGICAL KTJET_SELECT
C----------------------------------------------------------------------
      KTJET_RUN = .TRUE.
C
C: We are going to make JETS banks so make sure we are set up under
C: the RECO path.
C
      CALL PATHGT(PATH)
      CALL PATHST('RECO')
      CALL MKPATH
C      CALL BKCAPH(LCAPH)
      CALL PATHST(PATH)
C
C: Initialize zebra link area
C
      CALL MZLINT(IXCOM,'/KTJET/',DUMM,LLINK(NNLNK),DUMM)
C
C: Loop over all the algorithms we are supposed to do
C
      REUSE_BANKS = .FALSE.
      DO IALGO = 1, N_KTALG
        D_SEP     = D_SEP_KT( IALGO )
        F_CUT     = F_CUT_KT( IALGO )
        INPUT_TYPE= INPUT_TYPE_KT( IALGO )
C
C: Decide whether to try this event or not
C
        IF ( KTJET_SELECT() ) THEN
          IF ( .NOT. REUSE_BANKS ) THEN
C
C- First book the KT utility banks: KMAP and KVEC
C
            IF ( .NOT.  KTJET_BOOK_UTIL() ) GOTO 900

C
C- Fill KMAP and KVEC with whatever input objects we are using
C
            CALL KTJET_FILL_ARRAY

C
C- Precluster in lab frame before running real algorithm. This is
c  now obsoleted by Brad's new clustering code.
C
            KCF_SEE = 1
c            IF ( DO_PRECLUSTER) CALL KTJET_PRECLUSTER
            KCF_SEE = 2
C
C- Compress KVEC bank to weed out those cells that have been clustered
C- Leave the beam jets intact: KILL_BEAM = .FALSE.
C- 5/25/90 Put more pressure on the pre-clustering. Kill beam jets here
C- and dont do the usual initial y cut clustering
C
            KILL_BEAM = .TRUE.
            CALL KTJET_COMPRESS( KILL_BEAM )
C
C: Fill and test the KTCL bank
C
            IF ( MAKE_KTCL ) THEN
               CALL KTCLFL
               CALL KTCL_TEST
            ENDIF

C
C: Make a copy of our zebra banks in case we redo with a similar
C: algorithm and then can skip the usual filling and preclustering.
C:
C: If a copy already exists, than we are suppose to drop the old stuff
C: and use it instead.
c:
c: We don't need to do this copy operation if we aren't actually
c: making the jets.
C
            if (kt_make_jets) then
               CALL MZCOPY(IXMAIN,LKVEC,IXMAIN,KKVEC,2,' ')
               CALL MZCOPY(IXMAIN,LKMAP,IXMAIN,KKMAP,2,' ')
            endif

         ELSE
C
C: We can use what we had before
C
            CALL MZCOPY(IXMAIN,KKVEC,IXMAIN,LKVEC,2,' ')
            CALL MZCOPY(IXMAIN,KKMAP,IXMAIN,LKMAP,2,' ')

         ENDIF
C
C- Now run algorithm and make CAPH for each algorithm
C
         if (kt_make_jets) then
            CALL KTJET_CLUSTER  ! Find jets
            IF ( IPRINT .GT. 2 ) THEN
               IF (INPUT_TYPE .EQ. 1 ) CALL KTJET_CONE_DUMP
               CALL KTJET_DUMP
            ENDIF
C
C: Can we run again without refilling arrays and doing preclustering again
C
            REUSE_BANKS = .FALSE.
            IF ( IALGO .LT. N_KTALG ) THEN
               IF ( INPUT_TYPE_KT(IALGO) .EQ. INPUT_TYPE_KT(IALGO+1) )
     &              REUSE_BANKS = .TRUE.
            ENDIF
         endif
C
C- Drop KMAP and KVEC banks (and saved copies if we arent reusing)
C
         CALL KTJET_CLUSTER_DONE
         IF ( .NOT. KTJET_DROP_UTIL(REUSE_BANKS) ) GOTO 900
            
      ENDIF
      ENDDO

C
C: Done with jet making, run CAJETS_ADDON
C
      if ( N_KTALG .eq. 1 .and. input_type_kt(1) .eq. 3 ) then
        call cajets_addon
      endif


      KTJET_RUN = .TRUE.
  900 DUMM(1) = 0
      RETURN
      END
