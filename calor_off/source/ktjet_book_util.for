C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_BOOK_UTIL.FOR
C *1     3-FEB-1994 14:34:41 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_BOOK_UTIL.FOR
      FUNCTION KTJET_BOOK_UTIL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the utility banks (KVEC, KMAP ) for the
C-    Kt algorithm.  The banks hold information about whatever we are
C-    using to form jets (e.g. partons, particles, cells etc. )
C-
C-   ENTRY KTJET_DROP_UTIL : Drop KVEC and KMAP
C-
C-
C-   Created  12-JAN-1993   Richard V. Astur
c-   Updated   9-oct-1995   Modified to use new format KTCL banks
c-                          and utility access routines.
c-                          Gordon Watts
c-   Updated  24-oct-1995   Look for errors from kt_remove_low_et_cells
c-                          call.
c-                          Gordon Watts
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      LOGICAL KTJET_BOOK_UTIL, KTJET_DROP_UTIL
      INTEGER LISAE, GZISAE, LCAEH, GZCAEH, GZCAPH, LCAPH, LCAPH_MAX
      REAL KT_CUT_MAX, TEMPLATE(8)
      INTEGER NVECT, IOH_VEC, IOH_MAP, LDUM, LDUM1, LCATD
      INTEGER KTJET_FIND_CAPH, LISV1, LISP1, GZCATD
      INTEGER LKTCL
      SAVE IOH_VEC, IOH_MAP
      LOGICAL FIRST, REUSE_BANKS

      integer ier

      integer ktcl_get_ncluster
      integer ktcl_get_input_type

      integer gzktcl

      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      KTJET_BOOK_UTIL = .TRUE.          ! Assume we will find what we want
      NVECT           = 0               ! No vectors found yet
C
C: Make sure they are not booked already
C
      IF ( LKMAP .GT. 0 .OR. LKVEC .GT. 0 ) THEN
        KTJET_BOOK_UTIL = .FALSE.
        CALL ERRMSG('Bank already exists','KTJET_BOOK_UTIL',
     &    ' Cannot run KT algorithm', 'E' )
        RETURN
      ENDIF
C
C: Decide how big to make these banks. Depends on what input we are using.
C
      IF ( INPUT_TYPE .EQ. 1 ) THEN   ! Partons
        LISAE = GZISAE()
        IF ( LISAE .LE. 0 ) THEN
          CALL ERRMSG('No ISAQ','KTJET_BOOK_UTIL',
     &      'No parton banks present', 'W')
          KTJET_BOOK_UTIL = .FALSE.
          GOTO 666
        ENDIF
        NVECT = IQ( LISAE + 6 )

      ELSE IF ( INPUT_TYPE .EQ. 2 ) THEN
        LISAE = GZISAE()
        IF ( LISAE .LE. 0 ) THEN
          CALL ERRMSG('No ISP1','KTJET_BOOK_UTIL',
     &      'No particle banks present', 'W')
          KTJET_BOOK_UTIL = .FALSE.
          GOTO 666
        ENDIF
C: Lets count ISP1 banks
        NVECT = 0
        LISV1 = LQ(LISAE - IZISV1)
        DO WHILE ( LISV1 .GT. 0 )
          LISP1 = LQ( LISV1 - IZISP1 )
          DO WHILE ( LISP1 .GT. 0 )
            NVECT = NVECT + 1
            LISP1 = LQ( LISP1 )
          ENDDO
          LISV1 = LQ( LISV1 )
        ENDDO


      ELSE IF ( INPUT_TYPE .EQ. 3 ) THEN ! Cells
        LCAEH = GZCAEH()
        IF ( LCAEH .LE. 0 ) THEN
          CALL ERRMSG('No CAEH','KTJET_BOOK_UTIL',
     &      'No calorimeter cells present', 'W')
          KTJET_BOOK_UTIL = .FALSE.
          GOTO 666
        ENDIF
        NVECT = IQ( LCAEH + 3 )
      ELSE IF ( INPUT_TYPE .EQ. 4 ) THEN ! CATD BANK
C        CALL QCD_GZMDST_ALL( LDUM, LCATD, LDUM1 )
        LCATD = GZCATD()
        IF ( LCATD .LE. 0 ) THEN
          CALL ERRMSG('No CATD','KTJET_BOOK_UTIL',
     &      ' CANT FIND CATD BANK', 'W')
          KTJET_BOOK_UTIL = .FALSE.
          GOTO 666
        ELSE
          NVECT = IQ(LCATD + 8 )
          NVECT = NVECT + IQ( LCATD + NVECT + 9 )
        ENDIF
      ELSE IF ( INPUT_TYPE .EQ. 5 ) THEN ! JETS BANK USING KT
        LCAPH = KTJET_FIND_CAPH( KTCUT( IKTCUT ) -.0001)
        IF ( LCAPH .GT. 0 ) THEN
          NVECT = IQ( LCAPH + 3 )
        ELSE
          KTJET_BOOK_UTIL = .FALSE.
          GOTO 666
        ENDIF
      ELSEIF ( INPUT_TYPE.EQ.6 ) THEN !GIAN'S STUFF
        CALL KT_REMOVE_LOW_ET_CELLS (ier)
        ktjet_book_util = ier .eq. 0
        NVECT=NGIAN
      ELSE
        CALL ERRMSG('Invalid type','KTJET_BOOK_UTIL',
     &      'Invalid algorithm input type', 'W')
        KTJET_BOOK_UTIL = .FALSE.
        RETURN
      ENDIF
  666 CONTINUE
      IF ( .NOT. KTJET_BOOK_UTIL ) THEN ! look for KTCL
         lktcl = gzktcl ()
        DO WHILE ( .NOT. KTJET_BOOK_UTIL .AND. LKTCL .GT. 0 )
           if (INPUT_TYPE .eq. ktcl_get_input_type (lktcl)) then
              KTJET_BOOK_UTIL = .TRUE.
              NVECT = ktcl_get_ncluster (lktcl)
           ENDIF
           LKTCL = LQ(LKTCL)
        ENDDO
      ENDIF
C
C: Check to see if we have anything
C
      IF ( .NOT. KTJET_BOOK_UTIL) GOTO 999
C
C: Book KMAP and KVEC
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('KVEC','4I/1I14F', IOH_VEC )
        CALL MZFORM('KMAP','4I6F/3I', IOH_MAP )
      ENDIF

C: Make room for beam jets
      NVECT = NVECT + 2

      CALL MZBOOK( IXMAIN, LKVEC, LDUM, 2, 'KVEC', 0, 0,
     &     kvec_block_size*NVECT+4,
     &  IOH_VEC, 0 )
      IQ( LKVEC + 1 ) = 1               ! Version number
      IQ( LKVEC + 2 ) = INPUT_TYPE      ! 1=parton, 2=particles, 3=cells
      IQ( LKVEC + 3 ) = NVECT           ! # of (1=partons,2=particles etc.)
      IQ( LKVEC + 4 ) = kvec_block_size ! Bank repetition number

      CALL MZBOOK( IXMAIN, LKMAP, LDUM, 2, 'KMAP', 0, 0, 3*NVECT+10,
     &  IOH_MAP, 0 )
      IQ( LKMAP + 1 ) = 1
      IQ( LKMAP + 2 ) = INPUT_TYPE
      IQ( LKMAP + 3 ) = NVECT
      IQ( LKMAP + 4 ) = 3


  999 RETURN

      ENTRY KTJET_DROP_UTIL( REUSE_BANKS )
      KTJET_DROP_UTIL = .TRUE.
      CALL MZDROP( IXCOM, LKVEC, 'L')
      CALL MZDROP( IXCOM, LKMAP, 'L')
      LKVEC = 0
      LKMAP = 0
      IF ( .NOT. REUSE_BANKS ) THEN
        IF ( KKVEC .GT. 0 ) CALL MZDROP( IXCOM, KKVEC, 'L')
        IF ( KKMAP .GT. 0 ) CALL MZDROP( IXCOM, KKMAP, 'L')
        KKVEC = 0
        KKMAP = 0
      ENDIF
      RETURN

      END
