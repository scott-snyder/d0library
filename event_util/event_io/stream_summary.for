C VAX/DEC CMS REPLACEMENT HISTORY, Element STREAM_SUMMARY.FOR
C *2    17-OCT-1993 14:30:15 MEENA "WYATT MERRIT: V2 streaming release"
C *1    24-FEB-1993 12:52:34 MEENA "multiple streams by trigger filters"
C VAX/DEC CMS REPLACEMENT HISTORY, Element STREAM_SUMMARY.FOR
      FUNCTION STREAM_SUMMARY()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out statistics from stripping package
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-MAY-1992   K. Wyatt Merritt
C-   Modified 14-Jan-1993   L. Lueking added MAKE_SUMMARY flag
C-   Updated   3-MAY-1993   K. Wyatt Merritt  Get NSTREAM from array of
C-                           stream descriptions; add PACKAGE_OFF flag
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NMAX
      PARAMETER (NMAX = 40)
C
      LOGICAL STREAM_SUMMARY
      LOGICAL L,GET_STREAM_SUM,MAKE_SUMMARY,PACKAGE_OFF
      LOGICAL STRIP_ON_L1(NMAX),STRIP_ON_L2(NMAX),STRIP_ON_BOTH(NMAX),
     &  USER_ONLY(NMAX),LVAL(4)
C
      INTEGER NPASS,NREJECTED,NREJ_USER,NREJ_L1,NREJ_L2
      INTEGER LUN,SSUNIT,I
      INTEGER NELE,NSTREAM,STATUS,K,ISTART,NL1,NL2
      INTEGER TRIGSUM(32),FILTSUM(128)
      INTEGER NPROC,NPASS_TOT
C
      CHARACTER*5 STREAM_NAME(NMAX)
      CHARACTER*12 TRIGNAME(32),FILTNAME(128)
C
C----------------------------------------------------------------------
      STREAM_SUMMARY = .TRUE.
      LUN = SSUNIT()
C
      CALL EZPICK('STREAM_FILTER_RCP')
      CALL EZGET('PACKAGE_OFF',PACKAGE_OFF,STATUS)
      IF (PACKAGE_OFF) RETURN
      CALL EZGET('MAKE_SUMMARY',MAKE_SUMMARY,STATUS)
      IF(.NOT.MAKE_SUMMARY)RETURN
      CALL EZGETA('STREAM_DESCRIP', 0, 0, 0, NELE, STATUS)
      NSTREAM = NELE/7
      NPASS_TOT = 0
      DO K = 1,NSTREAM
        ISTART = (K-1)*7 + 1
        CALL EZGETC('STREAM_DESCRIP',ISTART,5,STREAM_NAME(K),STATUS)
        L = GET_STREAM_SUM(K,NPASS,NREJECTED,NREJ_L1,NREJ_L2,NREJ_USER,
     &    TRIGSUM,FILTSUM)
        CALL EZGETA('STREAM_DESCRIP',ISTART+2,ISTART+5,1,LVAL,
     &        STATUS)
        STRIP_ON_L1(K) = LVAL(1)
        STRIP_ON_L2(K) = LVAL(2)
        STRIP_ON_BOTH(K) = LVAL(3)
        USER_ONLY(K) = LVAL(4)
        NPASS_TOT = NPASS_TOT + NPASS
        NL1 = 0
        NL2 = 0
        IF (STRIP_ON_L1(K)) THEN
          DO I = 1,32
            NL1 = NL1 + TRIGSUM(I)
          ENDDO
        ENDIF
        IF (STRIP_ON_L2(K)) THEN
          DO I = 1,64
            NL2 = NL2 + FILTSUM(I)
          ENDDO
        ENDIF
C
        WRITE (LUN,1000) STREAM_NAME(K),
     &  LVAL,
     &  NPASS,NREJECTED,NREJ_L1,NREJ_L2,NREJ_USER
 1000   FORMAT(//,' STREAM_FILTER: Summary of stream ',A5,
     &  '- Strip  L1:',L2,' L2:',L2,' BOTH:',L2,' USR ONLY:',L2,
     &  /,5X,'Events passed            ',I15,
     &  /,5X,'Events rejected          ',I15,
     &  /,5X,'Events rej w/ lev1 bits  ',I15,
     &  /,5X,'Events rej w/ lev2 bits  ',I15,
     &  /,5X,'Events rej by user code  ',I15)
C
        CALL GET_TRIG_LISTS(TRIGNAME,FILTNAME)
        IF (STRIP_ON_L1(K)) 
     &    WRITE (LUN,2000) (I,TRIGNAME(I),TRIGSUM(I),I=1,32)
 2000   FORMAT(//,' Level 1 trigger summary for this stream:',/,
     &    4X,'BIT     NAME     # EVENTS',/,
     &    32(5X,I2,1X,A12,1X,I10,/))
        IF (STRIP_ON_L2(K)) 
     &    WRITE (LUN,2001) (I,FILTNAME(I),FILTSUM(I),I=1,64)
 2001   FORMAT(//,' Level 2 trigger summary for this stream:',/,
     &    4X,'BIT     NAME     # EVENTS',/,
     &    64(5X,I2,1X,A12,1X,I10,/))
        CALL USER_STREAM_SUMMARY(STREAM_NAME(K))
      ENDDO
C
      CALL EZRSET
C
  999 RETURN
      END
