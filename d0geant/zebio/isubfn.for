      SUBROUTINE ISUBFN(NT,NWORD,BUF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ISUBFN overwrites user data word in JKINE bank. The
C-                         number of user words is NWORD words. If there is no
C-                         bank for NT-th track, the program is forced to
C-                         abort. If there is no user words created, this
C-                         routine creates NWORD words user area under  JKINE
C-                         bank for NT-th track.
C-
C-                         Note that this routine is dependent on the JKINE
C-                         bank format in GEANT. Therefore we have to be
C-                         carefull about update of GEANT. If CERN changes the
C-                         bank format, we have to adjust to it.
C-
C-   Inputs  : NT    = Ordinal number of particle in JKINE bank
C-             NWORD = Number of user words in JKINE bank
C-             BUF   = Array of user words to be overwritten,
C-                      if -1.0, no change.
C-   Outputs : None
C-   Controls: None
C-
C-   Created  28-NOV-1988   Alan M. Jonckheere : based on ISUBUF by S.Kunori
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCBANK.INC/LIST'
      INCLUDE 'D0$INC:GCLINK.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INTEGER NT,NWORD
      REAL    BUF(NWORD)
      INTEGER JK,JUK,NWBUF,I
C-----------------------------------------------------------------------
C
      JK = LQ(JKINE-NT)
      IF ( JKINE.EQ.0.OR.JK.EQ.0 ) THEN
        WRITE(LOUT,60) NT,JKINE,JK
   60   FORMAT(' === ERROR IN S/R ISUBFN ==='
     *    /20X,'JKINE BANK FOR',I5,'-TH PARTICLE DOSE NOT EXIST.'
     *    /20X,'JOB WAS FORCED TO BE ABORTED IN S/R ISUBFN.'
     *    /20X,'       JKINE=',I10,10X,'JK=',I10)
      ENDIF
      JUK = LQ(JK-1)
      IF ( JUK.EQ.0 ) THEN
        NWBUF = 6
        CALL MZBOOK(IXDIV,JUK,JK,-1,'KINU',0,0,NWBUF,3,0)
        IQ(JUK-5) = NT
      ENDIF
C
C --over write data, if Ui is not -1.0....
C
C.N.O.===== Begin(Bug. on this seq. "Q(JUK+1)"--->"Q(JUK+I)")
      DO 10 I = 1, NWORD
        IF ( BUF(I).GE.0 ) Q(JUK+I) = BUF(I)
   10 CONTINUE
C.N.O.===== End
C
  999 RETURN
      END
