      SUBROUTINE CRUNCH_JPTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compress all the JPTS banks in the event.
C-      ** Warning ** - to uncompress you need all JETS banks back as
C-                      well as CAEH and PTCAEP
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   1-FEB-1995   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INCLUDE 'D0$LINKS:IZJPTS.LINK'    ! Old JPTS bank
      INTEGER LPROC, GZPROC, LCAPH, LJPTS, KJPTS
      INTEGER NJPTS, ISHFT, I, POINT, LDUM
      EXTERNAL GZPROC
      INCLUDE 'D0$INC:L2LINK.INC'
      EQUIVALENCE( L2LINK(5), LCAPH )
      EQUIVALENCE( L2LINK(6), LJPTS )
      EQUIVALENCE( L2LINK(7), KJPTS )
C----------------------------------------------------------------------
      CALL MZLINT(IXCOM,'/L2LINK/',DUM,L2LINK(NLNK),DUM)

      LPROC =   GZPROC()                ! Get PROC bank
      IF ( LPROC .LE. 0 ) GOTO 900
      LCAPH =   LQ( LPROC - IZCAPH )    ! Get first CAPH bank
      DO WHILE ( LCAPH .GT. 0 )         ! Loop over algorithms
        LJETS = LQ( LCAPH - IZJETS )    ! Get first JETS bank
        DO WHILE ( LJETS .GT. 0 )       ! Loop over JETS
          LJPTS = LQ( LJETS - IZJPTS )
          IF ( LJPTS .GT. 0 ) THEN      ! Compress this bank
            NJPTS= (IQ(LJPTS+2)+1)/2    ! Number of words
            CALL ZSHUNT(IXCOM, LJPTS, LDUM, 2, 0 )  ! Hold in space (temporary)
            CALL BKJPTS(LJETS, NJPTS, KJPTS )    ! Make a new one (half sized)
C            CALL ZSHUNT(IXCOM, KJPTS, LJETS, -8, 0 ) ! Move this to LJETS-3
            CALL ZSHUNT(IXCOM, LJPTS, KJPTS, 0 ,0 ) ! Move old one back
            CALL DCTOH(4,'CJPT', IQ(KJPTS-4))        ! Change name to 'CJPT'
C
C: Fill compressed JPTS bank
C
            DO I  = 1, NJPTS
              POINT = LJPTS + (I-1)*2 + 2
              IQ( KJPTS + 2 + I ) = ISHFT( IQ(POINT+1), 16 )
              IF ( (I-1)*2 + 2 .LE. IQ(LJPTS+2) ) IQ( KJPTS + 2 + I ) =
     &          IQ( KJPTS + I + 2) + IQ(POINT+2)
            ENDDO
          ENDIF
          LJETS = LQ(LJETS)
        ENDDO     ! End JETS loop
        LCAPH = LQ(LCAPH)
      ENDDO       ! End CAPH loop
  900 CONTINUE
      DUM(1)  = 0         ! Deactivate link area
      END
