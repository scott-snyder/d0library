      SUBROUTINE KTJET_POSTCLUSTER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do post clustering of KT Subjets INSTEAD of
C-                         preclustering. This will only run when
C-                         DO_PRECLUSTER is .FALSE. and DO_POSTCLUSTER is
C-                         .TRUE.
C-
C-                         Idea from Mike Seymour. Loop over all pairs and
C-                         consider those within angular distance PREC_WIDTH.
C-                         Loop over those and find pair with smallest KT,
C-                         and merge them. Continue until no more left.
C-
C-                         Unfortunately, if we change the KT jet list at
C-                         this point by doing postclustering, we can no
C-                         longer do any other ycuts because the jets have
C-                         been merged using this criteria. So we need to
C-                         UNDO the process after storing the jets
C-  ENTRY: KTJET_UNDO_POSTCLUSTER : Put the list back the way it was.
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-FEB-1995   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      REAL KTJET_MKL
      EXTERNAL KTJET_MKL
      LOGICAL SOME_WITHIN_PRECWIDTH
      INTEGER I,J, IMIN, JMIN
      REAL KTMIN
      INTEGER NMERGED
      INTEGER IJMERGED(2,256)
      LOGICAL POSTCLUSTERING_DONE, MORE_TO_GO
      SAVE NMERGED, POSTCLUSTERING_DONE, IJMERGED
      REAL DELTA_R, RETA1, RETA2, RPHI1, RPHI2
      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
C----------------------------------STATEMENT FUNCTIONS-------------------
C
C---Delta R in eta-phi space
C
      DELTA_R(RETA1,RETA2,RPHI1,RPHI2) = SQRT( ((RETA1)-(RETA2))**2 +
     &  MIN( MOD(ABS((RPHI1)-(RPHI2)),SNGL(TWOPI)) ,
     &  SNGL(TWOPI)-ABS(MOD(ABS((RPHI1)-(RPHI2)),SNGL(TWOPI))) )**2 )
C------------------------------------------------------------------------
C
C: Check that preclustering was not done
C
      IF ( DO_PRECLUSTER ) THEN
        CALL ERRMSG('Precluster ON','KTJET_POSTCLUSTER',
     &    'Asking for BOTH preclustering and postclusterin??','F')
        RETURN
      ENDIF
C
C: Init
C
      POSTCLUSTERING_DONE = .TRUE.
      NMERGED = 0
C
C: Loop over pairs and continue as long as some pairs are within PREC_WIDTH
C
      SOME_WITHIN_PRECWIDTH = .TRUE.            ! Start by assuming some are
      DO WHILE ( SOME_WITHIN_PRECWIDTH )        ! Stop when there arent
        SOME_WITHIN_PRECWIDTH = .FALSE.         ! Unless we find one
        DO  I = 1, IQ(LKVEC+3)                  ! Loop over subjets
          IF ( IQ( POINT(I) ) .GT. 0 ) THEN     ! is this a valid subjet?
            DO J = I+1, IQ(LKVEC+3)             ! For this one, loop over rest
              IF ( IQ( POINT(J) ) .GT. 0 ) THEN ! is this also valid
                IF ( DELTA_R( Q(KTETA(I)), Q(KTETA(J)), Q(KTPHI(I)),
     &            Q(KTPHI(J)) ) .LE. PREC_WIDTH ) THEN  ! Got a pair
                  IF ( .NOT. SOME_WITHIN_PRECWIDTH ) THEN
                    SOME_WITHIN_PRECWIDTH = .TRUE.    ! Set flag
                    KTMIN = KTJET_MKL(I,J)            ! KT(I,J)
                    IMIN  = I                         ! First minimum
                    JMIN  = J
                  ELSEIF ( KTJET_MKL(I,J) .LT. KTMIN ) THEN
                    KTMIN = KTJET_MKL(I,J)            ! New minimum
                    IMIN  = I
                    JMIN  = J
                  ENDIF
                ENDIF ! End IF pair are within PREC_WIDTH
              ENDIF ! End IF J is valid
            ENDDO ! End J loop
          ENDIF ! End IF I is valid
        ENDDO ! END I loop
C
C: If we have a minimum pair, merge them and continue. Else we are done
C
        IF ( SOME_WITHIN_PRECWIDTH ) THEN
          CALL KT_MERGE_CELLS(IMIN,JMIN)
          NMERGED = NMERGED + 1                       ! Keep track of number
          IF ( NMERGED .GT. 256 ) THEN
            CALL ERRMSG('ARRAY OVERFLOW','KTJET_POSTCLUSTER',
     &        ' IJMERGED ARRAY OVERFLOWED','F')
            RETURN
          ENDIF
          IJMERGED(1,NMERGED) = IMIN
          IJMERGED(2,NMERGED) = JMIN
        ENDIF
      ENDDO ! END WHILE a pair is within PREC_WIDTH
  999 RETURN

      ENTRY KTJET_UNDO_POSTCLUSTER
C
C: Check that preclustering was not done
C
      IF ( DO_PRECLUSTER ) THEN
        CALL ERRMSG('Precluster ON','KTJET_POSTCLUSTER',
     &    'Asking for BOTH preclustering and postclusterin??','F')
        RETURN
      ENDIF
C
C: Check again that POSTCLUSTERING was done!
C
      IF ( .NOT. POSTCLUSTERING_DONE ) THEN
        CALL ERRMSG('Confused','KTJET_UNDO_PRECLUSTER',
     &    'Undo called but POST not done!','F')
        RETURN
      ENDIF
C
C: Loop in reverse order, splitting them up again
C
      DO I = NMERGED, 1, -1
        CALL KT_SPLIT_CELLS( IJMERGED(1,I), IJMERGED(2,I) )
      ENDDO

C
C: Special, we can tell those that were POSTCLUSTERED merged because
C: the merged jet id is set to the negative of the one its merged with.
C: But we have to wait until the merged jet is also unmerged.
C      MORE_TO_GO  = .TRUE.                        ! Assume some should be split
C      DO WHILE ( MORE_TO_GO )                     ! Do while there are
C        MORE_TO_GO  = .FALSE.                     ! Until we find one
C        DO I = IQ(LKVEC+3), 2, -1                 ! I > J
C          DO J  = I-1, 1, -1
C            IF ( IQ(POINT(I)) .LT. 0 .AND.
C     &           IQ(POINT(I))/IQ(LKVEC+3) .EQ. -IQ(POINT(J)) ) THEN
C              CALL KT_SPLIT_CELLS(I,J)            ! Split these
C              IQ( POINT(I) )  = I                 ! Back to normal
C              IQ( POINT(J) )  = J
C              NMERGED = NMERGED - 1               ! One more down
C            ELSEIF ( IQ(POINT(I)) .LT. 0 .AND.
C     &               IQ(POINT(I)) .EQ. IQ(POINT(J)) ) THEN
C              MORE_TO_GO  = .TRUE.                ! Cant split this one yet
C            ENDIF
C          ENDDO
C        ENDDO
C      ENDDO
C
C: Done. Check that NMERGED = 0.
C
C      IF ( NMERGED .NE. 0 ) THEN
C        PRINT *, ' NMERGED IS ',NMERGED
C      ENDIF

      END
