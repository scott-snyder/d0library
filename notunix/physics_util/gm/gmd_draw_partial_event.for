      SUBROUTINE GMD_DRAW_PARTIAL_EVENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Project selected objects onto (eta, phi).
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  11-DEC-1991   Harrison B. Prosper
C-   Updated   6-MAY-1993   Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Corrected FLINT complaints
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:GM_4VECT.INC'
C----------------------------------------------------------------------
      INTEGER I, J, K
      INTEGER ID(MAXPART), ILIST(MAXPART), NID, TOTAL, VISIBILITY, IDX
C
      CHARACTER*16 OPERATION(2)
      CHARACTER*16 TITLE
      CHARACTER*40 PLIST(MAXPART)
      LOGICAL ADD
C----------------------------------------------------------------------
C
C ****  Create all objects
C
      CALL GMD_CREATE_SEGMENTS
C
C ****  Get selected list
C
      NID = 1
      IDX = 1
      OPERATION(1) = 'ADD     OBJECT'
      OPERATION(2) = 'REMOVE  OBJECT'
C
      DO WHILE ( NID .GT. 0 )
C
C ****  Display menu ADD/REMOVE
C
        CALL LISFIL(0, ' ')
        CALL LISFIL(2, OPERATION)
        CALL LISBOX('Operation', ' ', NID, ID, 1)
        ADD = ID(1) .EQ. 1
C
        DO WHILE ( NID .GT. 0 )
C
C ****  Set up menu
C
          J = 0
          IF ( ADD ) THEN
            VISIBILITY = 1
            TITLE = 'ADD OBJECTS'
            DO I = 1, NOBJECT
              IF ( .NOT. VISIBLE(I) ) THEN
                J = J + 1
                PLIST(J) = LIST(I)
                ILIST(J) = I
              ENDIF
            ENDDO
          ELSE
            VISIBILITY = 0
            TITLE = 'REMOVE OBJECTS'
            DO I = 1, NOBJECT
              IF ( VISIBLE(I) ) THEN
                J = J + 1
                PLIST(J) = LIST(I)
                ILIST(J) = I
              ENDIF
            ENDDO
          ENDIF
          TOTAL = J
C
C ****  Display menu
C
          IF ( TOTAL .GT. 0 ) THEN
            CALL LISFIL(0, ' ')
            CALL LISFIL(TOTAL, PLIST)
            CALL LISDEF(IDX)
            CALL LISBOX(TITLE, ' ', NID, ID, TOTAL)
C
            IF ( NID .GT. 0 ) THEN
C
C ****  Change visibility of segments
C
              CALL JBGBAT(0)    ! BEGIN batching updates
              DO I =  1, NID
                J = ID(I)
                K = ILIST(J)
                CALL JVISBL(SEGMENT(K), VISIBILITY)
                VISIBLE(K) = ADD
              ENDDO
              CALL JENBAT       ! END batching updates
C
              IDX = J
            ENDIF
          ELSE
            IF ( ADD ) THEN
              CALL INTMSG('     ** No more particles to ADD **')
            ELSE
              CALL INTMSG('     ** No more particles to REMOVE **')
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
      RETURN
      END
