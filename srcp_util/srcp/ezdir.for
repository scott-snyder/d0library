      SUBROUTINE EZDIR (BKNAME,NNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a list of all the SRCP banks in
C-   memory.
C-
C-   Inputs  : None
C-   Outputs : BKNAME(*)        [C*]    Names of SRCP banks
C-             NNAME            [I]     Number of names
C-   Controls: BKNAME(1)        Use this to specify a wildcard search,
C-                              either:
C-
C-                              xxx*, *xxx or *xxx*.
C-
C-   Created  10-MAY-1990   Harrison B. Prosper
C-   Updated   9-SEP-1990   Harrison B. Prosper
C-      added wild-carding feature
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) BKNAME(*)
      INTEGER NNAME
C
      CHARACTER*32 TEMPLATE
      INTEGER I,J,K,II,JJ,KK

      LOGICAL WILDCARD,PRE,MIDDLE,POST
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:NMSRCP.INC'
C----------------------------------------------------------------------
C
C ****  Check for wildcard character
C
      TEMPLATE = BKNAME(1)
      K = INDEX(TEMPLATE,'*')
      WILDCARD = K .GT. 0
C
      IF ( WILDCARD ) THEN
        CALL UPCASE(TEMPLATE,TEMPLATE)
C
C ****  Determine wildcard type
C
        IF ( K .GT. 1 ) THEN
          K = K - 1
          TEMPLATE = TEMPLATE(1:K)      ! Remove wildcard
          PRE = .TRUE.
        ELSE
          TEMPLATE = TEMPLATE(2:)       ! Remove wildcard
          K = INDEX(TEMPLATE,'*')       ! Look for second wildcard
          IF ( K .GT. 0 ) THEN
            K = K - 1
            TEMPLATE = TEMPLATE(1:K)    ! Remove second wildcard
            MIDDLE = .TRUE.
          ELSE
            CALL WORD(TEMPLATE,II,JJ,K)
            POST = .TRUE.
          ENDIF
        ENDIF
      ENDIF
C
C ****  Perform directory listing
C
      I = 0
      J = 0
      DO WHILE ( (I .LT. MXSRCP) .AND. (J .LT. NSRCP) )
        I = I + 1
        IF ( KSRCP(I) .GT. 0 ) THEN     ! Check for dropped banks
          IF ( WILDCARD ) THEN
            IF     ( PRE ) THEN
              IF ( TEMPLATE(1:K) .EQ. MSRCP(I)(1:K) ) THEN
                J = J + 1
                BKNAME(J) = MSRCP(I)
              ENDIF
            ELSEIF ( MIDDLE ) THEN
              IF ( INDEX(MSRCP(I),TEMPLATE(1:K)) .GT. 0) THEN
                J = J + 1
                BKNAME(J) = MSRCP(I)
              ENDIF
            ELSEIF ( POST ) THEN
              CALL WORD(MSRCP(I),II,JJ,KK)
              IF ( TEMPLATE(1:K) .EQ. MSRCP(I)(JJ-K+1:JJ) ) THEN
                J = J + 1
                BKNAME(J) = MSRCP(I)
              ENDIF
            ENDIF
          ELSE
            J = J + 1
            BKNAME(J) = MSRCP(I)        ! No wildcards
          ENDIF
        ENDIF
      ENDDO
      NNAME = J
C
  999 RETURN
      END
