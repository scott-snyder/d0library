      SUBROUTINE SORT_DEPENDENCY_LIST(NAME,NDEP,DLIST,MXDEP,NNAME,ORDER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sort a dependency list so that all the n
C-                         dependents DLIST(1,i)...DLIST(n,i) of a given
C-                         name NAME(i) precede that name in the list.
C-
C-                         The array ORDER can be used to map other arrays
C-                         into the same order as the array NAME. The map
C-                         is initialized INTERNALLY.
C-
C-                         IMPORTANT: The array NAME is returned ordered,
C-                         however, NDEP(*) and DLIST(*,*) are UNCHANGED.
C-                         Use ORDER to map them into the correct order.
C-
C-   Inputs  : NAME(*)          [C*]    Character array to be ordered
C-             NDEP(*)          [I]     Number of dependents/name
C-             DLIST(MXDEP,*)   [I]     Names of dependents
C-             MXDEP            [I]     Maximum number of dependents
C-             NNAME            [I]     Number of elements in NAME
C-
C-   Outputs : ORDER(*)         [I]     Order map
C-   Controls: None
C-
C-   Created   19-OCT-1989   Harrison B. Prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME(*)
      INTEGER       NDEP(*)
      INTEGER       MXDEP
      CHARACTER*(*) DLIST(MXDEP,*)
      INTEGER       NNAME
      INTEGER       ORDER(*)
C
      CHARACTER*132 TEMP
      INTEGER ITEMP
      LOGICAL SWAPPED
      INTEGER KOLD,JMAX,I,J,K,II,JJ,KK,LLIST
C----------------------------------------------------------------------
C
C ****  Initialize order map
C
      DO I =  1,NNAME
        ORDER(I) = I
      ENDDO
C
      LLIST = LEN(DLIST(1,1))           ! Get length of DLIST names
C
      DO I = 1, NNAME-1
        K    = I
        KOLD = K
        JMAX = NNAME
C
   40   CONTINUE
        J = I
        DO WHILE ( J .LT. JMAX )
          J = J + 1
C
          IF ( NDEP(ORDER(J)) .EQ. 0 ) THEN
            K = J
            GOTO 60
          ELSE
C
C ****  Check if NAME(J) is in list DLIST(...K...).
C ****  If current name NAME(J) is in dependency list of K'th name
C ****  then it should come before the K'th name.
C
            KK = ORDER(K)
            DO II =  1, NDEP(KK)
              IF (NAME(J) .EQ. DLIST(II,KK)) THEN
                K    = J
                JMAX = NNAME            ! Scan to end of list
                GOTO 50
              ENDIF
            ENDDO
C
          ENDIF
C
   50    CONTINUE
        ENDDO
C
C ****  If a swap has been made then re-scan the list of names
C ****  from J = I + 1 to J = JMAX if JMAX > I; that is check
C ****  only those names which come before NAME(K).
C
        SWAPPED = K .NE. KOLD
C
        IF ( SWAPPED ) THEN
          JMAX = K - 1                  ! Scan only up to NAME(K)
          KOLD = K
          IF ( JMAX .GT. I ) GOTO 40
        ENDIF
C
C ****  Perform name swaps
C
   60   CONTINUE
        TEMP    = NAME(K)
        NAME(K) = NAME(I)
        NAME(I) = TEMP
C
        ITEMP    = ORDER(K)
        ORDER(K) = ORDER(I)
        ORDER(I) = ITEMP
      ENDDO
C
  999 RETURN
      END
