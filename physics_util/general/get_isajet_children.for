      SUBROUTINE GET_ISAJET_CHILDREN
     &  (GZPARENT,GZCHILD,PLADDRESS,PARENTID,PBANKID,
     &  CHILDID,CBANKID,P,NP,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-   Return IDs and 8-vectors of CHILDREN given the pointer routines to
C-   the PARENT and CHILDREN banks, PARENT offset in L-address and PARENT
C-   specifications.
C-
C-   Call
C-   the routine GET_ISAJET_PARTON to get the PBANKID which can then be
C-   used along with the PARENTID to obtain the CHILDREN (decay products).
C-
C-   Inputs  : GZPARENT    *     Address of GZ routine for PARENT banks
C-             GZCHILD     *     Address of GZ routine for CHILD banks
C-             PLADDRESS  [I]    Offset in L-address of PARENT in CHILD bank
C-             PARENTID   [I]    ISAJET particle ID of PARENT
C-             PBANKID    [I]    ISAJET bank identifier of PARENT
C-
C-   Outputs : CHILDID(*) [I]    ISAJET particle ID's of CHILDREN
C-             CBANKID(*) [I]    ISAJET bank identifiers of CHILDREN
C-             P(8,*)     [R]    8-vectors of CHILDREN :
C                                (px, py, pz, p, mass, phi, theta, eta)
C-             NP         [I]    Number of CHILDREN
C-             IER        [I]     0 -- OK
C-                               -1 -- No PARENT bank 
C-                               -2 -- No bank with specified PARENT 
C-                               -3 -- No CHILD bank
C-                               -5 -- No CHILDREN found
C-                               -6 -- Too small size of output arrays
C-   Controls:
C-
C-   Created  28-APR-1992   Harrison B. Prosper and Stan M. Krzywdzinski
C-   Updated   2-FEB-1993   Herbert Greenlee
C-      Inserted EXTERNAL statements.
C-   Modified 12-FEB-1993   Stan M. Krzywdzinski
C-                          ENTRY SET_MAX_ISAJET_CHILDREN - to allow
C-                          user safeguard against arrays overflow
C-   Modified 26-FEB-1993   Stan M. Krzywdzinski 
C-                          Eliminate children same as parent
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  GZPARENT, GZCHILD
      EXTERNAL GZPARENT, GZCHILD
      INTEGER PLADDRESS, PARENTID, PBANKID
      INTEGER CHILDID(*), CBANKID(*)
      REAL    P(8,*)
      INTEGER NP,IER
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER LPARENT,LCHILD,LBANK
      INTEGER I
      LOGICAL SAME
      REAL PARENTP(8)
      INTEGER NPMAX, MAX_CHILDREN
      SAVE NPMAX
      DATA NPMAX /-1/
C----------------------------------------------------------------------
      IER = 0
      NP  = 0
C
      LPARENT = GZPARENT()
      IF ( LPARENT .LE. 0 ) THEN
        IER =-1                         ! No PARENT bank
        GOTO 999
      ENDIF
C
C ****  Find the address of the specified PARENT bank
C
      DO WHILE ( LPARENT .GT. 0 )
        IF ( IQ(LPARENT+1) .EQ. PARENTID ) THEN
          IF ( IQ(LPARENT-5) .EQ. PBANKID ) THEN
            LBANK = LPARENT
            CALL UCOPY(Q(LBANK+2),PARENTP,8)
            GOTO 100
          ENDIF
        ENDIF
        LPARENT = LQ(LPARENT)
      ENDDO
      IER = -2                          ! No specified PARENT bank
      GOTO 999
  100 CONTINUE
C
C ****  Scan CHILD banks for CHILDREN
C
      LCHILD = GZCHILD()
      IF ( LCHILD .LE. 0 ) THEN
        IER =-3                         ! No CHILD bank
        GOTO 999
      ENDIF
C
      DO WHILE ( LCHILD .GT. 0 )
        IF ( LQ(LCHILD-PLADDRESS) .EQ. LBANK ) THEN
          IF ( (NPMAX .GT. 0) .AND. (NP .GE. NPMAX) ) THEN
            IER = -6
            GO TO 999
          ELSE
C
C ***       Compare ID and (px,py,pz) of a child and the parent
C
            SAME = IQ(LCHILD+1).EQ.IQ(LBANK+1)      ! ID's
            IF ( SAME ) THEN
              DO I = 2,4
                SAME = SAME .AND. (Q(LCHILD+I).EQ.Q(LBANK+I))
              ENDDO
            ENDIF
            IF (.NOT. SAME) THEN
              NP = NP + 1
              CHILDID(NP) = IQ(LCHILD+1)
              CBANKID(NP) = IQ(LCHILD-5)
              CALL UCOPY(Q(LCHILD+2),P(1,NP),8)
            ENDIF
          ENDIF
        ENDIF
        LCHILD = LQ(LCHILD)
      ENDDO
C
      IF ( NP .LE.0 ) THEN
        IER = -3                        ! No CHILDREN found
      ENDIF
C
  999 RETURN
C
      ENTRY SET_MAX_ISAJET_CHILDREN(MAX_CHILDREN)
      NPMAX = MAX_CHILDREN
      RETURN
C
      END
