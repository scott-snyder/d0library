      SUBROUTINE GET_ISAJET_PARTON(GZPARTON,PARTID,ISAJID,P,NP,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the 8-vectors of all PARTONS with the
C-   specified PARTON identifier from specified ISAJET bank.
C-
C-   Inputs  : GZPARTON    *    Address of GZ routine for ISAJET bank
C-             PARTID     [I]   ISAJET particle ID of PARTON
C-             ISAJID(*)  [I]   ISAJID(1) = 0 - to return ALL PARTONS
C-                              ISAJID(1) > 0 - to return the data from
C-                                              the specified bank ID
C-
C-   Outputs : ISAJID(*) [I]    ISAJET Bank Ids
C-             P(8,*)    [R]    8-vectors of PARTONS :
C-                              (px, py, pz, p, mass, phi, theta, eta)
C-             NP        [I]    Number of PARTONS
C-             IER       [I]     0  --  OK
C-                              -1  --  No specified ISAJET bank
C-                              -2  --  No PARTONS found
C-                              -3  --  Too small size of output arrays
C-   Controls:
C-
C-   Created  28-APR-1992   Stan M. Krzywdzinski, Harrison B. Prosper
C-   Modified 12-FEB-1993   Stan M. Krzywdzinski
C-                          ENTRY SET_MAX_ISAJET_PARTONS - to allow
C-                          user safeguard against arrays overflow.
C-                          Inserted EXTERNAL statement.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  GZPARTON
      EXTERNAL GZPARTON
      INTEGER PARTID
      INTEGER ISAJID(*)
      REAL    P(8,*)
      INTEGER NP,IER
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER LPARTON
      INTEGER NPMAX, MAX_PARTONS
      SAVE NPMAX
      DATA NPMAX /-1/
C----------------------------------------------------------------------
      IER = 0
      NP  = 0
C
      LPARTON = GZPARTON()
      IF ( LPARTON .LE. 0 ) THEN
        IER =-1                         ! No PARTON bank
        GOTO 999
      ENDIF
C
C ****  Find specified PARTON banks
C
      DO WHILE ( LPARTON .GT. 0 )
C
C ****  Check particle ID word
C
        IF ( IQ(LPARTON+1) .EQ. PARTID ) THEN
C
          IF ( ISAJID(1) .EQ. 0 ) THEN
C
C ****  Return ALL PARTONS
C
            IF ( (NPMAX .GT. 0) .AND. (NP .GE. NPMAX) ) THEN
              IER = -3
              GO TO 999
            ELSE
              NP = NP + 1
              ISAJID(NP) = IQ(LPARTON-5)    ! ISAJET Bank Identifier
              CALL UCOPY(Q(LPARTON+2),P(1,NP),8)
            ENDIF
          ELSE
C
C ****  Return specific PARTON
C
            IF ( IQ(LPARTON-5) .EQ. ISAJID(1) ) THEN
              NP = NP + 1
              CALL UCOPY(Q(LPARTON+2),P(1,NP),8)
              GOTO 999
            ENDIF
          ENDIF
        ENDIF
        LPARTON = LQ(LPARTON)
      ENDDO
C
      IF ( NP .LE. 0 ) THEN
        IER = -2
      ENDIF
C
  999 RETURN
C
      ENTRY SET_MAX_ISAJET_PARTONS(MAX_PARTONS)
      NPMAX = MAX_PARTONS
      RETURN
C
      END
