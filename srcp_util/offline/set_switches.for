      SUBROUTINE SET_SWITCHES (RCPFILE,PREFIX,FLAG_PREFIX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read Run-time PACKAGE SWITCHES from specified 
C-   RCP file and turn ON/OFF PACKAGES according to value of switches.
C-   This routine automatically loops over all RCP switches with the
C-   given prefix. The PREFIX is stripped off the package switch and 
C-   replaced with the specified FLAG_PREFIX. If the latter is ' ' then
C-   DO_CAHITS would become CAHITS, which is recognised by the PBD.
C-
C-   Use INRCP to read in an RCP file.
C-
C-   Inputs  : RCPFILE  [C*]    Name of RCP file
C-             PREFIX   [C*]    Prefix for switches
C-                              Example DO_ as in DO_CAJETS.
C-             FLAG_PREFIX   [C*]    Prefix for PBD flags (' ')
C-   Outputs : None
C-   Controls: None
C-
C-   Created  26-SEP-1989   Chip Stewart, Harrison B. Prosper
C-   Updated  31-JAN-1990   Harrison B. Prosper  
C-      Made compatible with new PBD 
C-   Updated  15-APR-1991   Scott Snyder - Add EZRSET call.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) RCPFILE
      CHARACTER*(*) PREFIX
      CHARACTER*(*) FLAG_PREFIX
      CHARACTER*32 SWITCH
C
      LOGICAL ACTIVE,LSWITCH,PACKAGE_ON
      LOGICAL PBD_SET_FLAG
      INTEGER I,J,K,L,N,LENPRE,IER,LENFLG
      INTEGER NID,ID

C----------------------------------------------------------------------
      L = LEN(RCPFILE)
      CALL EZPICK (RCPFILE(1:L))
C
      LENPRE = LEN(PREFIX)
      CALL WORD (PREFIX(1:LENPRE),I,J,L)
      LENPRE = L
C
      LENFLG = LEN(FLAG_PREFIX)
      CALL WORD (FLAG_PREFIX(1:LENFLG),I,J,L)
      LENFLG = L
C
C ****  Loop over switches with given prefix in RCP file
C
      ACTIVE = .TRUE.
      NID = 1
      DO WHILE ( ACTIVE )
C
C ***  GET ID of NEXT SWITCH IN RCPFILE WITH GIVEN PREFIX
C
        CALL EZGNXT (PREFIX(1:LENPRE),NID,ID)
C
        ACTIVE = ID.NE.0
        IF (ACTIVE) THEN
C
C ****  Get name of switch with ID returned above
C
          CALL EZGETN (ID,SWITCH,L)
          CALL EZGET(SWITCH(1:L),LSWITCH,IER)
C
C ****  CREATE STRING WITH FLAG_PREFIX
C
          IF ( LENFLG .GT. 0 ) THEN
            SWITCH = FLAG_PREFIX(1:LENFLG)//SWITCH(LENPRE+1:L)
          ELSE
            SWITCH = SWITCH(LENPRE+1:L)
          ENDIF
C
C ****  Set PBD switch
C
          PACKAGE_ON = PBD_SET_FLAG (SWITCH,LSWITCH)
C
          IF ( LSWITCH .AND. (.NOT. PACKAGE_ON) ) THEN
            CALL WORD(SWITCH,I,J,L)
            CALL ERRMSG('SET_SWITCHES','SET_SWITCHES',
     &        'Package '//SWITCH(I:J)//' has NOT been linked in','W')
          ENDIF
        END IF
      ENDDO
      CALL EZRSET
  999 RETURN
      END
