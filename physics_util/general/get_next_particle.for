      SUBROUTINE GET_NEXT_PARTICLE(IDD,ISAJ_ID,X,P,CTRL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return particle data one particle at a time
C-   from the ISP1 banks. Set CRTL = 1 to get data for the FIRST particle.
C-   NOTE: CTRL is updated upon exit.
C-   It is set to -1 if the returned data is for the LAST particle. X(3)
C-   contains the vertex position associated with the particle and P(4)
C-   contains the 4-vector P = (Px,Py,Pz,E). Each particle is classified
C-   according to which ISAJ bank it is linked to (ISAJ_ID is the particle
C-   ID in the ISAJ bank).
C-
C-   Inputs  : None
C-
C-   Outputs : IDD      [I]     Particle ID (ISAJET id)
C-             ISAJ_ID  [I]     Primary parton ID
C-             X(3)     [R]     Vertex
C-             P(4)     [R]     4-vector
C-   Controls: CTRL     [I]     1 to start; if -1 returned then
C-                              current particle is the last.
C-
C-   Created  20-NOV-1990   Chip Stewart, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDD
      INTEGER ISAJ_ID
      REAL    X(3)
      REAL    P(4)
      INTEGER CTRL
C----------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      REAL    XX(3)
      INTEGER  LISAE,LISV1,LISP1,LISAJ,NISV1,NISP1,LOOP
      LOGICAL FIRST, LINKS_RESERVED
      SAVE FIRST, LINKS_RESERVED, LOOP, NISV1, NISP1, XX
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        LINKS_RESERVED = .FALSE.
        CALL INZLNK                     ! Initialize ZLINKA (done once)
      ENDIF
C
C ****  Check whether to initialize
C
      IF ( CTRL .LE. 1 ) THEN
C
C ****  Release links if reserved
C
        IF ( LINKS_RESERVED ) THEN
          CALL RSLINK('ISV1BONG',NISV1)
          CALL RSLINK('ISP1BONG',NISP1)
          LINKS_RESERVED = .FALSE.
        ENDIF
C
C ****  Reserve links for ISV1 and ISP1
C
        CALL GSLINK('ISV1BONG',NISV1)
        CALL GSLINK('ISP1BONG',NISP1)
        LINKS_RESERVED = .TRUE.
C
        LOOP  = 1                       ! Outer loop first
        LISV1 = 0                       ! Checked at end of routine
        LISAE = LQ(LHEAD-IZISAE)
C
C ****  Get pointer to bank ISAE
C
        IF ( LISAE .LE. 0 ) THEN
          CALL ERRMSG('GET_NEXT_PARTICLE','GET_NEXT_PARTICLE',
     &    'No ISAE bank','W')
          GOTO 999
        ENDIF
C
C ****  Initialize links
C
        LSLINK(NISV1) = LQ(LISAE-IZISV1)
        LSLINK(NISP1) = 0
      ENDIF
C
C ****  Links MUST be reserved
C
      IF ( .NOT. LINKS_RESERVED ) THEN
        CALL ERRMSG('GET_NEXT_PARTICLE','GET_NEXT_PARTICLE',
     &    'You MUST first initialize by setting CTRL = 1','W')
        GOTO 999
      ENDIF
C
      CTRL  = CTRL + 1
      LISP1 = LSLINK(NISP1)
      LISV1 = LSLINK(NISV1)
C
C ****  Determine where to go (100 or 200)
C
      GOTO (100,200), LOOP
      GOTO 999
C
  100 CONTINUE
      LOOP = 2                          ! Goto inner loop next time around
C
C ****  Get vertex
C
      XX(1) = Q(LISV1+7)
      XX(2) = Q(LISV1+8)
      XX(3) = Q(LISV1+9)
C
C ****  Get first ISP1 link
C
      LISP1 = LQ(LISV1-IZISP1)
      IF ( LISP1 .LE. 0 ) THEN
        CALL ERRMSG('GET_NEXT_PARTICLE','GET_NEXT_PARTICLE',
     &    'No ISP1 bank below ISV1','W')
        LISV1 = 0                       ! Checked at end of routine
        GOTO 999
      ENDIF
C
  200 CONTINUE
      X(1) = XX(1)
      X(2) = XX(2)
      X(3) = XX(3)
      IDD  = IQ(LISP1+1)                ! Particle ID
      P(1) =  Q(LISP1+2)                ! Px
      P(2) =  Q(LISP1+3)                ! Py
      P(3) =  Q(LISP1+4)                ! Pz
      p(4) =  Q(LISP1+5)                ! E
C
C ****  Classify particles according to contents of ISAJ
C
      LISAJ = LQ(LISP1-3)
      IF ( LISAJ .GT. 0 ) THEN
        ISAJ_ID = IQ(LISAJ+1)           ! Particle ID
      ELSE
        ISAJ_ID = 0                     ! Underlying event
      END IF
C
      LISP1 = LQ(LISP1)                 ! Get next ISP1 link
      LSLINK(NISP1) = LISP1             ! Save it
C
C ****  Check if this is the end of current ISP1 chain
C
      IF ( LISP1 .GT. 0 ) THEN
        GOTO 999
      ELSE
        LISV1 = LQ(LISV1)               ! Get next ISV1 link
        LSLINK(NISV1) = LISV1           ! Save it
        LOOP  = 1                       ! Goto outer loop next time around
      ENDIF
C
  999 CONTINUE
C
C ****  Check if this is the last particle; if so release links
C
      IF ( LISV1 .LE. 0 ) THEN
        CTRL = -1
        IF ( LINKS_RESERVED ) THEN
          CALL RSLINK('ISV1BONG',NISV1)
          CALL RSLINK('ISP1BONG',NISP1)
          LINKS_RESERVED = .FALSE.
        ENDIF
      ENDIF
C
      RETURN
      END
