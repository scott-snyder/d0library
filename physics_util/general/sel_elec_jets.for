      FUNCTION SEL_ELEC_JETS(TOPID,LUNOUT,ID_OUT,TID_OUT,PID_OUT,
     &                       P_OUT,NP_OUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select electron + jets using ISAJET info.
C-
C-   Returned value  : TRUE     if event should be kept.
C-
C-   Inputs  :  TOPID           [I]     Negative for Tbar --> electron + anue
C-                                      Positive for Top  --> positron + nue
C-              LUNOUT          [I]     If > 0 Print out to unit LUNOUT
C_
C-   Outputs :  ID_OUT(*)       [I]     ISAJET id's of outputed particles
C-              TID_OUT(*)      [I]     Family membership (TP or TB) for
C-                                      outputed particles - ISAJET id's of the
C-                                      very first quark (TP or TB)
C-              PID_OUT(*)      [I]     ISAJET id's of the closest parent;
C-                                      set to zero for TP and TB
C-              P_OUT(8,*)      [R]     4-vector (+ mass, phi, theta, eta)
C-                                      of outputed particles
C-              NP_OUT          [I]     Number of outputed particles
C-
C-   Created  19-JUN-1991   Stan M. Krzywdzinski, Harrison B. Prosper
C-   Modified  9-APR-1992   Stan M. Krzywdzinski 
C-                          Added PID_OUT argument.
C-   Modified 14-APR-1992   Stan M. Krzywdzinski
C-                          Replaced GET_ISAJ_DECAY by:
C-                                                     GET_ISA*_PARTON
C-                                                and  GET_ISA*_CHILDREN 
C-                          Added search for leptons from BB and BT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TOPID, LUNOUT, ID_OUT(*), TID_OUT(*), PID_OUT(*), NP_OUT
      REAL P_OUT(8,*)
      INCLUDE 'D0$PARAMS:ISAJET_CODES.DEF'
C----------------------------------------------------------------------
      LOGICAL SEL_ELEC_JETS
      LOGICAL OK, DEBUG
      LOGICAL WN_FOUND, BB_FOUND, WP_FOUND, BT_FOUND
      INTEGER     NPMAX
      PARAMETER ( NPMAX = 10 )
      INTEGER IDP,        IDPB(NPMAX), NP, IERP
      REAL    PP(8,NPMAX)
      INTEGER     NCMAX
      PARAMETER ( NCMAX = 100 )
      INTEGER IDC(NCMAX), IDCB(NCMAX), NC, IERC, NC1, NC2
      REAL    PC(8,NCMAX)
      INTEGER FLAG, I, NLIN
      CHARACTER LABEL*8, LINE*80
C----------------------------------------------------------------------
C
      SEL_ELEC_JETS = .FALSE.
C
C ****  FOR TOPID < 0  :
C ****                  SELECT TB --> W- BB (GL)    (W- --> E- ANUE)
C ****                         TP --> W+ BT (GL)    (W+ --> non-leptons)
C ****  FOR TOPID > 0  :
C ****                  SELECT TP --> W+ BT (GL)    (W+ --> E+ NUE)
C ****                         TB --> W- BB (GL)    (W- --> non-leptons)
C
C ****  Determine correct signs
C
      IF ( TOPID .LT. 0 ) THEN
        FLAG =-1
      ELSE
        FLAG = 1
      ENDIF
C
      DEBUG = LUNOUT .GT. 0
      NP_OUT = 0
C
C **********************************************************************
C ****  Get TB  ...
C **********************************************************************
C
      IDPB(1) = 0
      CALL GET_ISAJ_PARTON  (FLAG*TOP,IDPB,PP,NP,IERP)
      IF ( IERP .NE. 0 ) GOTO 999
      IF ( NP   .NE. 1 ) GOTO 999
C ****  ... and decay products of TB
      CALL GET_ISAJ_CHILDREN(FLAG*TOP,IDPB(1),IDC(1),    IDCB(1),    
     &                       PC(1,1),    NC1,IERC)
      CALL GET_ISAQ_CHILDREN(FLAG*TOP,IDPB(1),IDC(NC1+1),IDCB(NC1+1),
     &                       PC(1,NC1+1),NC2,IERC)
      NC = NC1 + NC2
      IF ( NC .LE. 0 ) GOTO 999
C
      NP_OUT = NP_OUT + 1
      ID_OUT(NP_OUT) = FLAG*TOP
      TID_OUT(NP_OUT)= FLAG*TOP
      PID_OUT(NP_OUT) = 0
      CALL UCOPY(PP(1,1),P_OUT(1,NP_OUT),8)
C
      IF ( DEBUG ) THEN
        LINE = LABEL(FLAG*TOP)//' --> '
        NLIN = 13
        DO I =  1,NC
          LINE = LINE(1:NLIN)//LABEL(IDC(I))
          NLIN = NLIN + 8
        ENDDO
        WRITE(LUNOUT,'('' '',A)') LINE(1:NLIN)
CCC        WRITE(6,'('' '',A)') LINE(1:NLIN)
      ENDIF
C
C ****  Check for W- and BB
C
      WN_FOUND = .FALSE.
      BB_FOUND= .FALSE.
      DO I = 1,NC
        IF ( .NOT. WN_FOUND ) THEN
          WN_FOUND = IDC(I) .EQ. FLAG*W_BOSON
        ENDIF
        IF ( .NOT. BB_FOUND)  THEN
          BB_FOUND = IDC(I) .EQ. FLAG*BOTTOM
        ENDIF
      ENDDO
      OK = WN_FOUND .AND. BB_FOUND
      IF ( OK ) THEN
        DO I = 1,NC
          NP_OUT = NP_OUT + 1
          ID_OUT(NP_OUT) = IDC(I)
          TID_OUT(NP_OUT)= FLAG*TOP
          PID_OUT(NP_OUT)= FLAG*TOP
          CALL UCOPY(PC(1,I),P_OUT(1,NP_OUT),8)
        ENDDO
      ENDIF
C
C ****  Get W-  ...
C
      IDPB(1) = 0
      CALL GET_ISAJ_PARTON  (FLAG*W_BOSON,IDPB,PP,NP,IERP)
      IF ( IERP .NE. 0 ) GOTO 999
      IF ( NP   .NE. 1 ) GOTO 999
C ****  ... and decay products of W-
      CALL GET_ISAQ_CHILDREN(FLAG*W_BOSON,IDPB(1),IDC(1),    IDCB(1), 
     &                       PC(1,1),    NC,IERC)
      IF ( IERC .NE. 0 ) GOTO 999
C
      IF ( DEBUG ) THEN
        LINE = ' '
        LINE = LINE(1:13)//LABEL(FLAG*W_BOSON)//' --> '
        NLIN = 26
        DO I =  1,NC
          LINE = LINE(1:NLIN)//LABEL(IDC(I))
          NLIN = NLIN + 8
        ENDDO
        WRITE(LUNOUT,'('' '',A)') LINE(1:NLIN)
CCC        WRITE(6,'('' '',A)') LINE(1:NLIN)
      ENDIF
C
      OK = OK .AND. (NC .EQ. 2)
      OK = OK .AND. (((IDC(1) .EQ. -FLAG*ELECTRON) .AND.
     &                (IDC(2) .EQ.  FLAG*NUE     ))     .OR.
     &               ((IDC(1) .EQ.  FLAG*NUE)      .AND.
     &                (IDC(2) .EQ. -FLAG*ELECTRON)))
      IF (OK) THEN
        DO I = 1,NC
          NP_OUT = NP_OUT + 1
          ID_OUT(NP_OUT) = IDC(I)
          TID_OUT(NP_OUT)= FLAG*TOP
          PID_OUT(NP_OUT)= FLAG*W_BOSON
          CALL UCOPY(PC(1,I),P_OUT(1,NP_OUT),8)
        ENDDO
      ENDIF
C
C ****  Get BB  ...
C
      IDPB(1) = 0
      CALL GET_ISAQ_PARTON  (FLAG*BOTTOM,IDPB,PP,NP,IERP)
      IF ( (IERP .EQ. 0) .AND. (NP .EQ. 1) ) THEN
C ****  ... and possible lepton decay product of BB
        CALL GET_ISAL_CHILDREN(FLAG*BOTTOM,IDPB(1),IDC(1),    IDCB(1),
     &                         PC(1,1),    NC,IERC)
        IF ( IERC .EQ. 0 ) THEN
C
          IF ( DEBUG ) THEN
            LINE = ' '
            LINE = LINE(1:21)//LABEL(FLAG*BOTTOM)//' --> '
            NLIN = 34
            DO I =  1,NC
              IF (NLIN .GE. 72) THEN
                WRITE(LUNOUT,'('' '',A)') LINE(1:NLIN)
CCC                WRITE(6,'('' '',A)') LINE(1:NLIN)
                LINE = ' '
                NLIN = 34
              ENDIF
              LINE = LINE(1:NLIN)//LABEL(IDC(I))
              NLIN = NLIN + 8
            ENDDO
            IF (NLIN .GE. 72) THEN
              WRITE(LUNOUT,'('' '',A)') LINE(1:NLIN)
CCC              WRITE(6,'('' '',A)') LINE(1:NLIN)
              LINE = ' '
              NLIN = 34
            ENDIF
            LINE = LINE(1:NLIN)//'...'
            NLIN = NLIN + 8
            WRITE(LUNOUT,'('' '',A)') LINE(1:NLIN)
CCC            WRITE(6,'('' '',A)') LINE(1:NLIN)
          ENDIF
          IF (OK) THEN
            DO I = 1,NC
              NP_OUT = NP_OUT + 1
              ID_OUT(NP_OUT) = IDC(I)
              TID_OUT(NP_OUT)= FLAG*TOP
              PID_OUT(NP_OUT)= FLAG*BOTTOM
              CALL UCOPY(PC(1,I),P_OUT(1,NP_OUT),8)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
C
C **********************************************************************
C ****  Get TP  ...
C **********************************************************************
C
      IDPB(1) = 0
      CALL GET_ISAJ_PARTON  (-FLAG*TOP,IDPB,PP,NP,IERP)
      IF ( IERP .NE. 0 ) GOTO 999
      IF ( NP   .NE. 1 ) GOTO 999
C ****  ... and decay products of TP
      CALL GET_ISAJ_CHILDREN(-FLAG*TOP,IDPB(1),IDC(1),    IDCB(1),    
     &                       PC(1,1),    NC1,IERC)
      CALL GET_ISAQ_CHILDREN(-FLAG*TOP,IDPB(1),IDC(NC1+1),IDCB(NC1+1),
     &                       PC(1,NC1+1),NC2,IERC)
      NC = NC1 + NC2
      IF ( NC .LE. 0 ) GOTO 999
C
      NP_OUT = NP_OUT + 1
      ID_OUT(NP_OUT) = -FLAG*TOP
      TID_OUT(NP_OUT)= -FLAG*TOP
      PID_OUT(NP_OUT)= 0
      CALL UCOPY(PP,P_OUT(1,NP_OUT),8)
      IF ( DEBUG ) THEN
        LINE = LABEL(-FLAG*TOP)//' --> '
        NLIN = 13
        DO I =  1,NC
          LINE = LINE(1:NLIN)//LABEL(IDC(I))
          NLIN = NLIN + 8
        ENDDO
        WRITE(LUNOUT,'('' '',A)') LINE(1:NLIN)
CCC        WRITE(6,'('' '',A)') LINE(1:NLIN)
      ENDIF
C
C ****  Check for W+ and BT
C
      WP_FOUND = .FALSE.
      BT_FOUND= .FALSE.
      DO I = 1,NC
        IF ( .NOT. WP_FOUND ) THEN
          WP_FOUND = IDC(I) .EQ. -FLAG*W_BOSON
        ENDIF
        IF ( .NOT. BT_FOUND)  THEN
          BT_FOUND = IDC(I) .EQ. -FLAG*BOTTOM
        ENDIF
      ENDDO
      OK = OK .AND. WP_FOUND .AND. BT_FOUND
      IF ( OK ) THEN
        DO I = 1,NC
          NP_OUT = NP_OUT + 1
          ID_OUT(NP_OUT) = IDC(I)
          TID_OUT(NP_OUT)= -FLAG*TOP
          PID_OUT(NP_OUT)= -FLAG*TOP
          CALL UCOPY(PC(1,I),P_OUT(1,NP_OUT),8)
        ENDDO
      ENDIF
C
C ****  Get W+ ...
C
      IDPB(1) = 0
      CALL GET_ISAJ_PARTON  (-FLAG*W_BOSON,IDPB,PP,NP,IERP)
      IF ( IERP .NE. 0 ) GOTO 999
      IF ( NP   .NE. 1 ) GOTO 999
C ****  ... and decay products of W+ (must be hadrons only)
      CALL GET_ISAQ_CHILDREN(-FLAG*W_BOSON,IDPB(1),IDC(1),    IDCB(1),  
     &                       PC(1,1),    NC,IERC)
      IF ( IERC .NE. 0 ) GOTO 999
C
      IF ( DEBUG ) THEN
        LINE = ' '
        LINE = LINE(1:13)//LABEL(-FLAG*W_BOSON)//' --> '
        NLIN = 26
        DO I =  1,NC
          LINE = LINE(1:NLIN)//LABEL(IDC(I))
          NLIN = NLIN + 8
        ENDDO
        WRITE(LUNOUT,'('' '',A)') LINE(1:NLIN)
CCC        WRITE(6,'('' '',A)') LINE(1:NLIN)
      ENDIF
C
      IF ( OK ) THEN
        DO I =  1,NC
          IF ( (IDC(I) .EQ. FLAG*ELECTRON) .OR.
     &         (IDC(I) .EQ. -FLAG*NUE)     .OR.
     &         (IDC(I) .EQ. FLAG*MUON)     .OR.
     &         (IDC(I) .EQ. -FLAG*NUMU)    .OR.
     &         (IDC(I) .EQ. FLAG*TAU)      .OR.
     &         (IDC(I) .EQ. -FLAG*NUTAU)       ) GOTO 999
        ENDDO
        SEL_ELEC_JETS = .TRUE.
        DO I = 1,NC
          NP_OUT = NP_OUT + 1
          ID_OUT(NP_OUT) = IDC(I)
          TID_OUT(NP_OUT)= -FLAG*TOP
          PID_OUT(NP_OUT)= -FLAG*W_BOSON
          CALL UCOPY(PC(1,I),P_OUT(1,NP_OUT),8)
        ENDDO
      ENDIF
C
C ****  Get BT  ...
C
      IDPB(1) = 0
      CALL GET_ISAQ_PARTON  (-FLAG*BOTTOM,IDPB,PP,NP,IERP)
      IF ( (IERP .EQ. 0) .AND. (NP .EQ. 1) ) THEN
C ****  ... and possible lepton decay product of BT
        CALL GET_ISAL_CHILDREN(-FLAG*BOTTOM,IDPB(1),IDC(1),    IDCB(1),
     &                         PC(1,1),    NC,IERC)
        IF ( IERC .EQ. 0 ) THEN
C
          IF ( DEBUG ) THEN
            LINE = ' '
            LINE = LINE(1:21)//LABEL(-FLAG*BOTTOM)//' --> '
            NLIN = 34
            DO I =  1,NC
              IF (NLIN .GE. 72) THEN
                WRITE(LUNOUT,'('' '',A)') LINE(1:NLIN)
CCC                WRITE(6,'('' '',A)') LINE(1:NLIN)
                LINE = ' '
                NLIN = 34
              ENDIF
              LINE = LINE(1:NLIN)//LABEL(IDC(I))
              NLIN = NLIN + 8
            ENDDO
            IF (NLIN .GE. 72) THEN
              WRITE(LUNOUT,'('' '',A)') LINE(1:NLIN)
CCC              WRITE(6,'('' '',A)') LINE(1:NLIN)
              LINE = ' '
              NLIN = 34
            ENDIF
            LINE = LINE(1:NLIN)//'...'
            NLIN = NLIN + 8
            WRITE(LUNOUT,'('' '',A)') LINE(1:NLIN)
CCC            WRITE(6,'('' '',A)') LINE(1:NLIN)
          ENDIF
          IF (OK) THEN
            DO I = 1,NC
              NP_OUT = NP_OUT + 1
              ID_OUT(NP_OUT) = IDC(I)
              TID_OUT(NP_OUT)= -FLAG*TOP
              PID_OUT(NP_OUT)= -FLAG*BOTTOM
              CALL UCOPY(PC(1,I),P_OUT(1,NP_OUT),8)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
