      SUBROUTINE MU_COOR_PARSE(ARG_LIST,L1_DATUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Interpret muon data string from COOR_SIM, encoding
C-                         it into a programming mask for MU_TRIG_MON.
C-
C-   Inputs  : ARG_LIST - Input data string from COOR_SIM
C-
C-   Outputs : L1_DATUM - Mask bits corresponding to the input data string
C-
C-   Original    OCT-1992   MACC
C-   Created  23-NOV-1992   G. Lima
C-   
C----------------------------------------------------------------------
      IMPLICIT NONE
C<<
C.. Global variables
      CHARACTER*(*) ARG_LIST
      INTEGER L1_DATUM
C<<
C.. Local variables
      INTEGER I, NN, NG, NNG, NF2
      INTEGER J,N,STRLEN
      LOGICAL SEND
      INTEGER L1_DATA(17)
      CHARACTER*2 GEOM(17), GEOMS(17)
      CHARACTER*1 CHAR
      CHARACTER*10 THRSH
C<<
      DATA GEOMS / 'PU',                                ! pulser bit
     &     'CF', 'WN', 'WS', 'ON', 'OS', 'SN', 'SS',    ! trigger regions
     &     'Y1', 'Y2', 'Y3', 'Y4', 'X1', 'X2', 'X3',    ! rapidity areas
     &     'EN', 'ES'/                                  ! double sectors
C<<
      DATA L1_DATA / '1'X,                                 ! pulser bit
     &    '02'X, '04'X, '08'X, '10'X, '20'X, '40'X, '80'X, ! trigger regions
     &    '02'X, '0E'X, '3E'X, 'FE'X, 'FC'X, 'F0'X, 'C0'X, ! rapidity areas
     &    '14'X, '28'X /                                   ! double sectors
C<<
C----------------------------------------------------------------------
      NN = 0
      NG = 0
      STRLEN = LEN(ARG_LIST)
      SEND = .TRUE.
      DOWHILE ( SEND )
        NN = NN + 1
        CHAR = ARG_LIST(NN:NN)
        IF ( CHAR .EQ. '(' ) THEN
          NG = NG + 1
          GEOM(NG) = ARG_LIST(NN+1:NN+2)
          NN = NN + 2
        ELSEIF ( CHAR .EQ. '+' ) THEN
          NG = NG + 1
          GEOM(NG) = ARG_LIST(NN+1:NN+2)
          NN = NN + 2
        ELSEIF ( CHAR .EQ. ')' ) THEN
          CALL WORD(ARG_LIST(NN+1:STRLEN),I,J,N)
          THRSH = ARG_LIST(NN+I+1:NN+J)               !HIGH-LOW THRESHOLD
          SEND = .FALSE.
        ELSEIF ( CHAR .EQ. ' ' ) THEN
          SEND = .FALSE.
        ENDIF
      ENDDO
C<<
      IF ( NG .EQ. 0 ) THEN
        CALL ERRMSG('MU-PARSE','MU_COOR_PARSE',' No trigger regions '
     &    //'specified in MUON PROGRAMMING FILE','I')
        RETURN
      ENDIF
C<<
C<<
c.. Decode trigger regions

      	L1_DATUM = 0
	IF(THRSH.EQ.'HIGH')L1_DATUM=256

      NF2 = 0
      DO NNG = 1, NG
        DO I = 1, 17
          IF ( GEOM(NNG) .EQ. GEOMS(I) ) THEN
            NF2 = NF2 + 1
            L1_DATUM = L1_DATUM + L1_DATA(I)
            GO TO 99
          ENDIF
        ENDDO
   99   CONTINUE
      ENDDO
C<<
      IF ( NF2 .NE. NG ) THEN
        CALL ERRMSG('MU-PARSE','MU_COOR_PARSE',' Undefined trigger '
     &    //'regions specified in MUON PROGRAMMING FILE','F')
        RETURN
      ENDIF
C<<
C----------------------------------------------------------------------
  999 RETURN
      END
