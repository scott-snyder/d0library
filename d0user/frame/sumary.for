      SUBROUTINE SUMARY
C---------------------------------------------------------------------
C-                                                                   -
C-        handle all summaries                                       -
C-                                                                   -
C-       SDP Apr.,1987                                               -
C-                                                                   -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
      CHARACTER*8 COMAND
C
C
    1   CALL MENUDO('SUMMARIES','SUMMARIES',COMAND)
C
        IF(COMAND(1:6).EQ.'HISPAK') THEN
            CALL HISPAK(.FALSE.)
            GOTO 1
C
        ELSE IF(COMAND(1:8).EQ.'STANDARD') THEN  ! select standard output
          CALL SETHPR('STANDARD')
          CALL USETSS
          GOTO 1
C
        ELSE IF(COMAND(1:8).EQ.'USER OUT') THEN  ! user output
          CALL USRUSM
          GOTO 1
C
        ELSE IF(COMAND(1:6).EQ.'OUTPUT') THEN    ! standard output
          CALL STDOUT
          GOTO 1
C
        ELSE IF(COMAND(1:6).EQ.'STATUS') THEN
          CALL USRPST
          GOTO 1
C
        ELSE IF(COMAND(1:4).EQ.'QUIT') THEN
          CALL QUIT
          GOTO 1
C
        ELSE IF(COMAND(1:6).EQ.'FINISH') THEN
          CALL FINISH
          GOTO 1
C
        ENDIF
C
      RETURN
      END
