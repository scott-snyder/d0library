      SUBROUTINE ZBINF2(I2UNIT,NOIN2)
C---------------------------------------------------------------------
C-                                                                   -
C-    Dialog to select second Zebra input file,after ZBINPF called   -
C-                                                                   -
C-    OUTPUT:                                                        -
C-    I2UNIT= input file unit for the second one                     -
C-    NOIN2  = .FALSE. if an input file was opened                   -
C-                                                                   -
C-    ENTRY NAMIN2(NAME2)                                            -
C-    OUTPUT:                                                        -
C-    NAME2=  name of the second input file                          -
C-                                                                   -
C-                       SDP Oct,1986, xia yi modified Oct.1988      -
C-                                                                   -
C---------------------------------------------------------------------
      IMPLICIT NONE
C
C        Communications with COMPACK
      INTEGER PFNUM
C
      INTEGER I2UNIT
      LOGICAL NOIN2
      INTEGER ERR
      CHARACTER*64 MESG,NAMIN
      CHARACTER*(*) NAME2
      LOGICAL FIRST,OK
      CHARACTER*1 XOPT
      SAVE FIRST
      DATA FIRST/.FALSE./
C---------------------------------------------------------------------
C
      IF(FIRST) THEN
        I2UNIT=0
        NOIN2=.TRUE.
        FIRST=.TRUE.
      ENDIF
    1 CONTINUE
      MESG=' If name of input file is NONE no file will be open.'
      CALL OUTMSG(MESG)
C
C          release input unit
      IF(I2UNIT.NE.0) THEN
        CALL FZENDI(I2UNIT,'T')
        CLOSE(I2UNIT)
        CALL RLUNIT(1,I2UNIT,ERR)  
      ENDIF
      CALL GETPAR(1,' Name of the second input file >','C',NAMIN)
C
      IF(PFNUM().EQ.4) RETURN
C
      IF(NAMIN(1:4).NE.'NONE') THEN
          MESG=' Input file modes are X (exchange), G (special X)'//
     &      ', or N (native)'
          CALL OUTMSG(MESG)
          CALL GETPAR(1,' File mode X,G or N ? [N]:>' ,'C',XOPT)
          CALL UPCASE(XOPT,XOPT)
          IF(XOPT.NE.'X'.AND.XOPT.NE.'G') XOPT=' '
        CALL EVOPIN(NAMIN,XOPT,I2UNIT,OK)
        NOIN2=.NOT.OK
        IF(.NOT.OK) THEN
          MESG=' Unable to open or close file:'
          CALL OUTMSG(MESG)
          CALL OUTMSG(NAMIN)
          GOTO 1
        ENDIF
      ENDIF
C
      RETURN
C
      ENTRY NAMIN2(NAME2)
      NAME2=NAMIN
      RETURN
      END
