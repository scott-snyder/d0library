      SUBROUTINE PRSCOM(COMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Parse a command
C-
C-   Inputs  : COMAND: Command to be parsed.
C-   Outputs : None
C-   Controls: POS is set to the number of the found command.
C-
C-   Created   1-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMAND
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER IJNT,INT,K,J,JOLD,IEND,TRULEN,JEND,I,L,IAMBIG
      CHARACTER*80 TRANUP,COM,COM1
      LOGICAL AMBIG
C----------------------------------------------------------------------
      POS=0
      AMBIG=.FALSE.
C
C     Find the space character in command following the last real character
C     OR the first ';' to indicate direct sub-level
C
      IEND=TRULEN(COMAND)+1
      COMPRT(0)=COMAND
      IF(IEND.GT.2) THEN
        J=INDEX(COMAND(2:IEND),' ')
      ELSE
        J=0
      ENDIF
      L=INDEX(COMAND(1:IEND),';')
      IF(L.GT.0) THEN
        NXTLEV=L
      ELSEIF(NXTLEV.GT.0) THEN
        NXTLEV=0                              !Indicate end of command line
      ENDIF
      K=INDEX(COMAND(1:IEND),'"')
      IF(K.GT.0.AND.K.LT.NXTLEV) THEN         !Exclude ';' inside character string
        NXTLEV=0
      ENDIF
      IF(NXTLEV.GT.0) THEN                    !There is a nested command here
        IF(J.GE.NXTLEV) THEN
          J=NXTLEV-1
        ENDIF
        NXTLEV=NXTLEV+1
      ENDIF
      JOLD=J
      COM(1:IEND)=TRANUP(COMAND(1:IEND))
  800 CONTINUE
      AMBIG = .FALSE.                   ! try again RR
      DO 901 K=1,MAXLIN(CURLEV)
C
C     First check if command contains a complete menu line
C
        IJNT=TRULEN(MENLIN(K,CURLEV))
        COM1(1:IJNT)=TRANUP(MENLIN(K,CURLEV)(1:IJNT))
        IF(COM(1:IEND).EQ.COM1(1:IJNT)) THEN    ! seems wrong!RR
          POS=K
          JOLD=IJNT
          GOTO 900
        ELSE
          INT=INDEX(TRANUP(MENLIN(K,CURLEV)(1:J)),COM(1:J))
          IF(INT.GT.0) THEN
            IF(POS.EQ.0) THEN
              POS=K
              AMBIG=.FALSE.            ! Not ambigous anymore
            ELSE
              AMBIG=.TRUE.             ! Ambiguity found
              IAMBIG=JOLD
              POS=0
              J=JOLD+INDEX(COMAND(JOLD+2:IEND),' ')
              IF(J.GT.JOLD) THEN
                JOLD=J
                GOTO 800
              ELSE
                GOTO 8000
              ENDIF
            ENDIF
          ENDIF
        ENDIF
  901 CONTINUE
  900 CONTINUE
      IF(POS.GT.0) THEN
C
C     Parse command line to select fields for input to commands
C     Put subfields in COMPRT and total number in COMNUM
C
        COMNUM=0
        IF(.NOT.FULSCR) THEN
          DO K=1,MAXCOM
            COMPRT(K)=' '
          ENDDO
          JOLD=JOLD+2
          IF(JOLD.LE.IEND) THEN
            COM=COMAND(JOLD:IEND)
            DO 9021 K=1,MAXCOM-1
 9011         CONTINUE
              JEND=TRULEN(COM)
              IF(JEND.EQ.0) GOTO 9022
              I=INDEX(COM,' ')
              IF(I.EQ.1) THEN
                COM=COM(2:JEND)
                GOTO 9011
              ENDIF
              J=INDEX(COM(1:1),'"')
              IF(J.EQ.0) THEN
                COMPRT(K)=TRANUP(COM(1:I-1))
                JOLD=I+1
              ELSE
                JOLD=INDEX(COM(2:JEND),'"')
                IF(JOLD.GT.0) THEN
                  COMPRT(K)=COM(2:JOLD)
                  JOLD=JOLD+3
                ELSE
                  COMPRT(K)=COM(2:JEND)
                  JOLD=JEND
                ENDIF
              ENDIF
              COMNUM=K
              IF(JOLD.LE.JEND) THEN
                COM=COM(JOLD:JEND)
              ELSE
                GOTO 9022
              ENDIF
 9021       CONTINUE
 9022       CONTINUE
            IF(JOLD.LE.JEND) THEN
              COMNUM=MAXCOM
              COMPRT(MAXCOM)=COM(1:JEND)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
 8000 CONTINUE
      IF(AMBIG) THEN
        CALL OUTMSG('0Ambigous command: '//TRANUP(COM(1:IAMBIG))
     &       //CHAR(7))
        CALL PFWAIT
        PF=4
      ENDIF
      RETURN
      END
