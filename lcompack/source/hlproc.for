      SUBROUTINE HLPROC (COMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display help information for one command or
C-                         whole menu if no command chosen.
C-
C-   Inputs  : COMAND: Number of command to display help information for
C-                     Display menu if 0
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Modified   16-MAY-1991   Scott Snyder
C-    Fetch help string with STRFET.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C
      INTEGER COMAND,ISTAT,LIBPUT,I,K,J,M,LIBBIG,N
      INTEGER READPF,TRULEN,COLUMN,LIBCUR,IBLNK,L
      LOGICAL NEWLIN
      CHARACTER*132 BLNK,TOPS
      CHARACTER*132 LINS(64)
      CHARACTER*2048 HLPINF
      DATA BLNK/' '/
C----------------------------------------------------------------------
      PF=0
      IF(MAXLIN(CURLEV).GT.0) THEN
        IF(COMAND.EQ.0) THEN
          CALL LINES0(MENLIN(1,CURLEV),MAXLIN(CURLEV),NUMCOL)
        ELSE
          COLUMN=PBCOLS/8
          J=TRULEN(MENLIN(COMAND,CURLEV))
          IBLNK=(PBCOLS-J-11)/2
          TOPS='Help for "'//MENLIN(COMAND,CURLEV)(1:J)//'"'
          CALL OUTMSG('0'//BLNK(1:IBLNK)//TOPS)
          CALL OUTMSG(' ')
C
C       Split up HLPTXT in lines filling the screen properly
C
          CALL STRFET(HELP_COOKIES(COMAND, CURLEV), HLPINF)
          K=1
          I=0
          J=TRULEN(HLPINF)
          IF(INDEX(HLPINF,CHAR(13)).EQ.0) THEN
            NEWLIN=.TRUE.
            DO WHILE (K.LT.J)
              I=I+1
              N=PBCOLS-2*COLUMN
              IF(NEWLIN) THEN
                N=N-2
                LINS(I)='  '//HLPINF(K:N+K-1)
              ELSE
                LINS(I)=HLPINF(K:N+K-1)
              ENDIF
              L=TRULEN(LINS(I))
              IF(L.GE.N) THEN
                DO WHILE (LINS(I)(L:L).NE.' ')
                  L=L-1
                  N=N-1
                ENDDO
                LINS(I)=LINS(I)(1:L)
              ENDIF
              M=INDEX(LINS(I)(1:TRULEN(LINS(I))),'.  ')
              IF(M.GT.0) THEN
                LINS(I)=LINS(I)(1:M)
                K=K+M+1
                I=I+1
                LINS(I)=' '
                NEWLIN=.TRUE.
              ELSE
                NEWLIN=.FALSE.
                K=N+K
              ENDIF
            ENDDO
            K=1
            DO J=1,I
              K=K+1
              CALL OUTMSG(' '//BLNK(1:COLUMN)//LINS(J))
            ENDDO
          ELSE
            CALL OUTMSG(' '//HLPINF)
          ENDIF
          CALL OUTMSG('0')
        ENDIF
      ELSE
        CALL OUTMSG('0No items defined for this level!'//CHAR(7))
        CALL PFWAIT
      ENDIF
  998 CONTINUE
      RETURN
      END
