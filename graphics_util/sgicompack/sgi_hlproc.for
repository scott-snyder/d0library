C VAX/DEC CMS REPLACEMENT HISTORY, Element SGI_HLPROC.FOR
C *1     9-FEB-1993 12:47:01 LUPE "SGI version, displays help strings"
C VAX/DEC CMS REPLACEMENT HISTORY, Element SGI_HLPROC.FOR
      SUBROUTINE HLPROC (COMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display help information for one command.
C-
C-   Inputs  : COMAND [I ]: Number of command to display help information
C-                          for Display menu if 0
C-   Outputs : None
C-
C-   Created 09-FEB-1993  Lupe howell, Based on the HLPROC by J. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER COMAND
C
      INTEGER I,K,J,M,N
      INTEGER TRULEN,COLUMN,L,PBCOLS,PBROWS
      LOGICAL NEWLIN
      CHARACTER*132 BLNK
      CHARACTER*132 LINS(64)
      CHARACTER*2048 HLPINF
C
      INCLUDE 'D0$GRAPHICS_UTIL$SGICOMPACK:MAINPACK.INC'
C
      DATA BLNK/' '/
      DATA PBCOLS/70/
      DATA PBROWS/25/
C----------------------------------------------------------------------
      IF ( MAXLIN(MENULEV) .GT. 0 ) THEN
C
C *** Split up HLPTXT in lines filling the screen properly
C
        CALL STRFET(HELP_COOKIES(COMAND,MENULEV), HLPINF)
        K=1
        I=0
        J=TRULEN(HLPINF)
C
C *** Getting the length of the string
C
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
C
C *** Displaying the string by line
C
          K=1
          DO J=1,I
            K=K+1
            CALL OUTMSG(' '//BLNK(1:COLUMN)//LINS(J))
          ENDDO
        ELSE
          CALL OUTMSG(' '//HLPINF)
        ENDIF
      ELSE
        CALL OUTMSG(' No items defined for this level!'//CHAR(7))
      ENDIF
  998 CONTINUE
      RETURN
      END
