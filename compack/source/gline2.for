      SUBROUTINE GLINE2(LABLIN,PARLIN,STARTPAR,MAXPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output a parameter list in LINE mode for GETDIS.
C-   Same as GLINE0 but it has an extra input parameter STARTPAR, this lets
C-   you decide what is the staring point (element) in the display. 
C-
C-   Inputs  : LABLIN [C*]: Character array of labels, one for each parameter 
C-             PARLIN [C*]: Character array of current value of each parameter
C-             STARTPAR[I]: Number to start the display in the parameter list
C-             MAXPAR  [I]: Maximun number of parameters to display
C-   Outputs : None
C-   Controls: None
C-
C-   Created  12-JUN-1990   Lupe Howell - Based in the routine 
C-                          GLINE0 by Jan S. Hoftun 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAXPAR,STARTPAR      
      CHARACTER*(*) LABLIN(1:MAXPAR),PARLIN(1:MAXPAR)
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      CHARACTER*80 MSGLIN,BLNK
      LOGICAL GETDEV                     
      INTEGER I,TRULEN,K,J,LI,CU,LIBGET,LIBLIN,ISTAT,LIBPUT
      DATA BLNK/' '/      
C----------------------------------------------------------------------
      CALL OUTMSG(' ')
      DO I=STARTPAR, MAXPAR             
         K=MIN0(TRULEN(LABLIN(I)),40)       
         J=MIN0(TRULEN(PARLIN(I)),30)    
         IF(TRMFLG.AND.GETDEV()) THEN
C
C ****  Inserting a number in front of every element (1->,2->)
C
            WRITE(MSGLIN,980) I,LABLIN(I)(1:K) 
980         FORMAT(I3,'-> ',A,' ')
            ISTAT=LIBGET(LI,CU)                  
            IF(LI.EQ.PBROWS) ISTAT=LIBLIN(' ',0)
C
C ****  Displaying the elements of the array
C
            ISTAT=LIBPUT(MSGLIN(1:K+8),LI,1,1)
            ISTAT=LIBPUT(PARLIN(I)(1:J),LI,50,0)
            ISTAT=LIBLIN(' ',0)
         ELSE
            WRITE(MSGLIN,981) I,LABLIN(I)(1:K),BLNK(1:50-(K+8)),
     *                        PARLIN(I)(1:J) 
981         FORMAT(' ',I3,'-> ',A,A,A)
            CALL OUTMSG(MSGLIN)
         ENDIF
      ENDDO
      CALL OUTMSG(' ')
      RETURN
      END
