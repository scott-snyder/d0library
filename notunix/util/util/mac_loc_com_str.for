      LOGICAL FUNCTION MAC_LOC_COM_STR(In_str,Com_Str)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compares In_Str and Out_Str, true if
C-    The Non-Blank characters in Com_Str are the First Non-Blank
C-    caharcters in Com_Str. We UPCASE Both strings before comparison
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-APR-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Character*(*) In_str,Com_Str
      Character*80 Temp_In_Str,Temp_Com_Str
      INTEGER eye,eye1,jay,jay1,kay,Min_Com_Len
C----------------------------------------------------------------------
      Call UpCase (In_Str,Temp_In_Str)
      Call UpCase (Com_Str, Temp_Com_Str)
      Call Swords(Temp_In_Str,eye,jay,kay)
      Call Swords(Temp_Com_Str,eye1,jay1,Min_Com_Len)
      Mac_Loc_Com_Str = (Temp_Com_Str(eye1:jay1).eq.
     +  Temp_In_Str(eye:(eye+Min_Com_Len-1)))
  999 RETURN
      END
