*
* Included file: QARRAY.INC - contains definition or query array for the
*                RZ query language.
*
*                                      -Ed Gardella February 1987
*
      INTEGER QARRAY(1:20, 1:5),IPTRQ

* Values for array elements indexes:
      INTEGER KEYNUM, LIMIT, RELOP, CONJUN

      PARAMETER (KEYNUM = 1)
      PARAMETER (LIMIT  = 2)
      PARAMETER (RELOP  = 3)
      PARAMETER (CONJUN = 4)
*
* The query array is organized as follows:
*
*    QARRAY(20,5) -
*
*          QARRAY(i,KEYNUM) - key number (1..10) for this limit
*          QARRAY(i,LIMIT) - limit
*          QARRAY(i,RELOP) - relational operator (EQ, NE, GT, GE, LT, LE)
*          QARRAY(i,CONJUN) - AND, OR
*
* The NOT operator isn't stored in the query array, but is taken care of
* by taking the complementary relational operator when the query array is
* created, i.e., ~ A > B becomes A <= B and is stored as such in the query
* array.
*
*   IPTRQ - is the pointer to the next position to be filled in the
*           query array, so it is always 1 greater than the number of
*           queries in the array.
*
* Values for parameters of search
*
*   Relational operators:
*
      INTEGER EQOP, GEOP, GTOP, LEOP, LTOP, NEOP

* Token identifiers
      PARAMETER ( EQOP =  1 )
      PARAMETER ( LTOP =  2 )
      PARAMETER ( GTOP =  4 )
      PARAMETER ( NEOP =  8 )
      PARAMETER ( GEOP = 16 )
      PARAMETER ( LEOP = 32 )
* 
*   The conjuction, disjunction and negation operators
*
      INTEGER ANDOP, OROP, NOTOP 
*
      PARAMETER ( NOTOP  =  64 )
      PARAMETER ( ANDOP  = 128 )
      PARAMETER ( OROP   = 256 )
*
