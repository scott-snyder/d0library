/* !*** MODULE $SMGDEF *** */
/* !  Definitions for RTL Screen Management Facility */
/* ! */
/* !  Input terminator codes */
/* ! */
/* ! */
/* !  Control characters */
/* ! */
#define SMG$K_TRM_NULL_CHAR 0x00000000    /* !  NUL */
#define SMG$K_TRM_CTRLA 0x00000001    /* !  SOH */
#define SMG$K_TRM_CTRLB 0x00000002    /* !  STX */
#define SMG$K_TRM_CTRLC 0x00000003    /* !  ETX */
#define SMG$K_TRM_CTRLD 0x00000004    /* !  EOT */
#define SMG$K_TRM_CTRLE 0x00000005    /* !  ENQ */
#define SMG$K_TRM_CTRLF 0x00000006    /* !  ACK */
#define SMG$K_TRM_CTRLG 0x00000007    /* !  BEL */
#define SMG$K_TRM_CTRLH 0x00000008    /* !  BS */
#define SMG$K_TRM_CTRLI 0x00000009    /* !  HT */
#define SMG$K_TRM_CTRLJ 0x0000000A    /* !  LF */
#define SMG$K_TRM_CTRLK 0x0000000B    /* !  VT */
#define SMG$K_TRM_CTRLL 0x0000000C    /* !  FF */
#define SMG$K_TRM_CTRLM 0x0000000D    /* !  CR */
#define SMG$K_TRM_CTRLN 0x0000000E    /* !  SO */
#define SMG$K_TRM_CTRLO 0x0000000F    /* !  SI */
#define SMG$K_TRM_CTRLP 0x00000010    /* !  DLE */
#define SMG$K_TRM_CTRLQ 0x00000011    /* !  DC1 */
#define SMG$K_TRM_CTRLR 0x00000012    /* !  DC2 */
#define SMG$K_TRM_CTRLS 0x00000013    /* !  DC3 */
#define SMG$K_TRM_CTRLT 0x00000014    /* !  DC4 */
#define SMG$K_TRM_CTRLU 0x00000015    /* !  NAK */
#define SMG$K_TRM_CTRLV 0x00000016    /* !  SYN */
#define SMG$K_TRM_CTRLW 0x00000017    /* !  ETB */
#define SMG$K_TRM_CTRLX 0x00000018    /* !  CAN */
#define SMG$K_TRM_CTRLY 0x00000019    /* !  EM */
#define SMG$K_TRM_CTRLZ 0x0000001A    /* !  SUB */
#define SMG$K_TRM_ESCAPE 0x0000001B    /* !  ESC */
#define SMG$K_TRM_FS 0x0000001C    /* !  FS */
#define SMG$K_TRM_GS 0x0000001D    /* !  GS */
#define SMG$K_TRM_RS 0x0000001E    /* !  RS */
#define SMG$K_TRM_US 0x0000001F    /* !  US */
#define SMG$K_TRM_SPACE 0x00000020    /* !  SP */
#define SMG$K_TRM_EXCLAMATION_POINT 0x00000021    /* !  ! */
#define SMG$K_TRM_DOUBLE_QUOTE 0x00000022    /* !  " */
#define SMG$K_TRM_NUMBER_SIGN 0x00000023    /* !  # */
#define SMG$K_TRM_DOLLAR_SIGN 0x00000024    /* !  $ */
#define SMG$K_TRM_PERCENT_SIGN 0x00000025    /* !  % */
#define SMG$K_TRM_AMPERSAND 0x00000026    /* !  & */
#define SMG$K_TRM_QUOTE 0x00000027    /* !  ' */
#define SMG$K_TRM_LEFT_PAREN 0x00000028    /* !  ( */)
#define SMG$K_TRM_RIGHT_PAREN 0x00000029    /* !  ) */
#define SMG$K_TRM_ASTERISK 0x0000002A    /* !  * */
#define SMG$K_TRM_PLUS_SIGN 0x0000002B    /* !  + */
#define SMG$K_TRM_COMMA_CHAR 0x0000002C    /* !  , */
#define SMG$K_TRM_DASH 0x0000002D    /* !  - */
#define SMG$K_TRM_DOT 0x0000002E    /* !  . */
#define SMG$K_TRM_SLASH 0x0000002F    /* !  / */
#define SMG$K_TRM_ZERO 0x00000030    /* !  0 */
#define SMG$K_TRM_ONE 0x00000031    /* !  1 */
#define SMG$K_TRM_TWO 0x00000032    /* !  2 */
#define SMG$K_TRM_THREE 0x00000033    /* !  3 */
#define SMG$K_TRM_FOUR 0x00000034    /* !  4 */
#define SMG$K_TRM_FIVE 0x00000035    /* !  5 */
#define SMG$K_TRM_SIX 0x00000036    /* !  6 */
#define SMG$K_TRM_SEVEN 0x00000037    /* !  7 */
#define SMG$K_TRM_EIGHT 0x00000038    /* !  8 */
#define SMG$K_TRM_NINE 0x00000039    /* !  9 */
#define SMG$K_TRM_COLON 0x0000003A    /* !  : */
#define SMG$K_TRM_SEMICOLON 0x0000003B    /* !  ; */
#define SMG$K_TRM_LESS_THAN 0x0000003C    /* !  < */
#define SMG$K_TRM_EQUAL 0x0000003D    /* !  = */
#define SMG$K_TRM_GREATER_THAN 0x0000003E    /* !  > */
#define SMG$K_TRM_QUESTION_MARK 0x0000003F    /* !  ? */
#define SMG$K_TRM_AT_SIGN 0x00000040    /* !  @ */
#define SMG$K_TRM_UPPERCASE_A 0x00000041    /* !  A */
#define SMG$K_TRM_UPPERCASE_B 0x00000042    /* !  B */
#define SMG$K_TRM_UPPERCASE_C 0x00000043    /* !  C */
#define SMG$K_TRM_UPPERCASE_D 0x00000044    /* !  D */
#define SMG$K_TRM_UPPERCASE_E 0x00000045    /* !  E */
#define SMG$K_TRM_UPPERCASE_F 0x00000046    /* !  F */
#define SMG$K_TRM_UPPERCASE_G 0x00000047    /* !  G */
#define SMG$K_TRM_UPPERCASE_H 0x00000048    /* !  H */
#define SMG$K_TRM_UPPERCASE_I 0x00000049    /* !  I */
#define SMG$K_TRM_UPPERCASE_J 0x0000004A    /* !  J */
#define SMG$K_TRM_UPPERCASE_K 0x0000004B    /* !  K */
#define SMG$K_TRM_UPPERCASE_L 0x0000004C    /* !  L */
#define SMG$K_TRM_UPPERCASE_M 0x0000004D    /* !  M */
#define SMG$K_TRM_UPPERCASE_N 0x0000004E    /* !  N */
#define SMG$K_TRM_UPPERCASE_O 0x0000004F    /* !  O */
#define SMG$K_TRM_UPPERCASE_P 0x00000050    /* !  P */
#define SMG$K_TRM_UPPERCASE_Q 0x00000051    /* !  Q */
#define SMG$K_TRM_UPPERCASE_R 0x00000052    /* !  R */
#define SMG$K_TRM_UPPERCASE_S 0x00000053    /* !  S */
#define SMG$K_TRM_UPPERCASE_T 0x00000054    /* !  T */
#define SMG$K_TRM_UPPERCASE_U 0x00000055    /* !  U */
#define SMG$K_TRM_UPPERCASE_V 0x00000056    /* !  V */
#define SMG$K_TRM_UPPERCASE_W 0x00000057    /* !  W */
#define SMG$K_TRM_UPPERCASE_X 0x00000058    /* !  X */
#define SMG$K_TRM_UPPERCASE_Y 0x00000059    /* !  Y */
#define SMG$K_TRM_UPPERCASE_Z 0x0000005A    /* !  Z */
#define SMG$K_TRM_LEFT_BRACKET 0x0000005B    /* !  [ */
#define SMG$K_TRM_BACKSLASH 0x0000005C    /* !  \ */
#define SMG$K_TRM_RIGHT_BRACKET 0x0000005D    /* !  ] */
#define SMG$K_TRM_CARET 0x0000005E    /* !  ^ */
#define SMG$K_TRM_UNDERLINE 0x0000005F    /* !  _ */
#define SMG$K_TRM_GRAVE_ACCENT 0x00000060    /* !  ` */
#define SMG$K_TRM_LOWERCASE_A 0x00000061    /* !  a */
#define SMG$K_TRM_LOWERCASE_B 0x00000062    /* !  b */
#define SMG$K_TRM_LOWERCASE_C 0x00000063    /* !  c */
#define SMG$K_TRM_LOWERCASE_D 0x00000064    /* !  d */
#define SMG$K_TRM_LOWERCASE_E 0x00000065    /* !  e */
#define SMG$K_TRM_LOWERCASE_F 0x00000066    /* !  f */
#define SMG$K_TRM_LOWERCASE_G 0x00000067    /* !  g */
#define SMG$K_TRM_LOWERCASE_H 0x00000068    /* !  h */
#define SMG$K_TRM_LOWERCASE_I 0x00000069    /* !  i */
#define SMG$K_TRM_LOWERCASE_J 0x0000006A    /* !  j */
#define SMG$K_TRM_LOWERCASE_K 0x0000006B    /* !  k */
#define SMG$K_TRM_LOWERCASE_L 0x0000006C    /* !  l */
#define SMG$K_TRM_LOWERCASE_M 0x0000006D    /* !  m */
#define SMG$K_TRM_LOWERCASE_N 0x0000006E    /* !  n */
#define SMG$K_TRM_LOWERCASE_O 0x0000006F    /* !  o */
#define SMG$K_TRM_LOWERCASE_P 0x00000070    /* !  p */
#define SMG$K_TRM_LOWERCASE_Q 0x00000071    /* !  q */
#define SMG$K_TRM_LOWERCASE_R 0x00000072    /* !  r */
#define SMG$K_TRM_LOWERCASE_S 0x00000073    /* !  s */
#define SMG$K_TRM_LOWERCASE_T 0x00000074    /* !  t */
#define SMG$K_TRM_LOWERCASE_U 0x00000075    /* !  u */
#define SMG$K_TRM_LOWERCASE_V 0x00000076    /* !  v */
#define SMG$K_TRM_LOWERCASE_W 0x00000077    /* !  w */
#define SMG$K_TRM_LOWERCASE_X 0x00000078    /* !  x */
#define SMG$K_TRM_LOWERCASE_Y 0x00000079    /* !  y */
#define SMG$K_TRM_LOWERCASE_Z 0x0000007A    /* !  z */
#define SMG$K_TRM_LEFT_BRACE 0x0000007B    /* !  left curly brace */
#define SMG$K_TRM_VERTICAL_LINE 0x0000007C    /* !  | */
#define SMG$K_TRM_RIGHT_BRACE 0x0000007D    /* !  right curly brace */
#define SMG$K_TRM_TILDE 0x0000007E    /* !  ~ */
#define SMG$K_TRM_DELETE 0x0000007F    /* !  DEL */
/* ! */
/* !  Synonyms for control characters */
/* ! */
#define SMG$K_TRM_BS 0x00000008
#define SMG$K_TRM_HT 0x00000009
#define SMG$K_TRM_LF 0x0000000A
#define SMG$K_TRM_VT 0x0000000B
#define SMG$K_TRM_FF 0x0000000C
#define SMG$K_TRM_CR 0x0000000D
/* ! */
/* !  Keypad keys */
/* ! */
#define SMG$K_TRM_PF1 0x00000100
#define SMG$K_TRM_PF2 0x00000101
#define SMG$K_TRM_PF3 0x00000102
#define SMG$K_TRM_PF4 0x00000103
#define SMG$K_TRM_KP0 0x00000104
#define SMG$K_TRM_KP1 0x00000105
#define SMG$K_TRM_KP2 0x00000106
#define SMG$K_TRM_KP3 0x00000107
#define SMG$K_TRM_KP4 0x00000108
#define SMG$K_TRM_KP5 0x00000109
#define SMG$K_TRM_KP6 0x0000010A
#define SMG$K_TRM_KP7 0x0000010B
#define SMG$K_TRM_KP8 0x0000010C
#define SMG$K_TRM_KP9 0x0000010D
#define SMG$K_TRM_ENTER 0x0000010E
#define SMG$K_TRM_MINUS 0x0000010F
#define SMG$K_TRM_COMMA 0x00000110
#define SMG$K_TRM_PERIOD 0x00000111
/* ! */
/* !  Cursor positioning keys */
/* ! */
#define SMG$K_TRM_UP 0x00000112
#define SMG$K_TRM_DOWN 0x00000113
#define SMG$K_TRM_LEFT 0x00000114
#define SMG$K_TRM_RIGHT 0x00000115
/* ! */
/* !  Function keys */
/* ! */
#define SMG$K_TRM_F1 0x00000119
#define SMG$K_TRM_F2 0x0000011A
#define SMG$K_TRM_F3 0x0000011B
#define SMG$K_TRM_F4 0x0000011C
#define SMG$K_TRM_F5 0x0000011D
#define SMG$K_TRM_F6 0x0000011E
#define SMG$K_TRM_F7 0x0000011F
#define SMG$K_TRM_F8 0x00000120
#define SMG$K_TRM_F9 0x00000121
#define SMG$K_TRM_F10 0x00000122
#define SMG$K_TRM_F11 0x00000123
#define SMG$K_TRM_F12 0x00000124
#define SMG$K_TRM_F13 0x00000125
#define SMG$K_TRM_F14 0x00000126
#define SMG$K_TRM_F15 0x00000127
#define SMG$K_TRM_F16 0x00000128
#define SMG$K_TRM_F17 0x00000129
#define SMG$K_TRM_F18 0x0000012A
#define SMG$K_TRM_F19 0x0000012B
#define SMG$K_TRM_F20 0x0000012C
/* ! */
/* !  Synonyms for function keys */
/* ! */
#define SMG$K_TRM_HELP 0x00000127
#define SMG$K_TRM_DO 0x00000128
/* ! */
/* !  Editing keys */
/* ! */
#define SMG$K_TRM_E1 0x00000137    /* !  FIND */
#define SMG$K_TRM_E2 0x00000138    /* !  INSERT_HERE */
#define SMG$K_TRM_E3 0x00000139    /* !  REMOVE */
#define SMG$K_TRM_E4 0x0000013A    /* !  SELECT */
#define SMG$K_TRM_E5 0x0000013B    /* !  PREV_SCREEN */
#define SMG$K_TRM_E6 0x0000013C    /* !  NEXT_SCREEN */
/* ! */
/* !  Synonyms for editing keys */
/* ! */
#define SMG$K_TRM_FIND 0x00000137    /* !  E1 */
#define SMG$K_TRM_INSERT_HERE 0x00000138    /* !  E2 */
#define SMG$K_TRM_REMOVE 0x00000139    /* !  E3 */
#define SMG$K_TRM_SELECT 0x0000013A    /* !  E4 */
#define SMG$K_TRM_PREV_SCREEN 0x0000013B    /* !  E5 */
#define SMG$K_TRM_NEXT_SCREEN 0x0000013C    /* !  E6 */
/* ! */
/* !  Locator keys */
/* ! */
#define SMG$K_TRM_FIRST_DOWN 0x00000141    /* !  Left button down */
#define SMG$K_TRM_SECOND_DOWN 0x00000142    /* !  Middle button down */
#define SMG$K_TRM_THIRD_DOWN 0x00000143    /* !  Right button down */
#define SMG$K_TRM_FOURTH_DOWN 0x00000144    /* !  Fourth button down */
#define SMG$K_TRM_FIRST_UP 0x00000145    /* !  Left button up */
#define SMG$K_TRM_SECOND_UP 0x00000146    /* !  Middle button up */
#define SMG$K_TRM_THIRD_UP 0x00000147    /* !  Right button up */
#define SMG$K_TRM_FOURTH_UP 0x00000148    /* !  Fourth button up */
/* ! */
/* !  Conditions */
/* ! */
#define SMG$K_TRM_CANCELLED 0x000001FC    /* !  I/O cancelled by SMG$CANCEL_INPUT */
#define SMG$K_TRM_TIMEOUT 0x000001FD    /* !  Timeout period expired */
#define SMG$K_TRM_BUFFER_FULL 0x000001FE    /* !  Buffer is full */
#define SMG$K_TRM_UNKNOWN 0x000001FF    /* !  Unknown terminator */
/* ! */
/* !  Screen Management request types - These constants are used internally */
/* !  by SMG$ and should not be used by users. */
/* ! */
#define SMG$C_CHANGE_RENDITION 0x0000000A
#define SMG$C_DELETE_CHARS 0x0000000B
#define SMG$C_ERASE_DISPLAY 0x0000000C
#define SMG$C_ERASE_LINE 0x0000000D
#define SMG$C_HOME_CURSOR 0x0000000E
#define SMG$C_INSERT_CHARS 0x0000000F
#define SMG$C_INSERT_LINE 0x00000010
#define SMG$C_PUT_CHARS 0x00000011
#define SMG$C_PUT_LINE 0x00000012
#define SMG$C_PUT_DISPLAY_ENCODED 0x00000013
#define SMG$C_RETURN_CURSOR_POS 0x00000014
#define SMG$C_PUT_WITH_SCROLL 0x00000015
#define SMG$C_SET_CURSOR_ABS 0x00000016
#define SMG$C_SET_CURSOR_REL 0x00000017
#define SMG$C_DELETE_LINE 0x00000018
#define SMG$C_ERASE_CHARS 0x00000019
#define SMG$C_SCROLL_DISPLAY_AREA 0x0000001A
#define SMG$C_CHANGE_VIRTUAL_DISPLAY 0x0000001B
#define SMG$C_LABEL_BORDER 0x0000001C
#define SMG$C_END_DISPLAY_UPDATE 0x0000001D
#define SMG$C_MOVE_TEXT 0x0000001E
/* ! */
/* !  	Character Set Codes */
/* ! */
#define SMG$C_UNITED_KINGDOM 0x00000000    /* !  Unused */
#define SMG$C_ASCII 0x00000001    /* !  ASCII char set */
#define SMG$C_SPEC_GRAPHICS 0x00000002    /* !  Line drawing set */
#define SMG$C_ALT_CHAR 0x00000003    /* !  Unused */
#define SMG$C_ALT_GRAPHICS 0x00000004    /* !  Unused */
/* ! */
/* ! 	The following constants define corner cursor positions used in */
/* ! 	SMG$HOME_CURSOR. */
/* ! */
#define SMG$C_UPPER_LEFT 0x00000000    /* !  Home cursor to upper left corner */
#define SMG$C_LOWER_LEFT 0x00000001    /* !  Home cursor to lower left corner */
#define SMG$C_UPPER_RIGHT 0x00000002    /* !  Home cursor to upper right corner */
#define SMG$C_LOWER_RIGHT 0x00000003    /* !  Home cursor to lower right corner */
/* ! */
/* ! 	The following constants define label positions used in */
/* ! 	SMG$LABEL_BORDER. */
/* ! */
#define SMG$K_TOP 0x00000000    /* !  Place label on top border */
#define SMG$K_BOTTOM 0x00000001    /* !  Place label on bottom border */
#define SMG$K_LEFT 0x00000002    /* !  Place label on left border */
#define SMG$K_RIGHT 0x00000003    /* !  Place label on right border */
/* ! */
/* ! 	The following constants define menu types used in */
/* ! 	SMG$CREATE_MENU. */
/* ! */
#define SMG$K_BLOCK 0x00000000    /* !  Matrix of menu items */
#define SMG$K_VERTICAL 0x00000001    /* !  Vertical vector of menu items */
#define SMG$K_HORIZONTAL 0x00000002    /* !  Horizontal vector of menu items */
/* ! */
/* ! 	Master color wheel for screen background colors. */
/* ! 	Used with SMG$CHANGE_PBD_CHARACTERISTICS. */
/* ! */
#define SMG$C_COLOR_UNKNOWN 0x00000000    /* !  Unknown background */
#define SMG$C_COLOR_WHITE 0x00000001    /* !  White background */
#define SMG$C_COLOR_BLACK 0x00000002    /* !  Black background */
#define SMG$C_COLOR_BLUE 0x00000003    /* !  Blue background */
#define SMG$C_COLOR_CYAN 0x00000004    /* !  Cyan background */
#define SMG$C_COLOR_GREEN 0x00000005    /* !  Green background */
#define SMG$C_COLOR_MAGENTA 0x00000006    /* !  Magenta background */
#define SMG$C_COLOR_RED 0x00000007    /* !  Red background */
#define SMG$C_COLOR_YELLOW 0x00000008    /* !  Yellow background */
#define SMG$C_COLOR_LIGHT 0x00000009    /* !  Light background */
#define SMG$C_COLOR_DARK 0x0000000A    /* !  Dark background */
#define SMG$C_COLOR_USER1 0x0000000B    /* !  User 1 background */
#define SMG$C_COLOR_USER2 0x0000000C    /* !  User 2 background */
#define SMG$C_COLOR_USER3 0x0000000D    /* !  User 3 background */
#define SMG$C_COLOR_USER4 0x0000000E    /* !  User 4 background */
#define SMG$C_COLOR_USER5 0x0000000F    /* !  User 5 background */
#define SMG$C_COLOR_USER6 0x00000010    /* !  User 6 background */
#define SMG$C_COLOR_USER7 0x00000011    /* !  User 7 background */
#define SMG$C_COLOR_USER8 0x00000012    /* !  User 8 background */
/* ! */
/* !  The following constants describe the type of terminal */
/* !  and are used with SMG$GET_PASTEBOARD_ATTRIBUTES. */
/* ! */
#define SMG$K_UNKNOWN 0x00000000    /* !  Unknown type */
#define SMG$K_VT05 0x00000001    /* !  Unused */
#define SMG$K_VT52 0x00000002    /* !  Unused */
#define SMG$K_VT100 0x00000003    /* !  Unused */
#define SMG$K_VTFOREIGN 0x00000004    /* !  Foreign terminal (ft1-8) */
#define SMG$K_HARDCOPY 0x00000005    /* !  Hardcopy device */
#define SMG$K_VTTERMTABLE 0x00000006    /* !  Video terminal */
#define SMG$K_FIRST_PRIV_TYPE 0x000000BF    /* !  Used internally by SMG$ */
/* ! */
/* ! 	Define bits, masks, and fields for the control modes. */
/* ! */
#define SMG$K_BUF_ENABLED 0x00000000    /* !  Enable Buffering */
#define SMG$K_MINUPD 0x00000001    /* !  Enable minimal update */
#define SMG$K_CLEAR_SCREEN 0x00000002    /* !  Clear screen on exit */
#define SMG$K_NOTABS 0x00000003    /* !  Don't use physical tabs */
#define SMG$K_PROTECT 0x00000004    /* !  Protect against ASTs */
#define SMG$K_IGNORE 0x00000005    /* !  Don't output PBD if batched */
#define SMG$K_RELEASE_PBD 0x00000006    /* !  Don't touch users part of PBD */
#define SMG$M_BUF_ENABLED 0x00000001
#define SMG$M_MINUPD 0x00000002
#define SMG$M_CLEAR_SCREEN 0x00000004
#define SMG$M_NOTABS 0x00000008
#define SMG$M_PROTECT 0x00000010
#define SMG$M_IGNORE 0x00000020
#define SMG$M_RELEASE_PBD 0x00000040
#define SMG$M_SPARE15 0xFFFFFF80
/* ! */
/* ! 	Key definition attributes */
/* ! */
#define SMG$M_KEY_NOECHO 0x00000001
#define SMG$M_KEY_TERMINATE 0x00000002
#define SMG$M_KEY_LOCK 0x00000004
#define SMG$M_KEY_PROTECTED 0x00000008
#define SMG$M_KEY_SETSTATE 0x00000010
#define SMG$M_SPARE18 0xFFFFFFE0
/* ! */
/* !         Define bit masks and values for display renditions. */
/* ! */
#define SMG$M_BOLD 0x00000001
#define SMG$M_REVERSE 0x00000002
#define SMG$M_BLINK 0x00000004
#define SMG$M_UNDERLINE 0x00000008
#define SMG$M_INVISIBLE 0x00000010
#define SMG$M_USER1 0x00000100
#define SMG$M_USER2 0x00000200
#define SMG$M_USER3 0x00000400
#define SMG$M_USER4 0x00000800
#define SMG$M_USER5 0x00001000
#define SMG$M_USER6 0x00002000
#define SMG$M_USER7 0x00004000
#define SMG$M_USER8 0x00008000
#define SMG$M_SPARE14 0xFFFF0000
#define SMG$M_NORMAL 0x00000000    /* !  no bits set */
/* ! */
/* ! 	The following masks define values to be used */
/* ! 	to specify a display attribute.  These may be added */
/* ! 	together to specify multiple attributes. */
/* ! */
#define SMG$M_BORDER 0x00000001
#define SMG$M_TRUNC_ICON 0x00000002
#define SMG$M_DISPLAY_CONTROLS 0x00000004
#define SMG$M_USER_DISPLAY 0x00000008
#define SMG$M_BLOCK_BORDER 0x00000010
#define SMG$M_PROTECT_DISPLAY 0x00000020
#define SMG$M_SPARE12 0xFFFFFFC0
/* ! */
/* ! 	The following defines bits used with subprocess manipulation. */
/* ! */
#define SMG$M_DATA_FOLLOWS 0x00000001
#define SMG$M_SEND_EOF 0x00000002
#define SMG$M_SPARE11 0xFFFFFFFC
/* ! */
/* ! 	The following defines bits used with the FLAGS parameter for */
/* ! 	SMG$DELETE_PASTEBOARD. */
/* ! */
#define SMG$M_ERASE_PBD 0x00000001
#define SMG$M_IGNORE_BATCHED 0x00000002
#define SMG$M_SPARE10 0xFFFFFFFC
/* ! */
/* ! 	The following defines bits used with the FLAGS parameter for */
/* ! 	SMG$CREATE_PASTEBOARD. */
/* ! */
#define SMG$M_KEEP_CONTENTS 0x00000001
#define SMG$M_WORKSTATION 0x00000002
#define SMG$M_SPARE9 0xFFFFFFFC
/* ! */
/* ! 	The following defines bits used with the FLAGS parameter for */
/* ! 	SMG$READ_xxxx. */
/* ! */
#define SMG$M_FUNC_KEYS 0x00000001
#define SMG$M_NOKEEP 0x00000002
#define SMG$M_NORECALL 0x00000004
#define SMG$M_SPARE8 0xFFFFFFF8
/* ! */
/* ! 	The following defines bits used with the FLAGS parameter for */
/* ! 	SMG$SET_CURSOR_MODE. */
/* ! */
#define SMG$M_CURSOR_OFF 0x00000001
#define SMG$M_CURSOR_ON 0x00000002
#define SMG$M_SCROLL_JUMP 0x00000004
#define SMG$M_SCROLL_SMOOTH 0x00000008
#define SMG$M_SPARE16 0xFFFFFFF0
/* ! */
/* ! 	The following defines bits used with the FLAGS parameter for */
/* ! 	SMG$GET_DISPLAY_ATTR. */
/* ! */
#define SMG$M_VIEWPORT 0x00000001
#define SMG$M_SUBPROCESS 0x00000002
#define SMG$M_MENU 0x00000004
#define SMG$M_SPARE7 0xFFFFFFF8
/* ! */
/* ! 	The following defines bits used with the FLAGS parameter for */
/* ! 	SMG$PUT_CHARS. */
/* ! */
#define SMG$M_ERASE_LINE 0x00000001
#define SMG$M_ERASE_TO_EOL 0x00000002
#define SMG$M_SPARE6 0xFFFFFFFC
/* ! */
/* ! 	The following defines bits used with the FLAGS parameter for */
/* ! 	SMG$MOVE_TEXT. */
/* ! */
#define SMG$M_TEXT_SAVE 0x00000001
#define SMG$M_TEXT_ONLY 0x00000002
#define SMG$M_SPARE17 0xFFFFFFFC
/* ! */
/* ! 	The following defines bits used with the FLAGS parameter for */
/* ! 	SMG$PUT_PASTEBOARD and SMG$SNAPSHOT. */
/* ! */
#define SMG$M_FORM_FEED 0x00000001
#define SMG$M_SPARE5 0xFFFFFFFE
/* ! */
/* ! 	The following defines bits used with the FLAGS parameter for */
/* ! 	SMG$PUT_xxxx. */
/* ! */
#define SMG$M_WRAP_CHAR 0x00000001
#define SMG$M_WRAP_WORD 0x00000002
#define SMG$M_SPARE4 0xFFFFFFFC
/* ! */
/* ! 	The following defines bits used with the FLAGS parameter for */
/* ! 	SMG$SET_KEYPAD_MODE. */
/* ! */
#define SMG$M_KEYPAD_APPLICATION 0x00000001
#define SMG$M_KEYPAD_NUMERIC 0x00000002
#define SMG$M_SPARE3 0xFFFFFFFC
/* ! */
/* ! 	The following defines bits used with the FLAGS parameter for */
/* ! 	SMG$GET_PASTING_INFO. */
/* ! */
#define SMG$M_DISPLAY_PASTED 0x00000001
#define SMG$M_SPARE19 0xFFFFFFFE
/* ! */
/* ! 	The following defines bits used with menu manipulation. */
/* ! */
#define SMG$M_REMOVE_ITEM 0x00000001
#define SMG$M_FIXED_FORMAT 0x00000002
#define SMG$M_DOUBLE_SPACE 0x00000004
#define SMG$M_RETURN_IMMED 0x00000008
#define SMG$M_ERASE_MENU 0x00000010
#define SMG$M_WIDE_MENU 0x00000020
#define SMG$M_SPARE2 0xFFFFFFC0
/* ! */
/* ! 	The following are used with SMG$PUT_LINE_xxxx, */
/* ! 	SMG$SCROLL_DISPLAY_AREA, and SMG$DRAW_CHAR. */
/* ! */
#define SMG$M_UP 0x00000001
#define SMG$M_DOWN 0x00000002
#define SMG$M_RIGHT 0x00000004
#define SMG$M_LEFT 0x00000008
#define SMG$M_SPARE1 0xFFFFFFF0
/* ! */
/* !  Define data structures */
/* ! */
#define SMG$C_BAND_INFORMATION_TABLE 0x0000000C    /* !  Length of table in bytes */
/* ! */
#define SMG$C_PASTEBOARD_INFO_BLOCK 0x00000020    /* !  Length of table in bytes */
/* ! */
#define SMG$C_KEYBOARD_INFO_BLOCK 0x00000014    /* !  Length of table in bytes */
/* ! */
/* !  Define the out-of-band information table used when an out-of-band */
/* !  AST routine is called as it's own record. */
/* ! */
#define SMG$C_OUT_OF_BAND_TABLE 0x0000000C    /* !  Length of table in bytes */
/* ! */
/* !  Define the subprocess information table used when a subprocess */
/* !  AST routine is called as it's own record. */
/* ! */
#define SMG$C_SUBPROCESS_INFO_TABLE 0x0000000C    /* !  Length of table in bytes */
/* ! */
/* !  Define an information table that can be used by both */
/* !  SMG$GET_PASTEBOARD_ATTRIBUTES and SMG$GET_KEYBOARD_ATTRIBUTES */
/* !  as it's own record. */
/* ! */
#define SMG$C_ATTRIBUTE_INFO_BLOCK 0x00000020    /* !  Length of table in bytes */

