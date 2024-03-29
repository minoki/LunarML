fun call f arg = print ((Int8.toString (f arg) handle Overflow => "Overflow" | Div => "Div") ^ "\n")
fun callS f arg = print ((f arg handle Overflow => "Overflow" | Div => "Div") ^ "\n")
fun callO f arg = print (((case f arg of SOME x => "SOME " ^ Int8.toString x | NONE => "NONE") handle Overflow => "Overflow" | Div => "Div") ^ "\n")
fun callP f arg = print (((case f arg of (x, y) => "(" ^ Int8.toString x ^ "," ^ Int8.toString y ^ ")") handle Overflow => "Overflow" | Div => "Div") ^ "\n")
fun bin (x, y) = ( print (Int8.toString x ^ " + " ^ Int8.toString y ^ " = ")
                 ; call Int8.+ (x, y)
                 ; print (Int8.toString x ^ " - " ^ Int8.toString y ^ " = ")
                 ; call Int8.- (x, y)
                 ; print (Int8.toString x ^ " * " ^ Int8.toString y ^ " = ")
                 ; call Int8.* (x, y)
                 ; print (Int8.toString x ^ " div " ^ Int8.toString y ^ " = ")
                 ; call Int8.div (x, y)
                 ; print (Int8.toString x ^ " mod " ^ Int8.toString y ^ " = ")
                 ; call Int8.mod (x, y)
                 ; print (Int8.toString x ^ " quot " ^ Int8.toString y ^ " = ")
                 ; call Int8.quot (x, y)
                 ; print (Int8.toString x ^ " rem " ^ Int8.toString y ^ " = ")
                 ; call Int8.rem (x, y)
                 );
fun un x = ( print ("~ " ^ Int8.toString x ^ " = ")
           ; call Int8.~ x
           ; print ("abs " ^ Int8.toString x ^ " = ")
           ; call Int8.abs x
           (* ; print ("fmt BIN " ^ Int8.toString x ^ " = ")
           ; callS (Int8.fmt StringCvt.BIN) x ... not implemented yet *)
           ; print ("fmt OCT " ^ Int8.toString x ^ " = ")
           ; callS (Int8.fmt StringCvt.OCT) x
           ; print ("fmt DEC " ^ Int8.toString x ^ " = ")
           ; callS (Int8.fmt StringCvt.DEC) x
           ; print ("fmt HEX " ^ Int8.toString x ^ " = ")
           ; callS (Int8.fmt StringCvt.HEX) x
           ; print ("toLarge " ^ Int8.toString x ^ " = ")
           ; callS (LargeInt.toString o Int8.toLarge) x
           );
List.app un
[~128
,~127
,~77
,~3
,~2
,~1
,0
,1
,2
,3
,78
,126
,127
,~80
,23
,~33
,~113
,80
,82
,17
,~105
,11
,116
,~78
,59
,5
,14
,~87
,120
,~34
,~36
,~37
,109
,~62
,~119
,~53
,27
,~6
,~104
,96
,~54
,34
,98
,107
,~97
,40
,91
,~94
,61
,~56
,~21
,112
,22
,56
,~9
,31
,117
,~90
];
List.app bin
[(~128,~128)
,(~128,~127)
,(~128,~35)
,(~128,~3)
,(~128,~2)
,(~128,~1)
,(~128,0)
,(~128,1)
,(~128,2)
,(~128,3)
,(~128,99)
,(~128,126)
,(~128,127)
,(~127,~128)
,(~127,~127)
,(~127,~35)
,(~127,~3)
,(~127,~2)
,(~127,~1)
,(~127,0)
,(~127,1)
,(~127,2)
,(~127,3)
,(~127,99)
,(~127,126)
,(~127,127)
,(~77,~128)
,(~77,~127)
,(~77,~35)
,(~77,~3)
,(~77,~2)
,(~77,~1)
,(~77,0)
,(~77,1)
,(~77,2)
,(~77,3)
,(~77,99)
,(~77,126)
,(~77,127)
,(~3,~128)
,(~3,~127)
,(~3,~35)
,(~3,~3)
,(~3,~2)
,(~3,~1)
,(~3,0)
,(~3,1)
,(~3,2)
,(~3,3)
,(~3,99)
,(~3,126)
,(~3,127)
,(~2,~128)
,(~2,~127)
,(~2,~35)
,(~2,~3)
,(~2,~2)
,(~2,~1)
,(~2,0)
,(~2,1)
,(~2,2)
,(~2,3)
,(~2,99)
,(~2,126)
,(~2,127)
,(~1,~128)
,(~1,~127)
,(~1,~35)
,(~1,~3)
,(~1,~2)
,(~1,~1)
,(~1,0)
,(~1,1)
,(~1,2)
,(~1,3)
,(~1,99)
,(~1,126)
,(~1,127)
,(0,~128)
,(0,~127)
,(0,~35)
,(0,~3)
,(0,~2)
,(0,~1)
,(0,0)
,(0,1)
,(0,2)
,(0,3)
,(0,99)
,(0,126)
,(0,127)
,(1,~128)
,(1,~127)
,(1,~35)
,(1,~3)
,(1,~2)
,(1,~1)
,(1,0)
,(1,1)
,(1,2)
,(1,3)
,(1,99)
,(1,126)
,(1,127)
,(2,~128)
,(2,~127)
,(2,~35)
,(2,~3)
,(2,~2)
,(2,~1)
,(2,0)
,(2,1)
,(2,2)
,(2,3)
,(2,99)
,(2,126)
,(2,127)
,(3,~128)
,(3,~127)
,(3,~35)
,(3,~3)
,(3,~2)
,(3,~1)
,(3,0)
,(3,1)
,(3,2)
,(3,3)
,(3,99)
,(3,126)
,(3,127)
,(78,~128)
,(78,~127)
,(78,~35)
,(78,~3)
,(78,~2)
,(78,~1)
,(78,0)
,(78,1)
,(78,2)
,(78,3)
,(78,99)
,(78,126)
,(78,127)
,(126,~128)
,(126,~127)
,(126,~35)
,(126,~3)
,(126,~2)
,(126,~1)
,(126,0)
,(126,1)
,(126,2)
,(126,3)
,(126,99)
,(126,126)
,(126,127)
,(127,~128)
,(127,~127)
,(127,~35)
,(127,~3)
,(127,~2)
,(127,~1)
,(127,0)
,(127,1)
,(127,2)
,(127,3)
,(127,99)
,(127,126)
,(127,127)
,(14,~125)
,(~31,~42)
,(~48,50)
,(91,~51)
,(~63,75)
,(38,~70)
,(3,122)
,(59,7)
,(4,~23)
,(~99,108)
,(98,~115)
,(108,30)
,(92,~101)
,(55,57)
,(~110,64)
,(67,72)
,(~84,~100)
,(~30,~87)
,(~59,~13)
,(18,50)
,(61,~88)
,(~45,89)
,(45,111)
,(8,~46)
,(79,~42)
,(~113,~66)
,(78,4)
,(~83,~22)
,(~80,~6)
,(113,119)
,(50,~34)
,(~82,11)
,(1,~77)
,(~23,~65)
,(~73,~81)
,(~41,32)
,(1,49)
,(97,44)
,(~37,~11)
,(96,44)
,(~127,~66)
,(42,~76)
,(~125,~66)
,(6,7)
,(~25,~39)
,(38,37)
,(~100,47)
,(111,75)
,(~8,67)
,(~3,~40)
,(~100,75)
,(84,109)
,(~49,~70)
,(17,~93)
,(~85,81)
,(~59,98)
,(~73,125)
,(18,25)
,(~110,82)
,(~106,89)
,(120,121)
,(~21,73)
,(112,28)
,(~52,~36)
,(51,~93)
,(64,125)
,(~98,~119)
,(10,~60)
,(~19,74)
,(~5,63)
,(~43,~103)
,(79,~84)
,(81,81)
,(~5,35)
,(86,~33)
,(65,102)
,(117,~72)
,(106,~44)
,(111,17)
,(~68,~121)
,(~49,~81)
,(~90,27)
,(~125,10)
,(88,~10)
,(99,~2)
,(126,15)
,(13,~37)
,(36,~104)
,(32,~85)
,(78,~25)
,(~74,95)
,(6,31)
,(~115,~22)
,(~42,~53)
,(~2,110)
,(13,~9)
,(25,37)
,(~119,~127)
,(~38,105)
,(88,~127)
,(~78,42)
,(~77,~49)
,(~93,13)
,(~46,111)
,(~6,62)
,(93,91)
,(103,~55)
,(~38,~60)
,(97,~117)
,(~42,94)
,(7,117)
,(~126,33)
,(35,50)
,(~21,~11)
,(29,~22)
,(19,~85)
,(56,~115)
,(117,33)
,(97,~68)
,(~73,~20)
,(88,12)
,(0,~116)
,(60,~112)
,(34,53)
,(~50,35)
,(~60,4)
,(~92,~84)
,(~110,~6)
,(~19,126)
,(105,~94)
,(~124,39)
,(~89,~84)
,(~62,~75)
,(~66,~48)
,(~119,~51)
,(6,~20)
,(9,33)
,(15,91)
,(88,28)
,(~113,93)
,(~49,38)
,(65,~38)
,(97,32)
,(42,126)
,(~82,23)
,(2,~117)
,(123,112)
,(2,92)
,(~15,20)
,(106,~23)
,(113,83)
,(60,~108)
,(53,~41)
,(78,~107)
,(111,31)
,(38,23)
,(~56,122)
,(~104,~82)
,(~128,30)
,(~11,73)
,(~16,~50)
,(~58,21)
,(~80,87)
,(38,14)
,(~119,71)
,(~24,18)
,(~36,81)
,(~32,~103)
,(~58,65)
,(~82,124)
,(~72,12)
,(~3,~69)
,(~108,~120)
,(21,95)
,(~27,106)
,(58,31)
,(72,123)
,(~105,62)
,(3,~113)
,(125,98)
,(86,~10)
,(100,~92)
,(75,~28)
,(113,~58)
,(45,85)
,(~91,119)
,(23,~30)
,(~21,~127)
,(~99,~74)
,(~103,124)
,(~53,~66)
,(~22,~72)
,(109,~40)
,(91,75)
,(~22,~43)
,(109,~45)
,(105,26)
,(26,27)
,(8,87)
,(28,~99)
,(85,~59)
,(27,~87)
,(~43,~115)
,(68,4)
,(38,~75)
,(~58,56)
,(~4,~109)
,(78,~37)
,(~86,~22)
,(~62,1)
,(0,61)
,(68,58)
,(79,~110)
,(124,~7)
,(~26,118)
,(123,99)
,(~35,~128)
,(~102,~27)
,(~98,~71)
,(102,~124)
,(66,~82)
,(~43,64)
,(59,~106)
,(~15,124)
,(~95,~69)
,(~4,106)
,(~100,31)
,(19,15)
,(22,~109)
,(24,~46)
,(26,~54)
,(11,104)
,(~96,61)
,(68,~37)
,(46,85)
,(~122,75)
,(92,87)
,(~22,~76)
,(110,7)
,(~14,~32)
,(97,87)
,(~123,~51)
,(~79,109)
,(127,10)
,(~112,~13)
,(66,117)
,(24,~61)
,(~107,~12)
,(~7,25)
,(53,~44)
,(~40,2)
,(85,89)
,(115,99)
,(97,~48)
,(~1,54)
,(84,122)
,(59,113)
,(~97,79)
,(~72,~2)
,(~66,85)
,(100,69)
,(88,~46)
,(~90,67)
,(~51,85)
,(~79,35)
,(~81,42)
,(54,~55)
,(~127,~28)
,(67,~14)
,(66,~66)
,(~36,~65)
,(111,~17)
,(79,~5)
,(17,~44)
,(125,~64)
,(74,112)
,(~30,43)
,(~81,~19)
,(~92,~1)
,(~80,~44)
,(60,~64)
,(~70,74)
,(80,15)
,(~109,113)
,(67,124)
,(~19,~42)
,(~11,111)
,(11,~44)
,(~1,91)
,(~50,~121)
,(~32,~96)
,(93,~128)
,(38,90)
,(~3,~49)
,(123,4)
,(39,~96)
,(22,6)
,(~107,~67)
,(~6,56)
,(31,8)
,(~61,100)
,(~11,~50)
,(~123,92)
,(~90,15)
,(82,~23)
,(102,~16)
,(106,~17)
,(111,~71)
,(~3,93)
,(~114,~8)
,(~13,~25)
,(17,~123)
,(51,~91)
,(11,~73)
,(68,7)
,(~25,~102)
,(18,85)
,(96,~58)
,(34,~84)
,(~61,88)
,(77,61)
,(9,40)
,(47,~60)
,(91,52)
,(25,49)
,(~115,109)
,(~65,~6)
,(~87,39)
,(59,~111)
,(75,~17)
,(~93,115)
,(~67,42)
,(~36,8)
,(~114,50)
,(~14,30)
,(54,~67)
,(84,73)
,(~54,92)
,(125,~84)
,(~37,~33)
,(~31,~93)
,(28,~4)
,(~63,56)
,(62,4)
,(~26,47)
,(~88,110)
,(31,~54)
,(~106,~16)
,(~101,~64)
,(~56,~90)
,(37,~96)
,(104,62)
,(~82,~57)
,(35,21)
,(17,65)
,(68,38)
,(105,~10)
,(112,~39)
,(~109,49)
,(69,89)
,(~67,~43)
,(50,68)
,(32,121)
,(~63,25)
,(~83,~45)
,(~33,~123)
,(~115,90)
,(53,~6)
,(~8,~77)
,(59,~28)
,(37,16)
,(14,37)
,(~68,~25)
,(~63,105)
,(~100,87)
,(~104,28)
,(~91,78)
,(85,~73)
,(18,95)
,(119,~53)
,(~67,47)
,(~30,21)
,(112,93)
,(17,~99)
,(~32,9)
,(118,~74)
,(62,~76)
,(~73,~68)
,(65,113)
,(89,~77)
,(~67,~41)
,(117,91)
,(~112,112)
,(48,23)
,(~72,~25)
,(64,~81)
,(35,10)
,(~93,63)
,(58,24)
,(6,74)
,(~10,~101)
,(95,7)
,(78,~61)
,(73,101)
,(38,~73)
,(85,76)
,(~29,~100)
,(90,12)
,(~69,43)
,(~51,31)
,(~16,~84)
,(24,53)
,(~114,~128)
,(88,63)
,(34,66)
,(~41,90)
,(~101,~121)
,(~23,~108)
,(8,22)
,(~81,~87)
,(15,~85)
,(123,68)
,(~51,~13)
,(90,116)
,(~76,115)
,(17,24)
,(103,~45)
,(34,~52)
,(~38,19)
,(~11,~106)
,(108,~108)
,(~110,~18)
,(83,~37)
,(~10,92)
,(~58,~97)
,(~92,6)
,(36,72)
,(92,57)
,(~5,~11)
,(~18,~101)
,(38,34)
,(61,96)
,(~83,62)
,(8,~4)
,(18,~128)
,(~29,20)
,(7,~22)
,(~4,~7)
,(~121,40)
,(34,~6)
,(114,118)
,(92,~107)
,(~54,0)
,(125,64)
,(~15,~92)
,(16,~46)
,(~91,30)
,(~111,~111)
,(~31,118)
,(~62,~95)
,(~121,~98)
,(~26,~65)
,(21,~30)
,(~51,~64)
,(~93,20)
,(~84,77)
,(116,101)
,(44,~51)
,(109,~66)
,(~43,~47)
,(56,~59)
,(60,52)
,(~33,~94)
,(26,~107)
,(~92,~111)
,(~121,~91)
,(~96,109)
,(96,~106)
,(28,85)
,(10,~61)
,(0,44)
,(64,~25)
,(39,~48)
,(~103,18)
,(~113,4)
,(11,~22)
,(~51,~101)
,(~12,~94)
,(15,~125)
,(~50,52)
,(~93,~13)
,(~24,~59)
,(54,~11)
,(~68,~16)
,(3,31)
,(84,~45)
,(88,~66)
,(~35,~10)
,(~106,~60)
,(~90,~89)
,(~47,71)
,(~76,111)
,(~63,92)
,(79,78)
,(~103,86)
,(103,50)
,(78,20)
,(74,~113)
,(~19,61)
,(~62,103)
,(~21,~57)
,(90,~54)
,(77,~83)
,(46,~71)
,(~47,13)
,(~112,~3)
,(~59,22)
,(~72,~79)
,(122,~91)
,(~45,~61)
,(67,~128)
,(7,~4)
,(~49,13)
,(51,~41)
,(~38,86)
,(~66,~51)
,(108,79)
,(44,22)
,(1,~53)
,(120,2)
,(~27,~88)
,(100,~46)
,(70,~105)
,(~47,~25)
,(37,~91)
,(~71,18)
,(~106,~1)
,(~110,42)
,(61,~106)
,(~97,~69)
,(72,56)
,(94,39)
,(~100,55)
,(4,~105)
,(~19,111)
,(~17,8)
,(~85,~47)
,(95,~57)
,(~30,36)
,(~88,97)
,(~76,~29)
,(11,~85)
,(~2,~102)
,(50,19)
,(~86,~61)
,(83,~80)
,(~46,30)
,(~50,~80)
,(22,8)
,(~105,~112)
,(~57,69)
,(30,54)
,(~106,101)
,(~96,100)
,(95,17)
,(79,3)
,(72,~37)
,(~102,~128)
,(~19,~88)
,(127,60)
,(91,~24)
,(~77,10)
,(~121,84)
,(~2,10)
,(~113,47)
,(~107,87)
,(74,~84)
,(126,39)
,(111,91)
,(~46,~59)
,(~95,76)
,(~122,56)
,(108,~68)
,(59,~70)
,(42,60)
,(~12,81)
,(~36,~102)
,(~52,~48)
,(~126,~54)
,(55,~67)
,(~36,~53)
,(4,~96)
,(122,~59)
,(~17,~127)
,(~73,~120)
,(61,94)
,(22,~64)
,(19,~39)
,(~15,21)
,(61,112)
,(~68,95)
,(13,~72)
,(86,18)
,(55,~57)
,(~19,24)
,(~2,73)
,(121,~38)
,(69,78)
,(34,14)
,(~88,~37)
,(~18,~54)
,(~117,~12)
,(~3,54)
,(44,117)
,(~39,50)
,(101,~116)
,(~43,101)
,(~91,94)
,(112,~26)
,(83,~11)
,(~122,113)
,(~44,~114)
,(~113,~125)
,(22,114)
,(7,~74)
,(~56,107)
,(35,~127)
,(79,107)
,(46,~80)
,(122,109)
,(~53,44)
,(~31,~10)
,(15,~111)
,(~126,~43)
,(124,~52)
,(~32,~7)
,(106,~106)
,(103,21)
,(~63,~37)
,(15,25)
,(~94,~29)
,(~13,~16)
,(~34,22)
,(~77,~118)
,(21,~104)
,(100,~118)
,(119,~73)
,(~100,116)
,(~21,111)
,(125,~60)
,(~66,3)
,(97,85)
,(~89,~13)
,(~34,62)
,(~95,~62)
,(~95,~12)
,(~15,88)
,(43,69)
,(~14,~24)
,(~15,15)
,(~41,~112)
,(124,~42)
,(~38,~10)
,(105,112)
,(40,122)
,(10,105)
,(42,57)
,(37,~10)
,(28,56)
,(~54,~102)
,(~109,80)
,(110,~102)
,(~72,119)
,(36,30)
,(64,~9)
,(~110,125)
,(101,96)
,(28,~14)
,(35,101)
,(2,18)
,(~47,~60)
,(~121,~118)
,(30,~110)
,(95,57)
,(4,82)
,(60,~126)
,(102,~46)
,(63,19)
,(51,~43)
,(~71,13)
,(~72,107)
,(~66,72)
,(127,~117)
,(14,~59)
,(37,43)
,(~47,~9)
,(74,~124)
,(~109,111)
,(~52,~108)
,(105,~125)
,(34,34)
,(19,29)
,(47,~81)
,(40,97)
,(118,92)
,(39,~26)
,(~108,109)
,(15,108)
,(~27,~69)
,(~55,~71)
,(77,~63)
,(106,31)
,(21,~112)
,(~34,36)
,(18,~72)
,(121,51)
,(~30,~57)
,(51,~122)
,(107,26)
,(28,~19)
,(41,~11)
,(~113,127)
,(~10,27)
,(~5,65)
,(~51,37)
,(~55,~52)
,(120,27)
,(121,~119)
,(38,70)
,(~8,~20)
,(38,75)
,(~44,~33)
,(41,~114)
,(57,~61)
,(9,~60)
,(125,55)
,(18,~54)
,(~87,~90)
,(~17,~74)
,(40,~113)
,(~44,~107)
,(77,112)
,(122,99)
,(~118,~20)
,(~46,17)
,(~27,~93)
,(94,62)
,(85,48)
,(14,~22)
,(~49,~19)
,(~111,48)
,(~42,42)
,(44,~44)
,(~11,34)
,(~61,3)
,(107,~16)
,(~123,~68)
,(84,~70)
,(15,56)
,(36,105)
,(22,~34)
,(~22,7)
,(62,98)
,(44,~60)
,(~83,~32)
,(18,34)
,(32,33)
,(87,~32)
,(13,~23)
,(~17,113)
,(~48,115)
,(19,~107)
,(~20,~53)
,(~7,11)
,(28,~58)
,(~59,84)
,(71,~120)
,(~72,19)
,(~80,16)
,(14,48)
,(49,~52)
,(~24,15)
,(~72,~44)
,(75,92)
,(85,38)
,(~13,48)
,(46,21)
,(69,~67)
,(~100,~1)
,(~54,96)
,(76,~88)
,(~61,44)
,(110,110)
,(~81,8)
,(44,~118)
,(~75,~70)
,(31,~103)
,(~44,~12)
,(~21,103)
,(~32,~104)
,(10,98)
,(7,57)
,(106,~103)
,(61,~53)
,(~111,5)
,(~54,~37)
,(~10,9)
,(~101,70)
,(~95,~98)
,(~6,~55)
,(101,9)
,(13,~58)
,(~76,~128)
,(7,66)
,(~74,~51)
,(104,~56)
,(72,118)
,(121,~98)
,(~109,~84)
,(~103,6)
,(~99,~26)
,(~22,~26)
,(~1,89)
,(27,106)
,(87,12)
,(82,~34)
,(66,119)
,(60,99)
,(~32,123)
,(76,8)
,(71,~16)
,(~124,27)
,(~39,11)
,(~90,13)
,(~79,~63)
,(37,0)
,(~30,~7)
,(~76,32)
,(~81,~32)
,(57,~79)
,(~60,~98)
,(89,~19)
,(90,61)
,(~84,82)
,(~27,~123)
,(~100,6)
,(~49,~60)
,(102,108)
,(~122,~46)
,(~35,~104)
,(~54,36)
,(~61,40)
,(9,8)
,(~44,100)
,(6,~60)
,(106,~28)
,(16,~13)
,(~79,~99)
,(~65,26)
,(4,9)
,(~82,~58)
,(46,~119)
,(29,~13)
,(51,88)
,(0,109)
,(~98,57)
,(~19,25)
,(~89,~73)
,(~41,~89)
,(85,~4)
,(124,114)
,(36,44)
,(~115,~39)
,(~44,~38)
,(~42,~40)
,(~4,~85)
,(28,42)
,(101,~75)
,(27,66)
,(0,85)
,(~25,~92)
,(~4,~91)
,(~34,26)
,(10,13)
,(~81,~88)
,(93,125)
,(~64,~16)
,(43,~39)
,(99,51)
,(102,100)
,(~22,~69)
,(~50,48)
,(~24,64)
,(118,~123)
,(54,~9)
,(~68,~20)
,(73,~84)
,(~81,~13)
,(8,~35)
,(20,~21)
,(35,9)
,(29,~17)
,(76,~65)
,(104,22)
,(126,~76)
,(104,~6)
,(~25,44)
,(~62,68)
,(38,~109)
,(~26,~51)
,(67,~32)
,(18,80)
,(13,47)
,(18,125)
,(58,60)
,(7,90)
,(~84,~119)
,(75,~91)
,(~2,101)
,(40,~40)
,(27,~80)
,(~35,~116)
,(~82,~125)
,(39,101)
,(~38,70)
,(~62,~79)
,(~116,100)
,(91,~81)
,(59,58)
,(~2,~11)
,(97,14)
,(112,~22)
,(~122,~92)
,(~17,~40)
,(68,47)
,(~60,~14)
,(~37,~35)
,(~118,88)
,(38,44)
,(28,20)
,(~20,114)
,(~51,41)
,(~125,62)
,(108,24)
,(~120,~9)
,(56,15)
,(81,~5)
,(~16,~93)
,(~38,~61)
,(~5,~39)
,(~22,106)
,(~116,~71)
,(~111,~72)
,(22,~113)
,(~109,27)
,(33,21)
,(7,72)
,(58,~47)
,(48,48)
,(53,~25)
,(~109,~107)
,(111,~46)
,(39,33)
,(~123,~26)
,(107,~121)
,(~68,~59)
,(~30,~74)
,(~70,~5)
,(86,62)
,(113,124)
,(106,~55)
,(~90,3)
,(~24,58)
,(~66,~72)
,(~102,~89)
,(103,33)
,(~12,~25)
,(~46,76)
,(~112,~126)
,(~109,~69)
,(~24,~27)
,(~43,~40)
,(~37,116)
,(~111,~33)
,(31,12)
,(117,36)
,(~42,35)
,(~92,~75)
,(~50,~61)
,(~82,59)
];
List.app (fn x => (print ("fromLarge " ^ LargeInt.toString x ^ " = "); call Int8.fromLarge x))
[~128
,~127
,~77
,~3
,~2
,~1
,0
,1
,2
,3
,78
,126
,127
,~80
,23
,~33
,~113
,80
,82
,17
,~105
,11
,116
,~78
,59
,5
,14
,~87
,120
,~34
,~36
,~37
,109
,~62
,~119
,~53
,27
,~6
,~104
,96
,~54
,34
,98
,107
,~97
,40
,91
,~94
,61
,~56
,~21
,112
,22
,56
,~9
,31
,117
,~90
,~11113
,60141
,11415
,~24799
,52581
,~29136
,~48921
,~55169
,46430
,~58
,18169
,~22745
,~21360
,59987
,51029
,~53359
,49927
,64236
,11314
,~60997
,~36003
,32827
,~51996
,711
,22843
,32590
,36440
,12967
,25363
,1675
,2411
,~38080
,64632
,25131
,~30218
,28261
,~56921
,~34766
,55009
,25463
,38658
,3182
,~34298
,~14079
,23614
,~33547
,13328
,8986
,~46686
,~8065
,59201
,64185
,31582
,~3409
,30147
,55445
,~16784
,61711
,~28832
,~43634
,~2629
,~60780
,9258
,~36259
,39118
,~10136
,~12706
,~20396
,47491
,61902
,60009
,57693
,47772
,2834
,15213
,60365
,45647
,56656
,~46047
,~21103
,46378
,63269
,19357
,26896
,41285
,21747
,11041
,5430
,~55734
,~59713
,9481
,402
,~34545
,54921
,~595
,34938
,46605
,17839
,44407
,~24395
];
List.app (fn s => (print ("fromString " ^ s ^ " = "); callO Int8.fromString s))
["-128"
,"-127"
,"-77"
,"-3"
,"-2"
,"-1"
,"0"
,"1"
,"2"
,"3"
,"78"
,"126"
,"127"
,"-80"
,"23"
,"-33"
,"-113"
,"80"
,"82"
,"17"
,"-105"
,"11"
,"116"
,"-78"
,"59"
,"5"
,"14"
,"-87"
,"120"
,"-34"
,"-36"
,"-37"
,"109"
,"-62"
,"-119"
,"-53"
,"27"
,"-6"
,"-104"
,"96"
,"-54"
,"34"
,"98"
,"107"
,"-97"
,"40"
,"91"
,"-94"
,"61"
,"-56"
,"-21"
,"112"
,"22"
,"56"
,"-9"
,"31"
,"117"
,"-90"
,"-11113"
,"60141"
,"11415"
,"-24799"
,"52581"
,"-29136"
,"-48921"
,"-55169"
,"46430"
,"-58"
,"18169"
,"-22745"
,"-21360"
,"59987"
,"51029"
,"-53359"
,"49927"
,"64236"
,"11314"
,"-60997"
,"-36003"
,"32827"
,"-51996"
,"711"
,"22843"
,"32590"
,"36440"
,"12967"
,"25363"
,"1675"
,"2411"
,"-38080"
,"64632"
,"25131"
,"-30218"
,"28261"
,"-56921"
,"-34766"
,"55009"
,"25463"
,"38658"
,"3182"
,"-34298"
,"-14079"
,"23614"
,"-33547"
,"13328"
,"8986"
,"-46686"
,"-8065"
,"59201"
,"64185"
,"31582"
,"-3409"
,"30147"
,"55445"
,"-16784"
,"61711"
,"-28832"
,"-43634"
,"-2629"
,"-60780"
,"9258"
,"-36259"
,"39118"
,"-10136"
,"-12706"
,"-20396"
,"47491"
,"61902"
,"60009"
,"57693"
,"47772"
,"2834"
,"15213"
,"60365"
,"45647"
,"56656"
,"-46047"
,"-21103"
,"46378"
,"63269"
,"19357"
,"26896"
,"41285"
,"21747"
,"11041"
,"5430"
,"-55734"
,"-59713"
,"9481"
,"402"
,"-34545"
,"54921"
,"-595"
,"34938"
,"46605"
,"17839"
,"44407"
,"-24395"
];
List.app (fn s => (print ("scan BIN " ^ s ^ " = "); callO (StringCvt.scanString (Int8.scan StringCvt.BIN)) s))
["-10000000"
,"-1111111"
,"-1001101"
,"-11"
,"-10"
,"-1"
,"0"
,"1"
,"10"
,"11"
,"1001110"
,"1111110"
,"1111111"
,"-1010000"
,"10111"
,"-100001"
,"-1110001"
,"1010000"
,"1010010"
,"10001"
,"-1101001"
,"1011"
,"1110100"
,"-1001110"
,"111011"
,"101"
,"1110"
,"-1010111"
,"1111000"
,"-100010"
,"-100100"
,"-100101"
,"1101101"
,"-111110"
,"-1110111"
,"-110101"
,"11011"
,"-110"
,"-1101000"
,"1100000"
,"-110110"
,"100010"
,"1100010"
,"1101011"
,"-1100001"
,"101000"
,"1011011"
,"-1011110"
,"111101"
,"-111000"
,"-10101"
,"1110000"
,"10110"
,"111000"
,"-1001"
,"11111"
,"1110101"
,"-1011010"
,"-10101101101001"
,"1110101011101101"
,"10110010010111"
,"-110000011011111"
,"1100110101100101"
,"-111000111010000"
,"-1011111100011001"
,"-1101011110000001"
,"1011010101011110"
,"-111010"
,"100011011111001"
,"-101100011011001"
,"-101001101110000"
,"1110101001010011"
,"1100011101010101"
,"-1101000001101111"
,"1100001100000111"
,"1111101011101100"
,"10110000110010"
,"-1110111001000101"
,"-1000110010100011"
,"1000000000111011"
,"-1100101100011100"
,"1011000111"
,"101100100111011"
,"111111101001110"
,"1000111001011000"
,"11001010100111"
,"110001100010011"
,"11010001011"
,"100101101011"
,"-1001010011000000"
,"1111110001111000"
,"110001000101011"
,"-111011000001010"
,"110111001100101"
,"-1101111001011001"
,"-1000011111001110"
,"1101011011100001"
,"110001101110111"
,"1001011100000010"
,"110001101110"
,"-1000010111111010"
,"-11011011111111"
,"101110000111110"
,"-1000001100001011"
,"11010000010000"
,"10001100011010"
,"-1011011001011110"
,"-1111110000001"
,"1110011101000001"
,"1111101010111001"
,"111101101011110"
,"-110101010001"
,"111010111000011"
,"1101100010010101"
,"-100000110010000"
,"1111000100001111"
,"-111000010100000"
,"-1010101001110010"
,"-101001000101"
,"-1110110101101100"
,"10010000101010"
,"-1000110110100011"
,"1001100011001110"
,"-10011110011000"
,"-11000110100010"
,"-100111110101100"
,"1011100110000011"
,"1111000111001110"
,"1110101001101001"
,"1110000101011101"
,"1011101010011100"
,"101100010010"
,"11101101101101"
,"1110101111001101"
,"1011001001001111"
,"1101110101010000"
,"-1011001111011111"
,"-101001001101111"
,"1011010100101010"
,"1111011100100101"
,"100101110011101"
,"110100100010000"
,"1010000101000101"
,"101010011110011"
,"10101100100001"
,"1010100110110"
,"-1101100110110110"
,"-1110100101000001"
,"10010100001001"
,"110010010"
,"-1000011011110001"
,"1101011010001001"
,"-1001010011"
,"1000100001111010"
,"1011011000001101"
,"100010110101111"
,"1010110101110111"
,"-101111101001011"
];
List.app (fn s => (print ("scan OCT " ^ s ^ " = "); callO (StringCvt.scanString (Int8.scan StringCvt.OCT)) s))
["-200"
,"-177"
,"-115"
,"-3"
,"-2"
,"-1"
,"0"
,"1"
,"2"
,"3"
,"116"
,"176"
,"177"
,"-120"
,"27"
,"-41"
,"-161"
,"120"
,"122"
,"21"
,"-151"
,"13"
,"164"
,"-116"
,"73"
,"5"
,"16"
,"-127"
,"170"
,"-42"
,"-44"
,"-45"
,"155"
,"-76"
,"-167"
,"-65"
,"33"
,"-6"
,"-150"
,"140"
,"-66"
,"42"
,"142"
,"153"
,"-141"
,"50"
,"133"
,"-136"
,"75"
,"-70"
,"-25"
,"160"
,"26"
,"70"
,"-11"
,"37"
,"165"
,"-132"
,"-25551"
,"165355"
,"26227"
,"-60337"
,"146545"
,"-70720"
,"-137431"
,"-153601"
,"132536"
,"-72"
,"43371"
,"-54331"
,"-51560"
,"165123"
,"143525"
,"-150157"
,"141407"
,"175354"
,"26062"
,"-167105"
,"-106243"
,"100073"
,"-145434"
,"1307"
,"54473"
,"77516"
,"107130"
,"31247"
,"61423"
,"3213"
,"4553"
,"-112300"
,"176170"
,"61053"
,"-73012"
,"67145"
,"-157131"
,"-103716"
,"153341"
,"61567"
,"113402"
,"6156"
,"-102772"
,"-33377"
,"56076"
,"-101413"
,"32020"
,"21432"
,"-133136"
,"-17601"
,"163501"
,"175271"
,"75536"
,"-6521"
,"72703"
,"154225"
,"-40620"
,"170417"
,"-70240"
,"-125162"
,"-5105"
,"-166554"
,"22052"
,"-106643"
,"114316"
,"-23630"
,"-30642"
,"-47654"
,"134603"
,"170716"
,"165151"
,"160535"
,"135234"
,"5422"
,"35555"
,"165715"
,"131117"
,"156520"
,"-131737"
,"-51157"
,"132452"
,"173445"
,"45635"
,"64420"
,"120505"
,"52363"
,"25441"
,"12466"
,"-154666"
,"-164501"
,"22411"
,"622"
,"-103361"
,"153211"
,"-1123"
,"104172"
,"133015"
,"42657"
,"126567"
,"-57513"
];
List.app (fn s => (print ("scan HEX " ^ s ^ " = "); callO (StringCvt.scanString (Int8.scan StringCvt.HEX)) s))
["-80"
,"-7f"
,"-4d"
,"-3"
,"-2"
,"-1"
,"0"
,"1"
,"2"
,"3"
,"4e"
,"7e"
,"7f"
,"-50"
,"17"
,"-21"
,"-71"
,"50"
,"52"
,"11"
,"-69"
,"b"
,"74"
,"-4e"
,"3b"
,"5"
,"e"
,"-57"
,"78"
,"-22"
,"-24"
,"-25"
,"6d"
,"-3e"
,"-77"
,"-35"
,"1b"
,"-6"
,"-68"
,"60"
,"-36"
,"22"
,"62"
,"6b"
,"-61"
,"28"
,"5b"
,"-5e"
,"3d"
,"-38"
,"-15"
,"70"
,"16"
,"38"
,"-9"
,"1f"
,"75"
,"-5a"
,"-2b69"
,"eaed"
,"2c97"
,"-60df"
,"cd65"
,"-71d0"
,"-bf19"
,"-d781"
,"b55e"
,"-3a"
,"46f9"
,"-58d9"
,"-5370"
,"ea53"
,"c755"
,"-d06f"
,"c307"
,"faec"
,"2c32"
,"-ee45"
,"-8ca3"
,"803b"
,"-cb1c"
,"2c7"
,"593b"
,"7f4e"
,"8e58"
,"32a7"
,"6313"
,"68b"
,"96b"
,"-94c0"
,"fc78"
,"622b"
,"-760a"
,"6e65"
,"-de59"
,"-87ce"
,"d6e1"
,"6377"
,"9702"
,"c6e"
,"-85fa"
,"-36ff"
,"5c3e"
,"-830b"
,"3410"
,"231a"
,"-b65e"
,"-1f81"
,"e741"
,"fab9"
,"7b5e"
,"-d51"
,"75c3"
,"d895"
,"-4190"
,"f10f"
,"-70a0"
,"-aa72"
,"-a45"
,"-ed6c"
,"242a"
,"-8da3"
,"98ce"
,"-2798"
,"-31a2"
,"-4fac"
,"b983"
,"f1ce"
,"ea69"
,"e15d"
,"ba9c"
,"b12"
,"3b6d"
,"ebcd"
,"b24f"
,"dd50"
,"-b3df"
,"-526f"
,"b52a"
,"f725"
,"4b9d"
,"6910"
,"a145"
,"54f3"
,"2b21"
,"1536"
,"-d9b6"
,"-e941"
,"2509"
,"192"
,"-86f1"
,"d689"
,"-253"
,"887a"
,"b60d"
,"45af"
,"ad77"
,"-5f4b"
];
