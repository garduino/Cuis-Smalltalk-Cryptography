'From Cuis 4.0 of 21 April 2012 [latest update: #1267] on 30 April 2012 at 7:10:28 pm'!
'Description Please enter a description for this package '!
!classDefinition: #Blowfish category: #Blowfish!
Object subclass: #Blowfish
	instanceVariableNames: 'rounds piArray s0 s1 s2 s3 xl xr current key data index'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Blowfish'!
!classDefinition: 'Blowfish class' category: #Blowfish!
Blowfish class
	instanceVariableNames: ''!

!classDefinition: #BlowfishProfiling category: #Blowfish!
Object subclass: #BlowfishProfiling
	instanceVariableNames: 'keys clear encrypted'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Blowfish'!
!classDefinition: 'BlowfishProfiling class' category: #Blowfish!
BlowfishProfiling class
	instanceVariableNames: ''!

!classDefinition: #BlowfishTests category: #Blowfish!
TestCase subclass: #BlowfishTests
	instanceVariableNames: 'keys clear encrypted'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Blowfish'!
!classDefinition: 'BlowfishTests class' category: #Blowfish!
BlowfishTests class
	instanceVariableNames: ''!


!Blowfish commentStamp: '<historical>' prior: 0!
This is just enough of the Blowfish algorithm from http://www.openbsd.org/cgi-bin/cvsweb/~checkout~/src/lib/libc/crypt/blowfish.cto be able to run BCrypt.  Instance Variables:	rounds	<AbstractSound | CalendarDuration | Collection | Color | DateAndTime | DhbDecimalFloatingNumber | DhbMatrix | DhbPolynomial | Duration | InfiniteDuration | Number | PassportNotAMetanumber | Point | ScientificDuration | TemporalInterval | Timespan | TraitComposition | TraitDescription | TraitTransformation>	piArray	<Object>	s0	<Matrix>	s1	<Object>	s2	<Object>	s3	<Object>	s4	<ProtoObject | PseudoContext>	xl	<Integer>	xr	<Integer>	current	<Integer>	key	<ProtoObject | PseudoContext>	data	<Object>!

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 12:03'!
blfDec: dataArray for: blocks	| j newDataArray |	newDataArray := Array new: dataArray size.	j := 1.	(1 to: blocks)		do: [ :each | 			xl := dataArray at: j.			xr := dataArray at: j + 1.			self decipher.			newDataArray at: j put: xl.			newDataArray at: j + 1 put: xr.			j := j + 2 ].	^ newDataArray! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'gsa 4/30/2012 18:48'!
blfEcbDecrypt: dataArray for: times

 |  newDataArray |
newDataArray:=dataArray copy.

	(1 to: times by: 8) do:[ :each | 
		
		xl:=((((newDataArray at: 1) bitShift: 24) bitOr: ((newDataArray at: 2) bitShift: 16) )bitOr: ((newDataArray at: 3) bitShift: 8)) bitOr: (newDataArray at: 4).  
		xr:=((((newDataArray at: 5) bitShift: 24) bitOr: ((newDataArray at: 6) bitShift: 16) )bitOr: ((newDataArray at: 7) bitShift: 8)) bitOr: (newDataArray at: 8).
		
		self decipher .
		
		newDataArray at: 1 put: ((xl  bitShift: -24) bitAnd: 16rFF).		
		newDataArray at: 2 put: ((xl  bitShift: -16) bitAnd: 16rFF).	
		newDataArray at: 3 put: ((xl  bitShift: -8) bitAnd: 16rFF).	
		newDataArray at: 4 put: (xl bitAnd: 16rFF).
		newDataArray at: 5 put: ((xr  bitShift: -24) bitAnd: 16rFF).	
		newDataArray at: 6 put: ((xr  bitShift: -16) bitAnd: 16rFF).
		newDataArray at: 7 put: ((xr  bitShift: -8) bitAnd: 16rFF).	
		newDataArray at: 8 put: (xr bitAnd: 16rFF).	
		]	.
		
	^newDataArray ! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'gsa 4/30/2012 18:50'!
blfEcbEncrypt: dataArray for: times

 |  newDataArray |
newDataArray:=dataArray copy.

	(1 to: times by: 8) do:[ :each | 
		
		xl:=((((newDataArray at: 1) bitShift: 24) bitOr: ((newDataArray at: 2) bitShift: 16) )bitOr: ((newDataArray at: 3) bitShift: 8)) bitOr: (newDataArray at: 4).  
		xr:=((((newDataArray at: 5) bitShift: 24) bitOr: ((newDataArray at: 6) bitShift: 16) )bitOr: ((newDataArray at: 7) bitShift: 8)) bitOr: (newDataArray at: 8).
		
		self encipher .
		
		newDataArray at: 1 put: ((xl  bitShift: -24) bitAnd: 16rFF).		
		newDataArray at: 2 put: ((xl  bitShift: -16) bitAnd: 16rFF).	
		newDataArray at: 3 put: ((xl  bitShift: -8) bitAnd: 16rFF).	
		newDataArray at: 4 put: (xl bitAnd: 16rFF).
		newDataArray at: 5 put: ((xr  bitShift: -24) bitAnd: 16rFF).	
		newDataArray at: 6 put: ((xr  bitShift: -16) bitAnd: 16rFF).
		newDataArray at: 7 put: ((xr  bitShift: -8) bitAnd: 16rFF).	
		newDataArray at: 8 put: (xr bitAnd: 16rFF).	
		]	.
		
	^newDataArray ! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 12:03'!
blfEnc: dataArray for: blocks	| j newDataArray |	newDataArray := dataArray copy.	j := 1.	(1 to: blocks)		do: [ :each | 			xl := newDataArray at: j.			xr := newDataArray at: j + 1.			self encipher.			newDataArray at: j put: xl.			newDataArray at: j + 1 put: xr.			j := j + 2 ].	^ newDataArray! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 13:39'!
blfKey: aKey	index:=0.	key:= aKey.	"self initializeLittleEndianBoxes."	self initializeBoxes.	self expandZeroState.! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/26/2011 19:16'!
calculateBlfRndFor:oneHalf with: otherHalf andPiAt: n"#define BLFRND(s,p,i,j,n) (i ^= F(s,j) ^ (p)[n])"	^ oneHalf bitXor: ((self feistelWith: otherHalf  ) bitXor: (piArray at: n)) .! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 12:04'!
decipher	| xL xR temp |	xL := xl copy.	xR := xr copy.	xL := xL bitXor: (piArray at: 18).	(17 to: 2 by: -2)		do: [ :each | 			xR := self calculateBlfRndFor: xR with: xL andPiAt: each.			xL := self calculateBlfRndFor: xL with: xR andPiAt: each - 1 ].	xl := xR bitXor: (piArray at: 1).	xr := xL! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/21/2011 15:29'!
decrypt: someData with:   aKeyString		self setRounds: self class defaultRounds .	self blfKey: aKeyString.	^self blfDec: someData for: someData size // 2.! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/21/2011 15:28'!
ecbDecrypt: someData with:   aKeyString	self setRounds: self class defaultRounds .	self blfKey: aKeyString.	^self blfEcbDecrypt: someData for: someData size // 2.! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/21/2011 15:28'!
ecbEncrypt: someData with:   aKeyString	self setRounds: self class defaultRounds .	self blfKey: aKeyString.	^self blfEcbEncrypt: someData for: someData size // 2.! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 12:05'!
encipher	| xL xR temp |	xL := xl copy.	xR := xr copy.	xL := xL bitXor: (piArray at: 1)."	index <= 2		ifTrue: [ 			Transcript				cr;				show: 'New encipher';				cr;				show: 'Left: ';				show: xL hex greaseString ]."	(2 to: 17 by: 2)		do: [ :each | 			xR := self calculateBlfRndFor: xR with: xL andPiAt: each.			xL := self calculateBlfRndFor: xL with: xR andPiAt: each + 1."			index = 2				ifTrue: [ 					Transcript						cr;						show: 'Right: ';						show: xR hex greaseString;						cr;						show: 'Left: ';						show: xL hex greaseString.					index = 0 ] ]."].	xR := xR bitXor: (piArray at: 18).	xl := xR.	xr := xL! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/22/2011 18:51'!
encrypt: someData with:   aKeyString	self setRounds: self class defaultRounds .	self blfKey: aKeyString.		^self blfEnc: someData  for: someData size // 2.! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/24/2011 22:20'!
expandZeroState	| dataL dataR s0Test s1Test s2Test s3Test  pTest|	current := 1.pTest:=piArray copy.	(1 to: rounds + 2)		do: [ :each | piArray at: each put: ((piArray at: each) bitXor: (self stream2word: key)) ]."xl := 16r0.xr := 16r0."xl:=#[0 0 0 0] unsignedLongAt: 1 bigEndian:   true.xr:=#[0 0 0 0] unsignedLongAt: 1 bigEndian:   true.pTest:=piArray copy.		(1 to: rounds + 2 by: 2)		do: [ :each | 					self encipher.					piArray at: each put: xl copy.					piArray at: each+1 put: xr copy.].s0Test :=s0 copy.s1Test :=s1 copy.s2Test :=s2 copy.s3Test :=s3 copy.		(1 to:256 by:2)		do: [ :each | self encipher.					s0 at: each put: xl copy.					s0 at: each+1 put: xr copy.					].		(1 to:256 by:2)		do: [ :each | self encipher.					s1 at: each put: xl copy.					s1 at: each+1 put: xr copy.].		(1 to:256 by:2)		do: [ :each | self encipher.					s2 at: each put: xl copy.					s2 at: each+1 put: xr copy.].		(1 to:256 by:2)		do: [ :each | self encipher.					s3 at: each put: xl copy.					s3 at: each+1 put: xr copy.].			"	s0Test:= s0 select:[:each | each asByteArray size >4].	s1Test:= s1 select:[:each | each asByteArray size >4].	s2Test:= s2 select:[:each | each asByteArray size >4].	s3Test:= s3 select:[:each | each asByteArray size >4].	(s0Test size + s1Test size + s2Test size + s3Test size) >0 ifTrue:[self halt]."! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'gsa 4/30/2012 18:47'!
feistelWith: otherHalf
	| a b c d y byteArray |
	a := ((otherHalf bitShift: -24) bitAnd: 16rFF) + 1.
	b := ((otherHalf bitShift: -16) bitAnd: 16rFF) + 1.
	c := ((otherHalf bitShift: -8) bitAnd: 16rFF) + 1.
	d := (otherHalf bitAnd: 16rFF) + 1.
	y := ((s0 at: a) + (s1 at: b)) \\ 4294967296.	" (2 raisedTo: 32)"
	y := y bitXor: (s2 at: c).
	y := (y + (s3 at: d)) \\ 4294967296.	"(2 raisedTo: 32)"
	^ y! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 12:12'!
initializeBoxes	piArray :=self class pBox copy.	s0:=self class s0Box copy.	s1:=self class s1Box copy.	s2:=self class s2Box copy.	s3:=self class s3Box copy.		! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/21/2011 11:02'!
setRounds: anInteger	rounds:=(anInteger >= self class minRounds and:[anInteger <= self class maxRounds]) ifTrue: [anInteger] ifFalse:[self class defaultRounds ]   ! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/24/2011 20:55'!
stream2word: someData |temp j dataBytes |temp:=0.dataBytes := someData size.1 to: 4 do:  [ :each | 		temp:=(temp bitShift: 8 ) bitOr: ((someData at: current) bitAnd: 16rFF).	current := (current \\ dataBytes) +1.	   ].^temp.! !

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/26/2011 12:09'!
stream2word: someData length: someBytes|temp j  |temp:=0.1 to: 4 do:  [ :each | 		temp:=(temp bitShift: 8 ) bitOr: ((someData atWrap: current) bitAnd: 16rFF).	current := (current \\ someBytes) +1.	   ].^temp.! !

!Blowfish class methodsFor: 'defaults' stamp: 'PaulDeBruicker 4/21/2011 11:02'!
blockSize	^8! !

!BlowfishProfiling methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:49'!
initialize	super initialize.	self		initializeClear;		initializeEncrypted;		initializeKeys! !

!BlowfishProfiling methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:49'!
initializeClear	| tmpClear |	clear := OrderedCollection new.	tmpClear := #('0000000000000000' 'FFFFFFFFFFFFFFFF' '1000000000000001' '1111111111111111' '1111111111111111' '0123456789ABCDEF' '0000000000000000' '0123456789ABCDEF' '01A1D6D039776742' '5CD54CA83DEF57DA' '0248D43806F67172' '51454B582DDF440A' '42FD443059577FA2' '059B5E0851CF143A' '0756D8E0774761D2' '762514B829BF486A' '3BDD119049372802' '26955F6835AF609A' '164D5E404F275232' '6B056E18759F5CCA' '004BD6EF09176062' '480D39006EE762F2' '437540C8698F3CFA' '072D43A077075292' '02FE55778117F12A' '1D9D5C5018F728C2' '305532286D6F295A' '0123456789ABCDEF' '0123456789ABCDEF' '0123456789ABCDEF' 'FFFFFFFFFFFFFFFF' '0000000000000000' '0000000000000000' 'FFFFFFFFFFFFFFFF').	tmpClear		do: [ :each | 			| array tmpByteArray |			array := WordArray new: 2.			array at: 1 put: ((ByteArray fromHexString: (each copyFrom: 1 to: 8)) unsignedLongAt: 1 bigEndian: true).			array at: 2 put: ((ByteArray fromHexString: (each copyFrom: 9 to: 16)) unsignedLongAt: 1 bigEndian: true).			clear add: array ]! !

!BlowfishProfiling methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:49'!
initializeEncrypted	| tmpEncrypted |	encrypted := OrderedCollection new.	tmpEncrypted := #('4EF997456198DD78' '51866FD5B85ECB8A' '7D856F9A613063F2' '2466DD878B963C9D' '61F9C3802281B096' '7D0CC630AFDA1EC7' '4EF997456198DD78' '0ACEAB0FC6A0A28D' '59C68245EB05282B' 'B1B8CC0B250F09A0' '1730E5778BEA1DA4' 'A25E7856CF2651EB' '353882B109CE8F1A' '48F4D0884C379918' '432193B78951FC98' '13F04154D69D1AE5' '2EEDDA93FFD39C79' 'D887E0393C2DA6E3' '5F99D04F5B163969' '4A057A3B24D3977B' '452031C1E4FADA8E' '7555AE39F59B87BD' '53C55F9CB49FC019' '7A8E7BFA937E89A3' 'CF9C5D7A4986ADB5' 'D1ABB290658BC778' '55CB3774D13EF201' 'FA34EC4847B268B2' 'A790795108EA3CAE' 'C39E072D9FAC631D' '014933E0CDAFF6E4' 'F21E9A77B71C49BC' '245946885754369A' '6B5C5A9C5D9E0A5A').	tmpEncrypted		do: [ :each | 			| array tmpByteArray |			array := WordArray new: 2.			array at: 1 put: ((ByteArray fromHexString: (each copyFrom: 1 to: 8)) unsignedLongAt: 1 bigEndian: true).			array at: 2 put: ((ByteArray fromHexString: (each copyFrom: 9 to: 16)) unsignedLongAt: 1 bigEndian: true).			encrypted add: array ]! !

!BlowfishProfiling methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:49'!
initializeKeys	| tempKeys |	keys := OrderedCollection new.	tempKeys := #('0000000000000000' 'FFFFFFFFFFFFFFFF' '3000000000000000' '1111111111111111' '0123456789ABCDEF' '1111111111111111' '0000000000000000' 'FEDCBA9876543210' '7CA110454A1A6E57' '0131D9619DC1376E' '07A1133E4A0B2686' '3849674C2602319E' '04B915BA43FEB5B6' '0113B970FD34F2CE' '0170F175468FB5E6' '43297FAD38E373FE' '07A7137045DA2A16' '04689104C2FD3B2F' '37D06BB516CB7546' '1F08260D1AC2465E' '584023641ABA6176' '025816164629B007' '49793EBC79B3258F' '4FB05E1515AB73A7' '49E95D6D4CA229BF' '018310DC409B26D6' '1C587F1C13924FEF' '0101010101010101' '1F1F1F1F0E0E0E0E' 'E0FEE0FEF1FEF1FE' '0000000000000000' 'FFFFFFFFFFFFFFFF' '0123456789ABCDEF' 'FEDCBA9876543210').	tempKeys do: [ :each | keys add: (ByteArray fromHexString: each) ]! !

!BlowfishProfiling methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:50'!
longDecryptionTest	(1 to: keys size)		do: [ :each | 			| key clearText cipherText enc |			key := keys at: each.			clearText := clear at: each.			cipherText := encrypted at: each.			enc := Blowfish decrypt: cipherText with: key ]! !

!BlowfishProfiling methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:50'!
longEncryptionTest	(1 to: keys size)		do: [ :each | 			| key clearText cipherText enc |			key := keys at: each.			clearText := clear at: each.			cipherText := encrypted at: each.			enc := Blowfish encrypt: clearText with: key ]! !

!BlowfishProfiling class methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:43'!
longTest	! !

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 13:39'!
setUp	"this data is from here: http://www.schneier.com/code/vectors.txt"	| tmpKeys tmpClear tmpEncrypted |	keys := OrderedCollection new.	clear := OrderedCollection new.	encrypted := OrderedCollection new.	tmpKeys := #('0000000000000000' 'FFFFFFFFFFFFFFFF' '3000000000000000' '1111111111111111' '0123456789ABCDEF' '1111111111111111' '0000000000000000' 'FEDCBA9876543210' '7CA110454A1A6E57' '0131D9619DC1376E' '07A1133E4A0B2686' '3849674C2602319E' '04B915BA43FEB5B6' '0113B970FD34F2CE' '0170F175468FB5E6' '43297FAD38E373FE' '07A7137045DA2A16' '04689104C2FD3B2F' '37D06BB516CB7546' '1F08260D1AC2465E' '584023641ABA6176' '025816164629B007' '49793EBC79B3258F' '4FB05E1515AB73A7' '49E95D6D4CA229BF' '018310DC409B26D6' '1C587F1C13924FEF' '0101010101010101' '1F1F1F1F0E0E0E0E' 'E0FEE0FEF1FEF1FE' '0000000000000000' 'FFFFFFFFFFFFFFFF' '0123456789ABCDEF' 'FEDCBA9876543210').	tmpClear := #('0000000000000000' 'FFFFFFFFFFFFFFFF' '1000000000000001' '1111111111111111' '1111111111111111' '0123456789ABCDEF' '0000000000000000' '0123456789ABCDEF' '01A1D6D039776742' '5CD54CA83DEF57DA' '0248D43806F67172' '51454B582DDF440A' '42FD443059577FA2' '059B5E0851CF143A' '0756D8E0774761D2' '762514B829BF486A' '3BDD119049372802' '26955F6835AF609A' '164D5E404F275232' '6B056E18759F5CCA' '004BD6EF09176062' '480D39006EE762F2' '437540C8698F3CFA' '072D43A077075292' '02FE55778117F12A' '1D9D5C5018F728C2' '305532286D6F295A' '0123456789ABCDEF' '0123456789ABCDEF' '0123456789ABCDEF' 'FFFFFFFFFFFFFFFF' '0000000000000000' '0000000000000000' 'FFFFFFFFFFFFFFFF').	tmpEncrypted := #('4EF997456198DD78' '51866FD5B85ECB8A' '7D856F9A613063F2' '2466DD878B963C9D' '61F9C3802281B096' '7D0CC630AFDA1EC7' '4EF997456198DD78' '0ACEAB0FC6A0A28D' '59C68245EB05282B' 'B1B8CC0B250F09A0' '1730E5778BEA1DA4' 'A25E7856CF2651EB' '353882B109CE8F1A' '48F4D0884C379918' '432193B78951FC98' '13F04154D69D1AE5' '2EEDDA93FFD39C79' 'D887E0393C2DA6E3' '5F99D04F5B163969' '4A057A3B24D3977B' '452031C1E4FADA8E' '7555AE39F59B87BD' '53C55F9CB49FC019' '7A8E7BFA937E89A3' 'CF9C5D7A4986ADB5' 'D1ABB290658BC778' '55CB3774D13EF201' 'FA34EC4847B268B2' 'A790795108EA3CAE' 'C39E072D9FAC631D' '014933E0CDAFF6E4' 'F21E9A77B71C49BC' '245946885754369A' '6B5C5A9C5D9E0A5A').			 "keys:=tmpKeys.	"	tmpKeys do: [ :each | keys add: (ByteArray fromHexString: each)  ].	tmpClear		do: [ :each | 			| array tmpByteArray |			array := WordArray new: 2.			array at: 1 put: ((ByteArray fromHexString: (each copyFrom: 1 to: 8)) unsignedLongAt: 1 bigEndian: true).			array at: 2 put: ((ByteArray fromHexString: (each copyFrom: 9 to: 16)) unsignedLongAt: 1 bigEndian: true).			clear add: array ].	tmpEncrypted		do: [ :each | 			| array tmpByteArray |			array := WordArray new: 2.			array at: 1 put: ((ByteArray fromHexString: (each copyFrom: 1 to: 8)) unsignedLongAt: 1 bigEndian: true).			array at: 2 put: ((ByteArray fromHexString: (each copyFrom: 9 to: 16)) unsignedLongAt: 1 bigEndian: true).			encrypted add: array ]! !

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'gsa 4/30/2012 18:52'!
testDecryption
|key data res|
key:='abcdefghijklmnopqrstuvwxyz'.
data:=WordArray  new: 2.
#(16r324ed0fe  16rF413a203) doWithIndex: [:each :i | data at: i put: ((each asByteArray) unsignedLongAt: 1 bigEndian:true)    ].
res:=(Blowfish decrypt: data with:  key asByteArray ).


self assert:((res at: 1) =  1112297303). "16r424c4f571"
self assert:((res at: 2) =   1179210568)."16r46495348"! !

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 12:12'!
testEcbDecrypt	|key cl enc res |key:=ByteArray fromHexString:   'FFFFFFFFFFFFFFFF'.cl := ByteArray fromHexString:  'FFFFFFFFFFFFFFFF'.enc :=ByteArray fromHexString:  '51866FD5B85ECB8A' . res:=(Blowfish ecbDecrypt: enc with: key ).self assert: (res = cl).! !

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 12:12'!
testEcbEncrypt	|key cl enc res |key:=ByteArray fromHexString:   'FFFFFFFFFFFFFFFF'.cl := ByteArray fromHexString:  'FFFFFFFFFFFFFFFF'.enc :=ByteArray fromHexString:  '51866FD5B85ECB8A' . res:=(Blowfish ecbEncrypt: cl with: key ) .self assert: (res= enc).! !

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/26/2011 15:38'!
testEncryptDecryptself assert:((Blowfish decryptToString:    (Blowfish encryptString: '0123456789' with: 'AAAAA'  )  with:'AAAAA'  )='0123456789')! !

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 13:42'!
testEncryption|key data res|key:='abcdefghijklmnopqrstuvwxyz'.data:=WordArray  new: 2.#(16r424c4f57  16r46495348) doWithIndex: [:each :i | data at: i put: ((each asByteArray) unsignedLongAt: 1 bigEndian:true)    ].res:=(Blowfish encrypt: data with:   key asByteArray).self assert:((res at: 1) = 844026110). "16r324ed0fe"self assert:((res at: 2) =  4094927363)."16rf413a203"! !

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 13:33'!
testLongDecryptionTest(1 to: keys size) do:[:each | |key clearText cipherText enc| 	key:= keys at: each.	clearText:=clear at: each.	cipherText:=encrypted at: each.	enc:=Blowfish decrypt: cipherText  with: key.   	self assert:   (enc = clearText asArray )	]! !

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 13:33'!
testLongEncryptionTest(1 to: keys size) do:[:each | |key clearText cipherText enc| 	key:= keys at: each.	clearText:=clear at: each.	cipherText:=encrypted at: each.	enc:=Blowfish encrypt: clearText  with: key.   	self assert:   (enc = cipherText )	]! !

!ByteArray class methodsFor: '*Blowfish' stamp: 'PaulDeBruicker 3/31/2012 10:54'!
fromHexString: aString	^ self readHexFrom: aString! !
