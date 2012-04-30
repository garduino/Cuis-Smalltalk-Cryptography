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
This is just enough of the Blowfish algorithm from 

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 12:03'!
blfDec: dataArray for: blocks

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
blfEnc: dataArray for: blocks

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 13:39'!
blfKey: aKey

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/26/2011 19:16'!
calculateBlfRndFor:oneHalf with: otherHalf andPiAt: n

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 12:04'!
decipher

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/21/2011 15:29'!
decrypt: someData with:   aKeyString

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/21/2011 15:28'!
ecbDecrypt: someData with:   aKeyString

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/21/2011 15:28'!
ecbEncrypt: someData with:   aKeyString

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 12:05'!
encipher

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/22/2011 18:51'!
encrypt: someData with:   aKeyString

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/24/2011 22:20'!
expandZeroState

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
initializeBoxes

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/21/2011 11:02'!
setRounds: anInteger

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/24/2011 20:55'!
stream2word: someData 

!Blowfish methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/26/2011 12:09'!
stream2word: someData length: someBytes

!Blowfish class methodsFor: 'defaults' stamp: 'PaulDeBruicker 4/21/2011 11:02'!
blockSize

!BlowfishProfiling methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:49'!
initialize

!BlowfishProfiling methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:49'!
initializeClear

!BlowfishProfiling methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:49'!
initializeEncrypted

!BlowfishProfiling methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:49'!
initializeKeys

!BlowfishProfiling methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:50'!
longDecryptionTest

!BlowfishProfiling methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:50'!
longEncryptionTest

!BlowfishProfiling class methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/10/2012 13:43'!
longTest

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 13:39'!
setUp

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
testEcbDecrypt

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 12:12'!
testEcbEncrypt

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/26/2011 15:38'!
testEncryptDecrypt

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 13:42'!
testEncryption

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 13:33'!
testLongDecryptionTest

!BlowfishTests methodsFor: 'as yet unclassified' stamp: 'PaulDeBruicker 4/25/2011 13:33'!
testLongEncryptionTest

!ByteArray class methodsFor: '*Blowfish' stamp: 'PaulDeBruicker 3/31/2012 10:54'!
fromHexString: aString