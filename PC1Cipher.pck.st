'From Cuis 4.1 of 12 December 2012 [latest update: #1583] on 5 February 2013 at 7:16:44 pm'!
'Description Please enter a description for this package '!
!classDefinition: #PC1Cipher category: #PC1Cipher!
Object subclass: #PC1Cipher
	instanceVariableNames: 'key box i j'
	classVariableNames: 'RandPool'
	poolDictionaries: ''
	category: 'PC1Cipher'!
!classDefinition: 'PC1Cipher class' category: #PC1Cipher!
PC1Cipher class
	instanceVariableNames: ''!


!PC1Cipher commentStamp: 'gsa 2/5/2013 19:13' prior: 0!
This class provides a stream encoder/decoder using the PC1 algorithm.
Portions �1997 by Stefan Matthias Aust 


Algunos ejemplos para el workspace, debuggeando:

PC1Cipher example1.

int := 6193852674810616066993431132236433207.

bytes := PC1Cipher byteArrayFromInteger: int.

thisCipher := PC1Cipher withKeyBytes: bytes.

thisCipher key -> (thisCipher cipherString: 'This is a PC1 test').

encr := thisCipher cipherString: 'This is a PC1 test'.

thisCipher cipherString: encr.



Ojo, que en el metodo byteArrayFromInteger: aPositiveIntegerKey 

cambie byteSize por size (ver en Dolphin la implementacion de byteSize para portarla, en LargePositiveInteger).
!

!Delay class methodsFor: '*PC1Cipher' stamp: 'gsa 1/8/2013 18:09'!
millisecondClockValue
	"Private - Answer the current millisecond clock value."
"
	<primitive: 174>
	^self primitiveFailed "
	
	" Needed for PC1Cipher port from Dolphin "
		^DateAndTime millisecondClockValue 
! !

!PC1Cipher methodsFor: 'as yet unclassified' stamp: 'gsa 12/29/2012 22:03'!
advance
	"Advance the state of the cypher stream by one step without actually
	encyphering anything.
	This cypher is known to have some slight weaknesses in the first few bytes
	that it emits (they reflect the initial state rather predictably) and this makes
	combinations of similar texts and similar keys rather fragile (in theory) for the
	first few iterations.  For that reason it's a good cautious move to 'advance'
	the cypher by a known (but, ideally, not too easily predictable) number of rounds
	before using it. Experts seem to think that anything over 255 (the nummber of
	boxes) is enough.
	Needless to say the encryption and decryption must agree
	on how many rounds to use."

	i := i + 1 bitAnd: 255.
	j := j + (box at: i + 1) bitAnd: 255.
	box swap: (i + 1) with: (j + 1).
! !

!PC1Cipher methodsFor: 'as yet unclassified' stamp: 'gsa 12/29/2012 22:03'!
advance: anInteger
	"Advance the state of the cypher stream the given number of steps without actually
	encyphering anything.  See the comment in #advance for why it you might want to do so."

	anInteger timesRepeat: [self advance].! !

!PC1Cipher methodsFor: 'as yet unclassified' stamp: 'gsa 12/30/2012 10:19'!
cipher: aByte
	"Cipher aByte (between 0 and 255). As PC1 is a symetric stream encoder, 
	the answer is either the encoded or decoded version."

	"deprecated because the name is dangerously misleading"
	" Notification deprecated."

	"this method is too dangerous to exist, even with a deprecated note,
	without this check since this method will malfunction *silently* if passed
	any integer except a byte value"
	self assert: [aByte between: 0 and: 255].

	^ self cipherByte: aByte.! !

!PC1Cipher methodsFor: 'as yet unclassified' stamp: 'gsa 12/29/2012 22:05'!
cipherByte: aByte
	"Cipher aByte (between 0 and 255). As PC1 is a symetric stream encoder, 
	the answer is either the encoded or decoded version."

	self advance.
	^aByte bitXor: (box at: (((box at: i + 1) + (box at: j + 1)) bitAnd: 255) + 1).! !

!PC1Cipher methodsFor: 'as yet unclassified' stamp: 'gsa 12/29/2012 22:05'!
cipherBytes: aByteArray
	"Answer an encoded (or decoded) version of aByteArray."

	^aByteArray collect: [:each | self cipherByte: each].! !

!PC1Cipher methodsFor: 'as yet unclassified' stamp: 'gsa 12/29/2012 22:06'!
cipherString: aString
	"Answer an encoded (or decoded) version of aString."

	^ aString collect: [:each | (self cipherByte: each asInteger) asCharacter].! !

!PC1Cipher methodsFor: 'as yet unclassified' stamp: 'gsa 12/29/2012 22:06'!
key
	"Answer the key of the receiver"

	^key
! !

!PC1Cipher methodsFor: 'as yet unclassified' stamp: 'gsa 12/29/2012 22:06'!
key: k
	"Private - Initialize box from k, a ByteArray of up to 256 bytes."

	key := k.	
	self reset.
! !

!PC1Cipher methodsFor: 'as yet unclassified' stamp: 'gsa 12/29/2012 22:07'!
postCopy
	box := box copy! !

!PC1Cipher methodsFor: 'as yet unclassified' stamp: 'gsa 12/29/2012 22:07'!
reset
	"Reset the state of the receiver"

	| n |
	box := (0 to: 255) asByteArray.
	j := 0.
	n := key size.
	0 to: 255
		do: 
			[:x | 
			j := (key at: x \\ n + 1) + (box at: x + 1) + j bitAnd: 255.
			box swap: x + 1 with: j + 1].
	i := j := 0! !

!PC1Cipher class methodsFor: 'as yet unclassified' stamp: 'gsa 2/5/2013 18:37'!
byteArrayFromInteger: aPositiveIntegerKey 
" ojo asByteArray"
" cuando uno intente devolver la coleccion con el metodo asByteArray no reconoce el metodo, pero si primero se asigna a una variable, si"

	| bytes n |
	bytes := OrderedCollection new: aPositiveIntegerKey size "byteSize".
	n := aPositiveIntegerKey.
	[n = 0] whileFalse: 
			[bytes add: (n bitAnd: 16rFF).
			n := n bitShift: -8].
	"	s := bytes asByteArray."
	" self halt."
	^ bytes asByteArray
		"^ s"! !

!PC1Cipher class methodsFor: 'as yet unclassified' stamp: 'gsa 12/30/2012 13:24'!
churnRandPool
	"Private - Churn the random seed pool with a real world event."

	| sha |
	sha := SecureHashAlgorithm new.
	sha hashInteger: RandPool.
	sha hashInteger: Random systemClockSeed.
	^ RandPool := sha finalHash! !

!PC1Cipher class methodsFor: 'as yet unclassified' stamp: 'gsa 12/30/2012 10:43'!
example1
	"Encrypt a simple string.
		self example1
	"

	| encoder |
	encoder := self withStrength: 128.
	self halt.
	^encoder key -> (encoder cipherString: 'This is a PC1 test')
 ! !

!PC1Cipher class methodsFor: 'as yet unclassified' stamp: 'gsa 12/29/2012 22:12'!
example2
	"
		self example2
	"

	| encoder |
	encoder := self withKeyString: 'Dolphin'.
	^encoder cipherString: '%`���1g^@T��8޼s$'
 ! !

!PC1Cipher class methodsFor: 'as yet unclassified' stamp: 'gsa 12/30/2012 13:25'!
generateRandomWithBits: bitSize 
	"Answers a random <Integer> with the supplied bitSize. 
	The bitSize must be >=16 and will be rounded up to a multiple of 8.

	Applied Cryptography (Schneier) p427.

	(self generateRandomWithBits: 26) digitLength*8
	"

	| len random count sha |
	len := (bitSize + 7) // 8.
	self churnRandPool.
	count := self randPool \\ 16r3FFF.
	sha := SecureHashAlgorithm new.
	random := 0.
	[len > 0] whileTrue: 
			[| n |
			sha hashInteger: self randPool.
			sha hashInteger: count.
			n := len min: 16.
			random := (random bitShift: n * 8) bitOr: (sha finalHash bitAnd: (2 raisedToInteger: n * 8) - 1).
			count := count + 1.
			len := len - n].
	^ random! !

!PC1Cipher class methodsFor: 'as yet unclassified' stamp: 'gsa 12/30/2012 10:21'!
initialize
	"Private - Initialize the receiver.
	
	self initialize.
	"

	RandPool := 0.
	10 timesRepeat: [self churnRandPool].
"	SessionManager current 
		when: #sessionStarted
		send: #churnRandPool
		to: self  "! !

!PC1Cipher class methodsFor: 'as yet unclassified' stamp: 'gsa 12/30/2012 10:22'!
randPool
	"Private - Answer the 160 bit random pool seed generated from real world events.

	Applied Cryptography (Schneier) p427
	"


	^RandPool.
 ! !

!PC1Cipher class methodsFor: 'as yet unclassified' stamp: 'gsa 12/30/2012 12:58'!
withKeyBytes: aKeyByteArray 
	"Answer an instance of the receiver with aKeyByteArray"
	

	^ self new key: aKeyByteArray! !

!PC1Cipher class methodsFor: 'as yet unclassified' stamp: 'gsa 12/30/2012 13:26'!
withKeyInteger: aPositiveIntegerKey 
	"Answer an instance of the receiver with aPositiveIntegerKey"

	^ self withKeyBytes: (self byteArrayFromInteger: aPositiveIntegerKey) 
! !

!PC1Cipher class methodsFor: 'as yet unclassified' stamp: 'gsa 12/30/2012 13:26'!
withKeyString: aKeyString
	"Answer an instance of the receiver with aKeyString"

	^ self withKeyBytes: aKeyString asByteArray ! !

!PC1Cipher class methodsFor: 'as yet unclassified' stamp: 'gsa 12/30/2012 13:10'!
withStrength: bits 
	"Answer an instance of the receiver with <Integer> bits strength"

	^ self withKeyInteger: (self generateRandomWithBits: bits) ! !

!Random class methodsFor: '*PC1Cipher' stamp: 'gsa 1/8/2013 18:01'!
systemClockSeed
	| seed |
	seed := Delay millisecondClockValue bitAnd: 16r3FFFFFFF.
	seed = 0 ifTrue: [seed := 1].
	^seed! !
PC1Cipher initialize!
