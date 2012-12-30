'From Cuis 4.1 of 12 December 2012 [latest update: #1524] on 30 December 2012 at 6:40:12 pm'!
'Description Please enter a description for this package '!
!classDefinition: #SecureHashAlgorithm category: #'Cuis-System-Hashing'!
Object subclass: #SecureHashAlgorithm
	instanceVariableNames: 'totalA totalB totalC totalD totalE totals'
	classVariableNames: 'K1 K2 K3 K4'
	poolDictionaries: ''
	category: 'Cuis-System-Hashing'!
!classDefinition: 'SecureHashAlgorithm class' category: #'Cuis-System-Hashing'!
SecureHashAlgorithm class
	instanceVariableNames: ''!

!classDefinition: #SecureHashAlgorithmTest category: #'Cuis-System-Hashing'!
TestCase subclass: #SecureHashAlgorithmTest
	instanceVariableNames: 'hash'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-System-Hashing'!
!classDefinition: 'SecureHashAlgorithmTest class' category: #'Cuis-System-Hashing'!
SecureHashAlgorithmTest class
	instanceVariableNames: ''!

!classDefinition: #ThirtyTwoBitRegister category: #'Cuis-System-Hashing'!
Object subclass: #ThirtyTwoBitRegister
	instanceVariableNames: 'hi low'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-System-Hashing'!
!classDefinition: 'ThirtyTwoBitRegister class' category: #'Cuis-System-Hashing'!
ThirtyTwoBitRegister class
	instanceVariableNames: ''!


!SecureHashAlgorithm commentStamp: '<historical>' prior: 0!
This class implements the Secure Hash Algorithm (SHA) described in the U.S. government's Secure Hash Standard (SHS). This standard is described in FIPS PUB 180-1, "SECURE HASH STANDARD", April 17, 1995.The Secure Hash Algorithm is also described on p. 442 of 'Applied Cryptography: Protocols, Algorithms, and Source Code in C' by Bruce Scheier, Wiley, 1996.See the comment in class DigitalSignatureAlgorithm for details on its use.Implementation notes:The secure hash standard was created with 32-bit hardware in mind. All arithmetic in the hash computation must be done modulo 2^32. This implementation uses ThirtyTwoBitRegister objects to simulate hardware registers; this implementation is about six times faster than using LargePositiveIntegers (measured on a Macintosh G3 Powerbook). Implementing a primitive to process each 64-byte buffer would probably speed up the computation by a factor of 20 or more.!

!SecureHashAlgorithmTest commentStamp: '<historical>' prior: 0!
This is the unit test for the class SecureHashAlgorithm. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 	- http://www.c2.com/cgi/wiki?UnitTest	- there is a chapter in the PharoByExample book (http://pharobyexample.org)	- the sunit class category!

!ThirtyTwoBitRegister commentStamp: '<historical>' prior: 0!
I represent a 32-bit register. An instance of me can hold any non-negative integer in the range [0..(2^32 - 1)]. Operations are performed on my contents in place, like a hardware register, and results are always modulo 2^32.This class is primarily meant for use by the SecureHashAlgorithm class.!

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/7/1999 23:25'!
constantForStep: i	"Answer the constant for the i-th step of the block hash loop. We number our steps 1-80, versus the 0-79 of the standard."	i <= 20 ifTrue: [^ K1].	i <= 40 ifTrue: [^ K2].	i <= 60 ifTrue: [^ K3].	^ K4! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 20:06'!
expandedBlock: aByteArray	"Convert the given 64 byte buffer into 80 32-bit registers and answer the result." 	| out src v |	out := Array new: 80.	src := 1.	1 to: 16 do: [:i |		out at: i put: (ThirtyTwoBitRegister new loadFrom: aByteArray at: src).		src := src + 4].	17 to: 80 do: [:i |		v := (out at: i - 3) copy.		v	bitXor: (out at: i - 8);			bitXor: (out at: i - 14);			bitXor: (out at: i - 16);			leftRotateBy: 1.		out at: i put: v].	^ out! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 20:02'!
finalHash	"Concatenate the final totals to build the 160-bit integer result."	"Details: If the primitives are supported, the results are in the totals array. Otherwise, they are in the instance variables totalA through totalE."	| r |	totals ifNil: [  "compute final hash when not using primitives"		^ (totalA asInteger bitShift: 128) +		  (totalB asInteger bitShift:  96) +		  (totalC asInteger bitShift:  64) +		  (totalD asInteger bitShift:  32) +		  (totalE asInteger)].	"compute final hash when using primitives"	r := 0.	1 to: 5 do: [:i |		r := r bitOr: ((totals at: i) bitShift: (32 * (5 - i)))].	^ r! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/7/1999 22:15'!
hashFunction: i of: x with: y with: z	"Compute the hash function for the i-th step of the block hash loop. We number our steps 1-80, versus the 0-79 of the standard."	"Details: There are four functions, one for each 20 iterations. The second and fourth are the same."	i <= 20 ifTrue: [^ x copy bitAnd: y; bitOr: (x copy bitInvert; bitAnd: z)].	i <= 40 ifTrue: [^ x copy bitXor: y; bitXor: z].	i <= 60 ifTrue: [^ x copy bitAnd: y; bitOr: (x copy bitAnd: z); bitOr: (y copy bitAnd: z)].	^ x copy bitXor: y; bitXor: z! !

!SecureHashAlgorithm methodsFor: 'public' stamp: 'jm 12/14/1999 11:56'!
hashInteger: aPositiveInteger	"Hash the given positive integer. The integer to be hashed should have 512 or fewer bits. This entry point is used in key generation."	| buffer dstIndex |	self initializeTotals.	"pad integer with zeros"	aPositiveInteger highBit <= 512		ifFalse: [self error: 'integer cannot exceed 512 bits'].	buffer := ByteArray new: 64.	dstIndex := 0.	aPositiveInteger digitLength to: 1 by: -1 do: [:i |		buffer at: (dstIndex := dstIndex + 1) put: (aPositiveInteger digitAt: i)].	"process that one block"	self processBuffer: buffer.	^ self finalHash! !

!SecureHashAlgorithm methodsFor: 'public' stamp: 'md 11/14/2003 17:17'!
hashInteger: aPositiveInteger seed: seedInteger	"Hash the given positive integer. The integer to be hashed should have 512 or fewer bits. This entry point is used in the production of random numbers"	| buffer dstIndex |	"Initialize totalA through totalE to their seed values."	totalA := ThirtyTwoBitRegister new		load: ((seedInteger bitShift: -128) bitAnd: 16rFFFFFFFF).	totalB := ThirtyTwoBitRegister new		load: ((seedInteger bitShift: -96) bitAnd: 16rFFFFFFFF).	totalC := ThirtyTwoBitRegister new		load: ((seedInteger bitShift: -64) bitAnd: 16rFFFFFFFF).	totalD := ThirtyTwoBitRegister new		load: ((seedInteger bitShift: -32) bitAnd: 16rFFFFFFFF).	totalE := ThirtyTwoBitRegister new		load: (seedInteger bitAnd: 16rFFFFFFFF).	self initializeTotalsArray.	"pad integer with zeros"	buffer := ByteArray new: 64.	dstIndex := 0.	aPositiveInteger digitLength to: 1 by: -1 do: [:i |		buffer at: (dstIndex := dstIndex + 1) put: (aPositiveInteger digitAt: i)].	"process that one block"	self processBuffer: buffer.	^ self finalHash! !

!SecureHashAlgorithm methodsFor: 'public' stamp: 'dc 5/30/2008 10:17'!
hashMessage: aStringOrByteArray 	"Hash the given message using the Secure Hash Algorithm."	^ self hashStream: aStringOrByteArray asByteArray readStream! !

!SecureHashAlgorithm methodsFor: 'public' stamp: 'StephaneDucasse 2/28/2010 11:07'!
hashStream: aPositionableStream	"Hash the contents of the given stream from the current position to the end using the Secure Hash Algorithm. The SHA algorithm is defined in FIPS PUB 180-1. It is also described on p. 442 of 'Applied Cryptography: Protocols, Algorithms, and Source Code in C' by Bruce Scheier, Wiley, 1996."	"SecureHashAlgorithm new hashStream: (ReadStream on: 'foo')"	"(SecureHashAlgorithm new hashMessage: '') radix: 16	=> 'DA39A3EE5E6B4B0D3255BFEF95601890AFD80709'"		| startPosition buf bitLength |	self initializeTotals.  		aPositionableStream atEnd ifTrue: [self processFinalBuffer: #() bitLength: 0].	startPosition := aPositionableStream position.	[aPositionableStream atEnd] whileFalse: 		[ buf := aPositionableStream next: 64.		(aPositionableStream atEnd not and: [buf size = 64])			ifTrue: [self processBuffer: buf]			ifFalse: [ bitLength := (aPositionableStream position - startPosition) * 8.					self processFinalBuffer: buf bitLength: bitLength]].	^ self finalHash! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 19:38'!
initializeTotals	"Initialize totalA through totalE to their seed values."	"total registers for use when primitives are absent"	totalA := ThirtyTwoBitRegister new load: 16r67452301.	totalB := ThirtyTwoBitRegister new load: 16rEFCDAB89.	totalC := ThirtyTwoBitRegister new load: 16r98BADCFE.	totalD := ThirtyTwoBitRegister new load: 16r10325476.	totalE := ThirtyTwoBitRegister new load: 16rC3D2E1F0.	self initializeTotalsArray.! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 19:38'!
initializeTotalsArray	"Initialize the totals array from the registers for use with the primitives."	totals := Bitmap new: 5.	totals at: 1 put: totalA asInteger.	totals at: 2 put: totalB asInteger.	totals at: 3 put: totalC asInteger.	totals at: 4 put: totalD asInteger.	totals at: 5 put: totalE asInteger.! !

!SecureHashAlgorithm methodsFor: 'primitives' stamp: 'jm 12/21/1999 20:11'!
primExpandBlock: aByteArray into: wordBitmap	"Expand the given 64-byte buffer into the given Bitmap of length 80."	<primitive: 'primitiveExpandBlock' module: 'DSAPrims'>	^ self primitiveFailed! !

!SecureHashAlgorithm methodsFor: 'primitives' stamp: 'jm 12/21/1999 22:58'!
primHasSecureHashPrimitive	"Answer true if this platform has primitive support for the Secure Hash Algorithm."	<primitive: 'primitiveHasSecureHashPrimitive' module: 'DSAPrims'>	^ false! !

!SecureHashAlgorithm methodsFor: 'primitives' stamp: 'jm 12/21/1999 20:13'!
primHashBlock: blockBitmap using: workingTotalsBitmap	"Hash the given block (a Bitmap) of 80 32-bit words, using the given workingTotals."	<primitive: 'primitiveHashBlock' module: 'DSAPrims'>	^ self primitiveFailed! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 19:43'!
processBuffer: aByteArray	"Process given 64-byte buffer, accumulating the results in totalA through totalE."	| a b c d e w tmp |	self primHasSecureHashPrimitive		ifTrue: [^ self processBufferUsingPrimitives: aByteArray]		ifFalse: [totals := nil].	"initialize registers a through e from the current totals" 	a := totalA copy.	b := totalB copy.	c := totalC copy.	d := totalD copy.	e := totalE copy.	"expand and process the buffer"	w := self expandedBlock: aByteArray.	1 to: 80 do: [:i |		tmp := (a copy leftRotateBy: 5)			+= (self hashFunction: i of: b with: c with: d);			+= e;			+= (w at: i);			+= (self constantForStep: i).		e := d.		d := c.		c := b copy leftRotateBy: 30.		b := a.		a := tmp].	"add a through e into total accumulators"	totalA += a.	totalB += b.	totalC += c.	totalD += d.	totalE += e.! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 23:32'!
processBufferUsingPrimitives: aByteArray	"Process given 64-byte buffer using the primitives, accumulating the results in totals."	| w |	"expand and process the buffer"	w := Bitmap new: 80.	self primExpandBlock: aByteArray into: w.	self primHashBlock: w using: totals.! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/14/1999 11:40'!
processFinalBuffer: buffer bitLength: bitLength	"Process given buffer, whose length may be <= 64 bytes, accumulating the results in totalA through totalE. Also process the final padding bits and length."	| out |	out := ByteArray new: 64.	out replaceFrom: 1 to: buffer size with: buffer startingAt: 1.	buffer size < 56 ifTrue: [  "padding and length fit in last data block"		out at: buffer size + 1 put: 128.  "trailing one bit"		self storeLength: bitLength in: out.  "end with length"		self processBuffer: out.		^ self].	"process the final data block"	buffer size < 64 ifTrue: [		out at: buffer size + 1 put: 128].  "trailing one bit"	self processBuffer: out.	"process one additional block of padding ending with the length"	out := ByteArray new: 64.  "filled with zeros"	buffer size = 64 ifTrue: [		"add trailing one bit that didn't fit in final data block"		out at: 1 put: 128].	self storeLength: bitLength in: out.	self processBuffer: out.! !

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/14/1999 11:10'!
storeLength: bitLength in: aByteArray	"Fill in the final 8 bytes of the given ByteArray with a 64-bit big-endian representation of the original message length in bits."	| n i |	n := bitLength.	i := aByteArray size.	[n > 0] whileTrue: [		aByteArray at: i put: (n bitAnd: 16rFF).		n := n bitShift: -8.		i := i - 1].! !

!SecureHashAlgorithm class methodsFor: 'initialization' stamp: 'jm 12/7/1999 23:25'!
initialize	"SecureHashAlgorithm initialize"	"For the curious, here's where these constants come from:	  #(2 3 5 10) collect: [:x | ((x sqrt / 4.0) * (2.0 raisedTo: 32)) truncated hex]"	K1 := ThirtyTwoBitRegister new load: 16r5A827999.	K2 := ThirtyTwoBitRegister new load: 16r6ED9EBA1.	K3 := ThirtyTwoBitRegister new load: 16r8F1BBCDC.	K4 := ThirtyTwoBitRegister new load: 16rCA62C1D6.! !

!SecureHashAlgorithmTest methodsFor: 'testing - examples' stamp: 'gsa 4/30/2012 20:29'!
testEmptyInput
	"self run: #testEmptyInput"
	
	self assert: ((SecureHashAlgorithm new hashMessage: '') radix: 16)
			= 'DA39A3EE5E6B4B0D3255BFEF95601890AFD80709'! !

!SecureHashAlgorithmTest methodsFor: 'testing - examples' stamp: 'md 4/21/2003 12:23'!
testExample1	"This is the first example from the specification document (FIPS PUB 180-1)"	hash := SecureHashAlgorithm new hashMessage: 'abc'.	self assert: (hash = 16rA9993E364706816ABA3E25717850C26C9CD0D89D).		! !

!SecureHashAlgorithmTest methodsFor: 'testing - examples' stamp: 'md 4/21/2003 12:23'!
testExample2	"This is the second example from the specification document (FIPS PUB 180-1)"	hash := SecureHashAlgorithm new hashMessage:		'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'.	self assert: (hash = 16r84983E441C3BD26EBAAE4AA1F95129E5E54670F1).! !

!SecureHashAlgorithmTest methodsFor: 'testing - examples' stamp: 'md 4/21/2003 12:25'!
testExample3	"This is the third example from the specification document (FIPS PUB 180-1). 	This example may take several minutes."	hash := SecureHashAlgorithm new hashMessage: (String new: 1000000 withAll: $a).	self assert: (hash = 16r34AA973CD4C4DAA4F61EEB2BDBAD27316534016F).! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:36'!
+= aThirtTwoBitRegister	"Replace my contents with the sum of the given register and my current contents."	| lowSum |	lowSum := low + aThirtTwoBitRegister low.	hi := (hi + aThirtTwoBitRegister hi + (lowSum bitShift: -16)) bitAnd: 16rFFFF.	low := lowSum bitAnd: 16rFFFF.! !

!ThirtyTwoBitRegister methodsFor: 'converting' stamp: 'len 8/7/2002 17:37'!
asByteArray	^ ByteArray with: (low bitAnd: 16rFF) with: (low bitShift: -8) with: (hi bitAnd: 16rFF) with: (hi bitShift: -8)! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/14/1999 16:03'!
asInteger	"Answer the integer value of my current contents."	^ (hi bitShift: 16) + low! !

!ThirtyTwoBitRegister methodsFor: 'converting' stamp: 'DSM 1/20/2000 17:17'!
asReverseInteger	"Answer the byte-swapped integer value of my current contents."	^ ((low bitAnd: 16rFF) bitShift: 24) +       ((low bitAnd: 16rFF00) bitShift: 8) +	  ((hi bitAnd: 16rFF) bitShift: 8) +       (hi bitShift: -8)! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:41'!
bitAnd: aThirtTwoBitRegister	"Replace my contents with the bitwise AND of the given register and my current contents."	hi := hi bitAnd: aThirtTwoBitRegister hi.	low := low bitAnd: aThirtTwoBitRegister low.! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:40'!
bitInvert	"Replace my contents with the bitwise inverse my current contents."	hi := hi bitXor: 16rFFFF.	low := low bitXor: 16rFFFF.! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:40'!
bitOr: aThirtTwoBitRegister	"Replace my contents with the bitwise OR of the given register and my current contents."	hi := hi bitOr: aThirtTwoBitRegister hi.	low := low bitOr: aThirtTwoBitRegister low.! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'RJT 10/28/2005 15:42'!
bitShift: anInteger	"Replace my contents with the bitShift of anInteger."	self load: (self asInteger bitShift: anInteger). ! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:38'!
bitXor: aThirtTwoBitRegister	"Replace my contents with the bitwise exclusive OR of the given register and my current contents."	hi := hi bitXor: aThirtTwoBitRegister hi.	low := low bitXor: aThirtTwoBitRegister low.! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'adrian_lienhard 7/21/2009 19:49'!
byte1: hi1 byte2: hi2 byte3: low1 byte4: low2	hi := (hi1 bitShift: 8) + hi2.	low := (low1 bitShift: 8) + low2.! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'len 8/15/2002 01:34'!
byteAt: anInteger	anInteger = 1 ifTrue: [^ hi bitShift: -8].	anInteger = 2 ifTrue: [^ hi bitAnd: 16rFF].	anInteger = 3 ifTrue: [^ low bitShift: -8].	anInteger = 4 ifTrue: [^ low bitAnd: 16rFF]! !

!ThirtyTwoBitRegister methodsFor: 'copying' stamp: 'jm 12/7/1999 15:26'!
copy	"Use the clone primitive for speed."	<primitive: 148>	^ super copy! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/7/1999 15:26'!
hi	^ hi! !

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 23:09'!
leftRotateBy: bits	"Rotate my contents left by the given number of bits, retaining exactly 32 bits."	"Details: Perform this operation with as little LargeInteger arithmetic as possible."	| bitCount s1 s2 newHi |	"ensure bitCount is in range [0..32]"	bitCount := bits \\ 32.	bitCount < 0 ifTrue: [bitCount := bitCount + 32].	bitCount > 16		ifTrue: [			s1 := bitCount - 16.			s2 := s1 - 16.			newHi := ((low bitShift: s1) bitAnd: 16rFFFF) bitOr: (hi bitShift: s2).			low := ((hi bitShift: s1) bitAnd: 16rFFFF) bitOr: (low bitShift: s2).			hi := newHi]		ifFalse: [			s1 := bitCount.			s2 := s1 - 16.			newHi := ((hi bitShift: s1) bitAnd: 16rFFFF) bitOr: (low bitShift: s2).			low := ((low bitShift: s1) bitAnd: 16rFFFF) bitOr: (hi bitShift: s2).			hi := newHi]! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/14/1999 16:07'!
load: anInteger	"Set my contents to the value of given integer."	low := anInteger bitAnd: 16rFFFF.	hi := (anInteger bitShift: -16) bitAnd: 16rFFFF.	self asInteger = anInteger		ifFalse: [self error: 'out of range: ', anInteger printString].! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/14/1999 16:07'!
loadFrom: aByteArray at: index	"Load my 32-bit value from the four bytes of the given ByteArray starting at the given index. Consider the first byte to contain the most significant bits of the word (i.e., use big-endian byte ordering)."	hi := ((aByteArray at: index) bitShift: 8) + ( aByteArray at: index + 1).	low := ((aByteArray at: index + 2) bitShift: 8) + ( aByteArray at: index + 3).! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/7/1999 15:26'!
low	^ low! !

!ThirtyTwoBitRegister methodsFor: 'printing' stamp: 'laza 3/29/2004 12:22'!
printOn: aStream	"Print my contents in hex with a leading 'R' to show that it is a register object being printed."	aStream nextPutAll: 'R:'.	self asInteger storeOn: aStream base: 16.! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'adrian_lienhard 7/21/2009 19:49'!
reverseLoadFrom: aByteArray at: index	"Load my 32-bit value from the four bytes of the given ByteArraystarting at the given index. Consider the first byte to contain the mostsignificant bits of the word (i.e., use big-endian byte ordering)."	hi := ((aByteArray at: index + 3) bitShift: 8) + ( aByteArray at: index + 2).	low := ((aByteArray at: index + 1) bitShift: 8) + ( aByteArray at: index).! !

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'len 8/15/2002 01:29'!
storeInto: aByteArray at: index	"Store my 32-bit value into the four bytes of the given ByteArray starting at the given index. Consider the first byte to contain the most significant bits of the word (i.e., use big-endian byte ordering)."	aByteArray at: index put: (hi bitShift: -8).	aByteArray at: index + 1 put: (hi bitAnd: 16rFF).	aByteArray at: index + 2 put: (low bitShift: -8).	aByteArray at: index + 3 put: (low bitAnd: 16rFF)! !

!ThirtyTwoBitRegister class methodsFor: 'instance creation' stamp: 'jm 12/14/1999 16:05'!
new	"Answer a new instance whose initial contents is zero."	^ super new load: 0! !
SecureHashAlgorithm initialize!
