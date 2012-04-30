'From Cuis 4.0 of 21 April 2012 [latest update: #1267] on 30 April 2012 at 8:36:25 pm'!
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
This class implements the Secure Hash Algorithm (SHA) described in the U.S. government's Secure Hash Standard (SHS). This standard is described in FIPS PUB 180-1, "SECURE HASH STANDARD", April 17, 1995.

!SecureHashAlgorithmTest commentStamp: '<historical>' prior: 0!
This is the unit test for the class SecureHashAlgorithm. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 

!ThirtyTwoBitRegister commentStamp: '<historical>' prior: 0!
I represent a 32-bit register. An instance of me can hold any non-negative integer in the range [0..(2^32 - 1)]. Operations are performed on my contents in place, like a hardware register, and results are always modulo 2^32.

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/7/1999 23:25'!
constantForStep: i

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 20:06'!
expandedBlock: aByteArray

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 20:02'!
finalHash

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/7/1999 22:15'!
hashFunction: i of: x with: y with: z

!SecureHashAlgorithm methodsFor: 'public' stamp: 'jm 12/14/1999 11:56'!
hashInteger: aPositiveInteger

!SecureHashAlgorithm methodsFor: 'public' stamp: 'md 11/14/2003 17:17'!
hashInteger: aPositiveInteger seed: seedInteger

!SecureHashAlgorithm methodsFor: 'public' stamp: 'dc 5/30/2008 10:17'!
hashMessage: aStringOrByteArray 

!SecureHashAlgorithm methodsFor: 'public' stamp: 'StephaneDucasse 2/28/2010 11:07'!
hashStream: aPositionableStream

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 19:38'!
initializeTotals

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 19:38'!
initializeTotalsArray

!SecureHashAlgorithm methodsFor: 'primitives' stamp: 'jm 12/21/1999 20:11'!
primExpandBlock: aByteArray into: wordBitmap

!SecureHashAlgorithm methodsFor: 'primitives' stamp: 'jm 12/21/1999 22:58'!
primHasSecureHashPrimitive

!SecureHashAlgorithm methodsFor: 'primitives' stamp: 'jm 12/21/1999 20:13'!
primHashBlock: blockBitmap using: workingTotalsBitmap

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 19:43'!
processBuffer: aByteArray

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/21/1999 23:32'!
processBufferUsingPrimitives: aByteArray

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/14/1999 11:40'!
processFinalBuffer: buffer bitLength: bitLength

!SecureHashAlgorithm methodsFor: 'private' stamp: 'jm 12/14/1999 11:10'!
storeLength: bitLength in: aByteArray

!SecureHashAlgorithm class methodsFor: 'initialization' stamp: 'jm 12/7/1999 23:25'!
initialize

!SecureHashAlgorithmTest methodsFor: 'testing - examples' stamp: 'gsa 4/30/2012 20:29'!
testEmptyInput
	"self run: #testEmptyInput"
	
	self assert: ((SecureHashAlgorithm new hashMessage: '') radix: 16)
			= 'DA39A3EE5E6B4B0D3255BFEF95601890AFD80709'! !

!SecureHashAlgorithmTest methodsFor: 'testing - examples' stamp: 'md 4/21/2003 12:23'!
testExample1

!SecureHashAlgorithmTest methodsFor: 'testing - examples' stamp: 'md 4/21/2003 12:23'!
testExample2

!SecureHashAlgorithmTest methodsFor: 'testing - examples' stamp: 'md 4/21/2003 12:25'!
testExample3

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:36'!
+= aThirtTwoBitRegister

!ThirtyTwoBitRegister methodsFor: 'converting' stamp: 'len 8/7/2002 17:37'!
asByteArray

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/14/1999 16:03'!
asInteger

!ThirtyTwoBitRegister methodsFor: 'converting' stamp: 'DSM 1/20/2000 17:17'!
asReverseInteger

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:41'!
bitAnd: aThirtTwoBitRegister

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:40'!
bitInvert

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:40'!
bitOr: aThirtTwoBitRegister

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'RJT 10/28/2005 15:42'!
bitShift: anInteger

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 15:38'!
bitXor: aThirtTwoBitRegister

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'adrian_lienhard 7/21/2009 19:49'!
byte1: hi1 byte2: hi2 byte3: low1 byte4: low2

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'len 8/15/2002 01:34'!
byteAt: anInteger

!ThirtyTwoBitRegister methodsFor: 'copying' stamp: 'jm 12/7/1999 15:26'!
copy

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/7/1999 15:26'!
hi

!ThirtyTwoBitRegister methodsFor: 'accumulator ops' stamp: 'jm 12/7/1999 23:09'!
leftRotateBy: bits

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/14/1999 16:07'!
load: anInteger

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/14/1999 16:07'!
loadFrom: aByteArray at: index

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'jm 12/7/1999 15:26'!
low

!ThirtyTwoBitRegister methodsFor: 'printing' stamp: 'laza 3/29/2004 12:22'!
printOn: aStream

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'adrian_lienhard 7/21/2009 19:49'!
reverseLoadFrom: aByteArray at: index

!ThirtyTwoBitRegister methodsFor: 'accessing' stamp: 'len 8/15/2002 01:29'!
storeInto: aByteArray at: index

!ThirtyTwoBitRegister class methodsFor: 'instance creation' stamp: 'jm 12/14/1999 16:05'!
new
SecureHashAlgorithm initialize!