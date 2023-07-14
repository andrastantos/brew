$rD <- VALUE
--------------------------

*Instruction code*: 0x.00f 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$rD` with the value of FIELD_E. The type of :code:`$rD` is not changed.


$pc <- VALUE
--------------------------

*Instruction code*: 0x20ef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$pc` with the value of FIELD_E. The LSB of the value has an implementation-defined meaning. This is an absolute jump operation. The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


$tpc <- VALUE
--------------------------

*Instruction code*: 0x30ef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$tpc` with the value of FIELD_E. The LSB of the value has an implementation-defined meaning. This is an absolute jump operation in TASK mode. In TASK mode the implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


type $r0...$r7 <- VALUE
--------------------------

*Instruction code*: 0x80ef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the type of :code:`$r0` through :code:`$r7` with the types encoded in each 4-bit niggle of FIELD_E. The lowest 4 bits determine the type of :code:`$r0`. The highest 4 bits determine the type of :code:`$r7`. If a nibble has a value of 0xf, the type of the corresponding register is not changed.


type $r8...$r14 <- VALUE
--------------------------

*Instruction code*: 0x90ef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the type of :code:`$r8` through :code:`$r14` with the types encoded in each 4-bit niggle of FIELD_E. The lowest 4 bits determine the type of :code:`$r8`. The next-to-highest 4 bits determine the type of :code:`$r14`. The highest nibble of FIELD_E is ignored. If a nibble has a value of 0xf, the type of the corresponding register is not changed.




$rD <- short VALUE
--------------------------

*Instruction code*: 0x.0f0 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$rD` with the sign-extended value of FIELD_E. The type of :code:`$rD` is not changed.


$pc <- short VALUE
--------------------------

*Instruction code*: 0x20fe 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$pc` with the sign-extended value of FIELD_E. The LSB of the value has an implementation-defined meaning. This is an absolute jump operation. The implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.


$tpc <- short VALUE
--------------------------

*Instruction code*: 0x30fe 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$tpc` with the sign-extended value of FIELD_E. The LSB of the value has an implementation-defined meaning. This is an absolute jump operation in TASK mode. In TASK mode the implementation can raise exceptions if the jump results in a violation of the memory access rights set up for the execution context.
