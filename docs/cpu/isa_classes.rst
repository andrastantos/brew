====================   =====================================================   =========   ========   =========   ========   =======
Instruction code       Assembly                                                Mandatory   Float      Int V       FP V       DSP
====================   =====================================================   =========   ========   =========   ========   =======
0x0000                 SWI 0                                                   Yes
0x1000                 SWI 1                                                   Yes
0x2000                 SWI 2                                                   Yes
0x3000                 SWI 3                                                   Yes
0x4000                 SWI 4                                                   Yes
0x5000                 SWI 5                                                   Yes
0x6000                 SWI 6                                                   Yes
0x7000                 SWI 7                                                   Yes
0x8000                 STM                                                     Yes
0x9000                 WOI                                                     Yes
0xa000                 PFLUSH                                                  Yes
0x0001                 FENCE_RW_RW                                             Yes
0x1001                 FENCE__W_RW                                             Yes
0x2001                 FENCE_R__RW                                             Yes
0x3001                 FENCE____RW                                             Yes
0x4001                 FENCE_RW__W                                             Yes
0x5001                 FENCE__W__W                                             Yes
0x6001                 FENCE_R___W                                             Yes
0x7001                 FENCE_____W                                             Yes
0x8001                 FENCE_RW_R\_                                            Yes
0x9001                 FENCE__W_R\_                                            Yes
0xa001                 FENCE_R__R\_                                            Yes
0xb001                 FENCE____R\_                                            Yes
0xc001                 FENCE_RW___                                             Yes
0xd001                 FENCE__W___                                             Yes
0xe001                 FENCE_R____                                             Yes
0x.002                 $pc <- $rD                                              Yes
0x.003                 $tpc <- $rD                                             Yes
0x.004                 $rD <- $pc                                              Yes
0x.005                 $rD <- $tpc                                             Yes
0x.0f8 0x****          $rD <- CSR[ADDR]                                        Yes
0x.0f9 0x****          CSR[ADDR] <- $rD                                        Yes
0x.01.                 $rD <- tiny CONST                                       Yes
0x.02.                 $rD <- $pc + CONST                                      Yes
0x.03.                 $rD <- -$rA                                             Yes
0x.04.                 $rD <- ~$rA                                             Yes
0x.05.                 $rD <- bse $rA                                          Yes
0x.06.                 $rD <- wse $rA                                          Yes
0x.07.                 $rD <- float $rA                                                    Yes                    Yes
0x.08.                 $rD <- int $rA                                                      Yes                    Yes
0x.09.                 $rD <- 1 / $rA                                                      Yes                    Yes
0x.0a.                 $rD <- rsqrt $rA                                                    Yes                    Yes
0x.0b.                 $rD <- size $rA
0x.0c.                 type $rD <- $rA                                         Yes
0x.0d.                 $rD <- type $rA                                         Yes
0x.0e.                 type $rD <- FIELD_A                                     Yes
0x.1..                 $rD <- $rA ^ $rB                                        Yes
0x.2..                 $rD <- $rA | $rB                                        Yes
0x.3..                 $rD <- $rA & $rB                                        Yes
0x.4..                 $rD <- $rA + $rB                                        Yes
0x.5..                 $rD <- $rA - $rB                                        Yes
0x.6..                 $rD <- $rA << $rB                                       Yes
0x.7..                 $rD <- $rA >> $rB                                       Yes
0x.8..                 $rD <- $rA >>> $rB                                      Yes
0x.9..                 $rD <- $rA * $rB                                        Yes
0x.a..                 $rD <- $rA & ~$rB                                       Yes
0x.b..                 $rD <- tiny $rB + CONST                                 Yes
0x.00f 0x**** 0x****   $rD <- VALUE                                            Yes
0x20ef 0x**** 0x****   $pc <- VALUE                                            Yes
0x30ef 0x**** 0x****   $tpc <- VALUE                                           Yes
0x80ef 0x**** 0x****   type $r0...$r7 <- VALUE                                 Yes         Yes        Yes         Yes
0x90ef 0x**** 0x****   type $r8...$r14 <- VALUE                                Yes         Yes        Yes         Yes
0x.1.f 0x**** 0x****   $rD <- VALUE ^ $rB                                      Yes
0x.2.f 0x**** 0x****   $rD <- VALUE | $rB                                      Yes
0x.3.f 0x**** 0x****   $rD <- VALUE & $rB                                      Yes
0x.4.f 0x**** 0x****   $rD <- VALUE + $rB                                      Yes
0x.5.f 0x**** 0x****   $rD <- VALUE - $rB                                      Yes
0x.6.f 0x**** 0x****   $rD <- VALUE << $rB                                     Yes
0x.7.f 0x**** 0x****   $rD <- VALUE >> $rB                                     Yes
0x.8.f 0x**** 0x****   $rD <- VALUE >>> $rB                                    Yes
0x.9.f 0x**** 0x****   $rD <- VALUE * $rB                                      Yes
0x.0f0 0x****          $rD <- short VALUE                                      Yes
0x20fe 0x****          $pc <- short VALUE                                      Yes
0x30fe 0x****          $tpc <- short VALUE                                     Yes
0x.1f. 0x****          $rD <- short VALUE ^ $rA                                Yes
0x.2f. 0x****          $rD <- short VALUE | $rA                                Yes
0x.3f. 0x****          $rD <- short VALUE & $rA                                Yes
0x.4f. 0x****          $rD <- short VALUE + $rA                                Yes
0x.5f. 0x****          $rD <- short VALUE - $rA                                Yes
0x.6f. 0x****          $rD <- short $rA << VALUE                               Yes
0x.7f. 0x****          $rD <- short $rA >> VALUE                               Yes
0x.8f. 0x****          $rD <- short $rA >>> VALUE                              Yes
0x.9f. 0x****          $rD <- short VALUE * $rA                                Yes
0xf00. 0x****          if any $rA == 0 $pc <- $pc + VALUE                      Yes
0xf01. 0x****          if any $rA != 0 $pc <- $pc + VALUE                      Yes
0xf02. 0x****          if any $rA < 0 $pc <- $pc + VALUE                       Yes
0xf03. 0x****          if any $rA >= 0 $pc <- $pc + VALUE                      Yes
0xf04. 0x****          if any $rA > 0 $pc <- $pc + VALUE                       Yes
0xf05. 0x****          if any $rA <= 0 $pc <- $pc + VALUE                      Yes
0xf08. 0x****          if all $rA == 0 $pc <- $pc + VALUE                      Yes
0xf09. 0x****          if all $rA != 0 $pc <- $pc + VALUE                      Yes
0xf0a. 0x****          if all $rA < 0 $pc <- $pc + VALUE                       Yes
0xf0b. 0x****          if all $rA >= 0 $pc <- $pc + VALUE                      Yes
0xf0c. 0x****          if all $rA > 0 $pc <- $pc + VALUE                       Yes
0xf0d. 0x****          if all $rA <= 0 $pc <- $pc + VALUE                      Yes
0xf1.. 0x****          if any $rB == $rA $pc <- $pc + VALUE                    Yes
0xf2.. 0x****          if any $rB != $rA $pc <- $pc + VALUE                    Yes
0xf3.. 0x****          if any signed $rB < $rA $pc <- $pc + VALUE              Yes
0xf4.. 0x****          if any signed $rB >= $rA $pc <- $pc + VALUE             Yes
0xf5.. 0x****          if any $rB < $rA $pc <- $pc + VALUE                     Yes
0xf6.. 0x****          if any $rB >= $rA $pc <- $pc + VALUE                    Yes
0xf9.. 0x****          if all $rB == $rA $pc <- $pc + VALUE                    Yes
0xfa.. 0x****          if all $rB != $rA $pc <- $pc + VALUE                    Yes
0xfb.. 0x****          if all signed $rB < $rA $pc <- $pc + VALUE              Yes
0xfc.. 0x****          if all signed $rB >= $rA $pc <- $pc + VALUE             Yes
0xfd.. 0x****          if all $rB < $rA $pc <- $pc + VALUE                     Yes
0xfe.. 0x****          if all $rB >= $rA $pc <- $pc + VALUE                    Yes
0xf0f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xf1f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xf2f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xf3f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xf4f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xf5f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xf6f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xf7f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xf8f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xf9f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xfaf. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xfbf. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xfcf. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xfdf. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xfef. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE                       Yes
0xf0.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xf1.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xf2.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xf3.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xf4.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xf5.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xf6.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xf7.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xf8.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xf9.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xfa.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xfb.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xfc.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xfd.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0xfe.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE                       Yes
0x.c**                 MEM[$rS + tiny OFFSET] <- $rD                           Yes
0x.d**                 $rD <- MEM[$rS + tiny OFFSET]                           Yes
0x.e4.                 $rD <- MEM8[$rA]                                        Yes
0x.e5.                 $rD <- MEM16[$rA]                                       Yes
0x.e6.                 $rD <- MEM[$rA]                                         Yes
0x.e7.                 $rD <- MEMLL[$rA]                                       Yes
0x.e8.                 MEM8[$rA] <- $rD                                        Yes
0x.e9.                 MEM16[$rA] <- $rD                                       Yes
0x.ea.                 MEM[$rA] <- $rD                                         Yes
0x.eb.                 MEMSC[$rA] <- $rD                                       Yes
0x.ec.                 $rD <- SMEM8[$rA]                                       Yes
0x.ed.                 $rD <- SMEM16[$rA]                                      Yes
0x1ee.                 INV[$rA]                                                Yes
0x2ee.                 $pc <- MEM[$rA]                                         Yes
0x3ee.                 $tpc <- MEM[$rA]                                        Yes
0x.f4. 0x****          $rD <- MEM8[$rA + VALUE]                                Yes
0x.f5. 0x****          $rD <- MEM16[$rA + VALUE]                               Yes
0x.f6. 0x****          $rD <- MEM[$rA + VALUE]                                 Yes
0x.f7. 0x****          $rD <- MEMLL[$rA + VALUE]                               Yes
0x.f8. 0x****          MEM8[$rA + VALUE] <- $rD                                Yes
0x.f9. 0x****          MEM16[$rA + VALUE] <- $rD                               Yes
0x.fa. 0x****          MEM[$rA + VALUE] <- $rD                                 Yes
0x.fb. 0x****          MEMSC[$rA + VALUE] <- $rD                               Yes
0x.fc. 0x****          $rD <- SMEM8[$rA + VALUE]                               Yes
0x.fd. 0x****          $rD <- SMEM16[$rA + VALUE]                              Yes
0x1fe. 0x****          INV[$rA + VALUE]                                        Yes
0x2fe. 0x****          $pc <- MEM[$rA + VALUE]                                 Yes
0x3fe. 0x****          $tpc <- MEM[$rA + VALUE]                                Yes
0x.f0. 0x****          $r0...$r14 <- MEM[$rD] @ $rA                            Optional    Optional   Yes         Yes        Yes
0x.f1. 0x****          MEM[$rD] <- $r0...$r14 @ $rA                            Optional    Optional   Yes         Yes        Yes
0x.f2. 0x****          $r0...$r14 <- POP[$rD] @ $rA                            Optional    Optional   Yes         Yes        Yes
0x.f3. 0x****          PUSH[$rD] <- $r0...$r14 @ $rA                           Optional    Optional   Yes         Yes        Yes
0x.f0f 0x****          $r0...$r14 <- MEM[$rD]                                  Optional    Optional   Yes         Yes        Yes
0x.f1f 0x****          MEM[$rD] <- $r0...$r14                                  Optional    Optional   Yes         Yes        Yes
0x.f2f 0x****          $r0...$r14 <- POP[$rD]                                  Optional    Optional   Yes         Yes        Yes
0x.f3f 0x****          PUSH[$rD] <- $r0...$r14                                 Optional    Optional   Yes         Yes        Yes
0x.f4f 0x**** 0x****   $rD <- MEM8[VALUE]                                      Yes
0x.f5f 0x**** 0x****   $rD <- MEM16[VALUE]                                     Yes
0x.f6f 0x**** 0x****   $rD <- MEM[VALUE]                                       Yes
0x.f7f 0x**** 0x****   $rD <- MEMLL[VALUE]                                     Yes
0x.f8f 0x**** 0x****   MEM8[VALUE] <- $rD                                      Yes
0x.f9f 0x**** 0x****   MEM16[VALUE] <- $rD                                     Yes
0x.faf 0x**** 0x****   MEM[VALUE] <- $rD                                       Yes
0x.fbf 0x**** 0x****   MEMSC[VALUE] <- $rD                                     Yes
0x.fcf 0x**** 0x****   $rD <- SMEM8[VALUE]                                     Yes
0x.fdf 0x**** 0x****   $rD <- SMEM16[VALUE]                                    Yes
0x1fef 0x**** 0x****   INV[VALUE]                                              Yes
0x2fef 0x**** 0x****   $pc <- MEM[VALUE]                                       Yes
0x3fef 0x**** 0x****   $tpc <- MEM[VALUE]                                      Yes
0x.eff 0x**** 0x****   MEM[VALUE] <- full $rD                                  Yes
0x.fff 0x**** 0x****   full $rD <- MEM[VALUE]                                  Yes
0x.ef.                 MEM[$rA] <- full $rD                                    Yes
0x.ff.                 full $rD <- MEM[$rA]                                    Yes
0x001f 0x**** 0x****   if any type $r0...$r3 != types $pc <- $pc + br_offs     Yes*        Yes        Yes         Yes        Yes
0x101f 0x**** 0x****   if any type $r4...$r7 != types $pc <- $pc + br_offs     Yes*        Yes        Yes         Yes        Yes
0x201f 0x**** 0x****   if any type $r8...$r11 != types $pc <- $pc + br_offs    Yes*        Yes        Yes         Yes        Yes
0x301f 0x**** 0x****   if any type $r12...$r14 != types $pc <- $pc + br_offs   Yes*        Yes        Yes         Yes        Yes
0x401f 0x**** 0x****   if any type $r0...$r3 == types $pc <- $pc + br_offs     Yes*        Yes        Yes         Yes        Yes
0x501f 0x**** 0x****   if any type $r4...$r7 == types $pc <- $pc + br_offs     Yes*        Yes        Yes         Yes        Yes
0x601f 0x**** 0x****   if any type $r8...$r11 == types $pc <- $pc + br_offs    Yes*        Yes        Yes         Yes        Yes
0x701f 0x**** 0x****   if any type $r12...$r14 == types $pc <- $pc + br_offs   Yes*        Yes        Yes         Yes        Yes
0x002f 0x**** 0x****   if all type $r0...$r3 != types $pc <- $pc + br_offs     Yes*        Yes        Yes         Yes        Yes
0x102f 0x**** 0x****   if all type $r4...$r7 != types $pc <- $pc + br_offs     Yes*        Yes        Yes         Yes        Yes
0x202f 0x**** 0x****   if all type $r8...$r11 != types $pc <- $pc + br_offs    Yes*        Yes        Yes         Yes        Yes
0x302f 0x**** 0x****   if all type $r12...$r14 != types $pc <- $pc + br_offs   Yes*        Yes        Yes         Yes        Yes
0x402f 0x**** 0x****   if all type $r0...$r3 == types $pc <- $pc + br_offs     Yes*        Yes        Yes         Yes        Yes
0x502f 0x**** 0x****   if all type $r4...$r7 == types $pc <- $pc + br_offs     Yes*        Yes        Yes         Yes        Yes
0x602f 0x**** 0x****   if all type $r8...$r11 == types $pc <- $pc + br_offs    Yes*        Yes        Yes         Yes        Yes
0x702f 0x**** 0x****   if all type $r12...$r14 == types $pc <- $pc + br_offs   Yes*        Yes        Yes         Yes        Yes
0x.03f 0x**** 0x****   if type $rD not in FIELD_F $pc <- $pc + FIELD_E         Yes*        Yes        Yes         Yes        Yes
0xf0ff 0x.00.          $rD <- $rA == 0                                                                Yes         Yes        Yes
0xf0ff 0x.01.          $rD <- $rA != 0                                                                Yes         Yes        Yes
0xf0ff 0x.02.          $rD <- $rA < 0                                                                 Yes         Yes        Yes
0xf0ff 0x.03.          $rD <- $rA >= 0                                                                Yes         Yes        Yes
0xf0ff 0x.04.          $rD <- $rA > 0                                                                 Yes         Yes        Yes
0xf0ff 0x.05.          $rD <- $rA <= 0                                                                Yes         Yes        Yes
0xf0ff 0x.1..          $rD <- $rB == $rA                                                              Yes         Yes        Yes
0xf0ff 0x.2..          $rD <- $rB != $rA                                                              Yes         Yes        Yes
0xf0ff 0x.3..          $rD <- signed $rB < $rA                                                        Yes         Yes        Yes
0xf0ff 0x.4..          $rD <- signed $rB >= $rA                                                       Yes         Yes        Yes
0xf0ff 0x.5..          $rD <- $rB < $rA                                                               Yes         Yes        Yes
0xf0ff 0x.6..          $rD <- $rB >= $rA                                                              Yes         Yes        Yes
0xf1ff 0x.01.          $rD <- sum $rA                                                                 Yes         Yes        Yes
0xf1ff 0x.02.          $rD <- set_vend $rA                                                            Yes         Yes        Yes
0xf1ff 0x.1..          $rD <- interpolate $rA, $rB                                                                           Yes
0xf1ff 0x.2..          $rD(i) <- $rA($rB(i))                                                          Yes         Yes        Yes
0xf1ff 0x.3..          $rD <- (cast TYPE_B)$rA                                             Yes        Yes         Yes        Yes
0xf1ff 0x.4..          $rD <- compress $rA & $rB                                                      Yes         Yes        Yes
0xf4ff 0x.*..          $rD <- full $rA * $rB >>> FIELD_C + 0                                                                 Yes
0xf5ff 0x.*..          $rD <- full $rA * $rB >>> FIELD_C + 8                                                                 Yes
0xf6ff 0x.*..          $rD <- full $rA * $rB >>> FIELD_C + 16                                                                Yes
0xf7ff 0x.*..          $rD <- full $rA * $rB >>> FIELD_C + 32                                                                Yes
0xf8ff 0x.*..          $rD <- full $rA * $rB >> FIELD_C + 0                                                                  Yes
0xf9ff 0x.*..          $rD <- full $rA * $rB >> FIELD_C + 8                                                                  Yes
0xfaff 0x.*..          $rD <- full $rA * $rB >> FIELD_C + 16                                                                 Yes
0xfbff 0x.*..          $rD <- full $rA * $rB >> FIELD_C + 32                                                                 Yes
0xff** ...             Type override (<type>)                                  Yes*        Yes        Yes         Yes        Yes
====================   =====================================================   =========   ========   =========   ========   =======


================   =========   ========   =========   ========   =======
Type               Mandatory   Float      Int V       FP V       DSP
================   =========   ========   =========   ========   =======
INT32              Yes
FP32                           Yes                    Yes
FP64                                                  Optional
VINT32                                    Yes         Yes
VINT16                                    Optional    Optional
VINT8                                     Optional    Optional
VFP64                                                 Optional
VFP32                                                 Yes
VFP16                                                 Optional
VUINT32S                                                         Yes
VSINT32S                                                         Yes
VUINT16S                                                         Yes
VSINT16S                                                         Yes
VUINT8S                                                          Yes
VSINT8S                                                          Yes
================   =========   ========   =========   ========   =======
