====================   ===========================================   ================
Instruction code       Assembly                                      Implemented
====================   ===========================================   ================
0x0000                 SWI 0                                      
0x1000                 SWI 1                                      
0x2000                 SWI 2                                      
0x3000                 SWI 3                                      
0x4000                 SWI 4                                      
0x5000                 SWI 5                                      
0x6000                 SWI 6                                      
0x7000                 SWI 7                                      
0x8000                 STM                                        
0x9000                 WOI                                        
0xa000                 PFLUSH                                     
0x0001                 FENCE_RW_RW                                
0x1001                 FENCE__W_RW                                
0x2001                 FENCE_R__RW                                
0x3001                 FENCE____RW                                
0x4001                 FENCE_RW__W                                
0x5001                 FENCE__W__W                                
0x6001                 FENCE_R___W                                
0x7001                 FENCE_____W                                
0x8001                 FENCE_RW_R\_                               
0x9001                 FENCE__W_R\_                               
0xa001                 FENCE_R__R\_                               
0xb001                 FENCE____R\_                               
0xc001                 FENCE_RW___                                
0xd001                 FENCE__W___                                
0xe001                 FENCE_R____                                
0x.002                 $pc <- $rD                                 
0x.003                 $tpc <- $rD                                
0x.004                 $rD <- $pc                                 
0x.005                 $rD <- $tpc                                
0x.0f8 0x****          $rD <- CSR[ADDR]                           
0x.0f9 0x****          CSR[ADDR] <- $rD                           
0x.01.                 $rD <- tiny CONST                          
0x.02.                 $rD <- $pc + CONST                         
0x.03.                 $rD <- -$rA                                
0x.04.                 $rD <- ~$rA                                
0x.05.                 $rD <- bse $rA                             
0x.06.                 $rD <- wse $rA                             
0x.07.                 $rD <- popcnt $rA                          
0x.08.                 $rD <- 1 / $rA                             
0x.09.                 $rD <- rsqrt $rA                           
0x.0c.                 type $rD <- $rA                            
0x.0d.                 $rD <- type $rA                            
0x.0e.                 type $rD <- FIELD_A                        
0x.1..                 $rD <- $rA ^ $rB                           
0x.2..                 $rD <- $rA | $rB                           
0x.3..                 $rD <- $rA & $rB                           
0x.4..                 $rD <- $rA + $rB                           
0x.5..                 $rD <- $rA - $rB                           
0x.6..                 $rD <- $rA << $rB                          
0x.7..                 $rD <- $rA >> $rB                          
0x.8..                 $rD <- $rA >>> $rB                         
0x.9..                 $rD <- $rA * $rB                           
0x.a..                 $rD <- TYPE_NAME $rB                       
0x.b..                 $rD <- tiny $rB + CONST                    
0x.00f 0x**** 0x****   $rD <- VALUE                               
0x20ef 0x**** 0x****   $pc <- VALUE                               
0x30ef 0x**** 0x****   $tpc <- VALUE                              
0x80ef 0x**** 0x****   type $r0...$r7 <- VALUE                    
0x90ef 0x**** 0x****   type $r8...$r14 <- VALUE                   
0x.1.f 0x**** 0x****   $rD <- VALUE ^ $rB                         
0x.2.f 0x**** 0x****   $rD <- VALUE | $rB                         
0x.3.f 0x**** 0x****   $rD <- VALUE & $rB                         
0x.4.f 0x**** 0x****   $rD <- VALUE + $rB                         
0x.5.f 0x**** 0x****   $rD <- VALUE - $rB                         
0x.6.f 0x**** 0x****   $rD <- VALUE << $rB                        
0x.7.f 0x**** 0x****   $rD <- VALUE >> $rB                        
0x.8.f 0x**** 0x****   $rD <- VALUE >>> $rB                       
0x.9.f 0x**** 0x****   $rD <- VALUE * $rB                         
0x.0f0 0x****          $rD <- short VALUE                         
0x20fe 0x****          $pc <- short VALUE                         
0x30fe 0x****          $tpc <- short VALUE                        
0x.1f. 0x****          $rD <- short VALUE ^ $rA                   
0x.2f. 0x****          $rD <- short VALUE | $rA                   
0x.3f. 0x****          $rD <- short VALUE & $rA                   
0x.4f. 0x****          $rD <- short VALUE + $rA                   
0x.5f. 0x****          $rD <- short VALUE - $rA                   
0x.6f. 0x****          $rD <- short $rA << VALUE                  
0x.7f. 0x****          $rD <- short $rA >> VALUE                  
0x.8f. 0x****          $rD <- short $rA >>> VALUE                 
0x.9f. 0x****          $rD <- short VALUE * $rA                   
0xf00. 0x****          if any $rA == 0 $pc <- $pc + VALUE         
0xf01. 0x****          if any $rA != 0 $pc <- $pc + VALUE         
0xf02. 0x****          if any $rA < 0 $pc <- $pc + VALUE          
0xf03. 0x****          if any $rA >= 0 $pc <- $pc + VALUE         
0xf04. 0x****          if any $rA > 0 $pc <- $pc + VALUE          
0xf05. 0x****          if any $rA <= 0 $pc <- $pc + VALUE         
0xf08. 0x****          if all $rA == 0 $pc <- $pc + VALUE         
0xf09. 0x****          if all $rA != 0 $pc <- $pc + VALUE         
0xf0a. 0x****          if all $rA < 0 $pc <- $pc + VALUE          
0xf0b. 0x****          if all $rA >= 0 $pc <- $pc + VALUE         
0xf0c. 0x****          if all $rA > 0 $pc <- $pc + VALUE          
0xf0d. 0x****          if all $rA <= 0 $pc <- $pc + VALUE         
0xf1.. 0x****          if any $rB == $rA $pc <- $pc + VALUE       
0xf2.. 0x****          if any $rB != $rA $pc <- $pc + VALUE       
0xf3.. 0x****          if any signed $rB < $rA $pc <- $pc + VALUE 
0xf4.. 0x****          if any signed $rB >= $rA $pc <- $pc + VALUE
0xf5.. 0x****          if any $rB < $rA $pc <- $pc + VALUE        
0xf6.. 0x****          if any $rB >= $rA $pc <- $pc + VALUE       
0xf9.. 0x****          if all $rB == $rA $pc <- $pc + VALUE       
0xfa.. 0x****          if all $rB != $rA $pc <- $pc + VALUE       
0xfb.. 0x****          if all signed $rB < $rA $pc <- $pc + VALUE 
0xfc.. 0x****          if all signed $rB >= $rA $pc <- $pc + VALUE
0xfd.. 0x****          if all $rB < $rA $pc <- $pc + VALUE        
0xfe.. 0x****          if all $rB >= $rA $pc <- $pc + VALUE       
0xf0f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xf1f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xf2f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xf3f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xf4f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xf5f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xf6f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xf7f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xf8f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xf9f. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xfaf. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xfbf. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xfcf. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xfdf. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xfef. 0x****          if $rA[C] == 1 $pc <- $pc + VALUE          
0xf0.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xf1.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xf2.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xf3.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xf4.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xf5.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xf6.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xf7.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xf8.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xf9.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xfa.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xfb.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xfc.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xfd.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0xfe.f 0x****          if $rB[C] == 0 $pc <- $pc + VALUE          
0x.c**                 MEM32[$rS + tiny OFFSET] <- $rD            
0x.d**                 $rD <- MEM32[$rS + tiny OFFSET]            
0x.e4.                 $rD <- MEM8[$rA]                           
0x.e5.                 $rD <- MEM16[$rA]                          
0x.e6.                 $rD <- MEM32[$rA]                          
0x.e7.                 $rD <- MEMLL[$rA]                          
0x.e8.                 MEM8[$rA] <- $rD                           
0x.e9.                 MEM16[$rA] <- $rD                          
0x.ea.                 MEM32[$rA] <- $rD                          
0x.eb.                 MEMSC[$rA] <- $rD                          
0x.ec.                 $rD <- SMEM8[$rA]                          
0x.ed.                 $rD <- SMEM16[$rA]                         
0x1ee.                 INV[$rA]                                   
0x2ee.                 $pc <- MEM32[$rA]                          
0x3ee.                 $tpc <- MEM32[$rA]                         
0x.f4. 0x****          $rD <- MEM8[$rA + VALUE]                   
0x.f5. 0x****          $rD <- MEM16[$rA + VALUE]                  
0x.f6. 0x****          $rD <- MEM32[$rA + VALUE]                  
0x.f7. 0x****          $rD <- MEMLL[$rA + VALUE]                  
0x.f8. 0x****          MEM8[$rA + VALUE] <- $rD                   
0x.f9. 0x****          MEM16[$rA + VALUE] <- $rD                  
0x.fa. 0x****          MEM32[$rA + VALUE] <- $rD                  
0x.fb. 0x****          MEMSC[$rA + VALUE] <- $rD                  
0x.fc. 0x****          $rD <- SMEM8[$rA + VALUE]                  
0x.fd. 0x****          $rD <- SMEM16[$rA + VALUE]                 
0x1fe. 0x****          INV[$rA + VALUE]                           
0x2fe. 0x****          $pc <- MEM32[$rA + VALUE]                  
0x3fe. 0x****          $tpc <- MEM32[$rA + VALUE]                 
0x.f4f 0x**** 0x****   $rD <- MEM8[VALUE]                         
0x.f5f 0x**** 0x****   $rD <- MEM16[VALUE]                        
0x.f6f 0x**** 0x****   $rD <- MEM32[VALUE]                        
0x.f7f 0x**** 0x****   $rD <- MEMLL[VALUE]                        
0x.f8f 0x**** 0x****   MEM8[VALUE] <- $rD                         
0x.f9f 0x**** 0x****   MEM16[VALUE] <- $rD                        
0x.faf 0x**** 0x****   MEM32[VALUE] <- $rD                        
0x.fbf 0x**** 0x****   MEMSC[VALUE] <- $rD                        
0x.fcf 0x**** 0x****   $rD <- SMEM8[VALUE]                        
0x.fdf 0x**** 0x****   $rD <- SMEM16[VALUE]                       
0x1fef 0x**** 0x****   INV[VALUE]                                 
0x2fef 0x**** 0x****   $pc <- MEM32[VALUE]                        
0x3fef 0x**** 0x****   $tpc <- MEM32[VALUE]                       
0xff** ...             Type override (<type>)                     
====================   ===========================================   ================
