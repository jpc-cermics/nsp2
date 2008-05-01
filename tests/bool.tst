// -*- Mode: scilab -*- 
//------------------------------------------- 

A=[%t,%t,%f,%f];
B=[%t,%f,%t,%f];

if and(A,B)<> [%t,%f,%f,%f] then pause;end 
if or(A,B)<>  [%t,%t,%t,%f] then pause;end 
if not(A) <>  [%f,%f,%t,%t] then pause;end 

if A & B <> [%t,%f,%f,%f] then pause;end 
if A | B <>  [%t,%t,%t,%f] then pause;end 
if ~A <>  [%f,%f,%t,%t] then pause;end 

if A && B <> [%t,%f,%f,%f] then pause;end 
if A || B <>  [%t,%t,%t,%f] then pause;end 
if ~A <>  [%f,%f,%t,%t] then pause;end 

// test sequentiel or and and 
// 0/0 should not be evaluated 

if %t || 0/0 then x=1;else x=0;end 
if %f && 0/0 then x=1;else x=0;end 

// unary and and or. 

A=[%t,%f,%t,%f; 
   %f,%f,%f,%f; 
   %t,%t,%f,%f];

if or(A,dim=1)<>[%t,%t,%t,%f] then pause;end 
if or(A,dim=2)<>[%t,%f,%t]' then pause;end 
if or(A,dim=0)<>%t then pause;end 

A=[%t,%f,%t,%f; 
   %f,%f,%t,%f; 
   %t,%t,%t,%f];

if and(A,dim=1)<>[%f,%f,%t,%f] then pause;end 
if and(A,dim=2)<>[%f,%f,%f]' then pause;end 
if and(A,dim=0)<>%f then pause;end 

if and(m2b(ones_new(5,8)))<>%t then pause;end 
if or(m2b(zeros_new(5,8)))<>%f then pause;end 


