// -*- Mode: scilab -*- 
// some simple tests for minmax function

//////////////////////////////////////////////////////////////////////
// part1 - minmax on empty matrices (must return [] for any output arg)
//////////////////////////////////////////////////////////////////////
A = [];
[m,M,im,iM] = minmax(A);
if ~m.equal[[]] | ~M.equal[[]] | ~im.equal[[]] | ~iM.equal[[]]  then pause;end

A = zeros(0,4);
[m,M,im,iM] = minmax(A);
rep=zeros(0,1);
if ~m.equal[rep] | ~M.equal[rep] | ~im.equal[rep] | ~iM.equal[rep]  then pause;end

A = zeros(4,0);
rep=zeros(1,0);
[m,M,im,iM] = minmax(A);
if ~m.equal[rep] | ~M.equal[rep] | ~im.equal[rep] | ~iM.equal[rep]  then pause;end

////////////////////////////////////////////////
// part2 - minmax on a non empty matrix
///////////////////////////////////////////////
A = [ 1,   2, -0.7, 1.67;
      2, 4.5,    4, 1e4 ;
     -9, 0.1, -8.1, %pi ];

// min and max for A as a big vector
Am = -9; indm = 3; AM = 1e4; indM = 11;

[m,M,im,iM] = minmax(A);
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end

[m,M,im,iM] = minmax(A,"*");
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end

[m,M,im,iM] = minmax(A,"F");
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end


// min and max row-wise
Am = [-9, 0.1, -8.1, 1.67];
indm = [ 3, 3  ,  3,   1];
AM = [ 2, 4.5,  4, 1e4];
indM = [ 2, 2,    2,   2];

[m,M,im,iM] = minmax(A,1);
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end

[m,M,im,iM] = minmax(A,"r");
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end


// min and max column-wise
Am = [-0.7; 2; -9];
indm = [3   ; 1;  1];
AM = [2; 1e4; %pi];
indM = [2; 4  ; 4];

[m,M,im,iM] = minmax(A,2);
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end

[m,M,im,iM] = minmax(A,"c");
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end


///////////////////////////////////////////////////////
// part3 - minmax on a non empty matrix with Nan values
//////////////////////////////////////////////////////
A = [ %nan, %nan,    2, -0.7,  1.67;
      2,     4.5, %nan,    4,  1e4 ;
     -9,     0.1, -8.1,  %pi, %nan ];

// min and max for A as a big vector
Am = -9; indm = 3; AM = 1e4; indM = 14;

[m,M,im,iM] = minmax(A);
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end

[m,M,im,iM] = minmax(A,"*");
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end

[m,M,im,iM] = minmax(A,"F");
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end


// min and max row-wise
Am =   [-9, 0.1, -8.1, -0.7, 1.67];
indm = [ 3, 3  ,  3,    1,   1];
AM =   [ 2, 4.5,  2,   4, 1e4];
indM = [ 2, 2,    1,   2, 2  ];

[m,M,im,iM] = minmax(A,1);
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end

[m,M,im,iM] = minmax(A,"r");
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end


// min and max column-wise
Am =   [-0.7; 2; -9];
indm = [4   ; 1;  1];
AM =   [2; 1e4; %pi];
indM = [3; 5  ; 4];

[m,M,im,iM] = minmax(A,2);
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end

[m,M,im,iM] = minmax(A,"c");
if ~m.equal[Am] | ~M.equal[AM] | ~im.equal[indm] | ~iM.equal[indM]  then pause;end


///////////////////////////////////////////////////////
// part4 - only nan
//////////////////////////////////////////////////////
A = %nan*zeros(2,3);

[m,M,im,iM] = minmax(A);
if ~isnan(m) | ~isnan(M) | ~im.equal[1] | ~iM.equal[1] then, pause;end

